/***********************************************************************/
/*                                                                     */
/*                Debugger support library for OCaml                   */
/*                                                                     */
/*  Copyright 2013--2014, Jane Street Holding                          */
/*                                                                     */
/*  Licensed under the Apache License, Version 2.0 (the "License");    */
/*  you may not use this file except in compliance with the License.   */
/*  You may obtain a copy of the License at                            */
/*                                                                     */
/*      http://www.apache.org/licenses/LICENSE-2.0                     */
/*                                                                     */
/*  Unless required by applicable law or agreed to in writing,         */
/*  software distributed under the License is distributed on an        */
/*  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,       */
/*  either express or implied.  See the License for the specific       */
/*  language governing permissions and limitations under the License.  */
/*                                                                     */
/***********************************************************************/

#define CAML_NAME_SPACE 1

#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/mlvalues.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/../asmrun/stack.h>
#define Gdb_hash_retaddr(addr,mask) (((uintnat)(addr) >> 3) & mask)

#include "gdb_ocaml_support.h"
#include "ml_utils.h"

#include "../frame.h"
#include "../infcall.h"
#include "../stack.h"
#include "../symtab.h"
#include "../valprint.h"

#include <string.h>

extern value caml_make_vect (value, value);

static int debug(void)
{
  return ((getenv("GOS_DEBUG")) != NULL);
}

int
gdb_ocaml_support_init (void)
{
  char* argv[2];
  argv[0] = "--";
  argv[1] = NULL;
  caml_startup (argv);
  return 1;
}

/* Attempt to find an accurate location (source file, line number, column number)
   that corresponds to a call point.  The call point is specified by the return
   address as seen by the callee.

   The source file and line number are available from the DWARF info via the symtab.
   The column number is available from the OCaml frame table.
*/
/* CR-someday mshinwell: We could consider having some kind of unique call point
   identifier.  However these would have to be allocated during normal compilation,
   so it isn't clear how we would ensure they are globally unique in any manner more
   robust than using locations. */
static value
find_location_from_return_address(CORE_ADDR return_address)
{
  struct symtab_and_line symtab_and_line;
  int line;
  struct symbol* target_caml_frame_descriptors_symbol;
  struct symbol* target_caml_frame_descriptors_mask_symbol;
  struct symbol* target_caml_init_frame_descriptors_symbol;
  int column = 0;
  CAMLparam0();
  CAMLlocal2(v_call_point, v_call_point_source_file);

  /* Find the address of the [caml_frame_descriptors] hashtable in the target's
     memory.  We also need [caml_frame_descriptors_mask]. */
  target_caml_frame_descriptors_symbol =
    lookup_symbol_global("caml_frame_descriptors", NULL, VAR_DOMAIN);
  target_caml_frame_descriptors_mask_symbol =
    lookup_symbol_global("caml_frame_descriptors_mask", NULL, VAR_DOMAIN);
  if (target_caml_frame_descriptors_symbol
        && target_caml_frame_descriptors_mask_symbol) {
    CORE_ADDR target_caml_frame_descriptors;
    int target_caml_frame_descriptors_mask;

    target_caml_frame_descriptors_mask =
      *(int*) value_contents(value_of_variable(
        target_caml_frame_descriptors_mask_symbol, NULL));

    /* If [target_caml_frame_descriptors_mask] is zero then it's probably the case
       that [caml_init_frame_descriptors] has not been run.  If this appears to be the
       case then run that function on the target.  It should be safe to do this
       anywhere---the function is guarded internally against being run multiple times,
       and if does actually do the main body of work, it shouldn't rely on any state
       external to it anyway (thus, it should be safe to run even if in the middle of
       some random runtime function).
    */
    /* CR-someday mshinwell: we could consider examining caml_frametable[...] directly
       to avoid having to do this.  This could in fact be done without actually having
       to interrogate target memory (just read it from the executable image). */
    /* CR-someday mshinwell: another approach would be to write a runtime function that
       could be called on the target to do this work. */
    if (target_caml_frame_descriptors_mask == 0) {
      if (debug()) {
        fprintf(stderr, "caml_frame_descriptors not initialized\n");
      }
      target_caml_init_frame_descriptors_symbol =
        lookup_symbol_global("caml_init_frame_descriptors", NULL, VAR_DOMAIN);
      if (target_caml_init_frame_descriptors_symbol) {
        struct value* target_caml_init_frame_descriptors;
        target_caml_init_frame_descriptors =
          address_of_variable(target_caml_init_frame_descriptors_symbol, NULL);
        if (target_caml_init_frame_descriptors) {
          if (debug()) {
            fprintf(stderr, "calling caml_init_frame_descriptors on the target\n");
          }
          (void) call_function_by_hand(target_caml_init_frame_descriptors, 0, NULL);
          /* With luck, re-reading the mask will now give the initialized value. */
          target_caml_frame_descriptors_mask =
            *(int*) value_contents(value_of_variable(
              target_caml_frame_descriptors_mask_symbol, NULL));
        }
      }
    }

    if (target_caml_frame_descriptors_mask != 0) {
      target_caml_frame_descriptors =
        *(CORE_ADDR*)
          value_contents(value_of_variable(target_caml_frame_descriptors_symbol, NULL));
      if (debug()) {
        fprintf(stderr, "caml_frame_descriptors=%p\n",
          (void*) target_caml_frame_descriptors);
      }
    }

    if (target_caml_frame_descriptors_mask != 0 && target_caml_frame_descriptors) {
      CORE_ADDR return_address_hash;
      CORE_ADDR frame_descr_ptr;
      int try;
      int found;
      int failed;
      frame_descr descr;
      const int max_tries = 10000;

      return_address_hash =
        Gdb_hash_retaddr(return_address, target_caml_frame_descriptors_mask);

      try = 0;
      found = 0;
      failed = 0;
      while (!found && !failed && try++ < max_tries) {
        CORE_ADDR frame_descr_ptr_addr;

        /* Calculate the address of the hashtable bucket. */
        frame_descr_ptr_addr = (CORE_ADDR)
          (((unsigned char*) target_caml_frame_descriptors)
           + sizeof(frame_descr*)*return_address_hash);
        if (debug()) {
          fprintf(stderr, "fdpa=%p ra=%p rah=%p\n",
            (void*) frame_descr_ptr_addr,
            (void*) return_address,
            (void*) return_address_hash);
        }

        /* Read the pointer to the frame descriptor from the hashtable. */
        if (!target_read_memory(frame_descr_ptr_addr, (gdb_byte*) &frame_descr_ptr,
                                sizeof(frame_descr_ptr))) {
          /* Now read the frame descriptor itself. */
          if (!target_read_memory(frame_descr_ptr, (gdb_byte*) &descr, sizeof(descr))) {
            if (debug()) {
              fprintf(stderr, "examining possible f.descr with ra=%p (want %p)\n",
                      (void*) descr.retaddr, (void*) return_address);
            }
            if (descr.retaddr == return_address) {
              found = 1;
            }
            else {
              return_address_hash =  /* Hash collision; move to the next bucket. */
                (return_address_hash + 1) & target_caml_frame_descriptors_mask;
            }
          }
          else {
            if (debug()) {
              fprintf(stderr, "read of frame descr (from %p) failed\n",
                      (void*) frame_descr_ptr);
            }
            failed = 1;
          }
        }
        else {
          if (debug()) {
            fprintf(stderr, "read of frame descr ptr (from %p) failed\n",
                    (void*) frame_descr_ptr_addr);
          }
          failed = 1;
        }
      }

      if (debug()) {
        fprintf(stderr, "looking for return address %p\n", (void*) return_address);
        fprintf(stderr, "found = %d\n", found);
      }

      if (found && !failed && try < max_tries
            && ((descr.frame_size & 1) == 1) /* ensure -g was used */) {
        /* This follows [extract_location_info] (asmrun/backtrace.c) in the runtime.
           We can't directly use that function since the info words have to be read from
           the target. */
        CORE_ADDR info2ptr;
        uint32 info2;

        info2ptr = (CORE_ADDR) ((((uintnat) frame_descr_ptr +
            sizeof(char*) + sizeof(short) + sizeof(short) +
            sizeof(short) * descr.num_live + sizeof(frame_descr*) - 1)
          & -sizeof(frame_descr*)) + sizeof(uint32) /* <-- this moves us to info2 */);
        if (!target_read_memory(info2ptr, (gdb_byte*) &info2, sizeof(info2))) {
          int line_from_frame_descr;

          /* Extract the line number and the column number from the frame descriptor.
             Either or both of these may be truncated (see below). */
          line_from_frame_descr = info2 >> 12;
          column = (info2 >> 4) & 0xFF;

          /* Extract the source file and line number from the symtab.  (The
             line number in the frame descriptor might be truncated, so we
             prefer the one from the symtab.  Likewise, the frame descriptor
             may not contain a full path.) */
          symtab_and_line = find_pc_line(return_address, 0);
          line = symtab_and_line.line;

          if (debug()) {
            fprintf(stderr, "source file %s st line %d fd line %d fd column %d\n",
                    symtab_and_line.symtab->filename, line, line_from_frame_descr,
                    column);
          }

          /* If [column] is [0xff], it was likely truncated, so we don't
             attempt any lookup since we might have the wrong location.  We
             also check that the portion of the line number that definitely is
             not truncated in the frame descriptor matches the corresponding
             portion of the line number from the symtab.
             (See asmcomp/emitaux.ml:emit_frames for the masks.) */

          if (line > 0 && column < 0xff
                && ((line_from_frame_descr & 0xfffff) == (line & 0xfffff))) {
            char* dirname = symtab_and_line.symtab->dirname;
            char* filename = symtab_and_line.symtab->filename;
            gdb_assert(filename != NULL);  /* cf. symtab.h */
            if (dirname != NULL) {
              v_call_point_source_file =
                caml_alloc_sprintf("%s/%s", dirname, filename);
            } else {
              v_call_point_source_file = caml_copy_string(filename);
            }
            v_call_point = caml_alloc_small(3, 0 /* Call_point.Some */);
            Field(v_call_point, 0) = v_call_point_source_file;
            Field(v_call_point, 1) = Val_long(line);
            Field(v_call_point, 2) = Val_long(column);
            if (debug()) {
              fprintf(stderr, "location found successfully\n");
            }
            CAMLreturn(v_call_point);
          }
        }
      }
    }
  }
  CAMLreturn(Val_long(0) /* Call_point.None */);
}

/* A heuristic to try to find the closest (in terms of stack frames) call point of a
   given function that is not itself in the same function.  The idea is to use any such
   call point to determine the instantiations of type variables when the given function
   is polymorphic.  The heuristic is very rough. */
static value
try_to_find_call_point_of_frame(struct frame_info* selected_frame)
{
  CORE_ADDR return_address;
  CORE_ADDR pc_in_original_selected_frame;
  struct symbol* original_function;
  int depth;
  int stop;

  stop = 0;
  depth = 0;
  return_address = 0;

  pc_in_original_selected_frame = get_frame_pc(selected_frame);
  original_function = find_pc_function(pc_in_original_selected_frame);

  while (!stop && depth++ < 100
           && original_function != NULL && selected_frame != NULL) {
    struct symbol* selected_function;
    CORE_ADDR this_return_address;
    if (frame_unwind_caller_pc_if_available(selected_frame, &this_return_address)) {
      selected_function = find_pc_function(this_return_address);

      if (!selected_function) {
        /* We couldn't find out what function [return_address] lies in.  We'll
           use the most recent return address we could resolve, if any. */
        stop = 1;
      }
      else if (selected_function != original_function) {
        /* [selected_frame] returns to an address that is not in the current
           function; use that return address. */
        return_address = this_return_address;
        stop = 1;
      }
      else {
        /* [selected_frame] returns to the current function, so continue searching
           up the stack. */
        selected_frame = get_prev_frame(selected_frame);
        return_address = this_return_address;
      }
    }
    else {
      /* The return address from [selected_frame] cannot be determined.  We'll use
         the most recent return address we could resolve, if any. */
      stop = 1;
    }
  }

  /* If we've found a return address, determine the name of the call point to which
     it corresponds, so that we can find its type information in the .cmt file.
     Unlike the rest of this function, this part is not a heuristic.
  */
  if (return_address) {
    return find_location_from_return_address(return_address);
  }

  return Val_long(0);
}

static void
ocaml_val_print (value* callback,
                 struct type *type, struct symbol *symbol,
                 const gdb_byte *valaddr,
                 int embedded_offset,
                 CORE_ADDR address, struct ui_file *stream,
                 int recurse, const struct value *val,
                 const struct value_print_options *options,
                 int depth)
{
  CAMLparam0();
  CAMLlocal4(v_type, v_call_point, v_stream, v_target);
  CAMLlocalN(args, 5);
  struct frame_info* selected_frame;
  /* CR mshinwell: I think we may need to explicitly take the lock here. */

#if 0
  /* Extract the linkage name (equivalent of [Ident.unique_name]) from the
     symbol that we're being asked to print, if such a symbol exists. */
  if (symbol) {
/*
    printf("linkage name '%s', natural name '%s'\n",
      SYMBOL_LINKAGE_NAME(symbol), SYMBOL_NATURAL_NAME(symbol));
*/
    symbol_linkage_name = SYMBOL_LINKAGE_NAME(symbol);
    if (symbol_linkage_name) {
      v_symbol = caml_copy_string(symbol_linkage_name);
      v_symbol_option = caml_alloc_small(1, 0 /* Some */);
      Field(v_symbol_option, 0) = v_symbol;
    }
    else {
      v_symbol_option = Val_long(0);  /* None */
    }
  }
  else {
    /* No symbol is available. */
    v_symbol_option = Val_long(0);  /* None */
  }

  /* Determine the source file that defines the symbol.  This will be used
     to load the appropriate .cmt file. */
  symtab = symbol ? SYMBOL_SYMTAB(symbol) : NULL;
  if (symtab) {
    gdb_assert(symtab->filename != NULL);  /* as per symtab.h */
    if (symtab->dirname) {
      char* temp;
      temp = xmalloc(strlen(symtab->dirname) + strlen(symtab->filename) + 2);
      sprintf(temp, "%s/%s", symtab->dirname, symtab->filename);
      v_source_path = caml_copy_string(temp);
      xfree(temp);
    }
    else {
      v_source_path = caml_copy_string(symtab->filename);
    }
    v_source_path_option = caml_alloc_small(1, 0 /* Some */);
    Field(v_source_path_option, 0) = v_source_path;
  } else {
    v_source_path_option = Val_long(0);  /* None */
  }
#endif

  gdb_assert(type != NULL && TYPE_NAME(type) != NULL);  /* enforced in ocaml-lang.c */
  v_type = caml_copy_string(TYPE_NAME(type));

  /* Work out if we're being asked to print the value of a symbol that is a formal
     parameter in the selected stack frame.  If so, then attempt to resolve
     the source location of the frame's return address, in order that we can later
     extract the type at which the function (corresponding to the selected frame) was
     applied.

     If the return address turns out to lie in the same function as we're in at the
     moment, then try looking further up the stack, making for the moment the assumption
     that we don't have polymorphic recursion.
  */
  /* CR mshinwell: consider how to handle polymorphic recursion */
  /* CR mshinwell: think harder about whether the non-uniqueness of stamped idents
     across compilation units means we could go wrong here. */
  v_call_point = Val_long(0);  /* Call_point.None */

  /* CR mshinwell: fix the problem of unavailability of [symbol] during e.g.
     "print arg". */
  if (symbol && SYMBOL_IS_ARGUMENT (symbol)) {
    selected_frame = get_selected_frame_if_set();

    if (selected_frame != NULL) {
      v_call_point = try_to_find_call_point_of_frame(selected_frame);
    }
  }

  v_target = Val_target (*(CORE_ADDR*)valaddr);
  v_stream = Val_ptr(stream);

  /* The printing code itself is written in OCaml. */
  Store_field(args, 0, v_target);
  Store_field(args, 1, v_stream);
  Store_field(args, 2, v_type);
  Store_field(args, 3, v_call_point);
  Store_field(args, 4, Val_bool(options->summary));
  (void) caml_callbackN (*callback, 5, args);

  CAMLreturn0;
}

void
gdb_ocaml_support_val_print (struct type *type, struct symbol *symbol,
                             const gdb_byte *valaddr,
                             int embedded_offset,
                             CORE_ADDR address, struct ui_file *stream,
                             int recurse, const struct value *val,
                             const struct value_print_options *options,
                             int depth)
{
  static value *callback = NULL;

  if (callback == NULL) {
    callback = caml_named_value ("gdb_ocaml_support_val_print");
  }

  if (callback != NULL) {
    ocaml_val_print (callback, type, symbol, valaddr, embedded_offset,
                     address, stream, recurse, val, options, depth);
  }
}

char*
gdb_ocaml_support_partially_mangle (char* name)
{
  static value *cb = NULL;
  CAMLparam0();
  CAMLlocal2 (caml_res, caml_name);

  char* res = NULL;

  if (cb == NULL) {
    cb = caml_named_value ("gdb_ocaml_support_partially_mangle");
  }

  if (cb != NULL) {
    caml_name = caml_copy_string (name);
    caml_res = caml_callback (*cb, caml_name);

    gdb_assert (Is_block(caml_res) && Tag_val(caml_res) == String_tag);
    res = strdup (String_val(caml_res));
  }

  CAMLreturnT (char*, res);
}

char*
gdb_ocaml_support_demangle (char* mangled, int options)
{
  static value *cb = NULL;
  CAMLparam0();
  CAMLlocal2 (caml_res, caml_mangled);

  char* res = NULL;

  if (cb == NULL) {
    cb = caml_named_value ("gdb_ocaml_support_demangle");
  }

  if (cb != NULL) {
    caml_mangled = caml_copy_string (mangled);
    /* CR mshinwell: establish why renaming [cb] to [callback] produces the
       following:
         error: called object ‘caml_callback’ is not a function
    */
    caml_res = caml_callback (*cb, caml_mangled);

    if (Is_block(caml_res)) {
      gdb_assert(Tag_val(caml_res) == 0 && Wosize_val(caml_res) == 1);
      res = strdup (String_val(Field(caml_res, 0)));
    }
  }

  CAMLreturnT (char*, res);
}

void
gdb_ocaml_support_print_type(struct type* type, struct ui_file* stream)
{
  CAMLparam0();
  CAMLlocal1(v_type);
  CAMLlocalN(args, 3);
  static value *callback = NULL;

  gdb_assert(type != NULL && TYPE_NAME(type) != NULL);  /* enforced in ocaml-lang.c */

  if (callback == NULL) {
    callback = caml_named_value ("gdb_ocaml_support_print_type");
  }

  v_type = caml_copy_string(TYPE_NAME(type));

  args[0] = v_type;
  args[1] = Val_ptr(stream);
  (void) caml_callbackN(*callback, 2, args);

  CAMLreturn0;
}

typedef enum {
  ARGUMENTS,
  LOCALS
} arguments_or_locals;

typedef struct {
  int next_index;
  int size;
  struct frame_info* frame;
  const char** human_names;
  const char** linkage_names;
  struct value** values;
  arguments_or_locals stage;
} vars_for_frame;

static void
count_block_args(const char* print_name, struct symbol* sym, void* user_data)
{
  int* num_args = (int*) user_data;
  *num_args = *num_args + 1;
}

static void
read_block_args(const char* incoming_print_name, struct symbol* sym, void* user_data)
{
  const char* print_name;
  const char* linkage_name;
  struct value* val;
  int ok;
  vars_for_frame* vars = (vars_for_frame*) user_data;

  gdb_assert(vars->next_index >= 0 && vars->next_index < vars->size);
  ok = 0;
  if (vars->stage == ARGUMENTS) {
    struct frame_arg arg;
    struct frame_arg entry_arg;
    read_frame_arg(sym, vars->frame, &arg, &entry_arg);
    if (arg.val && arg.sym && !arg.error) {
      /* CR mshinwell: Can we use [sym] instead of [arg.sym]? */
      print_name = SYMBOL_PRINT_NAME(arg.sym);
      linkage_name = SYMBOL_LINKAGE_NAME(arg.sym);
      val = arg.val;
      ok = 1;
    }
    if (arg.error) {
      xfree(arg.error);
    }
    if (entry_arg.error) {
      xfree(entry_arg.error);
    }
  }
  else if (vars->stage == LOCALS) {
    struct value* local;
    local = read_var_value(sym, vars->frame);
    if (local) {
      print_name = SYMBOL_PRINT_NAME(sym);
      linkage_name = SYMBOL_LINKAGE_NAME(sym);
      val = local;
      ok = 1;
    }
  }
  else {
    gdb_assert(0);
  }

  if (ok) {
    /* [val], which is an address in the memory space of gdb, might on the target
       lie in the OCaml heap.  Thus we make sure we can safely encode it as an
       integer. */
    if (((*(value*) value_contents(val)) & 1) == 0) {
      vars->human_names[vars->next_index] = print_name;
      vars->linkage_names[vars->next_index] = linkage_name;
      vars->values[vars->next_index] = val;
      vars->next_index++;
    }
  }
}

/* CR mshinwell: remove unused parameters */
void
gdb_ocaml_support_compile_and_run_expression (const char *expr_text,
                                              const char **vars_in_scope_names,
                                              CORE_ADDR *vars_in_scope_values,
                                              int num_vars_in_scope,
                                              struct ui_file *stream)
{
  CAMLparam0();
  CAMLlocal2(v_expr_text, v_vars_in_scope_human_names);
  CAMLlocal2(v_vars_in_scope_linkage_names, v_vars_in_scope_values);
  CAMLlocal2(v_source_file_path, v_call_point);
  CAMLlocalN(args, 7);
  static value *callback = NULL;
  struct frame_info* current_frame;
  struct symbol* current_function = NULL;
  CORE_ADDR pc;
  int num_args = 0;

  v_call_point = Val_long(0);

  current_frame = get_current_frame();
  if (get_frame_pc_if_available(current_frame, &pc)) {
    current_function = get_frame_function(current_frame);
    if (current_function != NULL) {
      iterate_over_block_arg_vars(SYMBOL_BLOCK_VALUE(current_function),
                                  count_block_args, &num_args);
      /* CR mshinwell: rename things that say "args" to "args_and_locals", now that
         we include locals. */
      iterate_over_block_local_vars(SYMBOL_BLOCK_VALUE(current_function),
                                    count_block_args, &num_args);
    }
  }

  if (num_args > 0) {
    vars_for_frame vars;
    vars.next_index = 0;
    vars.size = num_args;
    vars.frame = current_frame;
    vars.human_names = xmalloc(sizeof(const char*) * num_args);
    vars.linkage_names = xmalloc(sizeof(const char*) * num_args);
    vars.values = xmalloc(sizeof(value*) * num_args);
    vars.stage = ARGUMENTS;
    iterate_over_block_arg_vars(SYMBOL_BLOCK_VALUE(current_function),
                                read_block_args, &vars);
    vars.stage = LOCALS;
    iterate_over_block_local_vars(SYMBOL_BLOCK_VALUE(current_function),
                                  read_block_args, &vars);
    gdb_assert(vars.next_index <= vars.size);
    if (vars.next_index < vars.size) {
      /* For the moment we don't allow access to any arguments or locals if the reading
         of one or more of them failed. */
      num_args = 0;
    }
    else {
      int arg;

      v_call_point = try_to_find_call_point_of_frame(current_frame);

      v_vars_in_scope_human_names = caml_alloc(num_args, 0);
      v_vars_in_scope_linkage_names = caml_alloc(num_args, 0);
      v_vars_in_scope_values = caml_alloc(num_args, 0);
      for (arg = 0; arg < num_args; arg++) {
        Store_field(v_vars_in_scope_human_names, arg,
                    caml_copy_string(vars.human_names[arg]));
        Store_field(v_vars_in_scope_linkage_names, arg,
                    caml_copy_string(vars.linkage_names[arg]));
        /* See comment above re. the lowest bit of [vars.values[arg]]. */
        Store_field(v_vars_in_scope_values, arg, ((value) (vars.values[arg])) | 1);
      }
    }
    xfree(vars.human_names);
    xfree(vars.linkage_names);
    xfree(vars.values);
  }

  if (num_args <= 0) {
    v_vars_in_scope_human_names = caml_alloc(0, 0);
    v_vars_in_scope_linkage_names = caml_alloc(0, 0);
    v_vars_in_scope_values = caml_alloc(0, 0);
  }

  v_expr_text = caml_copy_string(expr_text);
  /* CR mshinwell: consider making this an option */
  v_source_file_path =
    (current_function ? caml_copy_string(current_function->symtab->filename)
      : caml_copy_string(""));

  if (callback == NULL) {
    callback = caml_named_value ("gdb_ocaml_support_compile_and_run_expression");
  }

  Store_field(args, 0, v_expr_text);
  Store_field(args, 1, v_source_file_path);
  Store_field(args, 2, v_vars_in_scope_human_names);
  Store_field(args, 3, v_vars_in_scope_linkage_names);
  Store_field(args, 4, v_vars_in_scope_values);
  Store_field(args, 5, v_call_point);
  Store_field(args, 6, Val_ptr(stream));
  (void) caml_callbackN(*callback, 7, args);

  CAMLreturn0;
}

value
gdb_ocaml_support_run_function_on_target(value v_var_args)
{
  struct value *result;
  struct symbol *veneer;
  struct value *veneer_func;
  struct value** args = NULL;
  struct gdbarch* gdbarch;
  int num_args;
  int arg;

  veneer = lookup_symbol_global("caml_natdynlink_gdb_run", NULL, VAR_DOMAIN);
  if (!veneer) {
    fprintf(stderr, "Failed to find expression evaluation entry point (1)\n");
    return caml_copy_int64 (-1);
  }

  veneer_func = address_of_variable(veneer, NULL);
  if (!veneer_func) {
    fprintf(stderr, "Failed to find expression evaluation entry point (2)\n");
    return caml_copy_int64 (-2);
  }

  num_args = Wosize_val(v_var_args);
  args = xmalloc(sizeof(struct value*) * (1 + num_args));
  gdbarch = get_frame_arch(get_current_frame());
  args[0] = value_from_longest(builtin_type(gdbarch)->builtin_int64, num_args);
  for (arg = 0; arg < num_args; arg++) {
    args[1 + arg] = (struct value*) (Field(v_var_args, arg) & ~((value) 1));
  }
  result = call_function_by_hand (veneer_func, 1 + num_args, args);
  /* CR mshinwell: do we have to free [args[0]]? */
  if (args) {
    xfree(args);
  }

  return caml_copy_int64 (value_as_address (result));
}

static int
compilation_directories_for_source_file_callback(struct symtab* symtab,
                                               void* directories_list_head)
{
  CAMLparam0();
  CAMLlocal1(v_dirname);

  if (symtab->dirname) {
    value v_list_cell;

    v_dirname = caml_copy_string(symtab->dirname);

    v_list_cell = caml_alloc_small(2, 0);
    Field(v_list_cell, 0) = v_dirname;
    Field(v_list_cell, 1) = *(value*) directories_list_head;

    *(value*) directories_list_head = v_list_cell;
  }

  CAMLreturnT(int, 0);  /* always continue the search */
}

value
gdb_ocaml_compilation_directories_for_source_file(value v_file)
{
  CAMLparam0();
  CAMLlocal1(v_directories_list);

  v_directories_list = Val_long(0);
  iterate_over_symtabs(String_val(v_file),
    &compilation_directories_for_source_file_callback,
    &v_directories_list);  /* take the address since we may cause a GC */

  CAMLreturn(v_directories_list);
}

/* These are initialized by way of [_initialize_ocaml_language]. */
static int value_printer_max_depth;
static char* search_path = NULL;

void
gdb_ocaml_support_set_value_printer_max_depth(int new_max_depth)
{
  value_printer_max_depth = new_max_depth;
}

void
gdb_ocaml_support_set_search_path(char *new_search_path)
{
  if (search_path) {
    free(search_path);
  }
  if (new_search_path) {
    search_path = strdup(new_search_path);
  } else {
    search_path = NULL;
  }
}

value
gdb_ocaml_value_printer_max_depth(value v_unit)
{
  return Val_long(value_printer_max_depth);
}

value
gdb_ocaml_search_path(value v_unit)
{
  return caml_copy_string(search_path);
}
