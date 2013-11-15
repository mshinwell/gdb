/***********************************************************************/
/*                                                                     */
/*                Debugger support library for OCaml                   */
/*                                                                     */
/*  Copyright 2013, Jane Street Holding                                */
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

#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/mlvalues.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/fail.h>

#include "gdb_ocaml_support.h"
#include "ml_utils.h"
#include "../symtab.h"
#include "../valprint.h"

#include <string.h>

int
gdb_ocaml_support_init (void)
{
  char* argv[2];
  argv[0] = "--";
  argv[1] = NULL;
  caml_startup (argv);
  return 1;
}

static int
ocaml_dwarf_type_name_indicates_parameter(const char* name)
{
  /* Rudimentary check as to whether [name], a DWARF OCaml type name, indicates
     that the corresponding identifier is a function parameter. */

  const char* space;
  const char* underscore;
  const char* dash;

  /* CR mshinwell: needs improvement */

  space = strrchr(name, ' ');
  underscore = strrchr(name, '_');
  dash = strrchr(name, '-');

  if (space < underscore && underscore && dash && underscore < dash) {
    return 1;
  }

  return 0;
}

static void
ocaml_val_print (value callback,
                 struct type *type, struct symbol *symbol,
                 const gdb_byte *valaddr,
                 int embedded_offset,
                 CORE_ADDR address, struct ui_file *stream,
                 int recurse, const struct value *val,
                 const struct value_print_options *options,
                 int depth)
{
  CAMLparam0();
  CAMLlocal3(v_type, v_call_point_source_file, v_call_point);
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

  if (ocaml_dwarf_type_name_indicates_parameter(TYPE_NAME(type))) {
    selected_frame = get_selected_frame_if_set();

    if (selected_frame != NULL) {
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

      /* If we've found a return address, attempt to translate it into a source
         location. */
      if (return_address) {
        int line;
        struct symtab_and_line symtab_and_line = find_pc_line(return_address, 0);
        line = symtab_and_line.line;
        if (line > 0) {
          /* CR mshinwell: what happens if we have more than one source file with
             the same name? */
          /* CR mshinwell: we need the character position as well, really... */
          char* filename = symtab_and_line.symtab->filename;
          gdb_assert(filename != NULL);  /* cf. symtab.h */
          v_call_point_source_file = caml_copy_string(filename);
          v_call_point = caml_alloc_small(2, 0 /* Call_point.Some */);
          Field(v_call_point, 0) = v_call_point_source_file;
          Field(v_call_point, 1) = Val_long(line);
        }
      }
    }
  }

  /* The printing code itself is written in OCaml. */
  args[0] = Val_target (*(CORE_ADDR*)valaddr);
  /* CR mshinwell: make it more explicit that [Val_ptr] allocates */
  args[1] = Val_ptr (stream);
  args[2] = v_type;
  args[3] = v_call_point;
  args[4] = Val_bool(options->summary);
  (void) caml_callbackN (callback, 5, args);

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
    ocaml_val_print (*callback, type, symbol, valaddr, embedded_offset,
                     address, stream, recurse, val, options, depth);
  }
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
