/* OCaml language support for GDB, the GNU debugger.
   Copyright (C) 2013--2015, Jane Street Holding

   Contributed by Mark Shinwell <mshinwell@janestreet.com>

   This file is part of GDB.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

#include "defs.h"
#include "gdb_string.h"
#include "gdbtypes.h"
#include "symtab.h"
#include "expression.h"
#include "parser-defs.h"
#include "symtab.h"
#include "language.h"
#include "c-lang.h"
#include "gdb_assert.h"
#include "ocaml-lang.h"
#include "ocaml-support.h"
#include "target.h"
#include "valprint.h"
#include "breakpoint.h"
#include "observer.h"
#include "arch-utils.h"
#include "gdbcmd.h"

#include <ctype.h>
#include <stdio.h>
#include <dlfcn.h>


extern const struct exp_descriptor exp_descriptor_c;
extern void inferior_created (struct target_ops *objfile, int from_tty);

const char* OCAML_MAIN = "caml_program";

struct gdb_ocaml_support {
  void (*val_print) (struct type *type, struct symbol *symbol,
                     const gdb_byte *valaddr,
                     int embedded_offset,
                     CORE_ADDR address, struct ui_file *stream,
                     int recurse, const struct value *val,
                     const struct value_print_options *options,
                     int depth);
  char* (*partially_mangle) (const char *name);
  char* (*demangle) (const char *name,
                     int options);
  void (*print_type) (struct type *type, struct ui_file *stream);
  void (*compile_and_run_expression) (const char *expr_text,
                                      const char **vars_in_scope_names,
                                      CORE_ADDR *vars_in_scope_values,
                                      int num_vars_in_scope,
                                      struct ui_file *stream);
  void (*set_value_printer_max_depth) (int max_depth);
  void (*set_search_path) (char *search_path);
};

#define LIB_SUPPORT "libocaml_debugger_support.so"

#define SET_STUB(stubs,handle,name) \
  stubs->name = dlsym(handle, "ocaml_debugger_support_" #name)

static void *
initialise_debugger_support_library (struct gdb_ocaml_support *stubs)
{
  void *handle = NULL;
  int (*support_init) (void) = NULL;

  handle = dlopen (LIB_SUPPORT, RTLD_LAZY | RTLD_GLOBAL);
  if (handle == NULL)
    {
      fprintf (stderr, "Cannot load OCaml debugger support library %s (%s)\n",
               LIB_SUPPORT, dlerror());
      return NULL;
    }

  support_init = dlsym (handle, "gdb_ocaml_support_init");
  if (support_init == NULL)
    {
      fprintf (stderr, 
               "Cannot use OCaml debugger support library: missing symbol "
               "ocaml_debugger_support_init in %s (%s)\n", 
               LIB_SUPPORT, dlerror());
      dlclose (handle);
      return NULL;
    }

  if (support_init () == 0)
    {
      fprintf (stderr, "Cannot use OCaml debugger support library: "
               "initialization failed\n");
      dlclose (handle);
      return NULL;
    }

  SET_STUB (stubs, handle, val_print);
  SET_STUB (stubs, handle, demangle);
  SET_STUB (stubs, handle, partially_mangle);
  SET_STUB (stubs, handle, print_type);
  SET_STUB (stubs, handle, compile_and_run_expression);
  SET_STUB (stubs, handle, set_value_printer_max_depth);
  SET_STUB (stubs, handle, set_search_path);

  return handle;
}

struct gdb_ocaml_support *
debugger_support_library (void)
{
  static int initialized = 0;
  static void *handle = NULL;
  static struct gdb_ocaml_support stubs;

  if (initialized == 0)
    {
      handle = initialise_debugger_support_library (&stubs);
      initialized = 1;
    }

  if (handle == NULL)
    return NULL;
  else
    return &stubs;
}

int
ocaml_support_val_print (struct type *type, struct symbol *symbol,
                         const gdb_byte *valaddr,
                         int embedded_offset,
                         CORE_ADDR address, struct ui_file *stream,
                         int recurse, const struct value *val,
                         const struct value_print_options *options,
                         int depth)
{
  return 0;
}

char*
ocaml_support_partially_mangle(const char* name)
{
  struct gdb_ocaml_support *stubs = ocaml_support_library ();
  if (stubs && stubs->val_print)
    {
      return (stubs->partially_mangle (name));
    }
  return NULL;
}

char*
ocaml_support_demangle(const char* mangled, int options)
{
  struct gdb_ocaml_support *stubs = ocaml_support_library ();
  if (stubs && stubs->val_print)
    {
      return (stubs->demangle (mangled, options));
    }
  return NULL;
}

void
ocaml_support_print_type (struct type *type, struct ui_file *stream)
{
  struct gdb_ocaml_support *stubs = ocaml_support_library ();
  if (stubs && stubs->print_type)
    {
      return (stubs->print_type (type, stream));
    }
}

void
ocaml_support_compile_and_run_expression (const char *expr_text,
                                          const char **vars_in_scope_names,
                                          CORE_ADDR *vars_in_scope_values,
                                          int num_vars_in_scope,
                                          struct ui_file *stream)
{
  struct gdb_ocaml_support *stubs = ocaml_support_library ();
  if (stubs && stubs->compile_and_run_expression)
    {
      stubs->compile_and_run_expression (expr_text, vars_in_scope_names,
                                         vars_in_scope_values, num_vars_in_scope,
                                         stream);
    }
}

void
ocaml_support_set_value_printer_max_depth (int max_depth)
{
  struct gdb_ocaml_support *stubs = ocaml_support_library ();
  if (stubs && stubs->set_value_printer_max_depth)
    {
      return (stubs->set_value_printer_max_depth (max_depth));
    }
}

void
ocaml_support_set_search_path (char *search_path)
{
  struct gdb_ocaml_support *stubs = ocaml_support_library ();
  if (stubs && stubs->set_search_path)
    {
      return (stubs->set_search_path (search_path));
    }
}

const char*
ocaml_main_name (void)
{
  struct minimal_symbol* msym;

  msym = lookup_minimal_symbol (OCAML_MAIN, NULL, NULL);
  if (msym != NULL) {
    return OCAML_MAIN;
  }

  return NULL;
}

char*
ocaml_demangle (const char* mangled, int options)
{
  return ocaml_support_demangle (mangled, options);
}

typedef unsigned long long value;
#define Is_block(x)  (((x) & 1) == 0)
#define Tag_val(val) (((unsigned char *) (val)) [-sizeof(value)])
#define Bp_val(v) ((char *) (v))
#define String_val(x) ((char *) Bp_val(x))

#define Lazy_tag 246
#define Closure_tag 247
#define Object_tag 248
#define Infix_tag 249
#define Forward_tag 250
#define No_scan_tag 251
#define Abstract_tag 251
#define String_tag 252
#define Double_tag 253
#define Double_array_tag 254
#define Custom_tag 255

static int
is_ocaml_type (struct type* type)
{
  return (TYPE_NAME (type)
    && strncmp (TYPE_NAME (type), "__ocaml", 7) == 0);
}

static void
ocaml_print_type (struct type *type, const char *varstring, struct ui_file *stream,
                  int show, int level, const struct type_print_options *flags)
{
  if (is_ocaml_type (type)) {
    ocaml_support_print_type(type, stream);
  }
  else {
    c_print_type(type, varstring, stream, show, level, flags);
  }
}

static struct gdbarch_data *ocaml_type_data;

const struct builtin_ocaml_type *
builtin_ocaml_type (struct gdbarch *gdbarch)
{
  return gdbarch_data (gdbarch, ocaml_type_data);
}

static void
ocaml_val_print (struct type *type, const gdb_byte *valaddr,
                 int embedded_offset, CORE_ADDR address,
                 struct ui_file *stream, int recurse,
                 const struct value *val,
                 const struct value_print_options *options)
{
  struct gdb_ocaml_support *stubs = ocaml_support_library ();

  if (stubs && stubs->val_print)
    {
      stubs->val_print (type, valaddr, embedded_offset, address, stream,
                        recurse, val, options, depth);
    }
  else
    {
      c_val_print (type, valaddr, embedded_offset, address, stream, recurse,
                   val, options);
    }

/*
  struct gdbarch *gdbarch = get_type_arch(type);
  if (is_ocaml_type (type)) {
    ocaml_support_val_print (type, valaddr, embedded_offset,
                             address, stream, recurse, val, options, 0);
  }
  else if (builtin_ocaml_type (gdbarch)
             && type == builtin_ocaml_type (gdbarch)->builtin_record_field) {
    fprintf(stderr, "printing of a record field\n");
  }
*/
}

enum ocaml_primitive_types {
  ocaml_primitive_type_int,
  ocaml_primitive_type_float,
  ocaml_primitive_type_char,
  ocaml_primitive_type_bool,
  ocaml_primitive_type_unit,
  ocaml_primitive_type_value,
  ocaml_primitive_type_record_field,
  nr_ocaml_primitive_types
};

static void
ocaml_language_arch_info(struct gdbarch* gdbarch,
                         struct language_arch_info* lai)
{
  const struct builtin_ocaml_type *builtin = builtin_ocaml_type (gdbarch);

  lai->string_char_type = builtin->builtin_char;

  lai->primitive_type_vector
    = GDBARCH_OBSTACK_CALLOC (gdbarch, nr_ocaml_primitive_types + 1, struct type *);

  lai->primitive_type_vector [ocaml_primitive_type_int] = builtin->builtin_int;
  lai->primitive_type_vector [ocaml_primitive_type_float] = builtin->builtin_float;
  lai->primitive_type_vector [ocaml_primitive_type_char] = builtin->builtin_char;
  lai->primitive_type_vector [ocaml_primitive_type_bool] = builtin->builtin_bool;
  lai->primitive_type_vector [ocaml_primitive_type_unit] = builtin->builtin_unit;
  lai->primitive_type_vector [ocaml_primitive_type_value] = builtin->builtin_value;
  lai->primitive_type_vector [ocaml_primitive_type_record_field] =
    builtin->builtin_record_field;

  lai->bool_type_symbol = "bool";
  lai->bool_type_default = builtin->builtin_bool;
}

static char *
ocaml_word_break_characters (void)
{
  /* CR mshinwell: should maybe remove some more chars.  ( and ) in particular might
     appear---for infix operators---but who knows what removing them from here does. */
  return " \t\n!@#$%^&*()+=|~`}{[]\"';:?/><,-.";
}

static VEC (char_ptr) *
ocaml_make_symbol_completion_list (char *text, char *word, enum type_code code)
{
  /* The "." ensures that tab completion works correctly on demanged OCaml symbols. */
  return default_make_symbol_completion_list_break_on (text, word, ".", code);
}

const struct language_defn ocaml_language_defn =
{
  "ocaml",			/* Language name */
  language_ocaml,
  range_check_off,
  case_sensitive_on,
  array_row_major,
  macro_expansion_c,
  &exp_descriptor_c,
  c__parse,
  c_error,
  null_post_parser,
  c_printchar,			/* Print a character constant */
  c_printstr,			/* Function to print string constant */
  c_emit_char,			/* Print a single char */
  ocaml_print_type,		/* Print a type using appropriate syntax */
  c_print_typedef,		/* Print a typedef using appropriate syntax */
  ocaml_val_print,		/* Print a value using appropriate syntax */
  c_value_print,		/* Print a top-level value */
  default_read_var_value,	/* la_read_var_value */
  NULL,				/* Language specific skip_trampoline */
  NULL,				/* name_of_this */
  basic_lookup_symbol_nonlocal,	/* lookup_symbol_nonlocal */
  basic_lookup_transparent_type,/* lookup_transparent_type */
  ocaml_demangle,               /* Language specific symbol demangler */
  NULL,				/* Language specific
				   class_name_from_physname */
  c_op_print_tab,		/* expression operators for printing */
  0,				/* c-style arrays */
  0,				/* String lower bound */
  ocaml_word_break_characters,
  ocaml_make_symbol_completion_list,
  ocaml_language_arch_info,
  default_print_array_index,
  default_pass_by_reference,
  c_get_string,
  NULL,				/* la_get_symbol_name_cmp */
  iterate_over_symbols,
  LANG_MAGIC
};

static void *
build_ocaml_types (struct gdbarch *gdbarch)
{
  struct builtin_ocaml_type *builtin_ocaml_type
    = GDBARCH_OBSTACK_ZALLOC (gdbarch, struct builtin_ocaml_type);

  builtin_ocaml_type->builtin_unit
    = arch_character_type (gdbarch, 63, 1, "unit");
  builtin_ocaml_type->builtin_char
    = arch_character_type (gdbarch, 8, 1, "char");
  builtin_ocaml_type->builtin_bool
    = arch_boolean_type (gdbarch, 8, 0, "bool");
  builtin_ocaml_type->builtin_int
    = arch_integer_type (gdbarch, 63, 0, "int");
  builtin_ocaml_type->builtin_float64
    = arch_float_type (gdbarch, 64, "float", NULL);
  builtin_ocaml_type->builtin_value
    = arch_integer_type (gdbarch, 64, 0, "value");
  builtin_ocaml_type->builtin_record_field
    = arch_character_type (gdbarch, 8, 1, "record_field");

  return builtin_ocaml_type;
}

extern initialize_file_ftype _initialize_ocaml_language;
extern int readnow_symbol_files;

static unsigned int value_printer_max_depth = 10;

static void
show_value_printer_max_depth (struct ui_file *file, int from_tty,
                              struct cmd_list_element *c, const char *value)
{
  fprintf_filtered (file, _("The maximum depth to which the OCaml value printer "
			    "will descend into values is %s.\n"),
		    value);
}

static char *search_path = NULL;

static void
show_search_path (struct ui_file *file, int from_tty,
                  struct cmd_list_element *c, const char *value)
{
  fprintf_filtered (file, _("The search path for loading OCaml "
			    ".cmi and .cmt files is %s.\n"),
		    value);
}

void
_initialize_ocaml_language (void)
{
  struct observer *observer;
  ocaml_type_data;

  initialise_

  type_data = gdbarch_data_register_post_init (build_ocaml_types);

  /* To work around the lack of support for symbol aliases in ELF,
     we force reading of full symtabs at the beginning.  This means that
     setting breakpoints on (e.g.) [B.bar] where b.ml says "let bar = A.foo"
     with [A] a different compilation unit will work. */
  readnow_symbol_files = 1;

  add_language (&ocaml_language_defn);

  observer = observer_attach_inferior_created (inferior_created);

  add_setshow_optional_filename_cmd ("ocaml-search-path", class_support,
				     &search_path, _("\
Set the search path (colon separated) for loading OCaml .cmi and .cmt files."),
				     _("\
Show the search path for loading OCaml .cmi and .cmt files."),
				     _(""),
				     NULL,
				     show_search_path,
				     &setlist, &showlist);

  add_setshow_uinteger_cmd ("ocaml-value-printer-max-depth", no_class,
			    &value_printer_max_depth, _("\
Set the maximum depth to which the OCaml value printer will descend into values."), _("\
Show the maximum depth to which the OCaml value printer will descend into values."),
                            _(""),
			    NULL,
			    show_value_printer_max_depth,
			    &setprintlist, &showprintlist);

  ocaml_support_set_value_printer_max_depth (10);
  ocaml_support_set_search_path (NULL);
}
