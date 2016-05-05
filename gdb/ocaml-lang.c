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
#include "gdbtypes.h"
#include "symtab.h"
#include "expression.h"
#include "parser-defs.h"
#include "symtab.h"
#include "language.h"
#include "c-lang.h"
#include "gdb_assert.h"
#include "ocaml-lang.h"
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
extern initialize_file_ftype _initialize_ocaml_language;
extern int readnow_symbol_files;

const char* OCAML_MAIN = "caml_program";

static unsigned int value_printer_max_depth = 10;

struct gdb_ocaml_support {
  void (*val_print) (struct type *type, const gdb_byte *valaddr,
                     int embedded_offset, CORE_ADDR address,
                     struct ui_file *stream, int recurse,
                     const struct value *val,
                     const struct value_print_options *options, int depth);
  char* (*demangle) (const char *name,
                     int options);
  void (*set_value_printer_max_depth) (int max_depth);
  void (*set_search_path) (char *search_path);
};

#define LIB_SUPPORT "libmonda.so"

#define SET_STUB(stubs,handle,name) \
  stubs->name = dlsym(handle, "monda_" #name)

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

  support_init = dlsym (handle, "monda_init");
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
  SET_STUB (stubs, handle, set_value_printer_max_depth);
  SET_STUB (stubs, handle, set_search_path);

  return handle;
}

static struct gdb_ocaml_support *
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

static void
set_value_printer_max_depth (int max_depth)
{
  struct gdb_ocaml_support *stubs = debugger_support_library ();
  if (stubs && stubs->set_value_printer_max_depth)
    {
      return (stubs->set_value_printer_max_depth (max_depth));
    }
}

static void
set_search_path (char *search_path)
{
  struct gdb_ocaml_support *stubs = debugger_support_library ();
  if (stubs && stubs->set_search_path)
    {
      return (stubs->set_search_path (search_path));
    }
}

const char*
ocaml_main_name (void)
{
  struct bound_minimal_symbol bound_minsym;

  bound_minsym = lookup_minimal_symbol (OCAML_MAIN, NULL, NULL);
  if (bound_minsym.minsym != NULL) {
    return OCAML_MAIN;
  }

  return NULL;
}

char*
ocaml_demangle (const char* mangled, int options)
{
  struct gdb_ocaml_support *stubs = debugger_support_library ();
  if (stubs && stubs->val_print)
    {
      return (stubs->demangle (mangled, options));
    }
  return xstrdup(mangled);
}

static void
ocaml_val_print (struct type *type, const gdb_byte *valaddr,
                 int embedded_offset, CORE_ADDR address,
                 struct ui_file *stream, int recurse,
                 const struct value *val,
                 const struct value_print_options *options)
{
  struct gdb_ocaml_support *stubs = debugger_support_library ();

  if (stubs && stubs->val_print)
    {
      stubs->val_print (type, valaddr, embedded_offset, address, stream,
                        recurse, val, options,
                        value_printer_max_depth);
    }
  else
    {
      c_val_print (type, valaddr, embedded_offset, address, stream, recurse,
                   val, options);
    }
}

static void
ocaml_language_arch_info(struct gdbarch* gdbarch,
                         struct language_arch_info* lai)
{
}

static char *
ocaml_word_break_characters (void)
{
  /* CR mshinwell: should maybe remove some more chars.  ( and ) in particular might
     appear---for infix operators---but who knows what removing them from here does. */
  return " \t\n!@#$%^&*()+=|~`}{[]\"';:?/><,-.";
}

static VEC (char_ptr) *
ocaml_make_symbol_completion_list (const char *text, const char *word,
                                   enum type_code code)
{
  /* The "." ensures that tab completion works correctly on demanged OCaml
     symbols. */

  return default_make_symbol_completion_list_break_on (text, word, ".", code);
}

const struct language_defn ocaml_language_defn =
{
  "ocaml",			/* Language name */
  "ocaml",
  language_ocaml,
  range_check_off,
  case_sensitive_on,
  array_row_major,
  macro_expansion_c,
  &exp_descriptor_c,
  c_parse,
  c_error,
  null_post_parser,
  c_printchar,			/* Print a character constant */
  c_printstr,			/* Function to print string constant */
  c_emit_char,			/* Print a single char */
  c_print_type,		        /* Print a type using appropriate syntax */
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
  NULL,
  NULL,
  NULL,
  LANG_MAGIC
};

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
  add_language (&ocaml_language_defn);

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

  set_value_printer_max_depth (10);
  set_search_path (NULL);
}
