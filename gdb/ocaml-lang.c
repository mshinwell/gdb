/* OCaml language support for GDB, the GNU debugger.
   Copyright (C) 2013--2018, Jane Street Group LLC

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
#include "arch-utils.h"
#include "gdbcmd.h"
#include "varobj.h"
#include "objfiles.h"

#include <ctype.h>
#include <stdio.h>
#include <dlfcn.h>

extern const struct exp_descriptor exp_descriptor_c;
extern void inferior_created (struct target_ops *objfile, int from_tty);
extern initialize_file_ftype _initialize_ocaml_language;
extern int readnow_symbol_files;

const char* OCAML_MAIN = "caml_program";

static unsigned int value_printer_max_depth = 1;
static unsigned int value_printer_max_string_length = 30;

struct gdb_ocaml_support {
  void (*val_print) (struct type *type,
                     int embedded_offset, CORE_ADDR address,
                     struct ui_file *stream, int recurse,
                     struct value *val,
                     const struct value_print_options *options, int depth,
                     int max_string_length, int only_print_short_type,
                     int only_print_short_value);
  void (*type_print) (struct type *, const char *, struct ui_file *, int, int,
		      const struct type_print_options *flags);
  int (*parse) (const char* expr, int length);
  CORE_ADDR (*evaluate) (const char* expr, int length,
                         char** type_name_out);
  char* (*demangle) (const char *name,
                     int options);
  void (*set_value_printer_max_depth) (int max_depth);
  void (*set_value_printer_max_string_length) (int max_string_length);
};

#define SET_STUB(stubs,handle,name) \
  stubs->name = dlsym(handle, "monda_" #name)

static void set_value_printer_max_depth (int max_depth);
static void set_value_printer_max_string_length (int max_string_length);

static void
late_initialisation (void)
{
  /* Initialisation requiring libmonda. */

  set_value_printer_max_depth (value_printer_max_depth);
  set_value_printer_max_string_length (value_printer_max_string_length);
}

static void *
initialise_debugger_support_library (const char *libmonda,
				     struct gdb_ocaml_support *stubs)
{
  void *handle = NULL;
  int (*support_init) (void) = NULL;

  handle = dlopen (libmonda, RTLD_LAZY | RTLD_GLOBAL);
  if (handle == NULL)
    {
      fprintf (stderr, "Cannot load OCaml debugger support library %s (%s)\n",
               libmonda, dlerror());
      return NULL;
    }

  support_init = (int (*) ()) dlsym (handle, "monda_init");
  if (support_init == NULL)
    {
      fprintf (stderr, 
               "Cannot use OCaml debugger support library: missing symbol "
               "[monda_init] in %s (%s)\n", 
               libmonda, dlerror());
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

  stubs->val_print =
    (void (*) (struct type *, int, CORE_ADDR,
	       struct ui_file *, int, struct value *,
	       const struct value_print_options *, int, int, int, int))
    dlsym (handle, "monda_val_print");

  stubs->type_print =
    (void (*) (struct type *, const char *, struct ui_file *, int, int,
	       const struct type_print_options *flags))
    dlsym (handle, "monda_type_print");

  stubs->evaluate =
    (CORE_ADDR (*) (const char *, int, char **))
    dlsym (handle, "monda_evaluate");

  stubs->demangle =
    (char * (*) (const char *, int))
    dlsym (handle, "monda_demangle");

  stubs->set_value_printer_max_depth =
    (void (*) (int))
    dlsym (handle, "monda_set_value_printer_max_depth");

  stubs->set_value_printer_max_string_length =
    (void (*) (int))
    dlsym (handle, "monda_set_value_printer_max_string_length");

  stubs->parse =
    (int (*) (const char *, int))
    dlsym (handle, "monda_parse");

  return handle;
}

static int do_not_retry = 0;

static char *
which_libmonda (void)
{
  struct objfile *objfile;
  struct compunit_symtab *cu;
  const char *compiler_version = NULL;
  const char *config_digest = NULL;
  const char *libmonda_prefix = "libmonda-";
  const char *libmonda_suffix = "_gdb.so";
  char *libmonda;
  size_t libmonda_size;

  ALL_COMPUNITS(objfile, cu)
    {
      /* The OCaml compiler will always emit the compiler version and the
	 config digest.  */
      if (cu->ocaml.compiler_version != NULL
	    && cu->ocaml.config_digest != NULL)
	{
	  if (compiler_version == NULL)
	    {
	      compiler_version = cu->ocaml.compiler_version;
	      config_digest = cu->ocaml.config_digest;
	    }
	  else
	    {
	      if (strcmp (compiler_version, cu->ocaml.compiler_version) != 0
		    || strcmp (config_digest, cu->ocaml.config_digest) != 0)
	        {
		  /* As per comment in [debugger_support_library], below.  */
		  return NULL;
		}
	    }
	}
    }

  if (compiler_version == NULL)
    {
      fprintf (stderr, "\nCould not determine OCaml compiler version\n");
      do_not_retry = 1;
      return NULL;
    }

  libmonda_size = strlen (libmonda_prefix) + strlen (compiler_version)
    + 1 + strlen (config_digest) + strlen (libmonda_suffix) + 1;

  libmonda = (char *) xmalloc (libmonda_size);
  if (libmonda == NULL)
    {
      return NULL;
    }

  snprintf (libmonda, libmonda_size, "%s%s_%s%s",
	    libmonda_prefix, compiler_version, config_digest,
	    libmonda_suffix);

  return libmonda;
}

static struct gdb_ocaml_support *
debugger_support_library (void)
{
  static int initialized = 0;
  static void *handle = NULL;
  static struct gdb_ocaml_support stubs;
  int old_initialized;

  if (initialized == 0 && do_not_retry == 0)
    {
      /* We assume that the OCaml compiler prevents linking or loading of
	 modules within the same program that have different incompatible
	 static compiler configuration values (for example Flambda enabled or
	 disabled).  This means in particular that we only ever check the
	 compilation units' [compiler_version] and [config_digest] fields
	 (which are OCaml-specific values transmitted through DWARF) when we
	 first load libmonda.  */

      char *libmonda = which_libmonda ();

      if (libmonda != NULL)
        {
	  handle = initialise_debugger_support_library (libmonda, &stubs);
	  xfree (libmonda);
        }
    }

  if (handle == NULL)
    {
      do_not_retry = 1;
      return NULL;
    }

  old_initialized = initialized;
  initialized = 1;

  if (old_initialized == 0)
    {
      late_initialisation ();
    }

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
set_value_printer_max_string_length (int max_string_length)
{
  struct gdb_ocaml_support *stubs = debugger_support_library ();
  if (stubs && stubs->set_value_printer_max_string_length)
    {
      return (stubs->set_value_printer_max_string_length (max_string_length));
    }
}

static struct gdbarch_data *ocaml_type_data;

struct builtin_ocaml_type
{
  struct type* builtin_value;
};

static const struct builtin_ocaml_type *
builtin_ocaml_type (struct gdbarch *gdbarch)
{
  return (const struct builtin_ocaml_type *)
    gdbarch_data (gdbarch, ocaml_type_data);
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
  /*
  struct gdb_ocaml_support *stubs = debugger_support_library ();
  if (stubs && stubs->val_print)
    {
      return (stubs->demangle (mangled, options));
    }*/
  return xstrdup(mangled);
}

static void
ocaml_val_print (struct type *type,
                 int embedded_offset, CORE_ADDR address,
                 struct ui_file *stream, int recurse,
                 struct value *val,
                 const struct value_print_options *options)
{
  struct gdb_ocaml_support *stubs = debugger_support_library ();

  if (stubs && stubs->val_print)
    {
      stubs->val_print (type, embedded_offset, address, stream,
                        recurse, val, options,
                        value_printer_max_depth,
			value_printer_max_string_length,
			options->ocaml_only_print_short_type,
			options->ocaml_only_print_short_value);
    }
  else
    {
      c_val_print (type, embedded_offset, address, stream, recurse,
                   val, options);
    }
}

static void
ocaml_type_print (struct type *type,
		  const char *varstring,
		  struct ui_file *stream,
		  int show, int level,
		  const struct type_print_options *flags)
{
  struct gdb_ocaml_support *stubs = debugger_support_library ();

  if (stubs && stubs->type_print)
    {
      stubs->type_print (type, varstring, stream, show, level, flags);
    }
  else
    {
      c_print_type (type, varstring, stream, show, level, flags);
    }
}

/* CR mshinwell: should maybe remove some more chars. ( and ) in particular
   might appear---for infix operators---but who knows what removing them from
   here does. */

static const char *word_break_characters =
  " \t\n!@#$%^&*()+=|~`}{[]\"';:?/><,-.";

static const char *
ocaml_word_break_characters (void)
{
  return word_break_characters;
}

static void
ocaml_collect_symbol_completion_matches (completion_tracker &tracker,
					 complete_symbol_mode mode,
					 symbol_name_match_type name_match_type,
					 const char *text, const char *word,
					 enum type_code code)
{
  /* The "." ensures that tab completion works correctly on demanged OCaml
     symbols. */
  return default_collect_symbol_completion_matches_break_on (tracker, mode,
							     name_match_type,
							     text, word, "",
							     code);
}

static int
ocaml_parse (struct parser_state* pstate)
{
  struct stoken stoken;
  struct gdb_ocaml_support *stubs = debugger_support_library ();

  stoken.ptr = lexptr;
  stoken.length = strlen(stoken.ptr);

  if (stubs && stubs->parse)
    {
      /* We need to use the standard parser for things such as the address
         expression when setting a breakpoint (e.g. "*0x123456"). */
      if (!stubs->parse (stoken.ptr, stoken.length))
        {
          write_exp_elt_opcode(pstate, OCAML_EXPRESSION);
          write_exp_string(pstate, stoken);
          write_exp_elt_opcode(pstate, OCAML_EXPRESSION);
          lexptr += stoken.length;
          return 0;
        }
      else
        {
          return c_parse (pstate);
        }
    }

  return c_parse (pstate);
}

static struct value *
evaluate_subexp_ocaml (struct type *expect_type, struct expression *exp,
                       int *pos, enum noside noside)
{
  enum exp_opcode op = exp->elts[*pos].opcode;

  switch (op)
    {
    case OCAML_EXPRESSION:
      {
        const char* ocaml_expr;
        int pc, length;
        struct gdb_ocaml_support *stubs = debugger_support_library ();

        pc = (*pos)++;
        length = longest_to_int (exp->elts[pc + 1].longconst);
        (*pos) += 3 + BYTES_TO_EXP_ELEM (length + 1);
        ocaml_expr = &exp->elts[pc + 2].string;
        if (stubs && stubs->evaluate)
          {
            char* type_name;
            struct type* gdb_type;
            CORE_ADDR result;

            result =
              (CORE_ADDR) (stubs->evaluate(ocaml_expr, length, &type_name));

	    if (result == (CORE_ADDR) -1)
	      {
		error (_("libmonda could not evaluate expression"));
	      }

            gdb_type = arch_integer_type (exp->gdbarch,
                                          sizeof(void*) * 8,
                                          1, /* unsigned */
                                          type_name);
            xfree((void*) type_name);

            return value_from_longest (gdb_type, result);
          }
        break;
      }

    default:
      return evaluate_subexp_standard (expect_type, exp, pos, noside);
    }

  /* CR mshinwell: this may be wrong */
  return NULL;
}

enum ocaml_primitive_types {
  ocaml_primitive_type_value,
  nr_ocaml_primitive_types
};

static void
ocaml_language_arch_info (struct gdbarch* gdbarch,
                          struct language_arch_info* lai)
{
  const struct builtin_ocaml_type *builtin = builtin_ocaml_type (gdbarch);

  lai->primitive_type_vector
    = GDBARCH_OBSTACK_CALLOC (gdbarch, nr_ocaml_primitive_types + 1,
                              struct type *);

  lai->primitive_type_vector [ocaml_primitive_type_value] =
    builtin->builtin_value;
}

/* CR mshinwell: may not be needed again, delete if so */
static void *
build_ocaml_types (struct gdbarch *gdbarch)
{
  struct builtin_ocaml_type *builtin_ocaml_type
    = GDBARCH_OBSTACK_ZALLOC (gdbarch, struct builtin_ocaml_type);

  builtin_ocaml_type->builtin_value
    = arch_integer_type (gdbarch, sizeof(void*) * 8, 0, "value");

  return builtin_ocaml_type;
}

const struct exp_descriptor exp_descriptor_ocaml =
{
  print_subexp_standard,
  operator_length_standard,
  operator_check_standard,
  op_name_standard,
  dump_subexp_body_standard,
  evaluate_subexp_ocaml
};

static const char *ocaml_extensions[] =
{
  ".ml", ".mli", ".mll", ".mlp", ".mly", NULL
};

extern const struct language_defn ocaml_language_defn =
{
  "ocaml",			/* Language name */
  "ocaml",
  language_ocaml,
  range_check_off,
  case_sensitive_on,
  array_row_major,
  macro_expansion_c,
  ocaml_extensions,
  &exp_descriptor_ocaml,
  ocaml_parse,
  null_post_parser,
  c_printchar,			/* Print a character constant */
  c_printstr,			/* Function to print string constant */
  c_emit_char,			/* Print a single char */
  ocaml_type_print,		/* Print a type using appropriate syntax */
  c_print_typedef,		/* Print a typedef using appropriate syntax */
  ocaml_val_print,		/* Print a value using appropriate syntax */
  c_value_print,		/* Print a top-level value */
  default_read_var_value,	/* la_read_var_value */
  NULL,				/* Language specific skip_trampoline */
  NULL,				/* name_of_this */
  true,				/* la_store_sym_names_in_linkage_form_p */
  basic_lookup_symbol_nonlocal,	/* lookup_symbol_nonlocal */
  basic_lookup_transparent_type,/* lookup_transparent_type */
  ocaml_demangle,               /* Language specific symbol demangler */
  NULL,
  NULL,				/* Language specific
				   class_name_from_physname */
  c_op_print_tab,		/* expression operators for printing */
  0,				/* c-style arrays */
  0,				/* String lower bound */
  ocaml_word_break_characters,
  ocaml_collect_symbol_completion_matches,
  ocaml_language_arch_info,
  default_print_array_index,
  default_pass_by_reference,
  c_get_string,
  c_watch_location_expression,
  NULL,				/* la_get_symbol_name_matcher */
  iterate_over_symbols,
  default_search_name_hash,
  &default_varobj_ops,
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

static void
show_value_printer_max_string_length (struct ui_file *file, int from_tty,
				      struct cmd_list_element *c,
				      const char *value)
{
  fprintf_filtered (file, _("The maximum number of characters to be printed "
			    "from OCaml strings is %s.\n"),
		    value);
}

void
_initialize_ocaml_language (void)
{
  ocaml_type_data = gdbarch_data_register_post_init (build_ocaml_types);

  add_setshow_uinteger_cmd ("ocaml-max-depth", no_class,
			    &value_printer_max_depth, _("\
Set the maximum depth to which the OCaml value printer will descend into values."), _("	\
Show the maximum depth to which the OCaml value printer will descend into values."),
                            _(""),
			    NULL,
			    show_value_printer_max_depth,
			    &setprintlist, &showprintlist);

  add_setshow_uinteger_cmd ("ocaml-max-string-length", no_class,
			    &value_printer_max_string_length, _("\
Set the maximum number of characters to be printed from OCaml strings."), _("\
Show the maximum number of characters to be printed from OCaml strings."),
                            _(""),
			    NULL,
			    show_value_printer_max_string_length,
			    &setprintlist, &showprintlist);
}
