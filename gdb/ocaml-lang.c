/* OCaml language support for GDB, the GNU debugger.
   FIX COPYRIGHT NOTICE.

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

#include <ctype.h>

extern const struct exp_descriptor exp_descriptor_c;

static const char* OCAML_MAIN = "caml_program";

const char*
ocaml_main_name (void)
{
  struct minimal_symbol* msym;

  msym = lookup_minimal_symbol (OCAML_MAIN, NULL, NULL);
  if (msym != NULL)
    return OCAML_MAIN;

  /* No known entry procedure found, the main program is probably not OCaml.  */
  return NULL;
}

static int
is_all_digits_after(char* chr)
{
  while (*++chr) {
    if (!isdigit(*chr)) {
      return 0;
    }
  }
  return 1;
}

char*
ocaml_demangle_cversion(const char* mangled, int options)
{
  char* demangled;
  int index;
  int output_index;
  int mangled_length;
  char* last_underscore;

  demangled = XNEWVEC(char, strlen(mangled) + 1);

  mangled_length = strlen(mangled);

  output_index = 0;
  if (strncmp(mangled, "caml", 4) != 0
        || mangled_length < 5
        || !isupper(mangled[4])
        || strstr(mangled, "constant__symbol__")) {
    strcpy(demangled, mangled);
  }
  else {
    last_underscore = strrchr(mangled, '_');
    if (last_underscore
/*          && (*(last_underscore - 1) == '_')*/
          && (/*!strcmp(last_underscore + 1, "entry")
                ||*/ !strcmp(last_underscore + 1, "frametable")
                || !strcmp(last_underscore + 1, "begin")
                || !strcmp(last_underscore + 1, "end"))) {
      strcpy(demangled, mangled);
    }
    else {
      for (index = 4; index < mangled_length; index++) {
        if (index > 4 &&
              index < mangled_length - 1
              && mangled[index] == '_' && mangled[index + 1] == '_') {
          demangled[output_index] = '.';
          index++;
        }
        else {
          demangled[output_index] = mangled[index];
        }

        output_index++;
      }

      demangled[output_index] = '\0';
    }

    if (!strstr(mangled, "__anonfun_")) {
      char* last_underscore = strrchr(demangled, '_');
      if (last_underscore && is_all_digits_after(last_underscore)) {
        *last_underscore = '\0';
      }
    }
  }

  return demangled;
}

char*
ocaml_demangle (const char* mangled, int options)
{
  char* unmangled = ocaml_support_demangle (mangled, options);

  if (unmangled)
      return unmangled;
  else
      return ocaml_demangle_cversion(mangled, options);
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
ocaml_val_print_ocaml_value (struct type *type, const gdb_byte *valaddr,
                 int embedded_offset,
                 CORE_ADDR address, struct ui_file *stream, int recurse,
                 const struct value *val,
                 const struct value_print_options *options,
                 int depth)
{
  value v = *(value*) valaddr;

  if (depth > 2) {
    fprintf_filtered (stream, "...");
    return;
  }

  if (Is_block(v)) {
    unsigned char header[8];
    unsigned int tag;
    unsigned long long wosize;

    if (target_read_memory (v - 8, header, 8) != 0) {
      fprintf_filtered (stream, "<header read failed>");
      return;
    }

    tag = (unsigned int) (header[0]);
    wosize = *((unsigned long long*) header) >> 10;

    if (tag < Closure_tag) {
      int field;

      if (tag != 0) {
        fprintf_filtered (stream, "tag %d:", tag);
      }

      fprintf_filtered (stream, "(");

      for (field = 0; field < wosize; field++) {
        value v_field;

        if (target_read_memory (v + 8*field, &v_field, 8) != 0) {
          fprintf_filtered (stream, "<field %d read failed>", field);
          return;
        }

        ocaml_val_print_ocaml_value(type, &v_field, embedded_offset,
                                    /* CR mshinwell: these need fixing */
                                    address, stream, recurse, val, options,
                                    depth + 1);
        if (field < wosize - 1) {
          fprintf_filtered (stream, ", ");
        }
      }

      fprintf_filtered (stream, ")");
    }
    else {
      switch (tag) {
        case Lazy_tag:
          fprintf_filtered (stream, "<lazy>");
          break;

        case Closure_tag: {
          struct symbol* func_sym;
          CORE_ADDR code_pointer;
          int field;

          if (target_read_memory (v, &code_pointer, 8) != 0) {
            fprintf_filtered (stream, "<closure, failed to read code ptr>");
          }
          else {
            struct symtab_and_line symtab_and_line;

            symtab_and_line = find_pc_line (code_pointer, 0);
            if (!symtab_and_line.symtab
                  || !symtab_and_line.symtab->filename) {
              fprintf_filtered (stream, "<closure>");
            }
            else if (!symtab_and_line.line) {
              fprintf_filtered (stream, "<closure: %s, unknown line number>",
                                symtab_and_line.symtab->filename);
            }
            else {
              fprintf_filtered (stream, "<%s:%d>",
                                symtab_and_line.symtab->filename,
                                symtab_and_line.line);
            }
          }
#if 0
          fprintf_filtered (stream, "env=(");

          for (field = 1; field < wosize; field++) {
            value v_field;

            if (target_read_memory (v + 8*field, &v_field, 8) != 0) {
              fprintf_filtered (stream, "<field %d read failed>", field);
              return;
            }

            ocaml_val_print_ocaml_value(type, &v_field, embedded_offset,
                                        /* CR mshinwell: these need fixing */
                                        address, stream, recurse, val, options);
            if (field < wosize - 1) {
              fprintf_filtered (stream, ", ");
            }
          }

          fprintf_filtered (stream, ")");
#endif

          break;
        }

        case Object_tag:
          fprintf_filtered (stream, "<object>");
          break;

        case Infix_tag:
          fprintf_filtered (stream, "<infix>");
          break;

        case Forward_tag:
          fprintf_filtered (stream, "<forward>");
          break;

        case Abstract_tag:
          fprintf_filtered (stream, "<abstract>");
          break;

        case String_tag: {
          char* string_data = xmalloc(wosize * 8);
          if (target_read_memory (v, string_data, wosize * 8) != 0) {
            fprintf_filtered (stream, "<string read failed>");
            return;
          }
          /* CR mshinwell: need to watch for intermediate nulls */
          fprintf_filtered (stream, "\"%s\"", string_data);
          break;
        }

        case Double_tag: {
          double fp_val;

          if (target_read_memory (v, &fp_val, 8) != 0) {
            fprintf_filtered (stream, "<double read failed>");
            return;
          }

          fprintf_filtered (stream, "%g", fp_val);
          break;
        }

        case Double_array_tag:
          fprintf_filtered (stream, "<double array>");
          break;

        case Custom_tag:
          fprintf_filtered (stream, "<custom>");
          break;

        default:
          fprintf_filtered (stream, "<v=%p, tag %u>", v, tag);
          break;
      }
    }
  }
  else {
    fprintf_filtered (stream, "%llu", v >> 1);
  }
}

static void
ocaml_val_print (struct type *type, struct symbol *symbol,
                 const gdb_byte *valaddr,
                 int embedded_offset,
                 CORE_ADDR address, struct ui_file *stream, int recurse,
                 const struct value *val,
                 const struct value_print_options *options)
{
  if (is_ocaml_type (type)) {
    if (ocaml_support_val_print (type, symbol, valaddr, embedded_offset,
                                 address, stream, recurse, val, options, 0));
    else
      /* CR mshinwell: deprecate this */
      ocaml_val_print_ocaml_value (type, valaddr, embedded_offset,
                                   address, stream, recurse, val, options, 0);
  }
  else {
    c_val_print (type, symbol, valaddr, embedded_offset,
                 address, stream, recurse, val, options);
  }
}

enum ocaml_primitive_types {
  ocaml_primitive_type_int,
  ocaml_primitive_type_float,
  ocaml_primitive_type_char,
  ocaml_primitive_type_bool,
  ocaml_primitive_type_unit,
  ocaml_primitive_type_value,
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

  lai->bool_type_symbol = "bool";
  lai->bool_type_default = builtin->builtin_bool;
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
  c_parse,
  c_error,
  null_post_parser,
  c_printchar,			/* Print a character constant */
  c_printstr,			/* Function to print string constant */
  c_emit_char,			/* Print a single char */
  c_print_type,			/* Print a type using appropriate syntax */
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
  1,				/* c-style arrays */
  0,				/* String lower bound */
  default_word_break_characters,
  default_make_symbol_completion_list,
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
    = arch_float_type (gdbarch, 64, "value", NULL);

  return builtin_ocaml_type;
}

static struct gdbarch_data *ocaml_type_data;

const struct builtin_ocaml_type *
builtin_ocaml_type (struct gdbarch *gdbarch)
{
  return gdbarch_data (gdbarch, ocaml_type_data);
}

/* Provide a prototype to silence -Wmissing-prototypes.  */
extern initialize_file_ftype _initialize_ocaml_language;

void
_initialize_ocaml_language (void)
{
  ocaml_type_data = gdbarch_data_register_post_init (build_ocaml_types);

  add_language (&ocaml_language_defn);
}
