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
#include "valprint.h"

#include <ctype.h>

extern const struct exp_descriptor exp_descriptor_c;

const char* OCAML_MAIN = "caml_program";

const char*
ocaml_main_name (void)
{
  struct minimal_symbol* msym;

  msym = lookup_minimal_symbol (OCAML_MAIN, NULL, NULL);
  if (msym != NULL) {
    return OCAML_MAIN;
}

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
ocaml_val_print (struct type *type, struct symbol *symbol,
                 const gdb_byte *valaddr,
                 int embedded_offset,
                 CORE_ADDR address, struct ui_file *stream, int recurse,
                 const struct value *val,
                 const struct value_print_options *options)
{
  struct gdbarch *gdbarch = get_type_arch(type);

  if (is_ocaml_type (type)) {
    /* CR mshinwell: [symbol] might not be needed any more
       ...although hmm, maybe we could use [symbol] to get the directory that contains
       the cmt file instead of putting it in the DWARF type. */
    ocaml_support_val_print (type, symbol, valaddr, embedded_offset,
                             address, stream, recurse, val, options, 0);
  }
  else if (builtin_ocaml_type (gdbarch)
             && type == builtin_ocaml_type (gdbarch)->builtin_record_field) {
    fprintf(stderr, "printing of a record field\n");
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

static int parsed_longident = 0;

static struct value *
evaluate_subexp_ocaml (struct type *expect_type, struct expression *exp,
                       int *pos, enum noside noside)
{
  enum exp_opcode op = exp->elts[*pos].opcode;
  struct value *arg1 = NULL;
  struct value *arg2 = NULL;
  struct type *type1, *type2;

printf("eval_subexp_ocaml  parsed_longident=%d\n", parsed_longident);fflush(stdout);
  switch (op)
    {
    case STRUCTOP_STRUCT:  /* Record field access */
      {
	int pc = (*pos)++;
	int tem = longest_to_int (exp->elts[pc + 1].longconst);

	(*pos) += 3 + BYTES_TO_EXP_ELEM (tem + 1);
	arg1 = evaluate_subexp (NULL_TYPE, exp, pos, noside);
	type1 = check_typedef (value_type (arg1));

        if (is_ocaml_type (type1))
          {
            char *field_name = &exp->elts[pc + 2].string;
            return value_string (field_name, strlen (field_name),
              builtin_ocaml_type (exp->gdbarch)->builtin_record_field);
          }
/*
	if (noside == EVAL_SKIP)
	  {
	    return value_from_longest (builtin_type (exp->gdbarch)->
				       builtin_int, 1);
	  }
	else if (TYPE_CODE (type1) == TYPE_CODE_ARRAY && TYPE_VECTOR (type1))
	  {
	    return opencl_component_ref (exp, arg1, &exp->elts[pc + 2].string,
					 noside);
	  }
	else
	  {
	    if (noside == EVAL_AVOID_SIDE_EFFECTS)
	      return
		  value_zero (lookup_struct_elt_type
			      (value_type (arg1),&exp->elts[pc + 2].string, 0),
			      lval_memory);
	    else
	      return value_struct_elt (&arg1, NULL,
				       &exp->elts[pc + 2].string, NULL,
				       "structure");
	  }
*/
      }
    default:
      break;
    }

  return evaluate_subexp_c (expect_type, exp, pos, noside);
}

static void
compile_and_run_expression (char *expr_text)
{
  ocaml_support_compile_and_run_expression (expr_text, NULL, NULL, 0, gdb_stderr);
}

static int parsing_call;

static int
is_longident(const char* p)
{
  /* CR mshinwell: use proper grammar.  In fact, go into OCaml and use the
     real one. */
  if (isalpha(*p)) {
    p++;
    while (*p) {
      if (!isalnum(*p) && *p != '_' && *p != '.') {
        return 0;
      }
      p++;
    }
    return 1;
  }
  return 0;
}

static int
ocaml_parse(void)
{
  struct stoken stoken = { lexptr, strlen (lexptr) };

/* to be continued... */
  parsed_longident = 0;
  if (is_longident(lexptr)) {
    char* mangled = ocaml_mangle(lexptr);
    struct stoken token = { mangled, strlen(mangled) };
    write_exp_string(stoken);
    lexptr += strlen(lexptr);
    parsed_longident = 1;
    return 0;
  }

  if (strchr (lexptr, ' ') || strchr (lexptr, '('))
    {
      /* CR mshinwell: gross hack for the moment to detect calls */

printf("ocaml_parse parsing_call=1\n");fflush(stdout);
      write_exp_string (stoken);
      lexptr += strlen (lexptr);
      parsing_call = 1;
      return 0;
    }

  parsing_call = 0;
  return c_parse ();
}

static void
ocaml_operator_length (const struct expression *expr, int endpos,
		       int *oplenp, int *argsp)
{
  if (parsing_call)
    {
      *oplenp = 1;
      *argsp = 0;
      return;
    }

  operator_length_standard (expr, endpos, oplenp, argsp);
}

static struct value *
ocaml_evaluate_exp (struct type *type, struct expression *expr,
                    int *foo, enum noside noside)
{
/*  if (parsed_longident)
    {

    }
  else*/ if (parsing_call)
    {
      int elt;
      long expr_length;  /* not including NULL terminator */
      char *expr_text;
      char *expr_text_ptr;
      long num_left;

      gdb_assert (expr->nelts > 2);
      expr_length = expr->elts[0].longconst;
      expr_text = xmalloc (expr_length + 1);
      expr_text_ptr = expr_text;
      num_left = expr_length;
      for (elt = 1; elt < expr->nelts - 1; elt++)
        {
          int pos;
          for (pos = 0; pos < sizeof (union exp_element) && num_left > 0; pos++)
            {
              *expr_text_ptr++ = (&expr->elts[elt].string)[pos];
              num_left--;
            }
        }
      expr_text[expr_length] = '\0';

      compile_and_run_expression (expr_text);

      /* xfree (expr_text); */

      return value_from_longest (
        builtin_ocaml_type (expr->gdbarch)->builtin_value, 1 /* Val_unit */);
    }

  return evaluate_subexp_standard (type, expr, foo, noside);
}

const struct exp_descriptor exp_descriptor_ocaml =
{
  print_subexp_standard,
  ocaml_operator_length,
  operator_check_standard,
  op_name_standard,
  dump_subexp_body_standard,
  ocaml_evaluate_exp,
};

const struct language_defn ocaml_language_defn =
{
  "ocaml",			/* Language name */
  language_ocaml,
  range_check_off,
  case_sensitive_on,
  array_row_major,
  macro_expansion_c,
  &exp_descriptor_ocaml,
  ocaml_parse,
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

void
_initialize_ocaml_language (void)
{
  ocaml_type_data = gdbarch_data_register_post_init (build_ocaml_types);

  add_language (&ocaml_language_defn);
}
