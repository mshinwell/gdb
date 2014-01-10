#if !defined (OCAML_LANG_H)
#define OCAML_LANG_H 1

#include "gdbtypes.h"
#include "symtab.h"
#include "value.h"

extern const char* OCAML_MAIN;

struct builtin_ocaml_type
{
  struct type* builtin_unit;
  struct type* builtin_char;
  struct type* builtin_bool;
  struct type* builtin_int;
  struct type* builtin_float64;
  struct type* builtin_value;
  struct type* builtin_float;
  struct type *builtin_record_field;
};

extern const char *ocaml_main_name (void);
extern const struct builtin_ocaml_type* builtin_ocaml_type (struct gdbarch *gdbarch);
extern char* ocaml_demangle (const char* mangled, int options);

#endif
