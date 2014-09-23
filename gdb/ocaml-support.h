#ifndef OCAML_SUPPORT_H
#define OCAML_SUPPORT_H

#include "defs.h"
#include "gdb_string.h"
#include "gdbtypes.h"
#include "symtab.h"
#include "expression.h"
#include "language.h"

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
};

extern struct gdb_ocaml_support *ocaml_support_library(void);

extern int ocaml_support_val_print (struct type *type, struct symbol *symbol,
                                    const gdb_byte *valaddr,
                                    int embedded_offset,
                                    CORE_ADDR address, struct ui_file *stream,
                                    int recurse, const struct value *val,
                                    const struct value_print_options *options,
                                    int depth);

extern char* ocaml_support_partially_mangle (const char* name);
extern char* ocaml_support_demangle (const char* mangled, int options);

extern void ocaml_support_print_type (struct type *type,
                                      struct ui_file *stream);

extern void ocaml_support_compile_and_run_expression (const char *expr_text,
                                                      const char **vars_in_scope_names,
                                                      CORE_ADDR *vars_in_scope_values,
                                                      int num_vars_in_scope,
                                                      struct ui_file *stream);

#endif /*!OCAML_SUPPORT_H*/
