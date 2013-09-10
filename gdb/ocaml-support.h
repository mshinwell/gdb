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
  char* (*demangle) (const char *name,
                     int options);
  void (*print_type) (struct type *type, struct ui_file *stream);
};

extern struct gdb_ocaml_support *ocaml_support_library(void);

extern int ocaml_support_val_print (struct type *type, struct symbol *symbol,
                                    const gdb_byte *valaddr,
                                    int embedded_offset,
                                    CORE_ADDR address, struct ui_file *stream,
                                    int recurse, const struct value *val,
                                    const struct value_print_options *options,
                                    int depth);

extern char* ocaml_support_demangle (const char* mangled, int options);

extern void ocaml_support_print_type (struct type *type,
                                      struct ui_file *stream);

#endif /*!OCAML_SUPPORT_H*/
