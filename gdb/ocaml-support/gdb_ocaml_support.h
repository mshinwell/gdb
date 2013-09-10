#ifndef GDB_OCAML_SUPPORT
#define GDB_OCAML_SUPPORT

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
#include "target.h"

int gdb_ocaml_support_init (void);

void gdb_ocaml_support_val_print (struct type *type, struct symbol *symbol,
                                  const gdb_byte *valaddr,
                                  int embedded_offset,
                                  CORE_ADDR address, struct ui_file *stream,
                                  int recurse, const struct value *val,
                                  const struct value_print_options *options,
                                  int depth);

void gdb_ocaml_support_print_type(struct type* type, struct ui_file* stream);

char* gdb_ocaml_support_demangle (char* mangled, int options) ;

#endif /*!GDB_OCAML_SUPPORT*/
