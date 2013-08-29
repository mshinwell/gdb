#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/mlvalues.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/fail.h>

#include "gdb_ocaml_support.h"

int
gdb_ocaml_support_init(void)
{
  char* argv[2];
  argv[0] = "--";
  argv[1] = NULL;
  caml_startup(argv);
  return 1;
}

void
gdb_ocaml_support_val_print (struct type *type, const gdb_byte *valaddr,
                            int embedded_offset,
                            CORE_ADDR address, struct ui_file *stream,
                            int recurse, const struct value *val,
                            const struct value_print_options *options,
                            int depth)
{
   
}
