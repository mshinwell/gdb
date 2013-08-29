#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/mlvalues.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/fail.h>

#include "gdb_ocaml_support.h"
#include "ml_utils.h"

int
gdb_ocaml_support_init (void)
{
  char* argv[2];
  argv[0] = "--";
  argv[1] = NULL;
  caml_startup (argv);
  return 1;
}

static void
ocaml_val_print (value callback,
                 struct type *type, const gdb_byte *valaddr,
                 int embedded_offset,
                 CORE_ADDR address, struct ui_file *stream,
                 int recurse, const struct value *val,
                 const struct value_print_options *options,
                 int depth)
{
  CAMLparam0();
  CAMLlocalN(args, 2);
  args[0] = Val_target (*(CORE_ADDR*)valaddr);
  args[1] = Val_ptr (stream);

  (void) caml_callbackN (callback, 2, args);

  CAMLreturn0;
}

void
gdb_ocaml_support_val_print (struct type *type, const gdb_byte *valaddr,
                             int embedded_offset,
                             CORE_ADDR address, struct ui_file *stream,
                             int recurse, const struct value *val,
                             const struct value_print_options *options,
                             int depth)
{
  static value *callback = NULL;
  if (callback == NULL)
    callback = caml_named_value ("gdb_ocaml_support_val_print");
  if (callback != NULL)
    ocaml_val_print (*callback, type, valaddr, embedded_offset, address,
                     stream, recurse, val, options, depth);
}
