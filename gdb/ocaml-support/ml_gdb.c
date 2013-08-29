#include <caml/memory.h>
#include "ml_utils.h"

value ml_gdb_target_read_memory (value core_addr, value buf, value len)
{
  CAMLparam3(core_addr, buf, len);
  CORE_ADDR addr = Target_val (core_addr);

  gdb_byte *ptr = (gdb_byte*) String_val (buf);
  int result = target_read_memory (addr, ptr, Int_val (len));
  CAMLreturn (Val_int (result));
}

value ml_gdb_target_read_field (value core_addr, value offset)
{
  CAMLparam2(core_addr, offset);
  CORE_ADDR addr = Target_val (core_addr);

  CORE_ADDR field;
  gdb_byte *ptr = (gdb_byte*) &field;
  int result = target_read_memory (addr + Int_val (offset) * TARGET_SIZE,
                                   ptr,
                                   TARGET_SIZE);
  CAMLreturn (result == 0 ? Val_target (field) : Val_int (result));
}

value ml_gdb_copy_int32 (value buf)
{
  CAMLparam1(buf);
  CAMLlocal1(ret);
  ret = caml_copy_int32 (*(int32*)String_val (buf));
  CAMLreturn(ret);
}

value ml_gdb_copy_int64 (value buf)
{
  CAMLparam1(buf);
  CAMLlocal1(ret);
  ret = caml_copy_int64 (*(int64*)String_val (buf));
  CAMLreturn(ret);
}

value ml_gdb_copy_double (value buf)
{
  CAMLparam1(buf);
  CAMLlocal1(ret);
  ret = caml_copy_double (*(double*)String_val (buf));
  CAMLreturn(ret);
}

value ml_gdb_print_filtered (value stream, value buf)
{
  CAMLparam2(stream, buf);
  struct ui_file *gdb_stream = Ptr_val (stream);
 
  const char *str = String_val (buf);

  if (str)
    fprintf_filtered(gdb_stream, "%s", str);

  CAMLreturn (Val_unit);
}
