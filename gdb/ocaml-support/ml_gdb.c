#include <caml/memory.h>
#include "ml_utils.h"

extern char* source_path;

value ml_gdb_find_in_path (value str)
{
    CAMLparam1 (str) ;
    CAMLlocal2 (opt, path_str) ;

    const char *filename = String_val (str) ;

    char *full_path;
    source_full_path_of (filename, &full_path) ;

    if (!full_path) {
        opt = Val_int (0) ;
    } else {
        path_str = caml_copy_string (full_path) ;
        opt = caml_alloc(1, 0);
        Store_field (opt, 0, path_str) ;
    }

    CAMLreturn (opt) ;
}

value ml_gdb_target_read_memory (value core_addr, value buf, value len)
{
  CAMLparam3(core_addr, buf, len);
  CORE_ADDR addr = Target_val (core_addr);

  gdb_byte *ptr = (gdb_byte*) String_val (buf);
  int result = target_read_memory (addr, ptr, Int_val (len));
  CAMLreturn (Val_int (result));
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

value ml_gdb_function_linkage_name_at_pc(value core_addr)
{
  CAMLparam0();
  CAMLlocal1(v_function_name);
  value v_result;
  const char* function_name;
  CORE_ADDR func_start, func_end;

  if (!find_pc_partial_function((CORE_ADDR) Target_val(core_addr),
                                &function_name, &func_start, &func_end)) {
    CAMLreturn(Val_long(0) /* None */);
  }

  v_function_name = caml_copy_string(function_name);
  v_result = caml_alloc_small(1, 0 /* Some */);
  Field(v_result, 0) = v_function_name;

  CAMLreturn(v_result);
}

value ml_gdb_find_pc_line (value core_addr, value not_current)
{
  CAMLparam2 (core_addr, not_current);
  CAMLlocal1 (result);
  CAMLlocal5 (v_sal, v_symtab, v_line, v_addr_pc, v_addr_end);

  CORE_ADDR addr = Target_val (core_addr);
  int notcurrent = Int_val (not_current);

  struct symtab_and_line sal;
  sal = find_pc_line (addr, notcurrent);
  
  if (!sal.symtab)
    CAMLreturn (/*None*/Val_unit);

  v_symtab   = Val_ptr (sal.symtab);
  v_addr_pc  = Val_ptr (sal.pc);
  v_addr_end = Val_ptr (sal.end);

  if (sal.line != 0)
  {
    v_line = caml_alloc (1,0);
    Store_field (v_line, 0, sal.line);
  }
  else
    v_line = Val_unit;

  v_sal = caml_alloc (4,0);
  Store_field (v_sal, 0, v_symtab);
  Store_field (v_sal, 1, v_line);
  Store_field (v_sal, 2, v_addr_pc);
  Store_field (v_sal, 3, v_addr_end);

  result = caml_alloc (1,0);
  Store_field (result, 0, v_sal);

  CAMLreturn (result);
}

value ml_gdb_symtab_filename (value v_symtab)
{
  CAMLparam1 (v_symtab);
  
  struct symtab* symtab = Ptr_val (v_symtab);
  CAMLreturn (caml_copy_string (symtab->filename));
}

