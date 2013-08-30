/***********************************************************************/
/*                                                                     */
/*                Debugger support library for OCaml                   */
/*                                                                     */
/*  Copyright 2013, Jane Street Holding                                */
/*                                                                     */
/*  Licensed under the Apache License, Version 2.0 (the "License");    */
/*  you may not use this file except in compliance with the License.   */
/*  You may obtain a copy of the License at                            */
/*                                                                     */
/*      http://www.apache.org/licenses/LICENSE-2.0                     */
/*                                                                     */
/*  Unless required by applicable law or agreed to in writing,         */
/*  software distributed under the License is distributed on an        */
/*  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,       */
/*  either express or implied.  See the License for the specific       */
/*  language governing permissions and limitations under the License.  */
/*                                                                     */
/***********************************************************************/

#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/mlvalues.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/fail.h>

#include "gdb_ocaml_support.h"
#include "ml_utils.h"
#include "../symtab.h"

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
                 struct type *type, struct symbol *symbol,
                 const gdb_byte *valaddr,
                 int embedded_offset,
                 CORE_ADDR address, struct ui_file *stream,
                 int recurse, const struct value *val,
                 const struct value_print_options *options,
                 int depth)
{
  CAMLparam0();
  CAMLlocal3(v_symbol, v_symbol_option, v_source_path);
  value v_source_path_option;
  CAMLlocalN(args, 3);
  char* symbol_linkage_name;
  struct symtab* symtab;
  /* CR mshinwell: I think we may need to explicitly take the lock here. */

  /* Extract the linkage name (equivalent of [Ident.unique_name]) from the
     symbol that we're being asked to print, if such a symbol exists. */
  if (symbol) {
    symbol_linkage_name = SYMBOL_LINKAGE_NAME(symbol);
    if (symbol_linkage_name) {
      v_symbol = caml_copy_string(symbol_linkage_name);
      v_symbol_option = caml_alloc_small(1, 0 /* Some */);
      Field(v_symbol_option, 0) = symbol;
    }
    else {
      v_symbol_option = Val_long(0);  /* None */
    }
  }
  else {
    /* No symbol is available. */
    v_symbol_option = Val_long(0);  /* None */
  }

  /* Use the address of [symbol] to determine the source file that defines
     the symbol.  This will be used to load the appropriate .cmt file. */
  symtab = find_pc_symtab(SYMBOL_VALUE_ADDRESS(symbol));
  if (symtab) {
    gdb_assert(symtab->filename != NULL);  /* as per symtab.h */
    if (symtab->dirname) {
      char* temp;
      temp = xmalloc(strlen(symtab->dirname) + strlen(symtab->filename) + 2);
      sprintf(temp, "%s/%s", symtab->dirname, symtab->filename);
      v_source_path = caml_copy_string(temp);
      xfree(temp);
    }
    else {
      v_source_path = caml_copy_string(symtab->filename);
    }
    v_source_path_option = caml_alloc_small(1, 0 /* Some */);
    Field(v_source_path_option, 0) = v_source_path;
  } else {
    v_source_path_option = Val_long(0);  /* None */
  }

  /* The printing code itself is written in OCaml. */
  args[0] = Val_target (*(CORE_ADDR*)valaddr);
  args[1] = Val_ptr (stream);
  args[2] = v_symbol_option;
  args[3] = v_source_path_option;
  (void) caml_callbackN (callback, 4, args);

  CAMLreturn0;
}

void
gdb_ocaml_support_val_print (struct type *type, struct symbol *symbol,
                             const gdb_byte *valaddr,
                             int embedded_offset,
                             CORE_ADDR address, struct ui_file *stream,
                             int recurse, const struct value *val,
                             const struct value_print_options *options,
                             int depth)
{
  static value *callback = NULL;

  if (callback == NULL) {
    callback = caml_named_value ("gdb_ocaml_support_val_print");
  }

  if (callback != NULL) {
    ocaml_val_print (*callback, type, symbol, valaddr, embedded_offset,
                     address, stream, recurse, val, options, depth);
  }
}
