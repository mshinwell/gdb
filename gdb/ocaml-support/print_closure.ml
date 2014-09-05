(***********************************************************************)
(*                                                                     *)
(*                 Debugger support library for OCaml                  *)
(*                                                                     *)
(*  Copyright 2013--2014, Jane Street Holding                          *)
(*                                                                     *)
(*  Licensed under the Apache License, Version 2.0 (the "License");    *)
(*  you may not use this file except in compliance with the License.   *)
(*  You may obtain a copy of the License at                            *)
(*                                                                     *)
(*      http://www.apache.org/licenses/LICENSE-2.0                     *)
(*                                                                     *)
(*  Unless required by applicable law or agreed to in writing,         *)
(*  software distributed under the License is distributed on an        *)
(*  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,       *)
(*  either express or implied.  See the License for the specific       *)
(*  language governing permissions and limitations under the License.  *)
(*                                                                     *)
(***********************************************************************)

let print ~summary ~formatter ~scrutinee:v =
  try
    if summary then
      Format.fprintf formatter "<fun>"
    else begin
      (* First we try to find out what this function is called.  If it's
         one of the special currying wrappers then we try to look in the
         closure's environment to find the "real" function pointer. *)
      let pc = Gdb.Target.read_field v 0 in
      let name = Gdb.Priv.gdb_function_linkage_name_at_pc pc in
      let is_currying_wrapper =
        match name with
        | None -> false  (* name not found; assume it isn't a wrapper *)
        | Some name ->
          let curry = "caml_curry" in
          let tuplify = "caml_tuplify" in
          (* CR mshinwell: this could maybe be made more precise *)
          String.sub name 0 (String.length curry) = curry
            || String.sub name 0 (String.length tuplify) = tuplify
      in
      let pc =
        if not is_currying_wrapper then
          pc
        else
          (* The "real" function pointer should be the last entry in the
             environment of the closure. *)
          let num_fields = Gdb.Obj.size v in
          Gdb.Target.read_field v (num_fields - 1)
      in
      match Gdb.Priv.gdb_find_pc_line pc ~not_current:false with
      | None -> Format.fprintf formatter "<fun> (0x%Lx)" pc
      | Some {Gdb.Priv. symtab; line = Some line} ->
        Format.fprintf formatter "<fun> (%s:%d)"
          (Gdb.Priv.gdb_symtab_filename symtab) line
      | Some {Gdb.Priv. symtab} ->
        Format.fprintf formatter "<fun> (%s, 0x%Lx)"
          (Gdb.Priv.gdb_symtab_filename symtab)
          pc
    end
  with Gdb.Read_error _ -> Format.fprintf formatter "<closure?>"
