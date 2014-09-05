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

let rec value_looks_like_list value =
  if Gdb.Obj.is_int value && Gdb.Obj.int value = 0 (* nil *) then
    true
  else
    if (not (Gdb.Obj.is_int value))
       && Gdb.Obj.is_block value
       && Gdb.Obj.tag value = 0
       && Gdb.Obj.size value = 2
    then
      value_looks_like_list (Gdb.Obj.field value 1)
    else
      false
