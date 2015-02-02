(***********************************************************************)
(*                                                                     *)
(*                 Debugger support library for OCaml                  *)
(*                                                                     *)
(*  Copyright 2015, Jane Street Holding                                *)
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

let is_currying_wrapper =
  match name with
  | None -> false  (* name not found; assume it isn't a wrapper *)
  | Some name ->
    let curry = "caml_curry" in
    let tuplify = "caml_tuplify" in
    (* CR mshinwell: this could maybe be made more precise *)
    String.sub name 0 (String.length curry) = curry
      || String.sub name 0 (String.length tuplify) = tuplify

let examine_custom_block_identifier = function
  | "_bigarray" -> `Bigarray
  | "_mutex" -> `Systhreads_mutex
  | "_condition" -> `Systhreads_condition
  | _ -> `Unknown
