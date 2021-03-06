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

let debug = try Sys.getenv "GOS_DEBUG" <> "" with Not_found -> false

let rec print_path = function
  | Path.Pident ident -> Ident.name ident
  | Path.Pdot (path, s, _) -> (print_path path) ^ "." ^ "<string: " ^ s ^ " >"
  | Path.Papply (path1, path2) ->
    (print_path path1) ^ " applied to " ^ (print_path path2)
