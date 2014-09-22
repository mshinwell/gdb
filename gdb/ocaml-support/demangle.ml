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

(* CR mshinwell: transition to using [Core_kernel] *)
open Std

let demangle mangled =
  let str = String.copy mangled in
  let rec loop i j =
    if j >= String.length str then
      i
    else if str.[j] = '_' && j + 1 < String.length str && str.[j + 1] = '_' then (
      (* So, here is the funny part: there's no way to distinguish between "__" inserted
          by [Compilenv.make_symbol] (see asmcomp/compilenv.ml) and names containing
          "__".
          We are just going to assume that people never use "__" in their name (although
          we know for a fact that this happens in Core.) *)
      (* CR mshinwell: fix the above *)
      str.[i] <- '.' ;
      loop (i + 1) (j + 2)
    ) else (
      str.[i] <- str.[j] ;
      loop (i + 1) (j + 1)
    )
  in
  let len = loop 0 0 in
  String.sub str ~pos:0 ~len

(* CR mshinwell: fix the compiler so that [code_begin] and friends gain the
   [__ocaml] prefix. *)
let is_legacy_name = function
  | "code_begin" | "code_end"
  | "data_begin" | "data_end"
  | "frametable" -> true
  | _ -> false

let is_probably_ocaml_name ~mangled_name =
  String.length mangled_name > 4
    && String.is_prefix mangled_name ~prefix:"caml"
    (* Beware: really naive *)
    && mangled_name.[4] <> '_'
    && mangled_name.[4] = (Char.uppercase mangled_name.[4])

let demangle mangled_name =
  Printf.printf "demangle '%s'\n%!" mangled_name;
let demangled =
  if not (is_probably_ocaml_name ~mangled_name) then
    (* CR mshinwell: hmm.  So this function gets called for printing names of
       parameters as well as random symbols.  Maybe the parameter names should have
       "__ocaml" on the front; then, we don't risk confusion in this case (and can
       remove this hack). *)
    let without_stamp = String.drop_stamp mangled_name in
    if without_stamp <> mangled_name then
      Some without_stamp (* just assume it's a parameter; see CR above *)
    else
      Some mangled_name
  else
    let maybe_stamped =
      demangle (String.sub mangled_name ~pos:4 ~len:(String.length mangled_name - 4))
    in
    let after_last_significant_dot =
      match try Some (String.rindex maybe_stamped '.') with Not_found -> None with
      | None -> None
      | Some dot when dot > 0 && maybe_stamped.[dot - 1] = '.' ->
        Some (String.sub maybe_stamped
          ~pos:dot
          ~len:(String.length maybe_stamped - dot))
      | Some dot ->
        Some (String.sub maybe_stamped
          ~pos:(dot + 1)
          ~len:(String.length maybe_stamped - dot - 1))
    in
    match after_last_significant_dot with
    | None -> None
    | Some after_last_significant_dot ->
      if String.is_prefix after_last_significant_dot ~prefix:".ocaml"
         || is_legacy_name after_last_significant_dot
         || (* A module component starting with a number must be a compiler-generated
               symbol (e.g. one for structured data). *)
            (String.length after_last_significant_dot > 0
              && let c = String.get after_last_significant_dot 0 in
                 Char.code c >= Char.code '0' && Char.code c <= Char.code '9') then
        None
      else begin
        let unstamped = String.drop_stamp maybe_stamped in
        let demangled_name =
          if String.is_suffix unstamped ~suffix:".fun" then
            maybe_stamped
          else
            unstamped
        in
        Some demangled_name
      end
in
Printf.printf "demangled name '%s'\n%!" (match demangled with None -> "<none>" | Some n -> n);
demangled

let () = Callback.register "gdb_ocaml_support_demangle" demangle
