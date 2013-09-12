(***********************************************************************)
(*                                                                     *)
(*                 Debugger support library for OCaml                  *)
(*                                                                     *)
(*  Copyright 2013, Jane Street Holding                                *)
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

let strip_parameter_index_from_unique_name unique_name =
  match try Some (String.rindex unique_name '-') with Not_found -> None with
  | None -> unique_name
  | Some dash ->
    match try Some (String.rindex unique_name '_') with Not_found -> None with
    | None -> unique_name
    | Some underscore ->
      if underscore < dash then begin
        let stamp_ok =
          try
            ignore (int_of_string (String.sub unique_name
                (dash + 1) ((String.length unique_name) - (dash + 1))));
            true
          with Failure _ -> false
        in
        if stamp_ok then
          String.sub unique_name 0 dash
        else
          unique_name
      end
      else
        unique_name

let parameter_index_of_unique_name unique_name =
  match try Some (String.rindex unique_name '-') with Not_found -> None with
  | None -> None
  | Some dash ->
    match try Some (String.rindex unique_name '_') with Not_found -> None with
    | None -> None
    | Some underscore ->
      if underscore < dash then
        try
          Some (int_of_string (String.sub unique_name
              (dash + 1) ((String.length unique_name) - (dash + 1))))
        with Failure _ -> None
      else
        None

(* CR mshinwell: we should have proper abstract types for the stamped names *)

let rec type_is_polymorphic type_expr =
  let rec check_type_desc = function
    | Types.Tvar _ -> true
    | Types.Tarrow (_, t1, t2, _) -> type_is_polymorphic t1 || type_is_polymorphic t2
    | Types.Tconstr (_, tys, _)
    | Types.Ttuple tys -> List.exists tys ~f:type_is_polymorphic
    | Types.Tobject _ -> true (* CR mshinwell: fixme *)
    | Types.Tnil -> false
    | Types.Tlink ty
    | Types.Tsubst ty -> type_is_polymorphic ty
    | Types.Tfield _ -> true (* CR mshinwell: fixme *)
    | Types.Tvariant _ -> true (* CR mshinwell: fixme *)
    | Types.Tunivar _ -> false
    | Types.Tpoly _ -> true (* CR mshinwell: fixme *)
    | Types.Tpackage _ -> true (* CR mshinwell: fixme *)
  in
  check_type_desc type_expr.Types.desc

module Call_site = struct
  type t =
  | None
  | Some of string * int
end

(* CR mshinwell: there are THREE functions by the name of [val_print] now *)
let val_print ~depth v out ~symbol_linkage_name ~cmt_file ~call_site =
  let type_of_ident =
    match symbol_linkage_name with
    | None -> None
    | Some symbol_linkage_name ->
      let action =
        let unique_name = strip_parameter_index_from_unique_name symbol_linkage_name in
        match Cmt_file.type_of_ident cmt_file ~unique_name with
        | None -> `Try_call_site_but_fallback_to None
        | Some (type_expr, env) ->
          if type_is_polymorphic type_expr then
            `Try_call_site_but_fallback_to (Some (type_expr, env))
          else
            `Have_type (type_expr, env)
      in
      match action with
      | `Have_type (type_expr, env) -> Some (type_expr, env)
      | `Try_call_site_but_fallback_to fallback ->
        match call_site with
        | Call_site.None -> fallback
        | Call_site.Some (source_file, line_number) ->
          let from_call_site =
            Cmt_file.find_argument_types cmt_file
              ~source_file_of_call_site:source_file
              ~line_number_of_call_site:line_number
          in
          match from_call_site with
          | None -> fallback
          | Some (type_exprs, env) ->
            match parameter_index_of_unique_name symbol_linkage_name with
            | None -> fallback
            | Some parameter_index ->
              if parameter_index < 0 || parameter_index >= List.length type_exprs then
                fallback
              else
                match (Array.of_list type_exprs).(parameter_index) with
                | `Ty type_expr -> Some (type_expr, env)
                | `Recover_label_ty label ->
                  (* CR mshinwell: need to fix this.  Unfortunately [label] is not
                     stamped, which might make it troublesome to find the correct
                     identifier. *)
                  fallback
  in
  Printer.value ~depth ~print_sig:true ~type_of_ident out v

let decode_dwarf_type dwarf_type =
  (* CR mshinwell: use [Core_kernel] and rewrite with sensible string
     functions *)
  let magic = "__ocaml" in
  if String.length dwarf_type <= String.length magic then
    None, None
  else
    let delimiter =
      try Some (String.rindex dwarf_type ' ') with Not_found -> None
    in
    match delimiter with
    | None -> None, None
    | Some delimiter when delimiter <= String.length magic -> None, None
    | Some delimiter ->
      let source_file_path =
        String.sub dwarf_type (String.length magic)
          (delimiter - (String.length magic))
      in
      let symbol_linkage_name =
        String.sub dwarf_type (delimiter + 1)
          ((String.length dwarf_type) - (delimiter + 1))
      in
      Some source_file_path, Some symbol_linkage_name

let cmt_file_of_source_file_path ~source_file_path =
  (* CR mshinwell: This desperately needs to cache the result. *)
  match source_file_path with
  | None -> Cmt_file.create_null ()
  | Some source_file_path ->
    if String.length source_file_path > 3
      && Filename.check_suffix source_file_path ".ml"
    then
      let filename = Filename.chop_extension source_file_path ^ ".cmt" in
      Cmt_file.load ~filename
    else
      Cmt_file.create_null ()

let val_print addr stream ~dwarf_type ~call_site =
  let source_file_path, symbol_linkage_name = decode_dwarf_type dwarf_type in
  let cmt_file = cmt_file_of_source_file_path ~source_file_path in
(*
  begin match call_site with
  | Call_site.None -> Printf.printf "no call point info\n%!"
  | Call_site.Some (file, line) -> Printf.printf "call point: %s:%d\n%!" file line
  end;
*)
  val_print ~depth:0 addr stream ~symbol_linkage_name ~cmt_file ~call_site

let () = Callback.register "gdb_ocaml_support_val_print" val_print

let demangle mangled =
  if
    String.is_suffix mangled ~suffix:"__frametable" ||
    String.is_suffix mangled ~suffix:"__begin" ||
    String.is_suffix mangled ~suffix:"__end"
  then
    mangled
  else
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

let demangle mangled_name =
  let is_caml_name =
    String.length mangled_name > 4 &&
    String.is_prefix mangled_name ~prefix:"caml" &&
    (* Beware: really naive *)
    mangled_name.[4] <> '_' &&
    mangled_name.[4] = (Char.uppercase mangled_name.[4])
  in
  let maybe_stamped =
    if not is_caml_name then mangled_name else
    demangle (String.sub mangled_name ~pos:4 ~len:(String.length mangled_name - 4))
  in
  let unstamped = String.drop_stamp maybe_stamped in
  if String.is_suffix unstamped ~suffix:"anon_fun" then
    maybe_stamped
  else
    unstamped

let () = Callback.register "gdb_ocaml_support_demangle" demangle

let print_type ~dwarf_type ~out =
  let source_file_path, symbol_linkage_name = decode_dwarf_type dwarf_type in
  (* CR mshinwell: we can share some of this code with above. *)
  let status =
    match symbol_linkage_name with
    | None ->
      Printf.printf "print_type: can't find symbol linkage name\n%!";
      `Unknown
    | Some symbol_linkage_name ->
      Printf.printf "print_type: linkage name='%s'\n%!" symbol_linkage_name;
      let cmt_file = cmt_file_of_source_file_path ~source_file_path in
      let type_of_ident =
        Cmt_file.type_of_ident cmt_file ~unique_name:symbol_linkage_name
      in
      match type_of_ident with
      | None ->
        Printf.printf "print_type: '%s' not found in cmt\n%!" symbol_linkage_name;
        `Unknown
      | Some (type_expr, _env) -> `Ok type_expr
  in
  match status with
  | `Unknown -> Gdb.print out "<unknown>"
  | `Ok type_expr ->
    let formatter =
      Format.make_formatter
        (fun str pos len -> Gdb.print out (String.sub str pos len))
        (fun () -> ())
    in
    Printtyp.type_expr formatter type_expr;
    Format.pp_print_flush formatter ()

let () = Callback.register "gdb_ocaml_support_print_type" print_type
