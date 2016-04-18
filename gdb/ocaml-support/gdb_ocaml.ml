(***********************************************************************)
(*                                                                     *)
(*                 Debugger support library for OCaml                  *)
(*                                                                     *)
(*  Copyright 2013--2015, Jane Street Holding                          *)
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

let debug = false

let cmt_file_of_source_file_path =
  let cache = Hashtbl.create 10 in
  fun ~source_file_path -> (* CR mshinwell: rename *)
    match source_file_path with
    | None ->
      if debug then Printf.printf "not looking for cmt (no source path)\n%!";
      Cmt_file.create_null ()
    | Some source_file_path ->
      try
        let cmt = Hashtbl.find cache source_file_path in
        if debug then
          Printf.printf "cmt %s already in the cache\n%!" source_file_path;
        cmt
      with Not_found ->
        let filename = source_file_path ^ ".cmt" in
        if debug then Printf.printf "looking for cmt: %s\n%!" filename;
        let cmt = Cmt_file.load ~filename in
        Hashtbl.add cache source_file_path cmt;
        cmt

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
  | Some of string * int * int  (* filename, line number, column number *)
end

let find_type_and_env ~symbol_linkage_name ~cmt_file ~call_site =
  match symbol_linkage_name with
  | None -> None
  | Some symbol_linkage_name ->
    if Debug.debug then
      Printf.printf "(1) symbol_linkage_name='%s'\n%!" symbol_linkage_name;
    let action =
      let unique_name = strip_parameter_index_from_unique_name symbol_linkage_name in
      if Debug.debug then Printf.printf "(1) unique_name='%s'\n%!" unique_name;
      match Cmt_file.type_of_ident cmt_file ~unique_name with
      | None ->
        if Debug.debug then Printf.printf "`Try_call_site_but_fallback_to None\n%!";
        `Try_call_site_but_fallback_to None
      | Some (type_expr, env) ->
        if type_is_polymorphic type_expr then
          `Try_call_site_but_fallback_to (Some (type_expr, env))
        else
          `Have_type (type_expr, env)
    in
    match action with
    | `Have_type (type_expr, env) -> Some (type_expr, env)
    | `Try_call_site_but_fallback_to fallback -> fallback
(* CR mshinwell: fix this--we need to develop the newer ideas, e.g. numbering call
   points
      match call_site with
      | Call_site.None -> fallback
      | Call_site.Some (source_file, line_number, column_number) ->
        if Debug.debug then
          Printf.printf "call site info: file %s, line %d, column %d\n%!"
            source_file line_number column_number;
        let from_call_site =
          Cmt_file.find_argument_types
            (cmt_file_of_source_file_path (Some source_file))
            ~source_file_of_call_site:source_file
            ~line_number_of_call_site:line_number
            ~column_number_of_call_site:column_number
        in
        match from_call_site with
        | None ->
          if Debug.debug then Printf.printf "find_argument_types failed\n%!";
          fallback
        | Some (type_exprs, env) ->
          if Debug.debug then
            Printf.printf "(2) symbol_linkage_name='%s'\n%!" symbol_linkage_name;
          match parameter_index_of_unique_name symbol_linkage_name with
          | None ->
            if Debug.debug then
              Printf.printf "(symbol linkage name doesn't give \
                the parameter index)\n%!";
            fallback
          | Some parameter_index ->
            if Debug.debug then
              Printf.printf "'%s': parameter index %d: "
                symbol_linkage_name parameter_index;
            if parameter_index < 0 || parameter_index >= List.length type_exprs then
              fallback
            else
              match (Array.of_list type_exprs).(parameter_index) with
              | `Ty type_expr ->
                if Debug.debug then Printf.printf "found type.\n%!";
                Some (type_expr, env)
              | `Recover_label_ty label ->
                (* CR mshinwell: need to fix this.  Unfortunately [label] is not
                   stamped, which might make it troublesome to find the correct
                   identifier. *)
                if Debug.debug then Printf.printf "using fallback.\n%!";
                fallback *)

(* CR mshinwell: there are THREE functions by the name of [val_print] now *)
let val_print ~depth v out ~symbol_linkage_name ~cmt_file ~call_site ~summary =
  let type_of_ident = find_type_and_env ~symbol_linkage_name ~cmt_file ~call_site in
  Printer.value ~depth ~print_sig:true ~type_of_ident ~summary out v

let val_print addr stream ~dwarf_type ~call_site ~summary =
  let source_file_path, symbol_linkage_name = Dwarf_type.decode_dwarf_type dwarf_type in
  let cmt_file = cmt_file_of_source_file_path ~source_file_path in
  if Debug.debug then begin
    match call_site with
    | Call_site.None -> Printf.printf "no call point info\n%!"
    | Call_site.Some (file, line, column) ->
      Printf.printf "call point: %s:%d:%d\n%!" file line column
  end;
  val_print ~depth:0 addr stream ~symbol_linkage_name ~cmt_file ~call_site ~summary

let () = Callback.register "gdb_ocaml_support_val_print" val_print

let print_type ~dwarf_type ~out =
  let source_file_path, symbol_linkage_name = Dwarf_type.decode_dwarf_type dwarf_type in
  (* CR mshinwell: we can share some of this code with above.
     mshinwell: MUST not can *)
  let status =
    match symbol_linkage_name with
    | None ->
      if Debug.debug then
        Printf.printf "print_type: can't find symbol linkage name\n%!";
      `Unknown
    | Some symbol_linkage_name ->
      let symbol_linkage_name =
        strip_parameter_index_from_unique_name symbol_linkage_name
      in
      if Debug.debug then
        Printf.printf "print_type: linkage name='%s'\n%!" symbol_linkage_name;
      let cmt_file = cmt_file_of_source_file_path ~source_file_path in
      let type_of_ident =
        Cmt_file.type_of_ident cmt_file ~unique_name:symbol_linkage_name
      in
      match type_of_ident with
      | None ->
        if Debug.debug then
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
    Printtyp.reset_and_mark_loops type_expr;
    Printtyp.type_expr formatter type_expr;
    Format.pp_print_flush formatter ()

let () = Callback.register "gdb_ocaml_support_print_type" print_type

let () =
  Callback.register "gdb_ocaml_support_compile_and_run_expression"
    Compile_and_run.compile_and_run_expression
