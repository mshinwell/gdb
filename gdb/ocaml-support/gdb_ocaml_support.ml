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

let debug = try Sys.getenv "GOS_DEBUG" <> "" with Not_found -> false

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
let val_print ~depth v out ~symbol_linkage_name ~cmt_file ~call_site ~summary =
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
          if debug then
            Printf.printf "call site info: file %s, line %d\n%!" source_file line_number;
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
              if debug then
                Printf.printf "'%s': parameter index %d: "
                  symbol_linkage_name parameter_index;
              if parameter_index < 0 || parameter_index >= List.length type_exprs then
                fallback
              else
                match (Array.of_list type_exprs).(parameter_index) with
                | `Ty type_expr ->
                  if debug then Printf.printf "found type.\n%!";
                  Some (type_expr, env)
                | `Recover_label_ty label ->
                  (* CR mshinwell: need to fix this.  Unfortunately [label] is not
                     stamped, which might make it troublesome to find the correct
                     identifier. *)
                  if debug then Printf.printf "using fallback.\n%!";
                  fallback
  in
  Printer.value ~depth ~print_sig:true ~type_of_ident ~summary out v

(* CR mshinwell: bad function name *)
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
    | Some delimiter -> begin
      let source_file_path =
        String.sub dwarf_type (String.length magic)
          (delimiter - (String.length magic))
      in
      let symbol_linkage_name =
        String.sub dwarf_type (delimiter + 1)
          ((String.length dwarf_type) - (delimiter + 1))
      in
      if debug then
        Printf.eprintf "source file path '%s' symbol linkage name '%s'\n%!"
          source_file_path symbol_linkage_name;
      Some source_file_path, Some symbol_linkage_name
    end

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

let val_print addr stream ~dwarf_type ~call_site ~summary =
  let source_file_path, symbol_linkage_name = decode_dwarf_type dwarf_type in
  let cmt_file = cmt_file_of_source_file_path ~source_file_path in
  if debug then begin
    match call_site with
    | Call_site.None -> Printf.printf "no call point info\n%!"
    | Call_site.Some (file, line) -> Printf.printf "call point: %s:%d\n%!" file line
  end;
  val_print ~depth:0 addr stream ~symbol_linkage_name ~cmt_file ~call_site ~summary

let () = Callback.register "gdb_ocaml_support_val_print" val_print

let print_type ~dwarf_type ~out =
  let source_file_path, symbol_linkage_name = decode_dwarf_type dwarf_type in
  (* CR mshinwell: we can share some of this code with above.
     mshinwell: MUST not can *)
  let status =
    match symbol_linkage_name with
    | None ->
      if debug then
        Printf.printf "print_type: can't find symbol linkage name\n%!";
      `Unknown
    | Some symbol_linkage_name ->
      if debug then
        Printf.printf "print_type: linkage name='%s'\n%!" symbol_linkage_name;
      let cmt_file = cmt_file_of_source_file_path ~source_file_path in
      let type_of_ident =
        Cmt_file.type_of_ident cmt_file ~unique_name:symbol_linkage_name
      in
      match type_of_ident with
      | None ->
        if debug then
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

external run_function_on_target : unit -> Gdb.Obj.t
  = "gdb_ocaml_support_run_function_on_target"

let counter = ref 0

let compile_and_run_expression ~expr_text ~vars_in_scope_names ~vars_in_scope_values
      ~out =
  let open Clflags in
  let open Compenv in
  assert (Array.length vars_in_scope_names = Array.length vars_in_scope_values);
  (* CR mshinwell: name clash on [debug] *)
  let base_name = Printf.sprintf "gdb_expr%d" !counter in
  let name = Printf.sprintf "/tmp/%s.ml" base_name in
  let source_file_name = Printf.sprintf "/tmp/%s" base_name in
  let output_name = ref (Some (source_file_name ^ ".cmxs")) in
  let source_file = open_out name in
  output_string source_file "external set_result : 'a -> unit = \
                               \"caml_natdynlink_gdb_set_result\";;\n";
  output_string source_file ("let v = (" ^ expr_text ^ ");;");
  output_string source_file "let () = set_result v;;\n";
  close_out source_file;
  objfiles := [];
  let opref = output_prefix name in
  let ppf = Format.err_formatter in
  try
    Clflags.binary_annotations := true;
    Clflags.dlcode := true;
    Clflags.debug := true;
    Clflags.debug_full := true;
    Optcompile.implementation ppf name opref;
    let stamp = 1015
(*
      let fresh_ident = Ident.create "foo" in
      (* CR mshinwell: work out how to do this properly *)
      fresh_ident.Ident.stamp - 8
*)
    in
    let dwarf_type = Printf.sprintf "__ocaml%s.ml v_%d" source_file_name stamp in
  (*  if our_debug then Printf.eprintf "dwarf type '%s'\n%!" dwarf_type; *)
    objfiles := (opref ^ ".cmx") :: !objfiles;
    Compmisc.init_path true;
    let target = extract_output !output_name in
    Asmlink.link_shared ppf (get_objfiles ()) target;
    Warnings.check_fatal ();
    let result = run_function_on_target () in
    incr counter;
    val_print result out ~dwarf_type ~call_site:Call_site.None ~summary:false;
    Gdb.print out "\n"
    (* CR mshinwell: remove temp files *)
  with exn ->
    Opterrors.report_error ppf exn

let () =
  Callback.register "gdb_ocaml_support_compile_and_run_expression"
    compile_and_run_expression
