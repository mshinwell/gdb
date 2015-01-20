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
open Debug

module Gdb_struct_value = struct type t end

external run_function_on_target : Gdb_struct_value.t array -> Gdb.Obj.t
  = "gdb_ocaml_support_run_function_on_target"

let counter = ref 0

let compile_and_run_expression ~expr_text:_ ~source_file_path:_ ~vars_in_scope_human_names:_
      ~vars_in_scope_linkage_names:_ ~vars_in_scope_values:_ ~call_site:_ ~out:_ =
  failwith "not currently working"
(* CR mshinwell: fix this and address the CR below at the same time.
  if debug then
    Printf.eprintf "srcfile %s text '%s' human_names %d, values %d\n%!"
      source_file_path expr_text
      (Array.length vars_in_scope_human_names) (Array.length vars_in_scope_values);
  (* CR mshinwell: remove hack once this is optionified *)
  let source_file_path =
    if String.length source_file_path > 0 then Some source_file_path else None
  in
  let cmt_file = cmt_file_of_source_file_path ~source_file_path in
  let open Clflags in
  let open Compenv in
  assert (Array.length vars_in_scope_human_names = Array.length vars_in_scope_values);
  assert
    (Array.length vars_in_scope_human_names = Array.length vars_in_scope_linkage_names);
  let num_vars = Array.length vars_in_scope_human_names in
  (* CR mshinwell: name clash on [debug] *)
  let base_name = Printf.sprintf "gdb_expr%d" !counter in
  let name = Printf.sprintf "/tmp/%s.ml" base_name in
  let source_file_name = Printf.sprintf "/tmp/%s" base_name in
  let output_name = ref (Some (source_file_name ^ ".cmxs")) in
  let source_file = open_out name in
  let var_seq = ref "" in
  let initial_env = ref None in
  for current_var = 0 to Array.length vars_in_scope_human_names - 1 do
    let human_name = vars_in_scope_human_names.(current_var) in
    let symbol_linkage_name = Some vars_in_scope_linkage_names.(current_var) in
    let arg =
      (* CR mshinwell: I think what we should actually be doing is taking the typing
         environment at the point we've stopped at in gdb.  However, since we're only
         dealing with arguments here rather than locals for the moment, then the types
         of such must be resolvable via the typing environment of our call point.  This
         is easy to obtain. *)
      match find_type_and_env ~symbol_linkage_name ~cmt_file ~call_site with
      | None -> human_name
      | Some (type_expr, env) ->
        initial_env := Some env;
        ignore ((Format.flush_str_formatter ()) : string);
        let formatter = Format.str_formatter in
        Printtyp.reset_and_mark_loops type_expr;
        Printtyp.type_expr formatter type_expr;
        Format.pp_print_flush formatter ();
        Printf.sprintf "(%s : %s)" human_name (Format.flush_str_formatter ())
    in
    var_seq := Printf.sprintf "%s %s" !var_seq arg
  done;
  let initial_env = !initial_env in
  let var_seq = !var_seq in
  let get_var_seq =
    let get_var_seq = ref "" in
    for arg_index = 0 to num_vars - 1 do
      get_var_seq := Printf.sprintf "%s (get_var %d)" !get_var_seq arg_index
    done;
    !get_var_seq
  in
  if debug then begin
    Printf.eprintf "calling var_seq: '%s'\n" var_seq;
    Printf.eprintf "calling get_var_seq: '%s'\n" get_var_seq
  end;
  output_string source_file "external set_result : 'a -> unit = \
                               \"caml_natdynlink_gdb_set_result\";;\n";
  output_string source_file "external get_var : int -> 'a = \
                               \"caml_natdynlink_gdb_get_var\";;\n";
  output_string source_file (Printf.sprintf "let %s = let gdb_expr %s = (%s) in gdb_expr %s;;\n" Cmt_file.distinguished_var_name var_seq expr_text get_var_seq);
  output_string source_file
    (Printf.sprintf "let () = set_result %s;;\n" Cmt_file.distinguished_var_name);
  close_out source_file;
  objfiles := [];
  let opref = output_prefix name in
  let ppf = Format.err_formatter in
  try
    Clflags.binary_annotations := true;
    Clflags.dlcode := true;
    Clflags.debug := true;
    Clflags.debug_full := true;
    Optcompile.implementation ?initial_env ppf name opref;
    let dwarf_type =
      Printf.sprintf "__ocaml%s.ml %s" source_file_name Cmt_file.distinguished_var_name
    in
  (*  if our_debug then Printf.eprintf "dwarf type '%s'\n%!" dwarf_type; *)
    objfiles := (opref ^ ".cmx") :: !objfiles;
    Compmisc.init_path true;
    let target = extract_output !output_name in
    Asmlink.link_shared ppf (get_objfiles ()) target;
    Warnings.check_fatal ();
    let result = run_function_on_target vars_in_scope_values in
    if Gdb.Obj.int result >= 0 (* CR mshinwell: not 32-bit safe *) then begin
      incr counter;
      val_print result out ~dwarf_type ~call_site:Call_site.None ~summary:false;
      Gdb.print out "\n"
      (* CR mshinwell: remove temp files *)
    end
  with exn ->
    Opterrors.report_error ppf exn
*)
