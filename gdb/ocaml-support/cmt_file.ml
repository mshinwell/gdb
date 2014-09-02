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

open Std

let debug = try Sys.getenv "GOS_DEBUG" <> "" with Not_found -> false

let distinguished_var_name = "camlaverydistinguishedvariableindeed"

module LocTable = Map.Make (struct
  type t = Location.t
  let compare x y =
    let lnum x = x.Location.loc_start.Lexing.pos_lnum in
    let cnum x = x.Location.loc_start.Lexing.pos_cnum in
    let start = compare (lnum x) (lnum y) in
    if start = 0 then compare (cnum x) (cnum y) else start
end)

type t = {
  cmi_infos : Cmi_format.cmi_infos option;
  cmt_infos : Cmt_format.cmt_infos option;
  (* CR mshinwell: we can almost certainly do better than a map from
      every identifier (at least in common cases). *)
  (* CR trefis for mshinwell: in ocp-index, they use a trie from names to
      locations, you might want to do the same (but for types instead of
      positions, ofc) here. *)
  idents_to_types : (Types.type_expr * Env.t) String.Map.t ;
  application_points :
    ([ `Recover_label_ty of string | `Ty of Types.type_expr ] list * Env.t) LocTable.t
}

(* CR mshinwell: use the new cache *)
let cache : (string, t) Hashtbl.t = Hashtbl.create 1

let create_null () = {
  cmi_infos = None;
  cmt_infos = None;
  idents_to_types    = String.Map.empty;
  application_points = LocTable.empty;
}

let rec process_pattern ~pat ~idents_to_types =
  match pat.Typedtree.pat_desc with
  | Typedtree.Tpat_var (ident, _loc) ->
    String.Map.add (Ident.unique_name ident)
      (pat.Typedtree.pat_type, pat.Typedtree.pat_env)
      idents_to_types
  | Typedtree.Tpat_alias (pat, ident, _loc) ->
    let idents_to_types =
      String.Map.add (Ident.unique_name ident)
        (pat.Typedtree.pat_type, pat.Typedtree.pat_env)
        idents_to_types
    in
    process_pattern ~pat ~idents_to_types
  | Typedtree.Tpat_tuple pats
  | Typedtree.Tpat_construct (_, _, pats)
  | Typedtree.Tpat_array pats ->
    List.fold_left pats
      ~init:idents_to_types
      ~f:(fun idents_to_types pat ->
            process_pattern ~pat ~idents_to_types)
  | Typedtree.Tpat_variant (_label, pat_opt, _row_desc) ->
    begin match pat_opt with
    | None -> idents_to_types
    | Some pat -> process_pattern ~pat ~idents_to_types
    end
  | Typedtree.Tpat_record (loc_desc_pat_list, _closed) ->
    List.fold_left loc_desc_pat_list
      ~init:idents_to_types
      ~f:(fun idents_to_types (_loc, _desc, pat) ->
            process_pattern ~pat ~idents_to_types)
  | Typedtree.Tpat_or (pat1, pat2, _row_desc) ->
    process_pattern ~pat:pat1
      ~idents_to_types:(process_pattern ~pat:pat2 ~idents_to_types)
  | Typedtree.Tpat_lazy pat ->
    process_pattern ~pat ~idents_to_types
  | Typedtree.Tpat_any
  | Typedtree.Tpat_constant _ -> idents_to_types


and process_expression ~exp ((idents_to_types, app_points) as init) =
  let open Typedtree in
  match exp.exp_desc with
  | Texp_let (_rec, value_binding, exp) ->
    let acc = process_value_binding ~value_binding init in
    process_expression ~exp acc
  | Texp_function (_label, cases, _partial) ->
    process_cases ~cases init
  | Texp_apply (exp, args) ->
    let app_points =
      let lst =
        List.map args ~f:(fun (label, expr_opt, _optional) ->
          match expr_opt with
          | None -> `Recover_label_ty label
          | Some e -> `Ty e.exp_type
        )
      in
      LocTable.add (exp.exp_loc) (lst, exp.exp_env) app_points
    in
    let init = process_expression ~exp (idents_to_types, app_points) in
    List.fold_left args ~init ~f:(fun acc (_label, expr_opt, _optional) ->
      match expr_opt with
      | None -> acc
      | Some exp -> process_expression ~exp acc
    )
  | Texp_match (exp, cases1, cases2, _) ->
    let acc = process_expression ~exp init in
    process_cases ~cases:cases2 (process_cases ~cases:cases1 acc)
  | Texp_try (exp, cases) ->
    let acc = process_expression ~exp init in
    process_cases ~cases acc
  | Texp_construct (_, _, expr_list)
    (* Texp_co... (loc, descr, list, "explicit arity":bool)
      * Note: that last bool disappeared on trunk. *)
  | Texp_array expr_list
  | Texp_tuple expr_list ->
    List.fold_left expr_list ~init ~f:(fun acc exp -> process_expression ~exp acc)
  | Texp_variant (_, Some exp) -> process_expression ~exp init
  | Texp_record (expr_list, _) ->
    List.fold_left expr_list ~init ~f:(fun acc (_loc, _desc, exp) ->
      process_expression ~exp acc
    )
  | Texp_ifthenelse (e1, e2, e_opt) ->
    let acc = process_expression ~exp:e1 init in
    let acc = process_expression ~exp:e2 acc in
    begin match e_opt with
    | None -> acc
    | Some exp -> process_expression ~exp acc
    end
  | Texp_sequence (e1, e2)
  | Texp_while (e1, e2) ->
    let acc = process_expression ~exp:e1 init in
    process_expression ~exp:e2 acc
  | Texp_for (ident, _, e1, e2, _, e3) ->
    let idents_to_types =
      String.Map.add (Ident.unique_name ident) (e1.exp_type, e1.exp_env)
        idents_to_types
    in
    let acc = process_expression ~exp:e1 (idents_to_types, app_points) in
    let acc = process_expression ~exp:e2 acc in
    process_expression ~exp:e3 acc
  | Texp_lazy exp
  | Texp_assert exp
  | Texp_field (exp, _, _) ->
    process_expression ~exp init
  | Texp_setfield (e1, _, _, e2) ->
    let acc = process_expression ~exp:e1 init in
    process_expression ~exp:e2 acc
  | Texp_send (exp, _meth, e_opt) ->
    (* TODO: handle methods *)
    let acc = process_expression ~exp init in
    begin match e_opt with
    | None -> acc
    | Some exp -> process_expression ~exp acc
    end
  | Texp_letmodule (ident, str_loc, mod_expr, exp) ->
    (* TODO: handle [mod_expr] *)
(*
    let idents_to_types =
      String.Map.add (Ident.unique_name ident)
        (mod_expr.mod_type, mod_expr.mod_env) idents_to_types
    in
*)
    process_expression ~exp init
  (* CR mshinwell: this needs finishing, yuck *)
  | Texp_ident _
  | Texp_constant _
  | Texp_variant _
  | Texp_new _
  | Texp_instvar _
  | Texp_setinstvar _
  | Texp_override _
  | Texp_object _
  | Texp_pack _ -> idents_to_types, app_points

and process_cases ~cases init =
  List.fold_left cases ~init ~f:(fun acc case ->
    let pat = case.Typedtree.c_lhs in
    let exp = case.Typedtree.c_rhs in
    let idents_to_types, app_points = process_expression ~exp acc in
    process_pattern ~pat ~idents_to_types, app_points
  )

and process_value_binding ~value_binding init =
  List.fold_left value_binding ~init ~f:(fun acc value_binding ->
    let pat = value_binding.Typedtree.vb_pat in
    let exp = value_binding.Typedtree.vb_expr in
    let idents_to_types, app_points = process_expression ~exp acc in
    process_pattern ~pat ~idents_to_types, app_points
  )

let rec process_module_expr ~mod_expr ((idents_to_types, app_points) as maps) =
  let open Typedtree in
  match mod_expr.mod_desc with
  | Tmod_ident _ -> maps
  | Tmod_structure structure ->
    process_implementation ~structure ~idents_to_types ~app_points
  | Tmod_constraint (mod_expr, _, _, _)
  | Tmod_functor (_, _, _, mod_expr) ->
    process_module_expr ~mod_expr maps
  | Tmod_apply (me1, me2, _) ->
    let maps = process_module_expr ~mod_expr:me1 maps in
    process_module_expr ~mod_expr:me2 maps
  | Tmod_unpack (_expr, _) -> (* TODO *) maps

and process_implementation ~structure ~idents_to_types ~app_points =
  List.fold_left structure.Typedtree.str_items
    ~init:(idents_to_types, app_points)
    ~f:(fun maps str_item ->
          match str_item.Typedtree.str_desc with
          | Typedtree.Tstr_value (_rec, value_binding) ->
            process_value_binding ~value_binding maps
          | Typedtree.Tstr_eval (exp, _) ->
            process_expression ~exp maps
          | Typedtree.Tstr_module module_binding ->
            process_module_expr ~mod_expr:module_binding.Typedtree.mb_expr maps
          | Typedtree.Tstr_recmodule lst ->
            List.fold_left lst ~init:(idents_to_types, app_points) ~f:(
              fun maps module_binding ->
                process_module_expr ~mod_expr:module_binding.Typedtree.mb_expr maps
            )
          | Typedtree.Tstr_primitive _
          | Typedtree.Tstr_type _
          | Typedtree.Tstr_exception _
          | Typedtree.Tstr_modtype _
          | Typedtree.Tstr_open _
          | Typedtree.Tstr_class _
          | Typedtree.Tstr_class_type _
          | Typedtree.Tstr_include _
          | Typedtree.Tstr_attribute _
          | Typedtree.Tstr_typext _ -> maps)

let create_idents_to_types_map ~cmt_infos =
  let cmt_annots = cmt_infos.Cmt_format.cmt_annots in
  match cmt_annots with
  | Cmt_format.Packed _
  | Cmt_format.Interface _
  (* CR mshinwell: find out what "partial" implementations and
      interfaces are, and fix cmt_format.mli so it tells you *)
  | Cmt_format.Partial_implementation _
  | Cmt_format.Partial_interface _ -> String.Map.empty, LocTable.empty
  | Cmt_format.Implementation structure ->
    process_implementation ~structure ~idents_to_types:String.Map.empty
      ~app_points:LocTable.empty

(*
let search_load_path ~load_path ~module_name =
  let load_path = String.split load_path ~on:':' in
  let rec test_existence = function
    | [] -> failwith (Printf.sprintf "cmt file for module '%s' not found" module_name)
    | dir::dirs ->
      let path =
        Filename.concat dir (Printf.sprintf "%s.cmt" (String.lowercase module_name))
      in
      if Sys.file_exists path then path
      else test_existence dirs
*)

let load ~filename =
  try Hashtbl.find cache filename
  with Not_found ->
    if debug then Printf.printf "attempting to load cmt file: %s\n%!" filename;
    let cmi_infos, cmt_infos =
      try
        Cmt_format.read filename
      with Sys_error _ ->
        None, None
    in
    let idents_to_types, application_points =
      match cmt_infos with
      | None -> String.Map.empty, LocTable.empty
      | Some cmt_infos ->
        let () =
          (* restores load path: needs to be done before calling
             [Env.env_of_only_summary] otherwise an exception will be thrown when
             trying to open "distant" modules.
             By restoring the load_path used when compiling this file [Env] then
             knows where to find such "distant" modules. *)
          (* CR trefis: do we really want to concat with [!Config.load_path] here?
             There might be garbage in it (and we restore it when we're done, so it's
             *really* likely there will be garbage in it).

             mshinwell: temporarily commented out
          *)
          let extra_load_path =
            match Filename.dirname filename with
            | "" -> []
            | dirname -> [dirname]
          in
          Config.load_path :=
            (List.map (fun leaf ->
              if Filename.is_relative leaf then
                Filename.concat cmt_infos.Cmt_format.cmt_builddir leaf
              else leaf)
              cmt_infos.Cmt_format.cmt_loadpath) @ extra_load_path;
          if debug then begin
            Printf.printf "cmt_builddir=%s\n%!" cmt_infos.Cmt_format.cmt_builddir;
            Printf.printf "the load path will be: %s\n%!"
              (String.concat "," !Config.load_path)
          end
        in
        let idents, app_points = create_idents_to_types_map ~cmt_infos in
        try
          let idents =
            String.Map.map (fun (type_expr, env) ->
              type_expr, Env.env_of_only_summary Envaux.env_from_summary env
            ) idents
          in
          let app_points =
            LocTable.map (fun (type_expr, env) ->
              type_expr, Env.env_of_only_summary Envaux.env_from_summary env
            ) app_points
          in
          let distinguished_ident =
            let ident = ref None in
            String.Map.iter
              (fun candidate type_expr_and_env -> 
                try
                  if String.sub candidate 0 (String.length distinguished_var_name)
                    = distinguished_var_name
                  then begin
                    ident := Some (candidate, type_expr_and_env)
                  end
                with _exn -> ())
              idents;
            !ident
          in
          let idents =
            (* CR mshinwell: work out a proper solution to this problem *)
            match distinguished_ident with
            | None -> idents
            | Some (_ident, type_expr_and_env) ->
              String.Map.add distinguished_var_name type_expr_and_env idents
          in
          idents, app_points
        with
        | Envaux.Error (Envaux.Module_not_found path) -> begin
          if debug then begin
            Printf.printf "cmt load failed: module '%s' missing\n%!" (Path.name path)
          end;
          String.Map.empty, LocTable.empty
        end
        | exn -> begin
          if debug then
            Printf.printf "exception whilst reading cmt file(s): %s\n%!"
              (Printexc.to_string exn);
          String.Map.empty, LocTable.empty
        end
    in 
    let t = {
      cmi_infos ;
      cmt_infos ;
      idents_to_types ;
      application_points ;
    }
    in
    Hashtbl.add cache filename t ;
    t

let type_of_ident t ~unique_name =
  try Some (String.Map.find unique_name t.idents_to_types)
  with Not_found -> None

let find_argument_types t ~source_file_of_call_site ~line_number_of_call_site
      ~column_number_of_call_site =
  (* CR mshinwell: this is dreadful, but will suffice for now *)
  let candidates =
    LocTable.fold (fun location type_expr candidates ->
      let start_file, start_line, start_char =
        Location.get_pos_info location.Location.loc_start
      in
      let end_file, end_line, end_char =
        Location.get_pos_info location.Location.loc_end
      in
      (* CR mshinwell: still not good enough---could have multiple source files
          with the same name, as noted elsewhere. *)
      if start_file = source_file_of_call_site && start_file = end_file
        && start_line <= line_number_of_call_site
        && end_line >= line_number_of_call_site
        && start_char <= column_number_of_call_site
        && end_char >= column_number_of_call_site
      then
        type_expr :: candidates
      else
        candidates
    ) t.application_points []
  in
  match candidates with
  | [] -> None
  | [candidate] -> Some candidate
  | _candidate :: _candidates ->
    if debug then
      Printf.printf "warning: find_argument_types found multiple things\n%!";
    None
