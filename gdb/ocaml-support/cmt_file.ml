open Std

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
  | Typedtree.Tpat_construct (_, _, pats, _)
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
  | Texp_let (_rec, pat_exp_list, exp) ->
    let acc = process_pat_exp_list ~pat_exp_list init in
    process_expression ~exp acc
  | Texp_function (_label, pat_exp_list, _partial) ->
    process_pat_exp_list ~pat_exp_list init
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
  | Texp_match (exp, pat_exp_list, _)
  | Texp_try (exp, pat_exp_list) ->
    let acc = process_expression ~exp init in
    process_pat_exp_list ~pat_exp_list acc
  | Texp_construct (_, _, expr_list, _)
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
  | Texp_when (e1, e2)
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
  | Texp_assertfalse
  | Texp_object _
  | Texp_pack _ -> idents_to_types, app_points

and process_pat_exp_list ~pat_exp_list init =
  List.fold_left pat_exp_list ~init ~f:(fun acc (pat, exp) ->
    let idents_to_types, app_points = process_expression ~exp acc in
    process_pattern ~pat ~idents_to_types, app_points
  )

let process_implementation ~structure ~idents_to_types ~app_points =
  List.fold_left structure.Typedtree.str_items
    ~init:(idents_to_types, app_points)
    ~f:(fun (idents_to_types, app_points) str_item ->
          match str_item.Typedtree.str_desc with
          | Typedtree.Tstr_value (_rec, pat_exp_list) ->
            process_pat_exp_list ~pat_exp_list (idents_to_types, app_points)
          | Typedtree.Tstr_eval _
          | Typedtree.Tstr_primitive _
          | Typedtree.Tstr_type _
          | Typedtree.Tstr_exception _
          | Typedtree.Tstr_exn_rebind _
          | Typedtree.Tstr_module _
          | Typedtree.Tstr_recmodule _
          | Typedtree.Tstr_modtype _
          | Typedtree.Tstr_open _
          | Typedtree.Tstr_class _
          | Typedtree.Tstr_class_type _
          | Typedtree.Tstr_include _ -> idents_to_types, app_points)

let create_idents_to_types_map ~cmt_infos =
  (* CR mshinwell: needs to keep track of the environment, to resolve
      [Path.t]s *)
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

let load ~filename =
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
      let idents, app_points = create_idents_to_types_map ~cmt_infos in
      let idents =
        String.Map.map (fun (type_expr, env) ->
          type_expr, Env.env_of_only_summary Envaux.env_from_summary_best_effort env
        ) idents
      in
      let app_points =
        LocTable.map (fun (type_expr, env) ->
          type_expr, Env.env_of_only_summary Envaux.env_from_summary_best_effort env
        ) app_points
      in
      idents, app_points
  in {
    cmi_infos ;
    cmt_infos ;
    idents_to_types ;
    application_points ;
  }

let type_of_ident t ~unique_name =
  try Some (String.Map.find unique_name t.idents_to_types)
  with Not_found -> None

let find_argument_types t ~source_file_of_call_site ~line_number_of_call_site =
  (* CR mshinwell: this is dreadful, but will suffice for now *)
  let candidates =
    LocTable.fold (fun location type_expr candidates ->
      let start_file, start_line, _start_char =
        Location.get_pos_info location.Location.loc_start
      in
      let end_file, end_line, _end_char =
        Location.get_pos_info location.Location.loc_end
      in
      (* CR mshinwell: still not good enough---could have multiple source files
          with the same name, as noted elsewhere. *)
      if start_file = source_file_of_call_site && start_file = end_file
        && start_line <= line_number_of_call_site
        && end_line >= line_number_of_call_site
      then
        type_expr :: candidates
      else
        candidates
    ) t.application_points []
  in
  match candidates with
  | [] -> None
  | [candidate] -> Some candidate
  | candidate :: _candidates -> Some candidate (* CR mshinwell: will suffice for now *)
