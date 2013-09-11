module List = ListLabels

module StringTable = Map.Make (struct
  type t = string
  let compare = Pervasives.compare
end)

type t = {
  cmi_infos : Cmi_format.cmi_infos option;
  cmt_infos : Cmt_format.cmt_infos option;
  (* CR mshinwell: we can almost certainly do better than a map from
      every identifier (at least in common cases). *)
  (* CR trefis for mshinwell: in ocp-index, they use a trie from names to
      locations, you might want to do the same (but for types instead of
      positions, ofc) here. *)
  idents_to_types : (Types.type_expr * Env.t) StringTable.t ;
}

  let create_null () = {
    cmi_infos = None;
    cmt_infos = None;
    idents_to_types = StringTable.empty;
  }

  let rec process_pattern ~pat ~idents_to_types =
    match pat.Typedtree.pat_desc with
    | Typedtree.Tpat_var (ident, _loc) ->
      StringTable.add (Ident.unique_name ident)
        (pat.Typedtree.pat_type, pat.Typedtree.pat_env)
        idents_to_types
    | Typedtree.Tpat_alias (pat, ident, _loc) ->
      let idents_to_types =
        StringTable.add (Ident.unique_name ident)
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

  and process_expression ~exp ~idents_to_types =
    let open Typedtree in
    match exp.exp_desc with
    | Texp_let (_rec, pat_exp_list, body) ->
      process_expression ~exp:body
        ~idents_to_types:(process_pat_exp_list ~pat_exp_list ~idents_to_types)
    | Texp_function (_label, pat_exp_list, _partial) ->
      process_pat_exp_list ~pat_exp_list ~idents_to_types
    | Texp_apply (exp, args) ->
      let init = process_expression ~exp ~idents_to_types in
      List.fold_left args ~init ~f:(fun map (_label, expr_opt, _optional) ->
        match expr_opt with
        | None -> map
        | Some exp -> process_expression ~exp ~idents_to_types:map
      )
    | Texp_match (exp, pat_exp_list, _)
    | Texp_try (exp, pat_exp_list) ->
      let idents_to_types = process_expression ~exp ~idents_to_types in
      process_pat_exp_list ~pat_exp_list ~idents_to_types
    | Texp_construct (_, _, expr_list, _)
      (* Texp_co... (loc, descr, list, "explicit arity":bool)
       * Note: that last bool disappeared on trunk. *)
    | Texp_array expr_list
    | Texp_tuple expr_list ->
      List.fold_left expr_list ~init:idents_to_types ~f:(fun map exp ->
        process_expression ~exp ~idents_to_types:map
      )
    | Texp_variant (_, Some exp) -> process_expression ~exp ~idents_to_types
    | Texp_record (lst, _) ->
      List.fold_left lst ~init:idents_to_types ~f:(fun map (_loc,_desc,exp) ->
        process_expression ~exp ~idents_to_types:map
      )
    | Texp_ifthenelse (e1, e2, e_opt) ->
      let idents_to_types = process_expression ~exp:e1 ~idents_to_types in
      let idents_to_types = process_expression ~exp:e2 ~idents_to_types in
      begin match e_opt with
      | None -> idents_to_types
      | Some exp -> process_expression ~exp ~idents_to_types
      end
    | Texp_sequence (e1, e2)
    | Texp_when (e1, e2)
    | Texp_while (e1, e2) ->
      let idents_to_types = process_expression ~exp:e1 ~idents_to_types in
      process_expression ~exp:e2 ~idents_to_types
    | Texp_for (ident, _, e1, e2, _, e3) ->
      let idents_to_types =
        StringTable.add (Ident.unique_name ident) (e1.exp_type, e1.exp_env)
          idents_to_types
      in
      let idents_to_types = process_expression ~exp:e1 ~idents_to_types in
      let idents_to_types = process_expression ~exp:e2 ~idents_to_types in
      process_expression ~exp:e3 ~idents_to_types
    | Texp_lazy exp
    | Texp_assert exp
    | Texp_field (exp, _, _) ->
      process_expression ~exp ~idents_to_types
    | Texp_setfield (e1, _, _, e2) ->
      let idents_to_types = process_expression ~exp:e1 ~idents_to_types in
      process_expression ~exp:e2 ~idents_to_types
    | Texp_send (exp, _meth, e_opt) ->
      (* TODO: handle methods *)
      let idents_to_types = process_expression ~exp ~idents_to_types in
      begin match e_opt with
      | None -> idents_to_types
      | Some exp -> process_expression ~exp ~idents_to_types
      end
    | Texp_letmodule (ident, str_loc, mod_expr, exp) ->
      (* TODO: handle [mod_expr] *)
(*
      let idents_to_types =
        StringTable.add (Ident.unique_name ident)
          (mod_expr.mod_type, mod_expr.mod_env) idents_to_types
      in
*)
      process_expression ~exp ~idents_to_types
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
    | Texp_pack _ -> idents_to_types

  and process_pat_exp_list ~pat_exp_list ~idents_to_types =
    List.fold_left pat_exp_list ~init:idents_to_types ~f:(fun map (pat, exp) ->
      let idents_to_types = process_expression ~exp ~idents_to_types:map in
      process_pattern ~pat ~idents_to_types
    )

  let process_implementation ~structure ~idents_to_types =
    List.fold_left structure.Typedtree.str_items
      ~init:idents_to_types
      ~f:(fun idents_to_types str_item ->
            match str_item.Typedtree.str_desc with
            | Typedtree.Tstr_value (_rec, pat_exp_list) ->
              process_pat_exp_list ~pat_exp_list ~idents_to_types
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
            | Typedtree.Tstr_include _ -> idents_to_types)

  let create_idents_to_types_map ~cmt_infos =
    (* CR mshinwell: needs to keep track of the environment, to resolve
       [Path.t]s *)
    let cmt_annots = cmt_infos.Cmt_format.cmt_annots in
    let empty = StringTable.empty in
    match cmt_annots with
    | Cmt_format.Packed _
    | Cmt_format.Interface _
    (* CR mshinwell: find out what "partial" implementations and
       interfaces are, and fix cmt_format.mli so it tells you *)
    | Cmt_format.Partial_implementation _
    | Cmt_format.Partial_interface _ -> empty
    | Cmt_format.Implementation structure ->
      process_implementation ~structure ~idents_to_types:empty

  let load ~filename =
    let cmi_infos, cmt_infos =
      try
        Cmt_format.read filename
      with Sys_error _ ->
        None, None
    in
    let idents_to_types =
      match cmt_infos with
      | None -> StringTable.empty
      | Some cmt_infos -> create_idents_to_types_map ~cmt_infos
    in {
      cmi_infos ;
      cmt_infos ;
      idents_to_types ;
    }

  let type_of_ident t ~unique_name =
    try 
      Some (StringTable.find unique_name t.idents_to_types)
    with Not_found -> None
