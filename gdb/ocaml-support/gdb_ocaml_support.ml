(* CR mshinwell: transition to using [Core_kernel] *)

module List = ListLabels

module Cmt_file : sig
  type t

  val load : filename:string -> t option
  val type_of_ident : t -> unique_name:string -> Types.type_expr option
end = struct
  type t = {
    cmi_infos : Cmi_format.cmi_infos option;
    cmt_infos : Cmt_format.cmt_infos option;
    (* CR mshinwell: once we have [Core_kernel], switch to a table. *)
    (* CR mshinwell: we can almost certainly do better than a map from
       every identifier (at least in common cases). *)
    idents_to_types : (string * Types.type_expr) list;
  }

  let create_idents_to_types_map ~cmt_infos =
    let rec process_pattern ~pat ~idents_to_types =
      match pat with
        | Tpat_var (ident, _loc) ->
          (Ident.unique_name ident, pat.Typedtree.pat_type)::idents_to_types
        | Tpat_alias of (pat, ident, _loc) ->
          process_pattern ~pat
            ~idents_to_types:
              (Ident.unique_name ident,
               pat.Typedtree.pat_type)::idents_to_types
        | Tpat_tuple pats
        | Tpat_construct of (_, _, pats, _)
        | Tpat_array pats ->
          List.fold pats
            ~init:idents_to_types
            ~f:(fun idents_to_types pat ->
                  process_pattern ~pat ~idents_to_types)
        | Tpat_variant (_label, pat_opt, _row_desc) ->
          begin match pat_opt with
          | None -> idents_to_types
          | Some pat -> process_pattern ~pat ~idents_to_types
          end
        | Tpat_record of (loc_desc_pat_list, _closed) ->
          List.fold loc_desc_pat_list
            ~init:idents_to_types
            ~f:(fun idents_to_types (_loc, _desc, pat) ->
                  process_pattern ~pat ~idents_to_types)
        | Tpat_or (pat1, pat2, _row_desc) ->
          process_pattern ~pat:pat1
            ~idents_to_types:(process_pattern ~pat:pat2 ~idents_to_types)
        | Tpat_lazy of pat ->
          process_pattern ~pat ~idents_to_types
        | Tpat_any
        | Tpat_constant _ -> idents_to_types
    and process_expression ~exp ~idents_to_types =
      match exp with
      | Texp_let (_rec, pat_exp_list, body) ->
        process_expression ~exp:body
          ~idents_to_types:
            (process_pat_exp_list ~pat_exp_list ~idents_to_types)
      | Texp_function (_label, pat_exp_list, _partial) ->
        process_pat_exp_list ~pat_exp_list ~idents_to_types
      (* CR mshinwell: this needs finishing, yuck *)
      | Texp_ident _
      | Texp_constant _
      | Texp_apply _
      | Texp_match _
      | Texp_try _
      | Texp_tuple _
      | Texp_construct _
      | Texp_variant _
      | Texp_record _
      | Texp_field _
      | Texp_setfield _
      | Texp_array _
      | Texp_ifthenelse _
      | Texp_sequence _
      | Texp_while _
      | Texp_for _
      | Texp_when _
      | Texp_send _
      | Texp_new _
      | Texp_instvar _
      | Texp_setinstvar _
      | Texp_override _
      | Texp_letmodule _
      | Texp_assert _
      | Texp_assertfalse
      | Texp_lazy _
      | Texp_object _
      | Texp_pack _ -> idents_to_types
    and process_pat_exp_list ~pat_exp_list ~idents_to_types =
      List.fold pat_exp_list
        ~f:(fun idents_to_types (pat, exp) ->
              process_pattern ~pat
                ~idents_to_types:(process_expression ~exp ~idents_to_types))
    in
    let process_implementation ~structure ~idents_to_types =
      List.fold structure.Typedtree.str_items
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
    in
    let cmt_annots = cmt_infos.Cmt_format.cmt_annots in
    List.fold cmt_annots
      ~init:[]
      ~f:(fun idents_to_types binary_annot ->
            match binary_annot with
            | Cmt_format.Packed _
            | Cmt_format.Interface _
            (* CR mshinwell: find out what "partial" implementations and
               interfaces are, and fix cmt_format.mli so it tells you *)
            | Cmt_format.Partial_implementation _
            | Cmt_format.Partial_interface _ ->
            | Cmt_format.Implementation structure ->
              process_implementation ~structure ~idents_to_types)

  let load ~filename =
    let cmi_infos, cmt_infos =
      (* CR mshinwell: find out what the failure behaviour of [read] is *)
      Cmt_format.read filename
    in
    let idents_to_types =
      match cmt_infos with
      | None -> []
      | Some cmt_infos -> create_idents_to_types_map ~cmt_infos
    in
    { cmi_infos;
      cmt_infos;
      idents_to_types;
    }

  let type_of_ident t ~unique_name =
    try Some (List.assoc unique_name t.idents_to_types) with Not_found -> None
end

let _ = prerr_endline "Hello from ml world!"

let rec val_print ~depth v out ~symbol_linkage_name =
  if depth > 2 then Gdb.print out ".." else 
  begin match symbol_linkage_name with
    | None -> ()
    | Some symbol_linkage_name ->
      Gdb.printf out "%s=" symbol_linkage_name
  end;
  if Gdb.Obj.is_int v
  then Gdb.printf out "%d" (Gdb.Obj.int v)
  else if Gdb.Obj.is_block v then
  begin match Gdb.Obj.tag v with
    | tag when tag < Gdb.Obj.closure_tag  ->
      begin
        if tag > 0 then Gdb.printf out "tag %d:" tag;
        Gdb.print out "(";
        for field = 0 to Gdb.Obj.size v - 1 do
          if field > 0 then Gdb.print out ", ";
          try 
            let v' = Gdb.Obj.field v field in
            val_print ~depth:(succ depth) v' out ~symbol_linkage_name:None
          with Gdb.Read_error _ ->
            Gdb.printf out "<field %d read failed>" field
        done;
        Gdb.print out ")"
      end
    | tag when tag = Gdb.Obj.string_tag ->
      Gdb.printf out "%S" (Gdb.Obj.string v)
    | tag -> Gdb.printf out "<tag %d, TODO>" tag
  end
  else Gdb.printf out "<unaligned object>"

let val_print addr stream ~symbol_linkage_name ~source_file_path =
  val_print ~depth:0 addr stream ~symbol_linkage_name ~cmt_file

let () = Callback.register "gdb_ocaml_support_val_print" val_print
