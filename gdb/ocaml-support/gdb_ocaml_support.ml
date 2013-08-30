(* CR mshinwell: transition to using [Core_kernel] *)

module List = ListLabels

module Cmt_file : sig
  type t

  val create_null : unit -> t
  val load : filename:string -> t

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

  let create_null () =
    { cmi_infos = None;
      cmt_infos = None;
      idents_to_types = [];
    }

  let create_idents_to_types_map ~cmt_infos =
    let rec process_pattern ~pat ~idents_to_types =
      match pat.Typedtree.pat_desc with
      | Typedtree.Tpat_var (ident, _loc) ->
        (Ident.unique_name ident, pat.Typedtree.pat_type)::idents_to_types
      | Typedtree.Tpat_alias (pat, ident, _loc) ->
        process_pattern ~pat
          ~idents_to_types:
            ((Ident.unique_name ident,
             pat.Typedtree.pat_type)::idents_to_types)
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
      match exp.Typedtree.exp_desc with
      | Typedtree.Texp_let (_rec, pat_exp_list, body) ->
        process_expression ~exp:body
          ~idents_to_types:
            (process_pat_exp_list ~pat_exp_list ~idents_to_types)
      | Typedtree.Texp_function (_label, pat_exp_list, _partial) ->
        process_pat_exp_list ~pat_exp_list ~idents_to_types
      (* CR mshinwell: this needs finishing, yuck *)
      | Typedtree.Texp_ident _
      | Typedtree.Texp_constant _
      | Typedtree.Texp_apply _
      | Typedtree.Texp_match _
      | Typedtree.Texp_try _
      | Typedtree.Texp_tuple _
      | Typedtree.Texp_construct _
      | Typedtree.Texp_variant _
      | Typedtree.Texp_record _
      | Typedtree.Texp_field _
      | Typedtree.Texp_setfield _
      | Typedtree.Texp_array _
      | Typedtree.Texp_ifthenelse _
      | Typedtree.Texp_sequence _
      | Typedtree.Texp_while _
      | Typedtree.Texp_for _
      | Typedtree.Texp_when _
      | Typedtree.Texp_send _
      | Typedtree.Texp_new _
      | Typedtree.Texp_instvar _
      | Typedtree.Texp_setinstvar _
      | Typedtree.Texp_override _
      | Typedtree.Texp_letmodule _
      | Typedtree.Texp_assert _
      | Typedtree.Texp_assertfalse
      | Typedtree.Texp_lazy _
      | Typedtree.Texp_object _
      | Typedtree.Texp_pack _ -> idents_to_types
    and process_pat_exp_list ~pat_exp_list ~idents_to_types =
      List.fold_left pat_exp_list
        ~init:idents_to_types
        ~f:(fun idents_to_types (pat, exp) ->
              process_pattern ~pat
                ~idents_to_types:(process_expression ~exp ~idents_to_types))
    in
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
    in
    let cmt_annots = cmt_infos.Cmt_format.cmt_annots in
    match cmt_annots with
    | Cmt_format.Packed _
    | Cmt_format.Interface _
    (* CR mshinwell: find out what "partial" implementations and
       interfaces are, and fix cmt_format.mli so it tells you *)
    | Cmt_format.Partial_implementation _
    | Cmt_format.Partial_interface _ -> []
    | Cmt_format.Implementation structure ->
      process_implementation ~structure ~idents_to_types:[]

  let load ~filename =
    let cmi_infos, cmt_infos =
      (* CR mshinwell: find out what the failure behaviour of [read] is *)
      Printf.printf "reading cmt file '%s'\n%!" filename;
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

let rec val_print ~depth v out ~symbol_linkage_name ~cmt_file =
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
              ~cmt_file
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
  let cmt_file =
    if String.length source_file_path > 3
      && String.get source_file_path
           (String.length source_file_path - 1) = 'l'
      && String.get source_file_path
           (String.length source_file_path - 2) = 'm'
      && String.get source_file_path
           (String.length source_file_path - 3) = '.'
    then
      let filename =
        (String.sub source_file_path 0 (String.length source_file_path - 3))
          ^ ".cmt"
      in
      Cmt_file.load ~filename
    else
      Cmt_file.create_null ()
  in
  val_print ~depth:0 addr stream ~symbol_linkage_name ~cmt_file

let () = Callback.register "gdb_ocaml_support_val_print" val_print
