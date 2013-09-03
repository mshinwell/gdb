(* CR mshinwell: transition to using [Core_kernel] *)

module List = ListLabels

module Cmt_file : sig
  type t

  val create_null : unit -> t
  val load : filename:string -> t

  val type_of_ident : t
    -> unique_name:string
    -> (Types.type_expr * Env.t) option
end = struct
  type t = {
    cmi_infos : Cmi_format.cmi_infos option;
    cmt_infos : Cmt_format.cmt_infos option;
    (* CR mshinwell: once we have [Core_kernel], switch to a table. *)
    (* CR mshinwell: we can almost certainly do better than a map from
       every identifier (at least in common cases). *)
    idents_to_types : (string * (Types.type_expr * Env.t)) list;
  }

  let create_null () =
    { cmi_infos = None;
      cmt_infos = None;
      idents_to_types = [];
    }

  let create_idents_to_types_map ~cmt_infos =
    (* CR mshinwell: needs to keep track of the environment, to resolve
       [Path.t]s *)
    let rec process_pattern ~pat ~idents_to_types =
      match pat.Typedtree.pat_desc with
      | Typedtree.Tpat_var (ident, _loc) ->
        (Ident.unique_name ident, (pat.Typedtree.pat_type,
         pat.Typedtree.pat_env))::idents_to_types
      | Typedtree.Tpat_alias (pat, ident, _loc) ->
        process_pattern ~pat
          ~idents_to_types:
            ((Ident.unique_name ident,
             (pat.Typedtree.pat_type, pat.Typedtree.pat_env))::idents_to_types)
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
(*
        let env = exp.Typedtree.exp_env in
        Printf.printf "--Texp_let values----\n%!";
        Env.fold_values
          (fun name path _decl_descrs () ->
            Printf.printf "'%s' path '%s'\n%!" name (Path.name path)
          ) None env ();
        Printf.printf "--Texp_let types----\n%!";
        Env.fold_types
          (fun name path _decl_descrs () ->
            Printf.printf "'%s' path '%s'\n%!" name (Path.name path)
          ) None env ();
        Printf.printf "------\n%!";
*)
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
      (* CR mshinwell: need to load cmts for our dependencies *)
(*
      List.iter cmt_infos.Cmt_format.cmt_imports
        ~f:(fun (name, _) -> Printf.printf "Import: %s\n%!" name);
      Printf.printf "end of imports\n%!";
*)
      process_implementation ~structure ~idents_to_types:[]

  let load ~filename =
    let cmi_infos, cmt_infos =
      try
        Cmt_format.read filename
      with Sys_error _ ->
        None, None
    in
    let idents_to_types =
      match cmt_infos with
      | None -> []
      | Some cmt_infos ->
        List.map (create_idents_to_types_map ~cmt_infos)
          ~f:(fun (ident, (type_expr, env)) ->
                let env =
                  Env.env_of_only_summary Envaux.env_from_summary_best_effort env
                in
                ident, (type_expr, env))
    in
(*
    List.iter idents_to_types
      ~f:(fun (ident, _type) ->
            Printf.printf "idents_to_types: '%s'\n" ident);
*)
    { cmi_infos;
      cmt_infos;
      idents_to_types;
    }

  let type_of_ident t ~unique_name =
(*
    Printf.printf "trying to find '%s'\n%!" unique_name;
*)
    try Some (List.assoc unique_name t.idents_to_types) with Not_found -> None
end
(*
let _ = prerr_endline "Hello from ml world!"
*)

let extract_constant_ctors ~cases =
  let constant_ctors, _ =
    List.fold_left cases
      ~init:([], 0)
      ~f:(fun (constant_ctors, next_ctor_number) (ident, args, _return_type) ->
            match args with
            | [] ->
              (Int64.of_int next_ctor_number, ident)::constant_ctors, next_ctor_number + 1
            | _ ->
              constant_ctors, next_ctor_number)
  in
  constant_ctors

let env_find_type ~env ~path =
  try Some (Env.find_type path env)
  with Not_found -> None

let val_print_int ~value ~gdb_stream ~type_of_ident =
  let default () =
    let value = Gdb.Obj.int value in
    Gdb.printf gdb_stream "%d" value
  in
  match type_of_ident with
  | None ->
    default ();
    Gdb.printf gdb_stream " [*]"
  | Some (type_expr, env) ->
(*
    Printf.printf "--values----\n%!";
    Env.fold_values
      (fun name path _decl_descrs () ->
        Printf.printf "'%s' path '%s'\n%!" name (Path.name path)
      ) None env ();
    Printf.printf "--types----\n%!";
    Env.fold_types
      (fun name path _decl_descrs () ->
        Printf.printf "'%s' path '%s'\n%!" name (Path.name path)
      ) None env ();
    Printf.printf "------\n%!";
*)
    let rec print_type_expr type_expr =
      match type_expr.Types.desc with
      | Types.Tconstr (path, [], _abbrev_memo_ref) ->
        begin match env_find_type ~env ~path with
        | Some type_decl ->
          begin match type_decl.Types.type_kind with
          | Types.Type_variant cases ->
            let constant_ctors = extract_constant_ctors ~cases in
            let value = Int64.shift_right value 1 in  (* undo the Caml encoding *)
            if Int64.compare value Int64.zero >= 0
               && Int64.compare value (Int64.of_int (List.length constant_ctors)) < 0
            then
              match
                try Some (List.assoc value constant_ctors) with Not_found -> None
              with
              | Some ident -> Gdb.printf gdb_stream "%s" (Ident.name ident)
              | None ->
                Printf.printf "couldn't find value %Ld, ctor list length %d\n%!"
                  value (List.length constant_ctors);
                default ()
            else
              default ()
          | Types.Type_abstract
          | Types.Type_record _ ->
            (* Neither of these are expected. *)
            default ()
          end
        | None ->
          Gdb.printf gdb_stream "<unk type %s>=" (Path.name path);
          default ()
        end
      | Types.Tvariant row_desc ->
        Gdb.printf gdb_stream "Tvariant"
      | Types.Tvar _ ->
        Gdb.printf gdb_stream "Tvar"
      | Types.Tarrow _ ->
        Gdb.printf gdb_stream ". -> ."
      | Types.Ttuple _ ->
        Gdb.printf gdb_stream "Tuple"
      | Types.Tconstr _ ->
        Gdb.printf gdb_stream "constr"
      | Types.Tobject _ ->
        Gdb.printf gdb_stream "obj"
      | Types.Tfield _ ->
        Gdb.printf gdb_stream "field"
      | Types.Tnil ->
        Gdb.printf gdb_stream "nil"
      | Types.Tlink type_expr -> print_type_expr type_expr
      | Types.Tsubst _ ->
        Gdb.printf gdb_stream "subst"
      | Types.Tunivar _ ->
        Gdb.printf gdb_stream "univar"
      | Types.Tpoly _ ->
        Gdb.printf gdb_stream "poly"
      | Types.Tpackage _ ->
        Gdb.printf gdb_stream "package"
    in
    print_type_expr type_expr

let rec val_print ~depth v out ~type_of_ident =
  if depth > 2 then Gdb.print out ".." else begin
    if Gdb.Obj.is_int v
    then val_print_int ~gdb_stream:out ~type_of_ident ~value:v
    else begin
      if Gdb.Obj.is_block v then
      begin match Gdb.Obj.tag v with
        | tag when tag < Gdb.Obj.closure_tag  ->
          begin
            let default () =
              if tag > 0 then Gdb.printf out "tag %d:" tag;
              Gdb.print out "(";
              for field = 0 to Gdb.Obj.size v - 1 do
                if field > 0 then Gdb.print out ", ";
                try 
                  let v' = Gdb.Obj.field v field in
                  val_print ~depth:(succ depth) v' out ~type_of_ident
                with Gdb.Read_error _ ->
                  Gdb.printf out "<field %d read failed>" field
              done;
              Gdb.print out ")"
            in
            match type_of_ident with
            | None -> default ()
            | Some (type_expr, env) ->
              let rec identify_value type_expr =
                match type_expr.Types.desc with
                | Types.Tconstr (path, _args, _abbrev_memo_ref) ->
                  begin match env_find_type ~env ~path with
                  | None -> `Type_decl_not_found
                  | Some type_decl ->
                    match type_decl.Types.type_kind with
                    | Types.Type_variant _ -> `Constructed_value
                    | Types.Type_abstract -> `Something_else
                    | Types.Type_record (field_decls, record_repr) ->
                      `Record (field_decls, record_repr)
                  end
                | Types.Tlink type_expr -> identify_value type_expr
                | Types.Ttuple _ -> `Tuple
                | Types.Tvariant _
                | Types.Tvar _
                | Types.Tarrow _
                | Types.Tobject _
                | Types.Tfield _
                | Types.Tnil
                | Types.Tsubst _
                | Types.Tunivar _
                | Types.Tpoly _
                | Types.Tpackage _ -> `Something_else
              in
              match identify_value type_expr with
              | `Record (field_decls, record_repr) ->
                let field_decls = Array.of_list field_decls in
                if Array.length field_decls <> Gdb.Obj.size v then begin
                  (* The record declaration doesn't appear to match the value; just
                     bail out. *)
                  Gdb.printf out "<type decl doesn't match value> ";
                  default ()
                end else begin
                  Gdb.print out "{ ";
                  for field = 0 to Gdb.Obj.size v - 1 do
                    if field > 0 then Gdb.print out ", ";
                    try 
                      let v' = Gdb.Obj.field v field in
                      let (field_name, _mutable, field_type) = field_decls.(field) in
                      Gdb.printf out "%s = " (Ident.name field_name);
                      val_print ~depth:(succ depth) v' out
                        ~type_of_ident:(Some (field_type, env))
                    with Gdb.Read_error _ ->
                      Gdb.printf out "<field %d read failed>" field
                  done;
                  Gdb.print out " }"
                end
              | `Constructed_value  (* CR mshinwell: can do much better for this one *)
              | `Tuple
              | `Something_else ->
                default ()
              | `Type_decl_not_found ->
(*                Printf.printf "** not found\n%!";*)
                default ()
          end
        | tag when tag = Gdb.Obj.string_tag ->
          Gdb.printf out "%S" (Gdb.Obj.string v)
        | tag when tag = Gdb.Obj.string_tag ->
          Gdb.printf out "%S" (Gdb.Obj.string v)
        | tag when tag = Gdb.Obj.double_tag ->
          begin try 
            Gdb.printf out "%f" (Gdb.Target.read_double v)
          with Gdb.Read_error _ ->
            Gdb.print out "<double read failed>"
          end
        | tag when tag = Gdb.Obj.closure_tag ->
          begin try
            let pc = Gdb.Target.read_field v 0 in
            match Gdb.Priv.gdb_find_pc_line pc ~not_current:false with
            | None -> Gdb.printf out "closure (0x%Lx)" pc
            | Some {Gdb.Priv. symtab; line = Some line} ->
              Gdb.printf out "closure (%s:%d)" (Gdb.Priv.gdb_symtab_filename symtab) line
            | Some {Gdb.Priv. symtab} ->
              Gdb.printf out "closure (%s, 0x%Lx)" (Gdb.Priv.gdb_symtab_filename symtab)
                pc
          with Gdb.Read_error _ ->
            Gdb.print out "closure, possibly corrupted (code pointer read failed)"
          end
        | tag when tag = Gdb.Obj.lazy_tag ->
          Gdb.printf out "<lazy>"
        | tag when tag = Gdb.Obj.object_tag ->
          Gdb.printf out "<object>"
        | tag when tag = Gdb.Obj.infix_tag ->
          Gdb.printf out "<infix>"
        | tag when tag = Gdb.Obj.forward_tag ->
          Gdb.printf out "<forward>"
        | tag when tag = Gdb.Obj.abstract_tag ->
          Gdb.printf out "<abstract>"
        | tag when tag = Gdb.Obj.custom_tag ->
          Gdb.printf out "<custom>"
        | tag when tag = Gdb.Obj.double_array_tag ->
          Gdb.printf out "<double array>"
        | tag -> Gdb.printf out "<v=%Ld, tag %d>" v tag
      end
      else Gdb.printf out "<unaligned object>"
    end;
    match type_of_ident with
    | None -> ()
      (* Printf.printf "'%s' not found in cmt\n" symbol_linkage_name *)
    | Some (type_expr, _env) ->
      let rec print_type_expr type_expr =
        match type_expr.Types.desc with
        | Types.Tconstr (path, [], _abbrev_memo_ref) ->
          Gdb.print out " : ";
          Gdb.print out (Path.name path)
        | Types.Tlink type_expr -> print_type_expr type_expr
        | Types.Tvariant _
        | Types.Tvar _
        | Types.Tarrow _
        | Types.Ttuple _
        | Types.Tconstr _
        | Types.Tobject _
        | Types.Tfield _
        | Types.Tnil
        | Types.Tsubst _
        | Types.Tunivar _
        | Types.Tpoly _
        | Types.Tpackage _ -> ()
      in
      print_type_expr type_expr

        (* CR mshinwell: work out how to use [Printtyp.type_expr].  The below doesn't
           print anything. *)
(*
        let formatter =
          Format.make_formatter
            (fun str pos len -> Gdb.print out (String.sub str pos len))
            (fun () -> ())
        in
        Printtyp.type_expr formatter type_expr;
        Format.print_flush ()
*)
  end

(*
        match type_expr.Types.desc with
        | Tconstr (path, _args, _abbrev_memo_ref) ->
          Printf.printf "constructor: %s\n" (Path.name path)
        | 
*)

let val_print ~depth v out ~symbol_linkage_name ~cmt_file =
  let type_of_ident =
    match symbol_linkage_name with
    | None -> None
    | Some symbol_linkage_name ->
      Cmt_file.type_of_ident cmt_file ~unique_name:symbol_linkage_name
  in
  val_print ~depth v out ~type_of_ident

let val_print addr stream ~symbol_linkage_name ~source_file_path =
  let cmt_file =
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
  in
  val_print ~depth:0 addr stream ~symbol_linkage_name ~cmt_file

let () = Callback.register "gdb_ocaml_support_val_print" val_print


let demangle name options = "TODO"
let () = Callback.register "gdb_ocaml_support_demangle" val_print
