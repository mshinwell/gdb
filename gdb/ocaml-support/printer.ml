open Std

(****************************************************************************************)
let extract_non_constant_ctors ~cases =
  let non_constant_ctors, _ =
    List.fold_left cases
      ~init:([], 0)
      ~f:(fun (non_constant_ctors, next_ctor_number) (ident, args, _return_type) ->
            match args with
            | [] ->
              non_constant_ctors, next_ctor_number
            | _ ->
              (* CR mshinwell: check [return_type] is just that, and use it.  Presumably
                 for GADTs. *)
              (next_ctor_number, (ident, args))::non_constant_ctors,
                next_ctor_number + 1)
  in
  non_constant_ctors

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
  with Not_found ->
    (* Nothing at the env active at that point of the file, but we might have more
       informations. *)
  try Some (TypeTable.(find table) path)
  with Not_found ->
    (* We don't know about that type declaration yet, but we might find it in the
       appropriate cmt file. *)
    None

let print_int out value ~type_of_ident =
  let default () =
    let value = Gdb.Obj.int value in
    Gdb.printf out "%d" value
  in
  match type_of_ident with
  | None ->
    default ();
    Gdb.printf out " [*]"
  | Some (type_expr, env) ->
    let rec print_type_expr type_expr =
      match type_expr.Types.desc with
      | Types.Tconstr (path, _, _abbrev_memo_ref) ->
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
              | Some ident -> Gdb.printf out "%s" (Ident.name ident)
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
          Gdb.printf out "<unk type %s>=" (Path.name path);
          default ()
        end
      | Types.Tvariant row_desc ->
        Gdb.printf out "Tvariant"
      | Types.Tvar _ ->
        Gdb.printf out "Tvar"
      | Types.Tarrow _ ->
        Gdb.printf out ". -> ."
      | Types.Ttuple _ ->
        Gdb.printf out "Tuple"
      | Types.Tobject _ ->
        Gdb.printf out "obj"
      | Types.Tfield _ ->
        Gdb.printf out "field"
      | Types.Tnil ->
        Gdb.printf out "nil"
      | Types.Tlink type_expr -> print_type_expr type_expr
      | Types.Tsubst _ ->
        Gdb.printf out "subst"
      | Types.Tunivar _ ->
        Gdb.printf out "univar"
      | Types.Tpoly _ ->
        Gdb.printf out "poly"
      | Types.Tpackage _ ->
        Gdb.printf out "package"
    in
    print_type_expr type_expr

(****************************************************************************************)

let default_printer ?prefix ~printers out value =
  begin match prefix with
  | None -> Gdb.printf out "tag %d: " (Gdb.Obj.tag value)
  | Some p -> Gdb.printf out "%s " p
  end ;
  for field = 0 to Gdb.Obj.size value - 1 do
    if field > 0 then Gdb.print out ", " ;
    try printers.(field) (Gdb.Obj.field value field)
    with Gdb.Read_error _ ->
      Gdb.printf out "<field %d read failed>" field
  done

let list ~print_element out l =
  let printf = Gdb.printf out in
  let rec aux v =
    if Gdb.Obj.is_block v then begin
      try
        let elt = Gdb.Obj.field v 0 in
        let next = Gdb.Obj.field v 1 in
        print_element elt ;
        if Gdb.Obj.is_block next then printf "; ";
        aux next
      with Gdb.Read_error _ ->
        printf "<list element read failed>"
    end
  in
  printf "[" ; aux l ; printf "]"

let record ~fields_helpers out r =
  let printf fmt = Gdb.printf out fmt in
  let nb_fields = Gdb.Obj.size r in
  printf "{ " ;
  if nb_fields > 1 then printf "\n" ;
  for field_nb = 0 to nb_fields - 1 do
    if field_nb > 0 then printf " ;\n" ;
    try
      let v = Gdb.Obj.field r field_nb in
      let (field_name, printer) = fields_helpers.(field_nb) in
      printf "%s%s = " (if nb_fields > 1 then "    " else "") field_name ;
      printer v
    with Gdb.Read_error _ ->
      printf "<field %d read failed>" field_nb
  done ;
  print_string " }"

let rec identify_value type_expr env =
  match type_expr.Types.desc with
  | Types.Tconstr (path, args, _abbrev_memo_ref) ->
    begin match env_find_type ~env ~path with
    | None -> `Type_decl_not_found
    | Some type_decl ->
      match type_decl.Types.type_kind with
      | Types.Type_variant cases -> `Constructed_value (cases, args)
      | Types.Type_abstract -> `Something_else
      | Types.Type_record (field_decls, record_repr) ->
        `Record (field_decls, record_repr)
    end
  | Types.Tlink type_expr -> identify_value type_expr env
  | Types.Ttuple component_types -> `Tuple component_types
  | Types.Tvariant _
  | Types.Tvar _
  | Types.Tarrow _
  | Types.Tobject _
  | Types.Tfield _
  | Types.Tnil
  | Types.Tsubst _
  | Types.Tunivar _
  | Types.Tpoly _
  | Types.Tpackage _ ->
    `Something_else

let rec value ?(depth=0) ?(print_sig=true) ~type_of_ident out v =
  if depth > 2 then Gdb.print out ".." else
  if Gdb.Obj.is_int v then print_int out v ~type_of_ident else
  if not (Gdb.Obj.is_block v) then Gdb.print out "<UNALIGNED OBJECT>" else
  begin match Gdb.Obj.tag v with
  | tag when tag < Gdb.Obj.lazy_tag ->
    let default_printers = lazy (
        Array.init (Gdb.Obj.size v) ~f:(fun _ v ->
          value ~depth:(succ depth) ~print_sig ~type_of_ident:None out v
        )
      )
    in
    begin match type_of_ident with
    | None ->
      default_printer ~printers:(Lazy.force default_printers) out v
    | Some (type_expr, env) ->
      (* TODO: move out *)
      match identify_value type_expr env with
      | `Type_decl_not_found
      | `Something_else ->
        default_printer ~printers:(Lazy.force default_printers) out v
      | `Tuple lst ->
        let component_types = Array.of_list lst in
        let printers =
          Array.map component_types ~f:(fun ty v ->
            value ~depth:(succ depth) ~print_sig:false
              ~type_of_ident:(Some (ty, env)) out v
          )
        in
        default_printer ~printers out v
      | `Record (field_decls, record_repr) ->
        let field_decls = Array.of_list field_decls in
        if Array.length field_decls <> Gdb.Obj.size v then
          (* The record declaration doesn't appear to match the value; just bail out. *)
          let () = Gdb.print out "<type decl doesn't match value> " in
          default_printer ~printers:(Lazy.force default_printers) out v
        else
          let fields_helpers =
            Array.map field_decls ~f:(fun (name, _, ty) ->
              let printer v =
                value ~depth:(succ depth) ~type_of_ident:(Some (ty, env))
                  ~print_sig:false out v
              in
              Ident.name name, printer
            )
          in
          record ~fields_helpers out v
      | `Constructed_value (cases, args) ->
        let non_constant_ctors = extract_non_constant_ctors cases in
        let ctor_info =
          try Some (List.assoc tag non_constant_ctors) with Not_found -> None
        in
        begin match ctor_info with
        | None ->
          default_printer ~printers:(Lazy.force default_printers) out v
        | Some (cident, _) when Ident.name cident = "::" && List.length args = 1 ->
          let print_element =
            match args with
            | [ elements_type ] ->
              value ~depth:(succ depth) ~print_sig:false out
                ~type_of_ident:(Some (elements_type, env))
            | _ -> assert false
          in
          list ~print_element out v
        | Some (cident, arg_types) ->
          let arg_types = Array.of_list arg_types in
          let printers =
            Array.map arg_types ~f:(fun ty v ->
              value ~depth:(succ depth) ~type_of_ident:(Some (ty, env))
                ~print_sig:false out v
            )
          in
          default_printer ~printers ~prefix:(Ident.name cident) out v
        end
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
  end ;
  if print_sig then
    match type_of_ident with
    | None -> ()
      (* Printf.printf "'%s' not found in cmt\n" symbol_linkage_name *)
    | Some (type_expr, _env) ->
      Gdb.print out " : ";
      let formatter =
        Format.make_formatter
          (fun str pos len -> Gdb.print out (String.sub str pos len))
          (fun () -> ())
      in
      Printtyp.type_expr formatter type_expr;
      Format.pp_print_flush formatter ()

