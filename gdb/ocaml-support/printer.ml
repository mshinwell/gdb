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

open Std

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
  try Some (Env.find_type path env) with Not_found -> None

let print_int out value ~type_of_ident =
  let default () =
    let value = Gdb.Obj.int value in
    Gdb.printf out "%d" value
  in
  match type_of_ident with
  | None ->
    default ();
    Gdb.printf out "[?]"
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
        default ();
        Gdb.printf out "[?]"
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

let rec value_looks_like_list value =
  if Gdb.Obj.is_int value && Gdb.Obj.int value = 0 (* nil *) then
    true
  else
    if (not (Gdb.Obj.is_int value))
       && Gdb.Obj.is_block value
       && Gdb.Obj.size value = 2
    then
      value_looks_like_list (Gdb.Obj.field value 1)
    else
      false

let default_printer ?prefix ~printers out value =
  (* See if the value looks like a list.  If it does, then print it as a list; it seems
     far more likely to be one of those than a set of nested pairs. *)
  if value_looks_like_list value then
    `Looks_like_list
  else begin
    begin match prefix with
    | None -> Gdb.printf out "tag %d: " (Gdb.Obj.tag value)
    | Some p -> Gdb.printf out "%s " p
    end;
    Gdb.print out "(";
    for field = 0 to Gdb.Obj.size value - 1 do
      if field > 0 then Gdb.print out ", ";
      try printers.(field) (Gdb.Obj.field value field)
      with Gdb.Read_error _ ->
        Gdb.printf out "<field %d read failed>" field
    done;
    Gdb.print out ")";
    `Done
  end

let list ~print_element out l =
  let printf = Gdb.printf out in
  let rec aux v =
    if Gdb.Obj.is_block v then begin
      try
        let elt = Gdb.Obj.field v 0 in
        let next = Gdb.Obj.field v 1 in
        print_element elt;
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
  printf "{ ";
  if nb_fields > 1 then printf "\n";
  for field_nb = 0 to nb_fields - 1 do
    if field_nb > 0 then printf " ;\n";
    try
      let v = Gdb.Obj.field r field_nb in
      let (field_name, printer) = fields_helpers.(field_nb) in
      printf "%s%s = " (if nb_fields > 1 then "    " else "") field_name;
      printer v
    with Gdb.Read_error _ ->
      printf "<field %d read failed>" field_nb
  done;
  printf " }"

let rec identify_value type_expr env =
  match type_expr.Types.desc with
  | Types.Tconstr (path, args, _abbrev_memo_ref) ->
    begin match env_find_type ~env ~path with
    | None -> `Type_decl_not_found
    | Some type_decl ->
      let args = List.combine type_decl.Types.type_params args in
      match type_decl.Types.type_kind with
      | Types.Type_variant cases -> `Constructed_value (cases, args)
      | Types.Type_abstract ->
        begin match type_decl.Types.type_manifest with
        | None -> `Something_else
        | Some ty -> identify_value ty env
        end
      | Types.Type_record (field_decls, record_repr) ->
        `Record (path, args, field_decls, record_repr)
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

let rec value ?(depth=0) ?(print_sig=true) ~type_of_ident ~summary out v =
  if (summary && depth > 2) || depth > 5 then Gdb.print out ".." else
  if Gdb.Obj.is_int v then print_int out v ~type_of_ident else
  if not (Gdb.Obj.is_block v) then Gdb.print out "<UNALIGNED OBJECT>" else
  begin match Gdb.Obj.tag v with
  | tag when tag < Gdb.Obj.lazy_tag ->
    let default_printers = lazy (
        Array.init (Gdb.Obj.size v) ~f:(fun _ v ->
          value ~depth:(succ depth) ~print_sig ~type_of_ident:None ~summary out v
        )
      )
    in
    let default_printer ?prefix ~printers out v =
      match default_printer ?prefix ~printers out v with
      | `Done -> ()
      | `Looks_like_list ->
        if summary then
          Gdb.print out "[...]"
        else begin
          let print_element =
            value ~depth:(succ depth) ~print_sig:false out ~type_of_ident:None ~summary
          in
          list ~print_element out v;
          Gdb.print out "[?]"
        end
    in
    begin match type_of_ident with
    | None ->
      if summary then
        Gdb.print out "..."
      else
        default_printer ~printers:(Lazy.force default_printers) out v
    | Some (type_expr, env) ->
      (* TODO: move out *)
      match identify_value type_expr env with
      | `Type_decl_not_found
      | `Something_else ->
        if summary then
          Gdb.print out "..."
        else
          default_printer ~printers:(Lazy.force default_printers) out v
      | `Tuple lst ->
        if summary && (List.length lst > 2 || depth > 0) then
          Gdb.print out "(...)"
        else
          let component_types = Array.of_list lst in
          let printers =
            Array.map component_types ~f:(fun ty v ->
              value ~depth:(succ depth) ~print_sig:false
                ~type_of_ident:(Some (ty, env)) ~summary out v
            )
          in
          default_printer ~printers out v
      | `Record (path, args, field_decls, record_repr) ->
        let field_decls = Array.of_list field_decls in
        if Array.length field_decls <> Gdb.Obj.size v then
          (* The record declaration doesn't appear to match the value; just bail out. *)
          let () = Gdb.print out "<type decl doesn't match value> " in
          default_printer ~printers:(Lazy.force default_printers) out v
        else
          if Array.length field_decls = 1
             && Ident.name (fst3 field_decls.(0)) = "contents"
             && Path.name path = "Pervasives.ref"
          then begin
            Gdb.print out "ref ";
            let contents_type =
              match args with
              | [_type_param, contents_type] -> Some (contents_type, env)
              | _ -> None
            in
            value ~depth:(succ depth) ~type_of_ident:contents_type
              ~print_sig:false ~summary out (Gdb.Obj.field v 0)
          end
          else if summary then
            Gdb.print out "{...}"
          else
            let fields_helpers =
              Array.map field_decls ~f:(fun (name, _, ty) ->
                let printer v =
                  value ~depth:(succ depth) ~type_of_ident:(Some (ty, env))
                    ~print_sig:false ~summary out v
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
          if summary then
            Gdb.print out "[...]"
          else
            let print_element =
              match args with
              | [ (_, elements_type) ] ->
                value ~depth:(succ depth) ~print_sig:false out
                  ~type_of_ident:(Some (elements_type, env))
                  ~summary
              | _ -> assert false
            in
            list ~print_element out v
        | Some (cident, arg_types) ->
          if summary && List.length arg_types > 1 then begin
            Gdb.print out (Ident.name cident);
            Gdb.print out " (...)"
          end else
            let arg_types = Array.of_list arg_types in
            let printers =
              Array.map arg_types ~f:(fun ty v ->
                let ty =
                  try List.assoc ty args
                  with Not_found ->
                    (* Either [ty] wasn't a type variable, or it's an existencial. *)
                    ty 
                in
                value ~depth:(succ depth) ~type_of_ident:(Some (ty, env))
                  ~print_sig:false out v
                  ~summary
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
  | tag when (tag = Gdb.Obj.closure_tag || tag = Gdb.Obj.infix_tag) ->
    begin try
      if summary then
        Gdb.print out "<fun>"
      else begin
        let pc = Gdb.Target.read_field v 0 in
        match Gdb.Priv.gdb_find_pc_line pc ~not_current:false with
        | None -> Gdb.printf out "<fun> (0x%Lx)" pc
        | Some {Gdb.Priv. symtab; line = Some line} ->
          Gdb.printf out "<fun> (%s:%d)" (Gdb.Priv.gdb_symtab_filename symtab) line
        | Some {Gdb.Priv. symtab} ->
          Gdb.printf out "<fun> (%s, 0x%Lx)" (Gdb.Priv.gdb_symtab_filename symtab)
            pc
      end
    with Gdb.Read_error _ ->
      Gdb.print out "closure, possibly corrupted (code pointer read failed)"
    end
  | tag when tag = Gdb.Obj.lazy_tag ->
    Gdb.printf out "<lazy>"
  | tag when tag = Gdb.Obj.object_tag ->
    Gdb.printf out "<object>"
  | tag when tag = Gdb.Obj.forward_tag ->
    Gdb.printf out "<forward>"
  | tag when tag = Gdb.Obj.abstract_tag ->
    Gdb.printf out "<abstract>"
  | tag when tag = Gdb.Obj.custom_tag ->
    Gdb.printf out "<custom>"
  | tag when tag = Gdb.Obj.double_array_tag ->
    Gdb.printf out "<double array>"
  | tag -> Gdb.printf out "<v=%Ld, tag %d>" v tag
  end;
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
      Printtyp.reset_and_mark_loops type_expr;
      Printtyp.type_expr formatter type_expr;
      Format.pp_print_flush formatter ()
