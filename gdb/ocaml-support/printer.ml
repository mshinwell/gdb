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

let print_int value ~type_of_ident ~formatter =
  let default () =
    let value = Gdb.Obj.int value in
    Format.fprintf formatter "%d" value
  in
  match type_of_ident with
  | None ->
    default ();
    Format.fprintf formatter "[?]"
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
              | Some ident -> Format.fprintf formatter "%s" (Ident.name ident)
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
          Format.fprintf formatter "<unk type %s>=" (Path.name path);
          default ()
        end
      | Types.Tvariant row_desc ->
        Format.fprintf formatter "Tvariant"
      | Types.Tvar _ ->
        default ();
        Format.fprintf formatter "[?]"
      | Types.Tarrow _ ->
        Format.fprintf formatter ". -> ."
      | Types.Ttuple _ ->
        Format.fprintf formatter "Tuple"
      | Types.Tobject _ ->
        Format.fprintf formatter "obj"
      | Types.Tfield _ ->
        Format.fprintf formatter "field"
      | Types.Tnil ->
        Format.fprintf formatter "nil"
      | Types.Tlink type_expr -> print_type_expr type_expr
      | Types.Tsubst _ ->
        Format.fprintf formatter "subst"
      | Types.Tunivar _ ->
        Format.fprintf formatter "univar"
      | Types.Tpoly _ ->
        Format.fprintf formatter "poly"
      | Types.Tpackage _ ->
        Format.fprintf formatter "package"
    in
    print_type_expr type_expr

let rec value_looks_like_list value =
  if Gdb.Obj.is_int value && Gdb.Obj.int value = 0 (* nil *) then
    true
  else
    if (not (Gdb.Obj.is_int value))
       && Gdb.Obj.is_block value
       && Gdb.Obj.tag value = 0
       && Gdb.Obj.size value = 2
    then
      value_looks_like_list (Gdb.Obj.field value 1)
    else
      false

let default_printer ?prefix ~force_never_like_list ~printers ~formatter value =
  (* See if the value looks like a list.  If it does, then print it as a list; it seems
     far more likely to be one of those than a set of nested pairs. *)
  if value_looks_like_list value && not force_never_like_list then
    `Looks_like_list
  else begin
    begin match prefix with
    | None -> Format.fprintf formatter "tag %d: " (Gdb.Obj.tag value)
    (* CR mshinwell: remove dreadful hack *)
    | Some "XXX" -> ()
    | Some p -> Format.fprintf formatter "%s " p
    end;
    Format.fprintf formatter "(@[";
    for field = 0 to Gdb.Obj.size value - 1 do
      if field > 0 then Format.fprintf formatter ",@;<1 0>";
      try printers.(field) (Gdb.Obj.field value field)
      with Gdb.Read_error _ ->
        Format.fprintf formatter "<field %d read failed>" field
    done;
    Format.fprintf formatter "@])";
    `Done
  end

let list ~print_element ~formatter l =
  let rec aux v =
    if Gdb.Obj.is_block v then begin
      try
        let elt = Gdb.Obj.field v 0 in
        let next = Gdb.Obj.field v 1 in
        print_element elt;
        if Gdb.Obj.is_block next then Format.fprintf formatter ";@;<1 0>";
        aux next
      with Gdb.Read_error _ ->
        Format.fprintf formatter "<list element read failed>"
    end
  in
  Format.fprintf formatter "[@[" ; aux l ; Format.fprintf formatter "@]]"

let record ~fields_helpers ~formatter r =
  let nb_fields = Gdb.Obj.size r in
  Format.fprintf formatter "{@[";
  for field_nb = 0 to nb_fields - 1 do
(*    if field_nb > 0 then
      Format.fprintf formatter ";@;<1 0>";*)
    try
      let v = Gdb.Obj.field r field_nb in
      let (field_name, printer) = fields_helpers.(field_nb) in
      Format.fprintf formatter "%s = @[" field_name;
      printer v;
      Format.fprintf formatter ";@]@;<1 0>"
    with Gdb.Read_error _ ->
      Format.fprintf formatter "<field %d read failed>" field_nb
  done;
  Format.fprintf formatter "@]}"

let rec identify_value type_expr env =
  match type_expr.Types.desc with
  | Types.Tconstr (path, args, _abbrev_memo_ref) ->
    begin match env_find_type ~env ~path with
    | None -> `Type_decl_not_found
    | Some type_decl ->
      (* CR mshinwell: not sure if this is correct, check *)
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

let rec value ?(depth=0) ?(print_sig=true) ~type_of_ident ~summary ~formatter v =
(*  Format.fprintf formatter "@[";*)
  if (summary && depth > 2) || depth > 5 then Format.fprintf formatter ".." else
  if Gdb.Obj.is_int v then print_int ~formatter v ~type_of_ident else
  if not (Gdb.Obj.is_block v) then Format.fprintf formatter "<UNALIGNED OBJECT>" else
  begin match Gdb.Obj.tag v with
  | tag when tag < Gdb.Obj.lazy_tag ->
    let default_printers = lazy (
        Array.init (Gdb.Obj.size v) ~f:(fun _ v ->
          value ~depth:(succ depth) ~print_sig ~type_of_ident:None ~summary ~formatter v
        )
      )
    in
    (* CR mshinwell: [force_never_like_list] is a hack.  Seems like the list guessing
       should only be applied in a couple of cases, so maybe invert the flag? *)
    let default_printer ?(force_never_like_list = false) ?prefix ~printers ~formatter v =
      match default_printer ?prefix ~force_never_like_list ~printers ~formatter v with
      | `Done -> ()
      | `Looks_like_list ->
        if summary then
          Format.fprintf formatter "[...]"
        else begin
          let print_element =
            value ~depth:(succ depth) ~print_sig:false ~formatter ~type_of_ident:None ~summary
          in
          list ~print_element ~formatter v;
          Format.fprintf formatter "[?]"
        end
    in
    begin match type_of_ident with
    | None ->
      if summary then
        Format.fprintf formatter "..."
      else
        default_printer ~printers:(Lazy.force default_printers) ~formatter v
    | Some (type_expr, env) ->
      (* TODO: move out *)
      match identify_value type_expr env with
      | `Type_decl_not_found
      | `Something_else ->
        if summary then
          Format.fprintf formatter "..."
        else
          default_printer ~printers:(Lazy.force default_printers) ~formatter v
      | `Tuple lst ->
        if summary && (List.length lst > 2 || depth > 0) then
          Format.fprintf formatter "(...)"
        else
          let component_types = Array.of_list lst in
          let printers =
            Array.map component_types ~f:(fun ty v ->
              value ~depth:(succ depth) ~print_sig:false
                ~type_of_ident:(Some (ty, env)) ~summary ~formatter v
            )
          in
          default_printer ~printers ~prefix:"XXX" ~force_never_like_list:true ~formatter v
      | `Record (path, args, field_decls, record_repr) ->
        let field_decls = Array.of_list field_decls in
        if Array.length field_decls <> Gdb.Obj.size v then
          (* The record declaration doesn't appear to match the value; just bail out. *)
          let () = Format.fprintf formatter "<type decl doesn't match value> " in
          default_printer ~printers:(Lazy.force default_printers) ~formatter v
        else
          if Array.length field_decls = 1
             && Ident.name (fst3 field_decls.(0)) = "contents"
             && Path.name path = "Pervasives.ref"
          then begin
            Format.fprintf formatter "ref ";
            let contents_type =
              match args with
              | [_type_param, contents_type] -> Some (contents_type, env)
              | _ -> None
            in
            value ~depth:(succ depth) ~type_of_ident:contents_type
              ~print_sig:false ~summary ~formatter (Gdb.Obj.field v 0)
          end
          else if summary then
            Format.fprintf formatter "{...}"
          else
            let fields_helpers =
              Array.map field_decls ~f:(fun (name, _, ty) ->
                let printer v =
                  value ~depth:(succ depth) ~type_of_ident:(Some (ty, env))
                    ~print_sig:false ~summary ~formatter v
                in
                Ident.name name, printer
              )
            in
            record ~fields_helpers ~formatter v
      | `Constructed_value (cases, args) ->
        let non_constant_ctors = extract_non_constant_ctors cases in
        let ctor_info =
          try Some (List.assoc tag non_constant_ctors) with Not_found -> None
        in
        begin match ctor_info with
        | None ->
          default_printer ~printers:(Lazy.force default_printers) ~formatter v
        | Some (cident, _) when Ident.name cident = "::" && List.length args = 1 ->
          if summary then
            Format.fprintf formatter "[...]"
          else
            let print_element =
              match args with
              | [ (_, elements_type) ] ->
                value ~depth:(succ depth) ~print_sig:false ~formatter
                  ~type_of_ident:(Some (elements_type, env))
                  ~summary
              | _ -> assert false
            in
            list ~print_element ~formatter v
        | Some (cident, arg_types) ->
          if summary && List.length arg_types > 1 then begin
            Format.fprintf formatter "%s (...)" (Ident.name cident);
          end else
            let arg_types = Array.of_list arg_types in
            let printers =
              Array.map arg_types ~f:(fun ty v ->
                let ty =
                  try List.assoc ty args
                  with Not_found ->
                    (* Either [ty] wasn't a type variable, or it's an existential. *)
                    ty 
                in
                value ~depth:(succ depth) ~type_of_ident:(Some (ty, env))
                  ~print_sig:false ~formatter v
                  ~summary
              )
            in
            default_printer ~printers ~prefix:(Ident.name cident) ~formatter v
              ~force_never_like_list:true
        end
    end
  | tag when tag = Gdb.Obj.string_tag ->
    Format.fprintf formatter "%S" (Gdb.Obj.string v)
  | tag when tag = Gdb.Obj.string_tag ->
    Format.fprintf formatter "%S" (Gdb.Obj.string v)
  | tag when tag = Gdb.Obj.double_tag ->
    begin try 
      Format.fprintf formatter "%f" (Gdb.Target.read_double v)
    with Gdb.Read_error _ ->
      Format.fprintf formatter "<double read failed>"
    end
  | tag when (tag = Gdb.Obj.closure_tag || tag = Gdb.Obj.infix_tag) ->
    begin try
      if summary then
        Format.fprintf formatter "<fun>"
      else begin
        (* First we try to find out what this function is called.  If it's one of
           the special currying wrappers then we try to look in the closure's environment
           to find the "real" function pointer. *)
        let pc = Gdb.Target.read_field v 0 in
        let name = Gdb.Priv.gdb_function_linkage_name_at_pc pc in
        let is_currying_wrapper =
          match name with
          | None -> false  (* if we can't find the name, assume it isn't a wrapper *)
          | Some name ->
            let curry = "caml_curry" in
            let tuplify = "caml_tuplify" in
            (* CR mshinwell: this could maybe be made more precise *)
            String.sub name 0 (String.length curry) = curry
              || String.sub name 0 (String.length tuplify) = tuplify
        in
        let pc =
          if not is_currying_wrapper then
            pc
          else
            (* The "real" function pointer should be the last entry in the environment of
               the closure. *)
            let num_fields = Gdb.Obj.size v in
            Gdb.Target.read_field v (num_fields - 1)
        in
        match Gdb.Priv.gdb_find_pc_line pc ~not_current:false with
        | None -> Format.fprintf formatter "<fun> (0x%Lx)" pc
        | Some {Gdb.Priv. symtab; line = Some line} ->
          Format.fprintf formatter "<fun> (%s:%d)"
            (Gdb.Priv.gdb_symtab_filename symtab) line
        | Some {Gdb.Priv. symtab} ->
          Format.fprintf formatter "<fun> (%s, 0x%Lx)"
            (Gdb.Priv.gdb_symtab_filename symtab)
            pc
      end
    with Gdb.Read_error _ ->
      Format.fprintf formatter "closure, possibly corrupted (code pointer read failed)"
    end
  | tag when tag = Gdb.Obj.lazy_tag ->
    Format.fprintf formatter "<lazy>"
  | tag when tag = Gdb.Obj.object_tag ->
    Format.fprintf formatter "<object>"
  | tag when tag = Gdb.Obj.forward_tag ->
    Format.fprintf formatter "<forward>"
  | tag when tag = Gdb.Obj.abstract_tag ->
    Format.fprintf formatter "<abstract>"
  | tag when tag = Gdb.Obj.custom_tag ->
    Format.fprintf formatter "<custom>"
  | tag when tag = Gdb.Obj.double_array_tag ->
    Format.fprintf formatter "<double array>"
  | tag -> Format.fprintf formatter "<v=%Ld, tag %d>" v tag
  end;
  if print_sig then begin
    match type_of_ident with
    | None -> ()
      (* Printf.printf "'%s' not found in cmt\n" symbol_linkage_name *)
    | Some (type_expr, _env) ->
      Format.fprintf formatter " : ";
      Printtyp.reset_and_mark_loops type_expr;
      Printtyp.type_expr formatter type_expr
  end
(*  Format.fprintf formatter "@]"*)

let value ?depth ?print_sig ~type_of_ident ~summary out v =
  let formatter =
    Format.make_formatter
      (fun str pos len -> Gdb.print out (String.sub str pos len))
      (fun () -> ())
  in
  Format.fprintf formatter "@[";
  value ?depth ?print_sig ~type_of_ident ~summary ~formatter v;
  Format.fprintf formatter "@]";
  Format.pp_print_flush formatter ()
