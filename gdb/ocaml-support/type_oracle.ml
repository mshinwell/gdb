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
let rec print_path = function
  | Path.Pident ident -> Ident.name ident
  | Path.Pdot (path, s, _) -> (print_path path) ^ "." ^ "<string: " ^ s ^ " >"
  | Path.Papply (path1, path2) ->
    (print_path path1) ^ " applied to " ^ (print_path path2)

let rec examine_type_expr ~formatter ~paths_visited_so_far ~type_expr ~env =
  match (Btype.repr type_expr).Types.desc with
  | Types.Tconstr (path, args, _abbrev_memo_ref) ->
    if Path.same path Predef.path_array then
      match args with
      | [arg] -> `Array arg
      | _ -> `Failure (`Wrong_number_of_args_for_predefined_path path)
    else if Path.same path Predef.path_list then
      match args with
      | [arg] -> `List arg
      | _ -> `Failure (`Wrong_number_of_args_for_predefined_path path)
    else begin
      match try Some (Env.find_type path env) with Not_found -> None with
      | None ->
        (* Even if the type is abstract, the declaration should still be in
           the environment. *)
        `Failure (`Couldn't_find_type_decl_in_environment path)
      | Some type_decl ->
        examine_type_decl ~formatter ~paths_visited_so_far ~type_expr ~env
          ~path ~args ~type_decl
    end
  | Types.Ttuple component_types -> `Tuple component_types
  | Types.Tarrow _ -> `Closure
  | Types.Tvariant _ | Types.Tvar _
  | Types.Tobject _ | Types.Tfield _ | Types.Tnil | Types.Tsubst _
  | Types.Tunivar _ | Types.Tpoly _ | Types.Tpackage _ ->
    `Obj  (* CR-someday mshinwell: support these *)
  | Types.Tlink type_expr ->
    (* Should have been eliminated by [Btype.repr]. *)
    assert false

(* CR mshinwell: Should we use [Ctype.extract_concrete_typedecl]? *)
and examine_type_decl ~formatter ~paths_visited_so_far ~type_expr ~env
      ~path ~args ~type_decl =
  let params = type_decl.Types.type_params in
  if List.length params <> List.length args then begin
    if debug then Printf.printf "type params/args don't match\n%!";
    `Failure (`Type_params_and_args_don't_match path)
  end else begin
    match type_decl.Types.type_manifest with
    | Some type_expr ->
      examine_type_expr ~formatter ~paths_visited_so_far ~type_expr ~env
    | None ->
      match type_decl.Types.type_kind with
      | Types.Type_variant cases -> `Constructed (path, cases, params, args)
      | Types.Type_abstract ->
        if List.mem path paths_visited_so_far then begin
          if debug then Printf.printf "loop resolving %s\n%!" (print_path path);
          `Failure (`Loop_resolving_abstract_types path)
        end else
          let paths_visited_so_far = path::paths_visited_so_far in
          discover_manifest ~formatter ~paths_visited_so_far ~type_expr ~path
            ~args ~env
      | Types.Type_record (field_decls, record_repr) ->
        if List.length field_decls = 1
          && List.length args = 1
          && Ident.name ((List.hd field_decls).Types.ld_id) = "contents"
          && Path.name path = "Pervasives.ref"
        then
          `Ref (List.hd args)
        else
          `Record (path, params, args, field_decls, record_repr)
      | Types.Type_open -> `Open
  end

and discover_manifest ~formatter ~paths_visited_so_far ~type_expr ~path
      ~args ~env =
  let manifest =
    Abstraction_breaker.find_manifest_of_abstract_type ~formatter ~path ~env
  in
  match manifest with
  | None -> `Failure (`Tried_to_find_manifest_but_failed path)
  | Some (path, type_decl) ->
    examine_type_decl ~formatter ~paths_visited_so_far ~type_expr ~env
      ~path ~args ~type_decl

let classify_value_below_lazy_tag ~formatter ~type_expr ~env =
  let result =
    examine_type_expr ~formatter ~paths_visited_so_far:[] ~type_expr ~env
  in
  match result with
  | `Array ty -> `Array (ty, env)
  | `List ty -> `List (ty, env)
  | `Constructed (path, cases, params, args) ->
    `Constructed (path, cases, params, args, env)
  | `Record (path, params, args, field_decls, record_repr) ->
    `Record (path, params, args, field_decls, record_repr, env)
  | `Ref ty -> `Ref (ty, env)
  | `Tuple tys -> `Tuple (tys, env)
  | `Closure -> `Closure
  | `Open (* CR-someday mshinwell: support these *)
  | `Obj -> `Obj
  | `Failure (`Wrong_number_of_args_for_predefined_path path)
  | `Failure (`Loop_resolving_abstract_types path)
  | `Failure (`Couldn't_find_type_decl_in_environment path)
  | `Failure (`Type_params_and_args_don't_match path)
  | `Failure (`Tried_to_find_manifest_but_failed path) -> `Abstract path

let boxed_find_type_information ~formatter ~type_expr_and_env ~tag =
  match tag with
  | tag when tag < Obj.lazy_tag ->
    begin match type_expr_and_env with
    | None -> `Obj
    | Some (type_expr, env) ->
      classify_value_below_lazy_tag ~formatter ~type_expr ~env
    end
  | tag when tag = Obj.string_tag -> `String
  | tag when tag = Obj.double_tag -> `Float
  | tag when (tag = Obj.closure_tag || tag = Gdb.Obj.infix_tag) -> `Closure
  | tag when tag = Obj.lazy_tag -> `Lazy
  | tag when tag = Obj.object_tag -> `Object
  | tag when tag = Obj.forward_tag -> `Obj
  | tag when tag = Obj.abstract_tag -> `Abstract_tag
  | tag when tag = Obj.custom_tag -> `Custom
  | tag when tag = Obj.double_array_tag -> `Float_array
  | tag when tag < Obj.no_scan_tag -> `Obj
  | tag -> `Obj_not_traversable

(* XXX needs rewriting/merging with the above *)
let print_int value ~type_of_ident ~formatter =
  let env_find_type ~env ~path =
    try Some (Env.find_type path env) with Not_found -> None
  in
  let extract_constant_ctors ~cases =
    let constant_ctors, _ =
      List.fold_left cases
        ~init:([], 0)
        ~f:(fun (constant_ctors, next_ctor_number) ctor_decl ->
              let ident = ctor_decl.Types.cd_id in
              match ctor_decl.Types.cd_args with
              | [] ->
                (Int64.of_int next_ctor_number, ident)::constant_ctors,
                  next_ctor_number + 1
              | _ ->
                constant_ctors, next_ctor_number)
    in
    constant_ctors
  in
  let default () =
    let value = Gdb.Obj.int value in
    Format.fprintf formatter "%d" value
  in
  match type_of_ident with
  | None ->
    default ();
    Format.fprintf formatter "?"
  | Some (type_expr, env) ->
    let rec print_type_expr type_expr =
      match type_expr.Types.desc with
      | Types.Tconstr (path, _, _abbrev_memo_ref) ->
        if Path.same path Predef.path_char then
          let value = Gdb.Obj.int value in
          if value >= 0 && value <= 255 then
            Format.fprintf formatter "'%s'" (Char.escaped (Char.chr value))
          else
            Format.fprintf formatter "%d" value
        else
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
            | Types.Type_open -> default ()  (* Not thought about *)
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
        let ctor_names_and_hashes =
          let labels = List.map row_desc.Types.row_fields ~f:fst in
          List.map labels ~f:(fun label -> label, Btype.hash_variant label)
        in
        let desired_hash = Gdb.Obj.int value in
        let matches =
          List.filter ctor_names_and_hashes
            ~f:(fun (ctor_name, hash) -> hash = desired_hash)
        in
        begin match matches with
        | [(ctor_name, _hash)] -> Format.fprintf formatter "`%s" ctor_name
        | _::_ | [] -> Format.fprintf formatter "`0x%x?" desired_hash
        end
      | Types.Tvar _ ->
        default ();
        Format.fprintf formatter "?"
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
