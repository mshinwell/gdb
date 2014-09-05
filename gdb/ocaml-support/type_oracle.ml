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

module Variant_kind = struct
  type t = Polymorphic | Non_polymorphic

  let to_string_prefix = function
    | Polymorphic -> "`"
    | Non_polymorphic -> ""
end
module Vk = Variant_kind

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
  | Types.Tvariant _ ->
    `Obj  (* CR mshinwell: support polymorphic variant constructors *)
  | Types.Ttuple component_types -> `Tuple component_types
  | Types.Tarrow _ -> `Closure
  | Types.Tvar _
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
      | Types.Type_variant cases ->
        (* CR mshinwell: change this when Tvariant case is filled in above *)
        let kind = Vk.Non_polymorphic in
        `Constructed (path, cases, params, args, kind)
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
  | `Constructed (path, cases, params, args, kind) ->
    `Non_constant_constructor (path, cases, params, args, env, kind)
  | `Record (path, params, args, field_decls, record_repr) ->
    `Record (path, params, args, field_decls, record_repr, env)
  | `Ref ty -> `Ref (ty, env)
  | `Tuple tys -> `Tuple (tys, env)
  | `Closure -> `Closure
  | `Open (* CR-someday mshinwell: support these *)
  | `Obj -> `Obj_boxed_traversable
  | `Failure (`Wrong_number_of_args_for_predefined_path path)
  | `Failure (`Loop_resolving_abstract_types path)
  | `Failure (`Couldn't_find_type_decl_in_environment path)
  | `Failure (`Type_params_and_args_don't_match path)
  | `Failure (`Tried_to_find_manifest_but_failed path) -> `Abstract path

let boxed_find_type_information ~formatter ~type_expr_and_env ~tag =
  match tag with
  | tag when tag < Obj.lazy_tag ->
    begin match type_expr_and_env with
    | None -> `Obj_boxed_traversable
    | Some (type_expr, env) ->
      classify_value_below_lazy_tag ~formatter ~type_expr ~env
    end
  | tag when tag = Obj.string_tag -> `String
  | tag when tag = Obj.double_tag -> `Float
  | tag when (tag = Obj.closure_tag || tag = Gdb.Obj.infix_tag) -> `Closure
  | tag when tag = Obj.lazy_tag -> `Lazy
  | tag when tag = Obj.object_tag -> `Object
  | tag when tag = Obj.forward_tag -> `Obj_boxed_traversable
  | tag when tag = Obj.abstract_tag -> `Abstract_tag
  | tag when tag = Obj.custom_tag -> `Custom
  | tag when tag = Obj.double_array_tag -> `Float_array
  | tag when tag < Obj.no_scan_tag -> `Obj_boxed_traversable
  | tag -> `Obj_boxed_not_traversable

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

let find_type_information ~formatter ~type_expr_and_env ~scrutinee =
  if not (Gdb.Obj.is_int scrutinee) then
    boxed_find_type_information ~formatter ~type_expr_and_env
      ~tag:(Gdb.Obj.tag scrutinee)
  else
    match type_expr_and_env with
    | None -> `Obj_unboxed
    | Some (type_expr, env) ->
      let type_expr = Btype.repr type_expr in
      match type_expr.Types.desc with
      | Types.Tconstr (path, _, _abbrev_memo_ref) ->
        if Path.same path Predef.path_int then `Int
        else if Path.same path Predef.path_char then `Char
        else
          let type_decl =
            try Some (Env.find_type path env) with Not_found -> None
          in
          begin match type_decl with
          | Some type_decl ->
            begin match type_decl.Types.type_kind with
            | Types.Type_variant cases ->
              let constant_ctors = extract_constant_ctors ~cases in
              let value = Int64.shift_right scrutinee 1 in (* undo the Caml encoding *)
              if Int64.compare value Int64.zero >= 0
                 && Int64.compare value
                   (Int64.of_int (List.length constant_ctors)) < 0
              then
                let ident =
                  try Some (List.assoc value constant_ctors) with Not_found -> None
                in
                match ident with
                | Some ident ->
                  `Constant_constructor (Ident.name ident, Vk.Non_polymorphic)
                | None -> `Obj_unboxed
              else
                `Obj_unboxed
            | Types.Type_open -> `Obj_unboxed  (* CR-someday mshinwell: fix this *)
            | Types.Type_abstract | Types.Type_record _ ->
              (* Neither of these are expected. *)
              `Obj_unboxed
            end
          | None -> `Obj_unboxed
          end
      | Types.Tvariant row_desc ->
        let ctor_names_and_hashes =
          let labels = List.map row_desc.Types.row_fields ~f:fst in
          List.map labels ~f:(fun label -> label, Btype.hash_variant label)
        in
        let desired_hash = Gdb.Obj.int scrutinee in
        let matches =
          List.filter ctor_names_and_hashes
            ~f:(fun (ctor_name, hash) -> hash = desired_hash)
        in
        begin match matches with
        | [(ctor_name, _hash)] -> `Constant_constructor (ctor_name, Vk.Polymorphic)
        | _::_ | [] -> `Obj_unboxed
        end
      (* Some of the following should probably never occur in conjunction with an
         unboxed value, but we handle that case gracefully. *)
      | Types.Tvar _ | Types.Tarrow _ | Types.Ttuple _ | Types.Tobject _
      | Types.Tfield _ | Types.Tnil | Types.Tsubst _ | Types.Tunivar _
      | Types.Tpoly _ | Types.Tpackage _ -> `Obj_unboxed
      | Types.Tlink _ -> assert false
