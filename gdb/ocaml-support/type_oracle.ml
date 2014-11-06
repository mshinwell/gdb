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
open Debug

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

module Variant_kind = struct
  type t = Polymorphic | Non_polymorphic

  let to_string_prefix = function
    | Polymorphic -> "`"
    | Non_polymorphic -> ""
end
module Vk = Variant_kind

let check_predef_paths ~path ~args ~env ~scrutinee =
  (* CR mshinwell: validate more things (e.g. value size) *)
  if Path.same path Predef.path_array then
    match scrutinee with
    | `Unboxed _ -> Some `Obj_unboxed_but_should_be_boxed
    | `Boxed _ ->
      match args with
      | [arg] -> Some (`Array (arg, env))
      | _ -> Some `Obj_boxed_traversable  (* wrong number of arguments *)
  else if Path.same path Predef.path_list then
    match args with
    | [arg] -> Some (`List (arg, env))
    | _ ->
      match scrutinee with  (* wrong number of arguments *)
      | `Unboxed _ -> Some `Obj_unboxed
      | `Boxed _ -> Some `Obj_boxed_traversable
  else if Path.same path Predef.path_int then
    match scrutinee with
    | `Unboxed _ -> Some `Int
    | `Boxed _ -> Some `Obj_boxed_traversable  (* should not be boxed *)
  else if Path.same path Predef.path_char then
    match scrutinee with
    | `Unboxed _ -> Some `Char
    | `Boxed _ -> Some `Obj_boxed_traversable  (* should not be boxed *)
  else
    None

let rec examine_type_expr ~formatter ~paths_visited_so_far ~type_expr ~env
      ~scrutinee =
  match (Btype.repr type_expr).Types.desc with
  | Types.Tconstr (path, args, _abbrev_memo_ref) ->
    begin match check_predef_paths ~path ~args ~env ~scrutinee with
    | Some result -> result
    | None ->
      if debug then
         Printf.printf "examine_type_expr: Env.find_type '%s': "
           (print_path path);
      begin match try Some (Env.find_type path env) with Not_found -> None with
      | None ->
        if debug then Printf.printf "not found.\n%!";
        (* Even if the type is abstract, the declaration should still be in
           the environment. *)
        begin match scrutinee with
        | `Unboxed _ -> `Obj_unboxed
        | `Boxed _ ->
          if debug then
            Printf.printf "examine_type_expr error case 1, load path is: %s\n%!"
              (String.concat "," !Config.load_path);
          `Obj_boxed_traversable
        end
      | Some type_decl ->
        if debug then Printf.printf "found.\n%!";
        examine_type_decl ~formatter ~paths_visited_so_far ~type_expr ~env
          ~path ~args ~type_decl ~scrutinee
      end
    end
  | Types.Tvariant row_desc ->
    begin match scrutinee with
    | `Boxed _ ->
      (* CR mshinwell: support boxed polymorphic variant constructors *)
      if debug then Printf.printf "examine_type_expr error case 2\n%!";
      `Obj_boxed_traversable
    | `Unboxed scrutinee ->
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
      | [(ctor_name, _hash)] ->
        `Constant_constructor (ctor_name, Vk.Polymorphic)
      | _::_ | [] -> `Obj_unboxed  (* cannot find ctor with given hash *)
      end
    end
  | Types.Ttuple component_types ->
    begin match scrutinee with
    | `Boxed _ -> `Tuple (component_types, env)
    | `Unboxed _ -> `Obj_unboxed_but_should_be_boxed
    end
  | Types.Tarrow _ ->
    begin match scrutinee with
    | `Boxed _ -> `Closure
    | `Unboxed _ -> `Obj_unboxed_but_should_be_boxed
    end
  | Types.Tvar _
  | Types.Tobject _ | Types.Tfield _ | Types.Tnil | Types.Tsubst _
  | Types.Tunivar _ | Types.Tpoly _ | Types.Tpackage _ ->
    (* CR mshinwell: more work to do here *)
    begin match scrutinee with
    | `Boxed _ ->
      let what =
        match (Btype.repr type_expr).Types.desc with
        | Types.Tvar _ -> "Tvar"
        | Types.Tobject _ -> "Tobject"
        | Types.Tfield _ -> "Tfield"
        | Types.Tnil -> "Tnil"
        | Types.Tsubst _ -> "Tsubst"
        | Types.Tunivar _ -> "Tunivar"
        | Types.Tpoly _ -> "Tpoly"
        | Types.Tpackage _ -> "Tpackage"
        | _ -> assert false
      in
      if debug then Printf.printf "examine_type_expr error case 3 %s\n%!" what;
      `Obj_boxed_traversable
    | `Unboxed _ -> `Obj_unboxed
    end
  | Types.Tlink type_expr ->
    (* Should have been eliminated by [Btype.repr]. *)
    assert false

(* CR mshinwell: Should we use [Ctype.extract_concrete_typedecl]? *)
and examine_type_decl ~formatter ~paths_visited_so_far ~type_expr ~env
      ~path ~args ~type_decl ~scrutinee =
  let params = type_decl.Types.type_params in
  if List.length params <> List.length args then begin
    if debug then Printf.printf "type params/args don't match\n%!";
    `Abstract path  (* fail gracefully *)
  end else begin
    match type_decl.Types.type_manifest with
    | Some type_expr ->
      examine_type_expr ~formatter ~paths_visited_so_far ~type_expr ~env
        ~scrutinee
    | None ->
      begin match type_decl.Types.type_kind with
      | Types.Type_variant cases ->
        begin match scrutinee with
        | `Boxed _ ->
          (* CR mshinwell: change this when Tvariant case is filled in above *)
          let kind = Vk.Non_polymorphic in
          `Non_constant_constructor (path, cases, params, args, env, kind)
        | `Unboxed scrutinee ->
          let constant_ctors = extract_constant_ctors ~cases in
          let value = Int64.shift_right scrutinee 1 in (* undo Caml encoding *)
          if Int64.compare value Int64.zero >= 0
             && Int64.compare value
               (Int64.of_int (List.length constant_ctors)) < 0
          then
            let ident =
              try Some (List.assoc value constant_ctors) with Not_found -> None
            in
            begin match ident with
            | Some ident ->
              `Constant_constructor (Ident.name ident, Vk.Non_polymorphic)
            | None -> `Obj_unboxed
            end
          else
            `Obj_unboxed
        end
      | Types.Type_abstract ->
        if List.mem path paths_visited_so_far then begin
          if debug then
            Printf.printf "loop resolving %s\n%!" (print_path path);
          `Abstract path  (* fail gracefully *)
        end else
          let paths_visited_so_far = path::paths_visited_so_far in
          discover_manifest ~formatter ~paths_visited_so_far ~type_expr ~path
            ~args ~env ~scrutinee
      | Types.Type_record (field_decls, record_repr) ->
        begin match scrutinee with
        | `Boxed _ ->
          if List.length field_decls = 1
            && List.length args = 1
            && Ident.name ((List.hd field_decls).Types.ld_id) = "contents"
            && Path.name path = "Pervasives.ref"
          then
            `Ref (List.hd args, env)
          else
            `Record (path, params, args, field_decls, record_repr, env)
        | `Unboxed _ ->
          (* Records should never be unboxed values, but behave gracefully. *)
          `Obj_unboxed
        end
      | Types.Type_open -> `Open
      end
  end

(* CR-soon mshinwell: try removing [type_expr], probably redundant *)
and discover_manifest ~formatter ~paths_visited_so_far ~type_expr ~path
      ~args ~env ~scrutinee =
  let manifest =
    Abstraction_breaker.find_manifest_of_abstract_type ~formatter ~path ~env
  in
  match manifest with
  | None -> `Abstract path  (* couldn't find manifest; fail gracefully *)
  | Some (path, type_decl, env) ->
    examine_type_decl ~formatter ~paths_visited_so_far ~type_expr ~env
      ~path ~args ~type_decl ~scrutinee

let string_of_result = function
  | `Obj_boxed_traversable -> "Obj_boxed_traversable"
  | `Obj_boxed_not_traversable -> "Obj_boxed_not_traversable"
  | `Obj_unboxed -> "Obj_unboxed"
  | `Obj_unboxed_but_should_be_boxed -> "Obj_unboxed_but_should_be_boxed"
  | `Abstract _ -> "Abstract"
  | `Array _ -> "Array"
  | `List _ -> "List"
  | `Tuple _ -> "Tuple"
  | `Char -> "Char"
  | `Int -> "Int"
  | `Float -> "Float"
  | `Float_array -> "Float_array"
  | `Constant_constructor _ -> "Constant_constructor"
  | `Non_constant_constructor _ -> "Non_constant_constructor"
  | `Record _ -> "Record"
  | `Open -> "Open"
  | `Ref _ -> "Ref"
  | `String -> "String"
  | `Closure -> "Closure"
  | `Lazy -> "Lazy"
  | `Object -> "Object"
  | `Abstract_tag -> "Abstract_tag"
  | `Custom -> "Custom"

let find_type_information ~formatter ~type_expr_and_env ~scrutinee =
  if debug then begin
    Printf.printf "find_type_information starting, type info present? %s\n%!"
      (match type_expr_and_env with None -> "no" | Some _ -> "yes")
  end;
  let result =
    if Gdb.Obj.is_int scrutinee then
      match type_expr_and_env with
      | None -> `Obj_unboxed
      | Some (type_expr, env) ->
        examine_type_expr ~formatter ~paths_visited_so_far:[] ~type_expr ~env
          ~scrutinee:(`Unboxed scrutinee)
    else
      let tag = Gdb.Obj.tag scrutinee in
      match tag with
      | tag when tag < Obj.lazy_tag ->
        begin match type_expr_and_env with
        | None -> `Obj_boxed_traversable
        | Some (type_expr, env) ->
          examine_type_expr ~formatter ~paths_visited_so_far:[] ~type_expr ~env
            ~scrutinee:(`Boxed scrutinee)
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
  in
  if debug then begin
    Printf.printf "find_type_information returning %s\n%!"
      (string_of_result result);
  end;
  result
