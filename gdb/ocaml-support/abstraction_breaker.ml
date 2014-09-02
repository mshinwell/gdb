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

let debug = try Sys.getenv "GOS_DEBUG" <> "" with Not_found -> false
let rec print_path = function
  | Path.Pident ident -> Ident.name ident
  | Path.Pdot (path, s, _) -> (print_path path) ^ "." ^ "<string: " ^ s ^ " >"
  | Path.Papply (path1, path2) ->
    (print_path path1) ^ " applied to " ^ (print_path path2)

module T = Typedtree

(* XXX work out how this is going to be set.
   Even if the tree uses packing, it should be possible to install only the toplevel
   modules' .cmi and .cmt files, into a distinguished directory. *)
let cmt_directory = "/mnt/local/sda1/mshinwell/jane-submissions/lib"

let rec find_module_binding ~cmt_cache ~dir_prefix ~path ~is_toplevel ~env =
  if debug then
    Printf.printf "find_module_binding: 1. path=%s\n%!" (print_path path);
  let path = Env.normalize_path None env path in
  if debug then
    Printf.printf "find_module_binding: 2. path=%s\n%!" (print_path path);
  match path with
  | Path.Pident ident ->
    let lowercase_module_name = String.lowercase (Ident.name ident) in
    let cmt_directory =
      match dir_prefix with
      | None -> cmt_directory
      | Some dir_prefix -> Filename.concat cmt_directory dir_prefix
    in
    let cmt_name = lowercase_module_name ^ ".cmt" in
    if debug then
      Printf.printf "trying to read cmt: %s/%s\n%!" cmt_directory cmt_name;
    let cmt =
      Cmt_cache.read cmt_cache
        ~pathname:(Filename.concat cmt_directory cmt_name)
    in
    if debug then
      Printf.printf "Cmt_cache.read finished\n%!";
    begin match cmt with
    | Some (_cmi_infos_opt, Some cmt) ->
      if debug then Printf.printf "cmt read was successful\n%!";
      begin match cmt.Cmt_format.cmt_annots with
      | Cmt_format.Implementation structure ->
        let mod_binding =
          { T.
            mb_id = ident;
            mb_name = Location.mkloc (Ident.name ident) Location.none;
            mb_expr =
              { T.
                mod_desc = T.Tmod_structure structure;
                mod_loc = Location.none;
                mod_type = Types.Mty_ident path;  (* bogus, but unused *)
                mod_env = env;  (* likewise *)
                mod_attributes = [];
              };
            mb_attributes = [];
            mb_loc = Location.none;
          }
        in
        `Found_module mod_binding
      | Cmt_format.Packed (signature, _) ->
        (* We look for all .cmt files in the same directory at the moment.  Of
           course, with packing, there may be name clashes.  Perhaps we can
           avoid those by not supporting packing in the future. *)
        `Found_pack ident
      | Cmt_format.Interface _
      | Cmt_format.Partial_implementation _
      | Cmt_format.Partial_interface _ ->
        (* CR mshinwell: no idea what to do with these *)
        `Not_found
      end
    | Some _ | None ->
      if debug then Printf.printf "cmt read failed, or not useful\n%!";
      `Not_found
    end
  | Path.Pdot (path, component, _) ->
    let binding =
      find_module_binding ~cmt_cache ~dir_prefix ~path ~is_toplevel:false ~env
    in
    begin match binding with
    | `Not_found -> `Not_found
    | `Found_type_decl _ -> assert false
    | `Found_pack ident ->
      let path = Path.Pident (Ident.create component) in
      (* [`Found_pack] means that we found a packed module.  In this case, we
         look for the .cmt of the next module down the path in a subdirectory
         corresponding to the packed module's name. *)
      let dir_prefix =
        let name = String.lowercase (Ident.name ident) in
        match dir_prefix with
        | None -> Some name
        | Some prefix -> Some (Filename.concat prefix name)
      in
      find_module_binding ~cmt_cache ~dir_prefix ~path ~is_toplevel:false ~env
    | `Found_module mod_binding ->
      if debug then Printf.printf "find_module_binding: Found_module case\n%!";
      match mod_binding.T.mb_expr.T.mod_desc with
      | T.Tmod_structure structure ->
        let rec traverse_structure ~structure_items =
          match structure_items with
          | [] -> `Not_found
          | structure_item::structure_items ->
            let rec traverse_modules ~mod_bindings =
              match mod_bindings with
              | [] -> `Not_found
              | mod_binding::mod_bindings ->
                if debug then
                  Printf.printf "checking component '%s'... "
                    (Ident.name mod_binding.T.mb_id);
                if (Ident.name mod_binding.T.mb_id) = component then begin
                  if debug then Printf.printf "matches\n%!";
                  `Found_module mod_binding
                end else begin
                  if debug then Printf.printf "does not match\n%!";
                  traverse_modules ~mod_bindings
                end
            in
            match structure_item.T.str_desc with
            | T.Tstr_type type_decls when is_toplevel ->
              let rec traverse_type_decls ~type_decls =
                match type_decls with
                | [] -> `Not_found
                | type_decl::type_decls ->
                  if debug then
                    Printf.printf "checking type decl '%s'... "
                      (Ident.unique_name type_decl.T.typ_id);
                  if (Ident.name type_decl.T.typ_id) = component then
                    `Found_type_decl type_decl
                  else
                    traverse_type_decls ~type_decls
              in
              traverse_type_decls ~type_decls
            | T.Tstr_module mod_binding when not is_toplevel ->
              traverse_modules ~mod_bindings:[mod_binding]
            | T.Tstr_recmodule mod_bindings when not is_toplevel ->
              traverse_modules ~mod_bindings
            | _ -> traverse_structure ~structure_items
        in
        traverse_structure ~structure_items:structure.T.str_items
      | T.Tmod_ident (path, _) ->
        (* This whole function is called when we're trying to find the manifest
           type for an abstract type.  As such, even if we get here and could
           look up types via [path] in the environment, we don't---it
           may well yield another abstract type.  Instead we go straight to
           the implementation, having used the environment only to normalize
           the path. *)
        (* XXX this probably isn't right; we're assuming [path] starts from
           the toplevel.  Should we prepend our [path] so far (from Pdot)?
           Then what happens if it was absolute? *)
        if debug then
          Printf.printf "find_module_binding: 3. path=%s\n%!"
            (print_path path);
        find_module_binding ~cmt_cache ~dir_prefix ~path ~is_toplevel:false
          ~env
      | T.Tmod_functor _
      | T.Tmod_apply _
      | T.Tmod_constraint _
      | T.Tmod_unpack _ -> `Not_found  (* CR mshinwell: handle these cases *)
    end
  | Path.Papply (path1, path2) ->
    `Not_found  (* CR mshinwell: handle this case *)

(* CR mshinwell: we should maybe try lookups via the environment from higher
   up the tree. *)

let find_manifest_of_abstract_type =
  let cmt_cache = Cmt_cache.create () in
  fun ~formatter ~path ~env ->
    if debug then
      Printf.printf "finding abstract type: %s\n%!" (print_path path);
    (* [path] must identify a type declaration.  However, watch out---it may be
       unqualified. *)
    (* XXX how do we cope with the unqualified cases?  Built-in types are one,
       but presumably there may be others?  Not sure. *)
    match path with
    | Path.Pident _ -> None
    | Path.Pdot _ | Path.Papply _ ->
      let binding =
        find_module_binding ~cmt_cache ~dir_prefix:None ~path ~is_toplevel:true
          ~env
      in
      match binding with
      | `Not_found | `Found_pack _ ->
        if debug then Printf.printf "find_manifest: Not_found or pack\n%!";
        None
      | `Found_module _ -> assert false
      | `Found_type_decl type_decl ->
        if debug then Printf.printf "find_manifest: type decl found\n%!";
        Some type_decl.T.typ_type
(*
        let env = structure.T.str_final_env in
        Env.iter_types (fun path1 (path2, (decl, _)) ->
          Format.fprintf formatter "path1=%s path2=%s decl=\n"
            (print_path path1) (print_path path2);
          Printtyp.type_declaration ident formatter decl
        ) env;
        ...this just gives us the abstract type again!
        try begin
          if debug then Printf.printf "find_manifest: env lookup OK\n%!";
          let decl = Env.find_type path env in
          Some decl
        end
        with Not_found -> begin
          if debug then Printf.printf "find_manifest: env lookup failed\n%!";
          None
        end
*)
