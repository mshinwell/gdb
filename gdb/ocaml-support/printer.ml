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

module Variant_kind = Type_oracle.Variant_kind

let debug = try Sys.getenv "GOS_DEBUG" <> "" with Not_found -> false

let extract_non_constant_ctors ~cases =
  let non_constant_ctors, _ =
    List.fold_left cases
      ~init:([], 0)
      ~f:(fun (non_constant_ctors, next_ctor_number) ctor_decl ->
            let ident = ctor_decl.Types.cd_id in
            match ctor_decl.Types.cd_args with
            | [] ->
              non_constant_ctors, next_ctor_number
            | _ ->
              (* CR mshinwell: check [return_type] is just that, and use it.  Presumably
                 for GADTs. *)
              (next_ctor_number, (ident, ctor_decl.Types.cd_args))::non_constant_ctors,
                next_ctor_number + 1)
  in
  non_constant_ctors

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

let default_printer ?(separator = ",") ?prefix ~force_never_like_list
      ~printers ~formatter value =
  (* See if the value looks like a list.  If it does, then print it as a list; it seems
     far more likely to be one of those than a set of nested pairs. *)
  if value_looks_like_list value && not force_never_like_list then
    `Looks_like_list
  else begin
    begin match prefix with
    | None -> Format.fprintf formatter "@[<1>[%d: " (Gdb.Obj.tag value)
    (* CR mshinwell: remove dreadful hack *)
    | Some "XXX" -> ()
    | Some p -> Format.fprintf formatter "@[%s " p
    end;
    for field = 0 to Gdb.Obj.size value - 1 do
      if field > 0 then Format.fprintf formatter "%s@;<1 0>" separator;
      try printers.(field) (Gdb.Obj.field value field)
      with Gdb.Read_error _ ->
        Format.fprintf formatter "<field %d read failed>" field
    done;
    begin match prefix with
    | None -> Format.fprintf formatter "]@]"
    | Some "XXX" -> ()
    | Some p -> Format.fprintf formatter "@]"
    end;
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
  Format.fprintf formatter "@[<hv>[" ; aux l ; Format.fprintf formatter "]@]"

let record ~fields_helpers ~formatter r =
  let nb_fields = Gdb.Obj.size r in
  Format.fprintf formatter "@[<v 0>{ ";
  for field_nb = 0 to nb_fields - 1 do
    if field_nb > 0 then Format.fprintf formatter "@   ";
    try
      let v = Gdb.Obj.field r field_nb in
      let (field_name, printer) = fields_helpers.(field_nb) in
      Format.fprintf formatter "@[<2>%s@ =@ " field_name;
      printer v;
      Format.fprintf formatter ";@]"
    with Gdb.Read_error _ ->
      Format.fprintf formatter "<could not read field %d>" field_nb
  done;
  Format.fprintf formatter "@ }@]"

let rec print_path = function
  | Path.Pident ident -> Ident.name ident
  | Path.Pdot (path, s, _) -> (print_path path) ^ "." ^ "<string: " ^ s ^ " >"
  | Path.Papply (path1, path2) ->
    (print_path path1) ^ " applied to " ^ (print_path path2)

module T = Typedtree

let rec value ?(depth=0) ?(print_sig=true) ~type_of_ident:type_expr_and_env
      ~summary ~formatter v =
(*  Format.fprintf formatter "@[";*)
  (* CR mshinwell: [force_never_like_list] is a hack.  Seems like the list guessing
     should only be applied in a couple of cases, so maybe invert the flag? *)
  let default_printer ?separator ?(force_never_like_list = false) ?prefix
      ~printers ~formatter v =
    match default_printer ?separator ?prefix ~force_never_like_list ~printers ~formatter v with
    | `Done -> ()
    | `Looks_like_list ->
      if summary then
        Format.fprintf formatter "[...]"
      else begin
        let print_element =
          value ~depth:(succ depth) ~print_sig:false ~formatter ~type_of_ident:None
            ~summary
        in
        list ~print_element ~formatter v;
        Format.fprintf formatter "?"
      end
  in
  begin if (summary && depth > 2) || depth > 5 then
    Format.fprintf formatter ".."
  else
    let type_info =
      Type_oracle.find_type_information ~formatter ~type_expr_and_env ~scrutinee:v
    in
    let default_printers =
      lazy (
        Array.init (Gdb.Obj.size v) ~f:(fun _ v ->
          value ~depth:(succ depth) ~print_sig ~type_of_ident:None ~summary
            ~formatter v
        )
      )
    in
    match type_info with
    | `Obj_unboxed | `Int -> Format.fprintf formatter "%Ld" v
    | `Char ->
      let value = Gdb.Obj.int v in
      if value >= 0 && value <= 255 then
        Format.fprintf formatter "'%s'" (Char.escaped (Char.chr value))
      else
        Format.fprintf formatter "%Ld" v
    | `Obj_boxed_traversable ->
      if summary then
        Format.fprintf formatter "..."
      else
        default_printer ~printers:(Lazy.force default_printers) ~formatter v
    | `Obj_boxed_not_traversable ->
      Format.fprintf formatter "<0x%Lx, tag %d>" v (Gdb.Obj.tag v)
    | `Abstract path ->
      Format.fprintf formatter "<%s>" (Path.name path)
    | `Array (ty, env) ->
      if summary then
        Format.fprintf formatter "[|...|]"
      else begin
        Format.fprintf formatter "[| ";
        let printers =
          Array.init (Gdb.Obj.size v) ~f:(fun _ v ->
            let type_of_ident = Some (ty, env) in
            value ~depth:(succ depth) ~print_sig
              ~type_of_ident ~summary ~formatter v
          )
        in
        default_printer ~separator:";" ~printers ~prefix:"XXX"
          ~force_never_like_list:true ~formatter v;
        Format.fprintf formatter " |]"
      end
    | `List (ty, env) ->
      let print_element =
        value ~depth:(succ depth) ~print_sig:false ~formatter
          ~type_of_ident:(Some (ty, env)) ~summary
      in
      list ~print_element ~formatter v
    | `Ref (ty, env) ->
      Format.fprintf formatter "ref ";
      value ~depth:(succ depth) ~type_of_ident:(Some (ty, env))
        ~print_sig:false ~summary ~formatter (Gdb.Obj.field v 0)
    | `Tuple (tys, env) ->
      if summary && (List.length tys > 2 || depth > 0) then
        Format.fprintf formatter "(...)"
      else
        let component_types = Array.of_list tys in
        let size_ok = Array.length component_types = Gdb.Obj.size v in
        let printers =
          Array.map component_types ~f:(fun ty v ->
            let type_of_ident = if size_ok then Some (ty, env) else None in
            value ~depth:(succ depth) ~print_sig:false
              ~type_of_ident ~summary ~formatter v
          )
        in
        if List.length tys > 1 then Format.fprintf formatter "(";
        default_printer ~printers ~prefix:"XXX" ~force_never_like_list:true
          ~formatter v;
        if List.length tys > 1 then Format.fprintf formatter ")"
    | `Constant_constructor (name, kind) ->
      Format.fprintf formatter "%s%s" (Variant_kind.to_string_prefix kind) name
    | `Non_constant_constructor
        (path, ctor_decls, params, instantiated_params, env, kind) ->
      let kind = Variant_kind.to_string_prefix kind in
      if debug then begin
        List.iter params ~f:(fun ty ->
          Format.fprintf formatter "param>>";
          Printtyp.reset_and_mark_loops ty;
          Printtyp.type_expr formatter ty;
          Format.fprintf formatter "<<");
        List.iter instantiated_params ~f:(fun ty ->
          Format.fprintf formatter "iparam>>";
          Printtyp.reset_and_mark_loops ty;
          Printtyp.type_expr formatter ty;
          Format.fprintf formatter "<<")
      end;
      let non_constant_ctors = extract_non_constant_ctors ctor_decls in
      let ctor_info =
        let tag = Gdb.Obj.tag v in
        try Some (List.assoc tag non_constant_ctors) with Not_found -> None
      in
      begin match ctor_info with
      | None ->
        default_printer ~printers:(Lazy.force default_printers) ~formatter v
      | Some (cident, args) ->
        if summary || List.length args <> Gdb.Obj.size v then begin
          Format.fprintf formatter "%s%s (...)" kind (Ident.name cident)
        end else begin
          let printers =
            let args = Array.of_list args in
            Array.map args ~f:(fun arg_ty v ->
              if debug then begin
                Format.fprintf formatter "arg>>";
                Printtyp.reset_and_mark_loops arg_ty;
                Printtyp.type_expr formatter arg_ty;
                Format.fprintf formatter "<<"
              end;
              let arg_ty =
                try Ctype.apply env params arg_ty instantiated_params
                with Ctype.Cannot_apply -> arg_ty
              in
              value ~depth:(succ depth) ~type_of_ident:(Some (arg_ty, env))
                ~print_sig:false ~formatter v
                ~summary
            )
          in
          let prefix =
            let name = Ident.name cident in
            if List.length args > 1 then Printf.sprintf "%s%s (" kind name
            else name
          in
          default_printer ~printers ~prefix ~formatter v
            ~force_never_like_list:true;
          if List.length args > 1 then Format.fprintf formatter ")"
        end
      end
    | `Record (path, params, args, fields, record_repr, env) ->
      if summary then
        Format.fprintf formatter "{...}"
      else begin
        let fields = Array.of_list fields in
        let type_for_field ~index =
          begin match record_repr with
          | Types.Record_float -> Predef.type_float
          | Types.Record_regular ->
            let field_type = fields.(index).Types.ld_type in
            try Ctype.apply env params field_type args
            with Ctype.Cannot_apply -> field_type
          end
        in
        let fields_helpers =
          Array.mapi fields ~f:(fun index ld ->
            let typ = type_for_field ~index in
            let printer v =
              value ~depth:(succ depth)
                ~type_of_ident:(Some (typ, env))
                ~print_sig:false ~summary ~formatter v
            in
            Ident.name ld.Types.ld_id, printer
          )
        in
        if depth = 0 && Array.length fields > 1 then begin
          Format.pp_print_newline formatter ();
          Format.fprintf formatter "@[<v>  "
        end;
        record ~fields_helpers ~formatter v;
        if depth = 0 && Array.length fields > 1 then begin
          Format.fprintf formatter "@]"
        end
      end
    | `Open ->
      Format.fprintf formatter "<value of open type>"
    | `String ->
      Format.fprintf formatter "%S" (Gdb.Obj.string v)
    | `Float ->
      begin try 
        Format.fprintf formatter "%f" (Gdb.Target.read_double v)
      with Gdb.Read_error _ ->
        Format.fprintf formatter "<double read failed>"
      end
    | `Float_array ->
      Format.fprintf formatter "<float array>"
    | `Closure ->
      begin try
        if summary then
          Format.fprintf formatter "<fun>"
        else begin
          (* First we try to find out what this function is called.  If it's
             one of the special currying wrappers then we try to look in the
             closure's environment to find the "real" function pointer. *)
          let pc = Gdb.Target.read_field v 0 in
          let name = Gdb.Priv.gdb_function_linkage_name_at_pc pc in
          let is_currying_wrapper =
            match name with
            | None -> false  (* name not found; assume it isn't a wrapper *)
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
              (* The "real" function pointer should be the last entry in the
                 environment of the closure. *)
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
        Format.fprintf formatter
          "closure, possibly corrupted (code pointer read failed)"
      end
    | `Lazy ->
      Format.fprintf formatter "<lazy>"
    | `Object ->
      Format.fprintf formatter "<object>"
    | `Abstract_tag ->
      Format.fprintf formatter "<block with Abstract_tag>"
    | `Custom ->
      Format.fprintf formatter "<custom block>"
  end;
  if print_sig then begin
    match type_expr_and_env with
    | None -> ()
    | Some (type_expr, _env) ->
      Format.fprintf formatter " : ";
      (* CR mshinwell: override with e.g. "string" if that's what it was, and
         [type_expr] isn't helpful *)
      Printtyp.reset_and_mark_loops type_expr;
      Printtyp.type_expr formatter type_expr
  end
(*  Format.fprintf formatter "@]"*)

let value ?depth ?print_sig ~type_of_ident ~summary out v =
  if debug then Printf.printf "Printer.value entry point\n%!";
  let formatter =
    Format.make_formatter
      (fun str pos len -> Gdb.print out (String.sub str pos len))
      (fun () -> ())
  in
  Format.fprintf formatter "@[";
  value ?depth ?print_sig ~type_of_ident ~summary ~formatter v;
  Format.fprintf formatter "@]";
  Format.pp_print_flush formatter ()
