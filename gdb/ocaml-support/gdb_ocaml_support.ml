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

(* CR mshinwell: transition to using [Core_kernel] *)

module List = ListLabels
module String = struct
  include StringLabels

  (* [is_suffix s ~suff] returns [true] if the string [s] ends with the suffix [suff] *)
  let is_suffix s ~suffix =
    let len_suff = String.length suffix in
    let len_s = String.length s in
    len_s >= len_suff
    && (let rec loop i =
          i = len_suff || (suffix.[len_suff - 1 - i] = s.[len_s - 1 - i] && loop (i + 1))
        in
        loop 0)

  let is_prefix s ~prefix =
    let len_pref = String.length prefix in
    String.length s >= len_pref
    && (let rec loop i =
          i = len_pref || (prefix.[i] = s.[i] && loop (i + 1))
        in
        loop 0)

  let drop_suffix str n =
    try sub str ~pos:0 ~len:(length str - n)
    with _ -> ""

  (* Examples of stamped names:
        x_1024     variable, Ident.t unique name x/1024
        x_1024-0   first function argument, Ident.t unique name x/1024
        x_1024-1   second function argument, Ident.t unique name x/1024
  *)
  let drop_stamp str =
    let len = length str - 1 in
    let is_digit_or_dash c = (c >= '0' && c <= '9') || (c = '-') in
    let rec find_stamp_len i =
      if i <= 0 then 0
      else if str.[i] = '_' then len - i + 1
      else if not (is_digit_or_dash str.[i]) then 0
      else find_stamp_len (i - 1)
    in
    drop_suffix str (find_stamp_len len)
end

let strip_parameter_index_from_unique_name unique_name =
  match try Some (String.rindex unique_name '-') with Not_found -> None with
  | None -> unique_name
  | Some dash ->
    match try Some (String.rindex unique_name '_') with Not_found -> None with
    | None -> unique_name
    | Some underscore ->
      if underscore < dash then begin
        let stamp_ok =
          try
            ignore (int_of_string (String.sub unique_name
                (dash + 1) ((String.length unique_name) - (dash + 1))));
            true
          with Failure _ -> false
        in
        if stamp_ok then
          String.sub unique_name 0 dash
        else
          unique_name
      end
      else
        unique_name

let parameter_index_of_unique_name unique_name =
  match try Some (String.rindex unique_name '-') with Not_found -> None with
  | None -> None
  | Some dash ->
    match try Some (String.rindex unique_name '_') with Not_found -> None with
    | None -> None
    | Some underscore ->
      if underscore < dash then
        try
          Some (int_of_string (String.sub unique_name
              (dash + 1) ((String.length unique_name) - (dash + 1))))
        with Failure _ -> None
      else
        None

(* CR mshinwell: we should have proper abstract types for the stamped names *)

module Cmt_file = Cmt

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

let rec val_print ~depth v out ~type_of_ident ~don't_print_type = (* CR mshinwell: rename [type_of_ident] *)
  if depth > 2 then Gdb.print out ".." else begin
    if Gdb.Obj.is_int v
    then val_print_int ~gdb_stream:out ~type_of_ident ~value:v
    else begin
      if Gdb.Obj.is_block v then
      begin match Gdb.Obj.tag v with
        | tag when tag < Gdb.Obj.closure_tag  ->
          begin
            let print_list v ~type_of_elements =
              Gdb.print out "[";
              let rec print_element v =
                if Gdb.Obj.is_block v then begin
                  try
                    let elt = Gdb.Obj.field v 0 in
                    let next = Gdb.Obj.field v 1 in
                    val_print ~depth:(succ depth) elt out ~type_of_ident:type_of_elements
                      ~don't_print_type:true;
                    if Gdb.Obj.is_block next then Gdb.print out "; ";
                    print_element next
                  with Gdb.Read_error _ ->
                    Gdb.print out "<list element read failed>"
                end
              in
              print_element v;
              Gdb.print out "]"
            in
            let default ?prefix_with ?type_of_field ?(don't_print_type = false) () =
              begin match prefix_with with
              | None -> if tag > 0 then Gdb.printf out "tag %d:" tag
              | Some prefix_with -> Gdb.printf out "%s " prefix_with
              end;
              Gdb.print out "(";
              for field = 0 to Gdb.Obj.size v - 1 do
                if field > 0 then Gdb.print out ", ";
                try 
                  let v' = Gdb.Obj.field v field in
                  let type_of_field =
                    match type_of_field with
                    | None -> None
                    | Some type_of_field -> Some (type_of_field ~field_number:field)
                  in
                  val_print ~depth:(succ depth) v' out ~type_of_ident:type_of_field
                    ~don't_print_type
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
                | Types.Tlink type_expr -> identify_value type_expr
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
                  let num_fields = Gdb.Obj.size v in
                  if num_fields > 1 then Gdb.print_endline out;
                  (* CR mshinwell: adapt [default] for this *)
                  for field = 0 to num_fields - 1 do
                    if field > 0 then begin
                      Gdb.print out ",";
                      Gdb.print_endline out;
                    end;
                    try 
                      let v' = Gdb.Obj.field v field in
                      let (field_name, _mutable, field_type) = field_decls.(field) in
                      if num_fields > 1 then Gdb.print out "    ";
                      Gdb.printf out "%s = " (Ident.name field_name);
                      val_print ~depth:(succ depth) v' out
                        ~type_of_ident:(Some (field_type, env))
                        ~don't_print_type
                    with Gdb.Read_error _ ->
                      Gdb.printf out "<field %d read failed>" field
                  done;
                  if num_fields > 1 then Gdb.print_endline out;
                  Gdb.print out "  }"
                end
              | `Constructed_value (cases, args) ->
                let non_constant_ctors = extract_non_constant_ctors cases in
                let ctor_info =
                  try Some (List.assoc tag non_constant_ctors) with Not_found -> None
                in
                begin match ctor_info with
                | None -> default ()
                | Some (ctor_ident, [_element_tyvar; _list_type])
                  (* CR mshinwell: Probably need to check that the type name is
                     [Pervasives.list], or something?  Use [path], above. *)
                  when Ident.name ctor_ident = "::" && List.length args = 1 ->
                  begin match args with
                  | [element_type] ->
                    print_list v ~type_of_elements:(Some (element_type, env))
                  | _ -> assert false
                  end
                | Some (ctor_ident, arg_types) ->
                  let arg_types = Array.of_list arg_types in
                  let type_of_field ~field_number =
                    assert (field_number >= 0 && field_number < Array.length arg_types);
                    arg_types.(field_number), env
                  in
                  default () ~prefix_with:(Ident.name ctor_ident) ~type_of_field
                    ~don't_print_type:true
                end
              | `Tuple component_types ->
                let component_types = Array.of_list component_types in
                  let type_of_field ~field_number =
                    assert (field_number >= 0
                        && field_number < Array.length component_types);
                    component_types.(field_number), env
                  in
                  default () ~type_of_field ~don't_print_type:true
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
    if not don't_print_type then begin
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
    end
  end

let rec type_is_polymorphic type_expr =
  let rec check_type_desc = function
    | Types.Tvar _ -> true
    | Types.Tarrow (_, t1, t2, _) -> type_is_polymorphic t1 || type_is_polymorphic t2
    | Types.Tconstr (_, tys, _)
    | Types.Ttuple tys -> List.exists tys ~f:type_is_polymorphic
    | Types.Tobject _ -> true (* CR mshinwell: fixme *)
    | Types.Tnil -> false
    | Types.Tlink ty
    | Types.Tsubst ty -> type_is_polymorphic ty
    | Types.Tfield _ -> true (* CR mshinwell: fixme *)
    | Types.Tvariant _ -> true (* CR mshinwell: fixme *)
    | Types.Tunivar _ -> false
    | Types.Tpoly _ -> true (* CR mshinwell: fixme *)
    | Types.Tpackage _ -> true (* CR mshinwell: fixme *)
  in
  check_type_desc type_expr.Types.desc

module Call_site = struct
  type t =
  | None
  | Some of string * int
end

(* CR mshinwell: there are THREE functions by the name of [val_print] now *)
let val_print ~depth v out ~symbol_linkage_name ~cmt_file ~call_site =
  let type_of_ident =
    match symbol_linkage_name with
    | None -> None
    | Some symbol_linkage_name ->
      let action =
        let unique_name = strip_parameter_index_from_unique_name symbol_linkage_name in
        match Cmt_file.type_of_ident cmt_file ~unique_name with
        | None -> `Try_call_site_but_fallback_to None
        | Some (type_expr, env) ->
          if type_is_polymorphic type_expr then
            `Try_call_site_but_fallback_to (Some (type_expr, env))
          else
            `Have_type (type_expr, env)
      in
      match action with
      | `Have_type (type_expr, env) -> Some (type_expr, env)
      | `Try_call_site_but_fallback_to fallback ->
        match call_site with
        | Call_site.None -> fallback
        | Call_site.Some (source_file, line_number) ->
          let from_call_site =
            Cmt_file.find_argument_types cmt_file
              ~source_file_of_call_site:source_file
              ~line_number_of_call_site:line_number
          in
          match from_call_site with
          | None -> fallback
          | Some (type_exprs, env) ->
            match parameter_index_of_unique_name symbol_linkage_name with
            | None -> fallback
            | Some parameter_index ->
              if parameter_index < 0 || parameter_index >= List.length type_exprs then
                fallback
              else
                match (Array.of_list type_exprs).(parameter_index) with
                | `Ty type_expr -> Some (type_expr, env)
                | `Recover_label_ty label ->
                  (* CR mshinwell: need to fix this.  Unfortunately [label] is not
                     stamped, which might make it troublesome to find the correct
                     identifier. *)
                  fallback
  in
  val_print ~depth v out ~type_of_ident ~don't_print_type:false

let decode_dwarf_type dwarf_type =
  (* CR mshinwell: use [Core_kernel] and rewrite with sensible string
     functions *)
  let magic = "__ocaml" in
  if String.length dwarf_type <= String.length magic then
    None, None
  else
    let delimiter =
      try Some (String.rindex dwarf_type ' ') with Not_found -> None
    in
    match delimiter with
    | None -> None, None
    | Some delimiter when delimiter <= String.length magic -> None, None
    | Some delimiter ->
      let source_file_path =
        String.sub dwarf_type (String.length magic)
          (delimiter - (String.length magic))
      in
      let symbol_linkage_name =
        String.sub dwarf_type (delimiter + 1)
          ((String.length dwarf_type) - (delimiter + 1))
      in
      Some source_file_path, Some symbol_linkage_name

let cmt_file_of_source_file_path ~source_file_path =
  (* CR mshinwell: This desperately needs to cache the result. *)
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

let val_print addr stream ~dwarf_type ~call_site =
  let source_file_path, symbol_linkage_name = decode_dwarf_type dwarf_type in
  let cmt_file = cmt_file_of_source_file_path ~source_file_path in
(*
  begin match call_site with
  | Call_site.None -> Printf.printf "no call point info\n%!"
  | Call_site.Some (file, line) -> Printf.printf "call point: %s:%d\n%!" file line
  end;
*)
  val_print ~depth:0 addr stream ~symbol_linkage_name ~cmt_file ~call_site

let () = Callback.register "gdb_ocaml_support_val_print" val_print

let demangle mangled =
  if
    String.is_suffix mangled ~suffix:"__frametable" ||
    String.is_suffix mangled ~suffix:"__begin" ||
    String.is_suffix mangled ~suffix:"__end"
  then
    mangled
  else
    let str = String.copy mangled in
    let rec loop i j =
      if j >= String.length str then
        i
      else if str.[j] = '_' && j + 1 < String.length str && str.[j + 1] = '_' then (
        (* So, here is the funny part: there's no way to distinguish between "__" inserted
            by [Compilenv.make_symbol] (see asmcomp/compilenv.ml) and names containing
            "__".
            We are just going to assume that people never use "__" in their name (although
            we know for a fact that this happens in Core.) *)
        (* CR mshinwell: fix the above *)
        str.[i] <- '.' ;
        loop (i + 1) (j + 2)
      ) else (
        str.[i] <- str.[j] ;
        loop (i + 1) (j + 1)
      )
    in
    let len = loop 0 0 in
    String.sub str ~pos:0 ~len

let demangle mangled_name =
  let is_caml_name =
    String.length mangled_name > 4 &&
    String.is_prefix mangled_name ~prefix:"caml" &&
    (* Beware: really naive *)
    mangled_name.[4] <> '_' &&
    mangled_name.[4] = (Char.uppercase mangled_name.[4])
  in
  let maybe_stamped =
    if not is_caml_name then mangled_name else
    demangle (String.sub mangled_name ~pos:4 ~len:(String.length mangled_name - 4))
  in
  let unstamped = String.drop_stamp maybe_stamped in
  if String.is_suffix unstamped ~suffix:"anon_fun" then
    maybe_stamped
  else
    unstamped

let () = Callback.register "gdb_ocaml_support_demangle" demangle

let print_type ~dwarf_type ~out =
  let source_file_path, symbol_linkage_name = decode_dwarf_type dwarf_type in
  (* CR mshinwell: we can share some of this code with above. *)
  let status =
    match symbol_linkage_name with
    | None ->
      Printf.printf "print_type: can't find symbol linkage name\n%!";
      `Unknown
    | Some symbol_linkage_name ->
      Printf.printf "print_type: linkage name='%s'\n%!" symbol_linkage_name;
      let cmt_file = cmt_file_of_source_file_path ~source_file_path in
      let type_of_ident =
        Cmt_file.type_of_ident cmt_file ~unique_name:symbol_linkage_name
      in
      match type_of_ident with
      | None ->
        Printf.printf "print_type: '%s' not found in cmt\n%!" symbol_linkage_name;
        `Unknown
      | Some (type_expr, _env) -> `Ok type_expr
  in
  match status with
  | `Unknown -> Gdb.print out "<unknown>"
  | `Ok type_expr ->
    let formatter =
      Format.make_formatter
        (fun str pos len -> Gdb.print out (String.sub str pos len))
        (fun () -> ())
    in
    Printtyp.type_expr formatter type_expr;
    Format.pp_print_flush formatter ()

let () = Callback.register "gdb_ocaml_support_print_type" print_type
