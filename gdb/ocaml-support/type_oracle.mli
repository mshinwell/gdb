(***********************************************************************)
(*                                                                     *)
(*                 Debugger support library for OCaml                  *)
(*                                                                     *)
(*  Copyright 2014--2015, Jane Street Holding                          *)
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

module Variant_kind : sig
  type t
  val to_string_prefix : t -> string
end

type t

val create : search_path:(unit -> string list) -> t

(* Given a value (either boxed or unboxed) read from the target, known as the
   scrutinee, recover as much type information as possible about the value.
   The starting point is taken to be a pair of a type expression and an
   environment, if available, known to correspond to the value.  (Typically
   the scrutinee would be the value of some named identifier, and the type
   along with its environment would be read from the .cmt file in which that
   identifier was declared.) *)
val find_type_information
   : t
  -> formatter:Format.formatter
  -> type_expr_and_env:(Types.type_expr * Env.t) option
  -> scrutinee:Gdb.Obj.t
  -> [ `Obj_boxed_traversable
     | `Obj_boxed_not_traversable
     | `Obj_unboxed
     | `Obj_unboxed_but_should_be_boxed
     | `Abstract of Path.t
     | `Array of Types.type_expr * Env.t
     | `List of Types.type_expr * Env.t
     | `Tuple of Types.type_expr list * Env.t
     | `Char
     | `Int
     | `Float
     | `Float_array
     | `Constant_constructor of string * Variant_kind.t
     | `Non_constant_constructor of Path.t * Types.constructor_declaration list
         * Types.type_expr list * Types.type_expr list * Env.t
         * Variant_kind.t
     | `Record of Path.t * Types.type_expr list * Types.type_expr list
         * Types.label_declaration list * Types.record_representation
         * Env.t
     | `Open
     | `Ref of Types.type_expr * Env.t
     | `String
     | `Closure
     | `Lazy
     | `Object
     | `Abstract_tag
     | `Custom
     ]
