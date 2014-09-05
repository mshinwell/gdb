(***********************************************************************)
(*                                                                     *)
(*                 Debugger support library for OCaml                  *)
(*                                                                     *)
(*  Copyright 2014, Jane Street Holding                                *)
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

val find_type_information
   : formatter:Format.formatter
  -> type_expr_and_env:(Types.type_expr * Env.t) option
  -> scrutinee:Gdb.Obj.t
  -> [ `Obj_boxed_traversable
     | `Obj_boxed_not_traversable
     | `Obj_unboxed
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
