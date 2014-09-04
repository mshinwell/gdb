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

val boxed_find_type_information
   : formatter:Format.formatter
  -> type_expr_and_env:(Types.type_expr * Env.t) option
  -> tag:int
  -> [ `Obj
     | `Obj_not_traversable
     | `Abstract of Path.t
     | `Array of Types.type_expr * Env.t
     | `List of Types.type_expr * Env.t
     | `Tuple of Types.type_expr list * Env.t
     | `Constructed of Path.t * Types.constructor_declaration list
         * Types.type_expr list * Types.type_expr list * Env.t
     | `Record of Path.t * Types.type_expr list * Types.type_expr list
         * Types.label_declaration list * Types.record_representation
         * Env.t
     | `Open
     | `Ref of Types.type_expr * Env.t
     | `String
     | `Float
     | `Float_array
     | `Closure
     | `Lazy
     | `Object
     | `Abstract_tag
     | `Custom
     ]

val print_int
   : Gdb.Obj.t
  -> type_of_ident:(Types.type_expr * Env.t) option
  -> formatter:Format.formatter
  -> unit
