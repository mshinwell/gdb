(***********************************************************************)
(*                                                                     *)
(*                 Debugger support library for OCaml                  *)
(*                                                                     *)
(*  Copyright 2015, Jane Street Holding                                *)
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

module type S = sig
  (* Values that have been read from the program being debugged. *)
  module Obj : sig
    type t
    exception Read_error of ??
    val tag : t -> int
    val size : t -> int
    val field : t -> int -> t
    val is_block : t -> bool
  end

  (* Access to the memory of the program being debugged. *)
  module Target : sig
    val read_field : Obj.t -> int -> Obj.t
  end

  (* Display of text to the user of the debugger. *)
  val print : ??? -> string -> unit
  module Stream : sig
    type t
    val to_formatter : t -> Format.formatter
  end

  (* The linkage name of the function containing the program counter
     value [pc]. *)
  val linkage_name_at_pc : pc:Obj.t -> string

  (* As much information as known about the filename and line number
     of the source text that was compiled to program counter [pc]. *)
  val filename_and_line_number_of_pc
     : pc:Obj.t
    -> (string * (int option)) option

  (* User preferences set in the debugger. *)
  val max_array_etc_elements : unit -> int
  val max_depth : unit -> int
  val cmt_search_path : unit -> string list
end
