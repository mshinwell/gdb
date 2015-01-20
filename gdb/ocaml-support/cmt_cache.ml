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

type cmt = Cmi_format.cmi_infos option * Cmt_format.cmt_infos option

type t = {
  search_path : (unit -> string list);
  mutable most_recent_search_path : string list option;
  cache : (string, cmt) Hashtbl.t;
}

let create ~search_path =
  { search_path;
    most_recent_search_path = None;
    cache = Hashtbl.create 42;
  }

(* Since we don't yet check digests of .cmt files, we flush the cache
   whenever the search path is changed. *)
let check_for_search_path_updates t =
  let current_search_path = (t.search_path) () in
  let should_flush =
    match t.most_recent_search_path with
    | None -> true
    | Some most_recent_search_path ->
      current_search_path <> most_recent_search_path
  in
  if should_flush then begin
    Hashtbl.clear t.cache;
    t.most_recent_search_path <- Some current_search_path
  end

let read t ~unit_name =
  check_for_search_path_updates t;
  try Some (Hashtbl.find t.cache unit_name)
  with Not_found ->
    match t.most_recent_search_path with
    | None -> assert false
    | Some search_path ->
      match Misc.find_in_path_uncap search_path (unit_name ^ ".cmt") with
      | exception Not_found -> None
      | pathname ->
        match Cmt_format.read pathname with
        | exception _exn -> None  (* CR mshinwell: improve error reporting *)
        | cmt ->
          Hashtbl.add t.cache unit_name cmt;
          Some cmt
