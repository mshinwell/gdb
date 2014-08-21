module Array = ArrayLabels
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

  (* CR mshinwell: this function does not belong in this module *)
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
(*
  let split_gen str ~on =
    let is_delim =
      match on with
      | `char c' -> (fun c -> c = c')
      | `char_list l -> (fun c -> char_list_mem l c)
    in
    let len = String.length str in
    let rec loop acc last_pos pos =
      if pos = -1 then
        String.sub str ~pos:0 ~len:last_pos :: acc
      else
        if is_delim str.[pos] then
          let pos1 = pos + 1 in
          let sub_str = String.sub str ~pos:pos1 ~len:(last_pos - pos1) in
          loop (sub_str :: acc) pos (pos - 1)
      else loop acc last_pos (pos - 1)
    in
    loop [] len (len - 1)
  ;;

  let split str ~on = split_gen str ~on:(`char on) ;;
*)
  module Map = Map.Make (struct
    type t = string
    let compare = compare
  end)
end

let fst3 (a, _b, _c) = a
let trd3 (_a, _b, c) = c
