type t

val create_null : unit -> t
val load : filename:string -> t

val type_of_ident : t -> unique_name:string -> (Types.type_expr * Env.t) option

val find_argument_types : t -> source_file_of_call_site:string ->
  line_number_of_call_site:int ->
  ([ `Recover_label_ty of string | `Ty of Types.type_expr ] list * Env.t) option
