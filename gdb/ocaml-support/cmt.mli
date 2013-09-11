type t

val create_null : unit -> t
val load : filename:string -> t

val type_of_ident : t -> unique_name:string -> (Types.type_expr * Env.t) option

