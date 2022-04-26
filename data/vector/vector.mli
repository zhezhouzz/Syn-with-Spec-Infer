type t = { dummy : int; mutable size : int; mutable data : int array }

val create : int -> int -> t ref

val copy : t ref -> t ref -> unit

val merge_right : t ref -> t ref -> unit
