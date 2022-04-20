type t = Type.t * string

val layout : t -> string

val layout_l : t list -> string

val compare : t -> t -> int

val eq : t -> t -> bool

val fresh_from_type : Type.t -> t
