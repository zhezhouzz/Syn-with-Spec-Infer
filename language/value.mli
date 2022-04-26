include Ast.Value with type t = Ast.V.t

val compare : t -> t -> int

val eq : t -> t -> bool

val flatten : t -> int list

val flatten_l : t list -> int list

val size : t -> int

val layout_size_l : t list -> string

val get_tp : t -> Type.t

val get_tp_l : t list -> Type.t list

val layout : t -> string

val layout_l : t list -> string

val remove_duplicates : t list -> t list

val remove_duplicates_l : t list list -> t list list

val remove_duplicates_arr : t list array -> int list -> int list

val intersection :
  t list list -> t list list -> t list list * t list list * t list list
