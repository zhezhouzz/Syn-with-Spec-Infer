open Datatype

type t =
  | U
  | I of int
  | B of bool
  | IL of int list
  | IT of int Tree.t
  | Tu of t list
  | NotADt
[@@deriving sexp]

val layout : t -> string

val layout_l : t list -> string

val compare : t -> t -> int

val eq : t -> t -> bool

val flatten : t -> int list

val flatten_l : t list -> int list

val size : t -> int

val layout_size_l : t list -> string

val get_tp : t -> Type.t

val get_tp_l : t list -> Type.t list
