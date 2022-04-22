type t =
  (* basic types *)
  | Unit
  | Bool
  | Int
  | Nat
  (* function type *)
  | Arr of t * t
  (* reference *)
  | Ref of t
  (* datatypes *)
  | List of t
  | Tree of t
  | Tuple of t list
  | Uninterp of string
[@@deriving sexp]

val to_string : t -> string

val of_string : string -> t

val layout : t -> string

val layout_short : t -> string

val layout_l : t list -> string

val compare : t -> t -> int

val eq : t -> t -> bool

val is_dt : t -> bool

val is_basic : t -> bool
