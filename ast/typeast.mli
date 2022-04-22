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

type tvar = t * string [@@deriving sexp]
