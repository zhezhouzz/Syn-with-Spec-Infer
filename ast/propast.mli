type t =
  | True
  | Bvar of string
  | Implies of t * t
  | Ite of t * t * t
  | Not of t
  | And of t list
  | Or of t list
  | Iff of t * t
  | Pred of string * string list
