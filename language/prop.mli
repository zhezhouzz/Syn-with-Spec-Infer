type t =
  | True
  | Bvar of string
  | Implies of t * t
  | Ite of t * t * t
  | Not of t
  | And of t list
  | Or of t list
  | Iff of t * t
  | Pred of string * Typedvar.t list

val layout : t -> string

val pretty_layout : t -> string

val subst : (Typedvar.t -> Typedvar.t) -> t -> t
