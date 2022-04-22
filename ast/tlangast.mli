type t =
  | Const of Valueast.t
  | Loc of int
  | Fun of (Typeast.tvar list * t)
  | Tu of t list
  | Var of Typeast.tvar
  | App of (string * Typeast.tvar list)
  | Ref of t
  | Ite of (t * t * t)
  | Let of (Typeast.tvar list * t * t)
  | Ret of t
(* TODO: match *)
