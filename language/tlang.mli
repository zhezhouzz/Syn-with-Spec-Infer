type t =
  | Const of Value.t
  | Loc of int
  | Fun of (Type.tvar list * t)
  | Tu of t list
  | Var of Type.tvar
  | App of (string * Type.tvar list)
  | Ref of t
  | Ite of (t * t * t)
  | Let of (Type.tvar list * t * t)
  | Ret of t
(* TODO: match *)
