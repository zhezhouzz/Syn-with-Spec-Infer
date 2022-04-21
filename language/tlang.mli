type t =
  | Const of Value.t
  | Loc of int
  | Fun of (Typedvar.t list * t)
  | Tu of t list
  | Var of Typedvar.t
  | App of (string * Typedvar.t list)
  | Ref of t
  | Ite of (t * t * t)
  | Let of (Typedvar.t list * t * t)
  | Ret of t
(* TODO: match *)
