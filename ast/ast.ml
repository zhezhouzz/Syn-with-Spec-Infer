module type Type = sig
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
end

module T : Type = struct
  open Sexplib.Std

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
end

module type Value = sig
  type t =
    | U
    | I of int
    | B of bool
    | IL of int list
    | IT of int Datatype.Tree.t
    | Tu of t list
    | NotADt
  [@@deriving sexp]
end

module V : Value = struct
  open Sexplib.Std

  type t =
    | U
    | I of int
    | B of bool
    | IL of int list
    | IT of int Datatype.Tree.t
    | Tu of t list
    | NotADt
  [@@deriving sexp]
end

module type Prop = sig
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
  [@@deriving sexp]
end

module P : Prop = struct
  open Sexplib.Std

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
  [@@deriving sexp]
end

module type FOL = sig
  type uf = T.tvar list * P.t [@@deriving sexp]

  type spec = {
    name : string;
    args : T.tvar list;
    qv : T.tvar list;
    body : P.t;
  }
  [@@deriving sexp]
end

module F : FOL = struct
  open Sexplib.Std

  type uf = T.tvar list * P.t [@@deriving sexp]

  type spec = {
    name : string;
    args : T.tvar list;
    qv : T.tvar list;
    body : P.t;
  }
  [@@deriving sexp]
end

module type Slang = sig
  type t =
    | Const of V.t
    | Loc of int
    | Fun of (T.tvar list * t)
    | Tu of t list
    | Var of T.tvar
    | App of (string * T.tvar list)
    | Ref of t
    | Ite of (t * t * t)
    | Let of (T.tvar list * t * t)
    | Ret of t
  (* TODO: match *)
  [@@deriving sexp]
end

module S : Slang = struct
  open Sexplib.Std

  type t =
    | Const of V.t
    | Loc of int
    | Fun of (T.tvar list * t)
    | Tu of t list
    | Var of T.tvar
    | App of (string * T.tvar list)
    | Ref of t
    | Ite of (t * t * t)
    | Let of (T.tvar list * t * t)
    | Ret of t
  (* TODO: match *)
  [@@deriving sexp]
end
