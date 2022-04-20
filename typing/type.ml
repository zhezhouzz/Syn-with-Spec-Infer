open Sexplib.Std

type t =
  (* basic types *)
  | Unit
  | Bool
  | Int
  | Nat
  (* reference *)
  | Ref of t
  (* datatypes *)
  | List of t
  | Tree of t
  | Tuple of t list
  | Uninterp of string
[@@deriving sexp]

open Datatype
open Sugar

let to_string t =
  let rec aux = function
    | Unit -> "unit"
    | Bool -> "bool"
    | Int -> "int"
    | Nat -> "nat"
    | Ref t -> spf "(%s) ref" @@ aux t
    | List t -> spf "(%s) list" @@ aux t
    | Tree t -> spf "(%s) tree" @@ aux t
    | Tuple t -> spf "%s" @@ List.split_by "*" aux t
    | Uninterp name -> name
  in
  aux t

let of_string = function
  | "unit" -> Unit
  | "bool" -> Bool
  | "int" -> Int
  | "nat" -> Nat
  | "(int) list" -> List Int
  | "(int) tree" -> Tree Int
  | "binomialhp" -> Uninterp "binomialhp"
  | "binomialt" -> Uninterp "binomialt"
  | "pairinghp" -> Uninterp "pairinghp"
  | "pairingl" -> Uninterp "pairingl"
  | "physicistsq" -> Uninterp "physicistsq"
  | "realtimeq" -> Uninterp "realtimeq"
  | "skewhp" -> Uninterp "skewhp"
  | "skewt" -> Uninterp "skewt"
  | _ as tp -> failwith (Printf.sprintf "unknown type name(%s)" tp)

let layout t =
  let rec aux = function
    | Unit -> "unit"
    | Bool -> "bool"
    | Int -> "int"
    | Nat -> "nat"
    | Ref t -> spf "(%s) ref" @@ aux t
    | List t -> spf "(%s) list" @@ aux t
    | Tree t -> spf "(%s) tree" @@ aux t
    | Tuple t -> spf "%s" @@ List.split_by "*" aux t
    | Uninterp name -> name
  in
  aux t

let layout_short t =
  let rec aux = function
    | Unit -> "u"
    | Bool -> "b"
    | Int -> "i"
    | Nat -> "n"
    | Ref t -> spf "%sr" @@ aux t
    | List t -> spf "%sl" @@ aux t
    | Tree t -> spf "%st" @@ aux t
    | Tuple t -> spf "%s" @@ List.split_by "_" aux t
    | Uninterp name -> name
  in
  aux t

let layout_l t = List.split_by " * " layout t

let compare t1 t2 = Sexplib.Sexp.compare (sexp_of_t t1) (sexp_of_t t2)

let eq t1 t2 = compare t1 t2 == 0

let is_basic = function Unit | Bool | Int | Nat -> true | _ -> false

(* TODO: reference *)
let is_dt t = not @@ is_basic t
