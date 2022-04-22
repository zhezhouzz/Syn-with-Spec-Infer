include Ast.T
include Parsing.Type

let compare t1 t2 = Sexplib.Sexp.compare (sexp_of_t t1) (sexp_of_t t2)

let eq t1 t2 = compare t1 t2 == 0

let is_basic = function Unit | Bool | Int | Nat -> true | _ -> false

(* TODO: reference *)
let is_dt t = not @@ is_basic t

let compare_tvar t1 t2 =
  Sexplib.Sexp.compare (sexp_of_tvar t1) (sexp_of_tvar t2)

let eq_tvar t1 t2 = compare_tvar t1 t2 == 0

open Sugar
open Datatype

let layout_short t =
  let rec aux = function
    | Unit -> "u"
    | Bool -> "b"
    | Int -> "i"
    | Nat -> "n"
    | Arr (t1, t2) -> spf "%s-%s" (aux t1) (aux t2)
    | Ref t -> spf "%sr" @@ aux t
    | List t -> spf "%sl" @@ aux t
    | Tree t -> spf "%st" @@ aux t
    | Tuple t -> spf "%s" @@ List.split_by "_" aux t
    | Uninterp name -> name
  in
  aux t

let fresh_from_type t =
  let tname = layout_short t in
  (t, Renaming.unique tname)
