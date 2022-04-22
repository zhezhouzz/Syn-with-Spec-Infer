open Type
open Datatype
open Sugar
include To_type

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
