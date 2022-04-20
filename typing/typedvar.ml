open Datatype
open Sugar
open Sexplib.Std

type t = Type.t * string [@@deriving sexp]

let layout (t, name) = spf "(%s: %s)" name (Type.layout t)

let layout_l l = List.split_by_comma layout l

let compare t1 t2 = Sexplib.Sexp.compare (sexp_of_t t1) (sexp_of_t t2)

let eq t1 t2 = compare t1 t2 == 0

let fresh_from_type t =
  let tname = Type.layout_short t in
  (t, Renaming.unique tname)
