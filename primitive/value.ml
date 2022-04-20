open Datatype
open Sexplib.Std
open Sugar

type t =
  | U
  | I of int
  | B of bool
  | IL of int list
  | IT of int Tree.t
  | Tu of t list
  | NotADt
[@@deriving sexp]

let layout = function
  | U -> "()"
  | I i -> string_of_int i
  | B b -> string_of_bool b
  | NotADt -> "_"
  | _ -> failwith "un-imp"

let layout_l l = spf "{%s}" @@ List.split_by_comma layout l

let compare t1 t2 =
  let rec aux = function
    | U, U -> 0
    | I x, I y -> compare x y
    | B x, B y -> compare x y
    | IL x, IL y -> List.compare compare x y
    | IT x, IT y -> Tree.compare compare x y
    | Tu a, Tu b -> List.compare (fun a b -> aux (a, b)) a b
    | _, _ -> failwith "un-imp"
  in
  aux (t1, t2)

let eq t1 t2 =
  let rec aux = function
    | U, U -> true
    | I x, I y -> x == y
    | B x, B y -> x == y
    | IL x, IL y -> List.eq ( == ) x y
    | IT x, IT y -> Tree.eq ( == ) x y
    | Tu a, Tu b -> List.eq (fun a b -> aux (a, b)) a b
    | _, _ -> false
  in
  aux (t1, t2)

(* TODO: remove dup *)
let flatten = function
  | U | B _ | NotADt -> []
  | I x -> [ x ]
  | IL x -> x
  | IT x -> Tree.flatten_forall x
  | _ -> failwith "un-imp"

let flatten_l l = List.concat @@ List.map flatten l

let size = function
  | U | I _ | B _ -> 1
  | NotADt -> 0
  | IL il -> List.length il
  | IT it -> TreeTailCall.deep it
  | _ -> failwith "un-imp"

let layout_size_l l =
  spf "[%s]" @@ List.split_by_comma string_of_int @@ List.map size l

let get_tp v =
  let open Type in
  let rec aux = function
    | U -> Unit
    | I _ -> Int
    | B _ -> Bool
    | IL _ -> List Int
    | IT _ -> Tree Int
    | Tu l -> Tuple (List.map aux l)
    | _ -> failwith "un-imp"
  in
  aux v

let get_tp_l = List.map get_tp

(* let flatten_forall_l_unique_paddled l = *)
(*   let lens = List.map len l in *)
(*   let l = flatten_forall_l l in *)
(*   let l = List.remove_duplicates @@ lens @ l in *)
(*   let s = List.fold_left (fun s elem -> IntSet.add elem s) IntSet.empty l in *)
(*   match IntSet.min_elt_opt s with *)
(*   | None -> [ 0; 1 ] *)
(*   | Some minmial -> *)
(*       (minmial - 2) :: (minmial - 1) :: (List.of_seq @@ IntSet.to_seq s) *)
