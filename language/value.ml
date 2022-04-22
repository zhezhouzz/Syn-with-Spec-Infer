open Datatype
open Sugar
include Ast.V
include Parsing.Value

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

module ValueVector = struct
  type t = Ast.V.t list

  let compare = List.compare compare
end

module ValueVectorMap = Map.Make (ValueVector)

module ValueSet = Set.Make (struct
  let compare = compare

  type t = Ast.V.t
end)

let remove_duplicates l =
  let s = ValueSet.add_seq (List.to_seq l) ValueSet.empty in
  List.of_seq @@ ValueSet.to_seq s

module ValueLSet = Set.Make (struct
  let compare = List.compare compare

  type t = Ast.V.t list
end)

let remove_duplicates_l l =
  let s = ValueLSet.add_seq (List.to_seq l) ValueLSet.empty in
  List.of_seq @@ ValueLSet.to_seq s

type label = L1 | L2 | L12

let intersection l1 l2 =
  let m = ValueVectorMap.empty in
  let m =
    List.fold_left
      (fun m v ->
        match ValueVectorMap.find_opt v m with
        | None -> ValueVectorMap.add v L1 m
        | Some _ -> m)
      m l1
  in
  let m =
    List.fold_left
      (fun m v ->
        match ValueVectorMap.find_opt v m with
        | None -> ValueVectorMap.add v L2 m
        | Some L1 -> ValueVectorMap.add v L12 m
        | Some _ -> m)
      m l2
  in
  ValueVectorMap.fold
    (fun v label (s1, s2, s12) ->
      match label with
      | L1 -> (v :: s1, s2, s12)
      | L2 -> (s1, v :: s2, s12)
      | L12 -> (s1, s2, v :: s12))
    m ([], [], [])

module ValueArrSet = Set.Make (struct
  let compare (arr1, idx1) (arr2, idx2) =
    List.compare compare arr1.(idx1) arr2.(idx2)

  type t = Ast.V.t list array * int
end)

let remove_duplicates_arr arr l =
  let s =
    ValueArrSet.add_seq
      (List.to_seq (List.map (fun idx -> (arr, idx)) l))
      ValueArrSet.empty
  in
  List.map snd @@ List.of_seq @@ ValueArrSet.to_seq s
