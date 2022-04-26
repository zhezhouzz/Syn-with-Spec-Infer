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
    | Arrow (t1, t2) -> spf "%s-%s" (aux t1) (aux t2)
    | Ref t -> spf "%sr" @@ aux t
    | List t -> spf "%sl" @@ aux t
    | Array t -> spf "%sa" @@ aux t
    | Tree t -> spf "%st" @@ aux t
    | Tuple t -> spf "%s" @@ List.split_by "_" aux t
    | Record t ->
        spf "%s"
        @@ List.split_by "_" (fun (name, x) -> spf "%s%s" name (aux x)) t
    | Uninterp name -> name
  in
  aux t

let fresh_from_type t =
  let tname = layout_short t in
  (t, Renaming.unique tname)

let subst m t =
  let rec aux = function
    | Unit | Bool | Int | Nat -> None
    | Arrow (a, b) -> (
        match (aux a, aux b) with
        | None, None -> None
        | Some a, None -> Some (Arrow (a, b))
        | None, Some b -> Some (Arrow (a, b))
        | Some a, Some b -> Some (Arrow (a, b)))
    | Ref a ->
        let* a = aux a in
        Some (Ref a)
    | List a ->
        let* a = aux a in
        Some (List a)
    | Array a ->
        let* a = aux a in
        Some (Array a)
    | Tree a ->
        let* a = aux a in
        Some (Tree a)
    | Tuple l ->
        let l' = List.map aux l in
        if List.exists (function None -> true | _ -> false) l' then
          Some
            (Tuple
               (List.map (fun (x, x') ->
                    match x' with None -> x | Some x' -> x')
               @@ List.combine l l'))
        else None
    | Record l ->
        let l' = List.map (fun x -> aux @@ snd x) l in
        if List.exists (function None -> true | _ -> false) l' then
          Some
            (Record
               (List.map (fun ((name, x), x') ->
                    match x' with None -> (name, x) | Some x' -> (name, x'))
               @@ List.combine l l'))
        else None
    | Uninterp name ->
        let k = Hashtbl.find_opt m name in
        k
  in
  aux t
