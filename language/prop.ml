include Ast.P
include Parsing.Prop
open Datatype

(* let desugar (prop : t) : t = *)
(*   let rec aux = function *)
(*     | True -> True *)
(*     | Bvar (t, b) -> Bvar (t, b) *)
(*     | Pred (mp, args) -> Pred (MP.instantization mp @@ List.map fst args, args) *)
(*     | Implies (e1, e2) -> Implies (aux e1, aux e2) *)
(*     | Ite (e1, e2, e3) -> Ite (aux e1, aux e2, aux e3) *)
(*     | Not e -> Not (aux e) *)
(*     | And l -> And (List.map aux l) *)
(*     | Or l -> Or (List.map aux l) *)
(*     | Iff (e1, e2) -> Iff (aux e1, aux e2) *)
(*   in *)
(*   aux prop *)

let subst update e =
  let rec aux e =
    match e with
    | True -> e
    | Bvar b -> Bvar (update b)
    | Pred (mp, args) -> Pred (mp, List.map update args)
    | Implies (e1, e2) -> Implies (aux e1, aux e2)
    | Ite (e1, e2, e3) -> Ite (aux e1, aux e2, aux e3)
    | Not e -> Not (aux e)
    | And l -> And (List.map aux l)
    | Or l -> Or (List.map aux l)
    | Iff (e1, e2) -> Iff (aux e1, aux e2)
  in
  aux e

(* let sym_and = "&&" *)

(* let sym_or = "||" *)

(* let sym_not = "not" *)

(* let sym_implies = "implies" *)

(* let sym_iff = "iff" *)

let is_op op =
  match op with "==" | "<" | ">" | "<=" | ">=" -> true | _ -> false

let pretty_layout = layout
