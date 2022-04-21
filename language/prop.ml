type t =
  | True
  | Bvar of string
  | Implies of t * t
  | Ite of t * t * t
  | Not of t
  | And of t list
  | Or of t list
  | Iff of t * t
  | Pred of string * Typedvar.t list

open Sugar
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
    | Bvar b ->
        let tp', b' = update (Type.Bool, b) in
        if not @@ Type.eq tp' Type.Bool then failwith "subst: detect bad naming"
        else Bvar b'
    | Pred (mp, args) -> Pred (mp, List.map update args)
    | Implies (e1, e2) -> Implies (aux e1, aux e2)
    | Ite (e1, e2, e3) -> Ite (aux e1, aux e2, aux e3)
    | Not e -> Not (aux e)
    | And l -> And (List.map aux l)
    | Or l -> Or (List.map aux l)
    | Iff (e1, e2) -> Iff (aux e1, aux e2)
  in
  aux e

let sym_and = "&&"

let sym_or = "||"

let sym_not = "not"

let sym_implies = "implies"

let sym_iff = "iff"

let is_op op =
  match op with "==" | "<" | ">" | "<=" | ">=" -> true | _ -> false

let rec layout = function
  | True -> "true"
  | Bvar b -> b
  | Pred (mp, args) ->
      if is_op mp then
        match args with
        | [ a; b ] -> spf "(%s %s %s)" (snd a) mp (snd b)
        | _ -> spf "%s(%s)" mp (List.split_by_comma snd args)
      else
        spf "(%s %s)" mp
          (* (Method_predicate.poly_name mp) *)
          (List.split_by " " snd args)
  | Implies (p1, p2) -> spf "(%s %s %s)" sym_implies (layout p1) (layout p2)
  | And ps -> spf "(%s)" @@ List.split_by sym_and layout ps
  | Or ps -> spf "(%s)" @@ List.split_by sym_or layout ps
  | Not p -> spf "(%s %s)" sym_not @@ layout p
  | Iff (p1, p2) -> spf "(%s %s %s)" sym_iff (layout p1) (layout p2)
  | Ite (p1, p2, p3) ->
      spf "(if %s then %s else %s)" (layout p1) (layout p2) (layout p3)

let sym_and = "&&"

let sym_or = "||"

let sym_not = "!"

let sym_implies = "==>"

let sym_iff = "<==>"

(* TODO: desugar *)
let pretty_layout_ indent e =
  let mk_indent indent = String.init indent (fun _ -> ' ') in
  let rec is_display_level_lit = function
    | True | Bvar _ | Pred (_, _) -> true
    | Not x -> is_display_level_lit x
    | _ -> false
  in
  let rec aux indent = function
    | True -> "true"
    | Bvar b -> b
    | Pred ("==", [ a; b ]) -> spf "(%s == %s)" (snd a) (snd b)
    | Pred ("<", [ a; b ]) -> spf "(%s < %s)" (snd a) (snd b)
    | Pred (mp, args) -> spf "%s(%s)" mp (List.split_by_comma snd args)
    | Implies (p1, p2) ->
        if is_display_level_lit p2 then
          spf "%s(%s %s %s)" (mk_indent indent) (aux 0 p1) sym_implies
            (aux 0 p2)
        else
          spf "%s(\n%s %s \n%s %s\n%s)" (mk_indent indent)
            (aux (indent + 1) p1)
            sym_implies (mk_indent indent)
            (aux (indent + 1) p2)
            (mk_indent indent)
    | And [] -> raise @@ failwith "epr does not involve void conj"
    | And [ p ] -> aux indent p
    | And ps ->
        if List.for_all is_display_level_lit ps then
          spf "%s(%s)" (mk_indent indent)
          @@ List.split_by (spf " %s " sym_and) (aux 0) ps
        else
          spf "%s(\n%s\n%s)" (mk_indent indent)
            (List.inner_layout
               (List.map (aux (indent + 1)) ps)
               (" " ^ sym_and ^ "\n")
               "true")
            (mk_indent indent)
    | Or [] -> raise @@ failwith "epr does not involve void disconj"
    | Or [ p ] -> aux indent p
    | Or ps ->
        if List.for_all is_display_level_lit ps then
          spf "%s(%s)" (mk_indent indent)
          @@ List.split_by (spf " %s " sym_or) (aux 0) ps
        else
          spf "%s(\n%s\n%s)" (mk_indent indent)
            (List.inner_layout
               (List.map (aux (indent + 1)) ps)
               (" " ^ sym_or ^ "\n")
               "true")
            (mk_indent indent)
    | Not (Pred ("==", [ a; b ])) -> spf "(%s != %s)" (snd a) (snd b)
    | Not (Pred ("<", [ a; b ])) -> spf "(%s > %s)" (snd a) (snd b)
    | Not p -> spf "%s%s%s" (mk_indent indent) sym_not (aux 0 p)
    | Iff (p1, p2) ->
        spf "%s(%s %s \n%s)" (mk_indent indent) (aux 0 p1) sym_iff
          (aux (indent + 1) p2)
    | Ite (p1, p2, p3) ->
        spf "%s(ite %s\n%s\n%s)" (mk_indent indent) (aux 1 p1)
          (aux (indent + 4) p2)
          (aux (indent + 4) p3)
  in
  aux indent e

let pretty_layout t = pretty_layout_ 0 t
