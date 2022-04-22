module T = Ast.T
open Ocaml_parser
open Parsetree

let rec core_type_to_t ct = core_type_desc_to_t ct.ptyp_desc

and core_type_desc_to_t t =
  match t with
  | Ptyp_any | Ptyp_var _
  | Ptyp_object (_, _)
  | Ptyp_class (_, _)
  | Ptyp_alias (_, _)
  | Ptyp_variant (_, _, _)
  | Ptyp_poly (_, _)
  | Ptyp_package _ | Ptyp_extension _ ->
      failwith "die"
  | Ptyp_arrow (_, t1, t2) -> T.Arr (core_type_to_t t1, core_type_to_t t2)
  | Ptyp_tuple ts -> T.Tuple (List.map core_type_to_t ts)
  | Ptyp_constr (lc, ts) -> (
      match (Longident.flatten lc.txt, ts) with
      | [ "unit" ], [] -> T.Unit
      | [ "list" ], [ t ] -> T.List (core_type_to_t t)
      | _ -> failwith "un-imp")

let rec t_to_core_type t =
  {
    ptyp_desc = t_to_core_type_desc t;
    ptyp_loc = Location.none;
    ptyp_loc_stack = [];
    ptyp_attributes = [];
  }

and t_to_core_type_desc t =
  let open Longident in
  let open Location in
  let mk0 name = Ptyp_constr (mknoloc @@ Lident name, []) in
  let mk1 name t = Ptyp_constr (mknoloc @@ Lident name, [ t ]) in
  let aux = function
    | T.Unit -> mk0 "unit"
    | T.Bool -> mk0 "bool"
    | T.Int -> mk0 "int"
    | T.Nat -> mk0 "nat"
    | T.Ref t -> mk1 "ref" (t_to_core_type t)
    | T.List t -> mk1 "list" (t_to_core_type t)
    | T.Tree t -> mk1 "tree" (t_to_core_type t)
    | T.Tuple t -> Ptyp_tuple (List.map t_to_core_type t)
    | T.Arr (t1, t2) ->
        Ptyp_arrow (Asttypes.Nolabel, t_to_core_type t1, t_to_core_type t2)
    | T.Uninterp _ -> failwith "un-imp"
  in
  aux t

let layout t =
  let _ = Format.flush_str_formatter () in
  Pprintast.core_type Format.str_formatter (t_to_core_type t);
  Format.flush_str_formatter ()

let of_string str = core_type_to_t @@ Parse.core_type @@ Lexing.from_string str

open Datatype

let layout_l ts = List.split_by_comma layout ts
