module T = Ast.T
open Ocaml_parser
open Parsetree

let layout_ t =
  let _ = Format.flush_str_formatter () in
  Pprintast.core_type Format.str_formatter t;
  Format.flush_str_formatter ()

let desc_to_ct t =
  {
    ptyp_desc = t;
    ptyp_loc = Location.none;
    ptyp_loc_stack = [];
    ptyp_attributes = [];
  }

let rec core_type_to_t ct = core_type_desc_to_t ct.ptyp_desc

and core_type_desc_to_t t =
  match t with
  | Ptyp_any | Ptyp_var _ | Ptyp_object (_, _) -> failwith "type object"
  | Ptyp_class (_, _)
  | Ptyp_alias (_, _)
  | Ptyp_variant (_, _, _)
  | Ptyp_poly (_, _)
  | Ptyp_package _ | Ptyp_extension _ ->
      failwith "die"
  | Ptyp_arrow (_, t1, t2) -> T.Arrow (core_type_to_t t1, core_type_to_t t2)
  | Ptyp_tuple ts -> T.Tuple (List.map core_type_to_t ts)
  | Ptyp_constr (lc, ts) -> (
      match (Longident.flatten lc.txt, ts) with
      | [ "unit" ], [] -> T.Unit
      | [ "bool" ], [] -> T.Bool
      | [ "int" ], [] -> T.Int
      | [ "ref" ], [ t ] -> T.Ref (core_type_to_t t)
      | [ "list" ], [ t ] -> T.List (core_type_to_t t)
      | [ "array" ], [ t ] ->
          T.Array (core_type_to_t t)
          (* HACK: here, assume we know the name of abstact type name *)
      | [ "t" ], [] -> T.Uninterp "t"
      | [ a; "t" ], [] -> T.Uninterp Sugar.(spf "%s.t" a)
      | _, _ -> failwith @@ Printf.sprintf "un-imp: %s" (layout_ @@ desc_to_ct t)
      )

let rec t_to_core_type t = desc_to_ct (t_to_core_type_desc t)

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
    | T.Array t -> mk1 "array" (t_to_core_type t)
    | T.Tree t -> mk1 "tree" (t_to_core_type t)
    | T.Tuple t -> Ptyp_tuple (List.map t_to_core_type t)
    | T.Record t ->
        Ptyp_object
          ( List.map
              (fun (name, t) ->
                {
                  pof_desc = Otag (Location.mknoloc name, t_to_core_type t);
                  pof_loc = Location.none;
                  pof_attributes = [];
                })
              t,
            Asttypes.Closed )
    | T.Arrow (t1, t2) ->
        Ptyp_arrow (Asttypes.Nolabel, t_to_core_type t1, t_to_core_type t2)
    | T.Uninterp name -> mk0 name
  in
  aux t

let layout t = layout_ (t_to_core_type t)

let of_string str = core_type_to_t @@ Parse.core_type @@ Lexing.from_string str

let layout_l ts = Datatype.(List.split_by_comma layout ts)
