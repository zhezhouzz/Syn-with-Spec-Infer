open Ocaml_parser
open Parsetree
module L = Ast.S

let rec pattern_to_slang pattern =
  match pattern.ppat_desc with
  | Ppat_tuple ps -> L.Tu (List.map pattern_to_slang ps)
  | Ppat_var ident -> L.Var (None, [ ident.txt ])
  | Ppat_constraint (ident, tp) -> (
      match pattern_to_slang ident with
      | L.Var (None, name) -> L.Var (Some (Type.core_type_to_t tp), name)
      | _ -> failwith Sugar.(spf "pattern_to_string:%s" "??"))
  | _ ->
      Pprintast.pattern Format.std_formatter pattern;
      raise @@ failwith "wrong pattern name, maybe untyped"

let is_typed_slang x =
  let open L in
  let rec aux = function
    | Var (None, _) -> false
    | Var (Some _, _) -> true
    | Tu xs -> List.for_all aux xs
    | _ -> failwith "not a patten"
  in
  aux x

(* TODO: Check nested tuple *)
let to_typed_slang x =
  let open L in
  let rec aux l = function
    | Var (None, name) ->
        failwith
        @@ Sugar.(spf "untyped variable: %s" @@ Datatype.StrList.to_string name)
    | Var (Some tp, name) -> l @ [ (tp, name) ]
    | Tu xs -> List.fold_left aux l xs
    | _ -> failwith "not a patten"
  in
  aux [] x
