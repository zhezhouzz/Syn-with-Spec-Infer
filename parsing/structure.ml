open Ocaml_parser
open Parsetree
module L = Ast.S

let client_of_ocamlstruct_one structure =
  match structure.pstr_desc with
  | Pstr_value (_, [ value_binding ]) ->
      let name =
        match Pat.pattern_to_slang value_binding.pvb_pat with
        | L.Var (_, [ name ]) -> name
        | _ -> failwith "die"
      in
      let args, body =
        match Expr.expr_of_ocamlexpr value_binding.pvb_expr with
        | L.Fun (args, body) -> (args, body)
        | _ -> failwith "die"
      in
      let args =
        List.filter_map
          (fun (t, names) ->
            match names with
            | [ n ] -> Some (t, n)
            | _ -> failwith "client_of_ocamlstruct")
          args
      in
      Ast.C.{ name; args; body }
  | _ -> raise @@ failwith "translate not a function value"

let client_of_ocamlstruct structures =
  List.map client_of_ocamlstruct_one structures
