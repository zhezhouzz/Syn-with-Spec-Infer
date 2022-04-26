open Ocaml_parser
open Parsetree
open Datatype
module L = Ast.S

let expr_of_ocamlexpr expr =
  let rec aux expr =
    match expr.pexp_desc with
    | Pexp_tuple es -> L.Tu (List.map aux es)
    | Pexp_constraint (expr, _) -> aux expr
    | Pexp_ident id ->
        L.Var (None, Longident.flatten id.txt)
        (*                                           ( *)
        (* match Longident.flatten id.txt with *)
        (* | [ x ] -> L.Var (None, Longident.flatten id.txt) *)
        (* | _ -> *)
        (*     failwith *)
        (*     @@ Sugar.( *)
        (*          spf "expr_of_ocamlexpr: %s" *)
        (*          @@ StrList.to_string @@ Longident.flatten id.txt)) *)
    | Pexp_construct (_, _) | Pexp_constant _ ->
        L.Const (Value.expr_to_value expr)
    | Pexp_let (_, vbs, e) ->
        List.fold_right
          (fun vb body ->
            let leftvar = Pat.pattern_to_slang vb.pvb_pat in
            let leftvars = Pat.to_typed_slang leftvar in
            L.Let (leftvars, aux vb.pvb_expr, body))
          vbs (aux e)
    | Pexp_apply (func, args) ->
        let funcname =
          match aux func with
          | L.Var (None, x) -> x
          | _ -> raise @@ failwith "Can only allow function name as function"
        in
        let args = List.map (fun x -> aux @@ snd x) args in
        L.App (funcname, args)
    | Pexp_ifthenelse (e1, e2, Some e3) -> L.Ite (aux e1, aux e2, aux e3)
    | Pexp_ifthenelse (_, _, None) -> raise @@ failwith "no else branch in ite"
    | Pexp_match (case_target, cases) ->
        let handle_match_args match_arg =
          let e = aux match_arg in
          let rec aux e =
            match e with
            | L.Var (_, var) -> [ var ]
            | L.Tu vars -> List.flatten @@ List.map aux vars
            | _ -> failwith "parser: wrong format in match"
          in
          aux e
        in
        let case_target = handle_match_args case_target in
        let handle_case case =
          match case.pc_guard with
          | None ->
              Printf.printf "%s\n" @@ Pprintast.string_of_expression case.pc_rhs;
              failwith "handle_case"
          | Some guard -> (guard, case.pc_rhs)
        in
        let cs =
          List.map
            (fun case ->
              let case_e, body = handle_case case in
              (aux case_e, aux body))
            cases
        in
        L.Match (case_target, cs)
    | Pexp_fun (_, _, _, _) ->
        (* un-curry *)
        let rec parse_function args expr =
          match expr.pexp_desc with
          | Pexp_fun (_, _, arg, expr) ->
              let arg = Pat.to_typed_slang @@ Pat.pattern_to_slang arg in
              parse_function (args @ arg) expr
          | _ -> L.Fun (args, aux expr)
        in
        parse_function [] expr
    | _ ->
        raise
        @@ failwith
             (Sugar.spf "not imp client parsing:%s"
             @@ Pprintast.string_of_expression expr)
  in
  aux expr
