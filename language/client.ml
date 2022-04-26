include Ast.C
open Sugar
open Datatype

let layout_one { name; args; _ } =
  spf "let %s %s = %s" name
    (List.split_by " " (fun (t, n) -> spf "(%s: %s)" n (Type.layout t)) args)
    "body"

let layout l = spf "Client:\n%s\n" (List.split_by "\n" layout_one l)
