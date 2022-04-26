include Ast.Si
open Sugar
open Datatype

let layout_tmp m : string =
  List.fold_left
    (fun (str : string) (name, tp) ->
      spf "%s%s: %s\n" str name (Type.layout tp))
    ""
    (List.of_seq @@ Hashtbl.to_seq m)

let layout_tmp' tm m : string =
  (* let () = Printf.printf "layout_tmp': %s\n" (layout_tmp tm) in *)
  List.fold_left
    (fun (str : string) (name, tp) ->
      let tp = match Type.subst tm tp with None -> tp | Some tp' -> tp' in
      spf "%s%s: %s\n" str name (Type.layout tp))
    ""
    (List.of_seq @@ Hashtbl.to_seq m)

let layout { type_decl_map; func_type_map } =
  let s1 = spf "\ttype declarations:\n%s" (layout_tmp type_decl_map) in
  spf "%s\tfunction declarations:\n%s\n" s1
    (layout_tmp' type_decl_map func_type_map)
