module T = Type
module V = Value
open Datatype
open Sugar

type t = string

type mp_info = {
  poly_name : string;
  name : string;
  tps : T.t list;
  bbimp : string;
}

let name_of_poly_name_and_tps name tps =
  let open T in
  let aux = function
    | "mem", [ List Int; Int ] -> "il_mem"
    | poly_name, tps ->
        failwith @@ spf "un-imp: %s(%s)" poly_name @@ layout_l tps
  in
  aux (name, tps)

let mp_info_to_json { poly_name; tps; bbimp; _ } =
  `Assoc
    [
      ("poly_name", `String poly_name);
      ("tps", `List (List.map (fun x -> `String (T.layout x)) tps));
      ("bbimp", `String bbimp);
    ]

let table = ref StrMap.empty

let mp_info_of_json j =
  let poly_name = Json.load_string j "poly_name" in
  let tps =
    List.map (fun x -> T.of_string @@ Yojson.Basic.Util.to_string x)
    @@ Json.load_list j "tps"
  in
  let bbimp = Json.load_string j "bbimp" in
  let name = name_of_poly_name_and_tps poly_name tps in
  { poly_name; name; tps; bbimp }

let load_mp_infos filename =
  let j = Json.load_json filename in
  let l = List.map mp_info_of_json @@ Yojson.Basic.Util.to_list j in
  table := StrMap.from_kv_list @@ List.map (fun info -> (info.name, info)) l

let find_info_by_name name =
  StrMap.find "cannot find method predicate %s" !table name

let find_info_by_polyname_tps poly_name tps =
  let name = name_of_poly_name_and_tps poly_name tps in
  find_info_by_name name

let poly_name_of_name name =
  let r = find_info_by_name name in
  r.poly_name
