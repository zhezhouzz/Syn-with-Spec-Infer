type size = SmallNat | SizeBound of int

type element = SmallUnsign | LowUpper of int * int

type tree_fq = { fq_leaf : int; fq_node : int }

type t = {
  int_conf : element;
  list_conf : element * size;
  tree_conf : element * size * tree_fq;
  treei_conf : element * size * tree_fq;
  treeb_conf : element * size * tree_fq;
  binomialhp_conf : element * size;
  pairinghp_conf : element * size;
  physicistsq_conf : element * size;
  realtimeq_conf : element * size;
  skewhp_conf : element * size;
}

open Json
open Yojson.Basic.Util

let qcconf = ref None

let str_to_size = function
  | "SmallNat" -> SmallNat
  | n -> SizeBound (int_of_string n)

let str_to_element = function
  | "SmallUnsign" -> SmallUnsign
  | lu -> (
      match String.split_on_char ',' lu with
      | [ l; u ] -> LowUpper (int_of_string l, int_of_string u)
      | _ -> raise @@ failwith "str_to_size")

let parse_tree_fq j =
  { fq_leaf = load_int j "fq_leaf"; fq_node = load_int j "fq_node" }

let parse_element j = load_string j "element" |> str_to_element

let parse_size j = load_string j "size" |> str_to_size

let parse_list j = (parse_element j, parse_size j)

let parse_tree j =
  (parse_element j, parse_size j, j |> member "fq" |> parse_tree_fq)

let load_qc_config fname =
  let j = load_json fname in
  let qc =
    try
      {
        int_conf = j |> member "int_conf" |> parse_element;
        list_conf = j |> member "list_conf" |> parse_list;
        tree_conf = j |> member "tree_conf" |> parse_tree;
        treei_conf = j |> member "treei_conf" |> parse_tree;
        treeb_conf = j |> member "treeb_conf" |> parse_tree;
        binomialhp_conf = j |> member "binomialhp_conf" |> parse_list;
        pairinghp_conf = j |> member "pairinghp_conf" |> parse_list;
        physicistsq_conf = j |> member "physicistsq_conf" |> parse_list;
        realtimeq_conf = j |> member "realtimeq_conf" |> parse_list;
        skewhp_conf = j |> member "skewhp_conf" |> parse_list;
      }
    with _ -> raise @@ failwith "cannot load config::quich check"
  in
  qcconf := Some qc
