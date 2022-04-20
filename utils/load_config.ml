open Config
open Json
open Yojson.Basic.Util

let make_dir name = Core.Unix.mkdir_p name

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

let conf = ref { debug_flag = Opt; z3_ctx = None; qc = None }

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
  conf := { !conf with qc = Some qc }

let logdir = ".logdir"

let logfile = ".log"

let load_config configfile =
  let open Yojson.Basic.Util in
  let j = load_json configfile in
  let _ = Random.init 0 in
  let debug_flag =
    try j |> member "debug_flag" |> to_string
    with _ -> raise @@ failwith "cannot load config::debug_flag"
  in
  let debug_flag =
    match debug_flag with
    | "debug" ->
        make_dir logdir;
        Debug { logc = Core.Out_channel.create (logdir ^ "/" ^ logfile) }
    | "opt" -> Opt
    | _ -> raise @@ failwith "config error"
  in
  let j = load_json configfile in
  let if_z3 =
    try j |> member "if_z3" |> to_bool
    with _ -> raise @@ failwith "cannot load config::z3"
  in
  let z3_ctx =
    if if_z3 then
      Some
        (Z3.mk_context
           [ ("model", "true"); ("proof", "false"); ("timeout", "1999") ])
    else None
  in
  conf := { debug_flag; z3_ctx; qc = None }

let load_all configfile qcfile =
  load_config configfile;
  load_qc_config qcfile

let release_config () =
  match !conf.debug_flag with
  | Debug { logc; _ } -> Core.Out_channel.close logc
  | Opt -> ()

let refresh_logfile name =
  let new_name = logfile ^ "_" ^ name in
  let flag' =
    match !conf.debug_flag with
    | Debug { logc; _ } ->
        Core.Out_channel.close logc;
        Core.Unix.rename
          ~src:(logdir ^ "/" ^ logfile)
          ~dst:(logdir ^ "/" ^ new_name);
        Debug { logc = Core.Out_channel.create (logdir ^ "/" ^ logfile) }
    | Opt -> Opt
  in
  conf := { !conf with debug_flag = flag' }

let exec_main configfile main =
  load_config configfile;
  main ();
  release_config ()

let exec_main_with_qc configfile qcfile main =
  load_all configfile qcfile;
  main ();
  release_config ()
