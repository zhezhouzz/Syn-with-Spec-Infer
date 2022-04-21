type env = {
  log_env : Zlog.env ref;
  z3_ctx : Z3.context option;
  qc : Qcconfig.t option ref;
  mp_table : Method_predicate.mp_info Datatype.StrMap.t ref;
}

let env =
  ref
    {
      log_env = Zlog.env;
      z3_ctx = None;
      qc = Qcconfig.qcconf;
      mp_table = Method_predicate.table;
    }

open Json

let load_config configfile =
  let open Yojson.Basic.Util in
  let j = load_json configfile in
  let if_z3 =
    try j |> member "if_z3" |> to_bool
    with _ -> raise @@ failwith "cannot load config::z3"
  in
  let log_flag =
    try j |> member "log_flag" |> to_string
    with _ -> raise @@ failwith "cannot load config::log_env"
  in
  (log_flag, if_z3)

let mk_z3 if_z3 =
  if if_z3 then
    Some
      (Z3.mk_context
         [ ("model", "true"); ("proof", "false"); ("timeout", "1999") ])
  else None

let mp_file = "config/mp.json"

let load_all configfile qcfile mp_file =
  let log_flag, if_z3 = load_config configfile in
  let ctx = mk_z3 if_z3 in
  let () = Zlog.set_env log_flag in
  let () = Method_predicate.load_mp_infos mp_file in
  let () = Qcconfig.load_qc_config qcfile in
  env := { !env with z3_ctx = ctx }

let load_basic configfile mp_file =
  let log_flag, if_z3 = load_config configfile in
  let ctx = mk_z3 if_z3 in
  let () = Zlog.set_env log_flag in
  let () = Method_predicate.load_mp_infos mp_file in
  env := { !env with z3_ctx = ctx }

let exec_main configfile main =
  let _ = Random.init 0 in
  load_basic configfile mp_file;
  main ();
  Zlog.release_logfile ()

let exec_main_with_qc configfile qcfile main =
  let _ = Random.init 0 in
  load_all configfile qcfile mp_file;
  main ();
  Zlog.release_logfile ()
