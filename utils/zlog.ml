open Sugar

type env = Debug of { logc : Core.Out_channel.t } | Opt

let logdir = ".logdir"

let logfile = ".log"

let mk_env flag =
  match flag with
  | "debug" ->
      make_dir logdir;
      Debug { logc = Core.Out_channel.create (logdir ^ "/" ^ logfile) }
  | "opt" -> Opt
  | _ -> raise @@ failwith "config error"

let env = ref (mk_env "debug")

let set_env flag = env := mk_env flag

let time f =
  let t = Sys.time () in
  let fx = f () in
  let delta = Sys.time () -. t in
  (fx, delta)

type log_error = Ldebug | LWarning

let log_write ?log_level:(level = Ldebug) message =
  match !env with
  | Debug { logc; _ } -> (
      match level with
      | Ldebug -> Printf.fprintf logc "%s\n" message
      | LWarning -> Printf.fprintf logc "[Warning] %s\n" message)
  | _ -> ()

let event_time_ message f =
  match !env with
  | Debug _ ->
      let _ = log_write (Printf.sprintf "------%s start-----" message) in
      let start_time = Sys.time () in
      let result = f () in
      let end_time = Sys.time () in
      let _ =
        log_write
          (Printf.sprintf "------%s end(exec time: %f)-----" message
             (end_time -. start_time))
      in
      (result, end_time -. start_time)
  | Opt -> (f (), 0.0)

let event_ message f = fst @@ event_time_ message f

let event ?msg:(message = "") f = event_ message f

let debug_event eventname f =
  match !env with
  | Debug _ ->
      let write x = log_write (Printf.sprintf "[%s] %s" eventname x) in
      let start_time = Sys.time () in
      let result = f () in
      let end_time = Sys.time () in
      let _ =
        write @@ Printf.sprintf "exec time:%f\n" (end_time -. start_time)
      in
      result
  | Opt -> ()

let time_tick_start_time = ref (Sys.time ())

let time_tick_init () = time_tick_start_time := Sys.time ()

let time_tick bound =
  let t' = Sys.time () in
  let diff = t' -. !time_tick_start_time in
  let () = log_write @@ Printf.sprintf "time diff:%f" diff in
  if diff > bound then raise @@ failwith "time_tick"
  else time_tick_start_time := t'

let load_from_file configfile =
  let open Yojson.Basic.Util in
  let open Json in
  let j = load_json configfile in
  let log_env =
    try j |> member "log_env" |> to_string
    with _ -> raise @@ failwith "cannot load config::log_env"
  in
  set_env log_env

let refresh_logfile name =
  let new_name = logfile ^ "_" ^ name in
  let flag' =
    match !env with
    | Debug { logc; _ } ->
        Core.Out_channel.close logc;
        Core.Unix.rename
          ~src:(logdir ^ "/" ^ logfile)
          ~dst:(logdir ^ "/" ^ new_name);
        Debug { logc = Core.Out_channel.create (logdir ^ "/" ^ logfile) }
    | Opt -> Opt
  in
  env := flag'

let release_logfile () =
  match !env with Debug { logc; _ } -> Core.Out_channel.close logc | Opt -> ()
