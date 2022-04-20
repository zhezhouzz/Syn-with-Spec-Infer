open Load_config;;

let time f =
  let t = Sys.time () in
  let fx = f () in
  let delta = Sys.time () -. t in
  (fx, delta)

type log_error = Ldebug | LWarning

let log_write ?log_level:(level = Ldebug) message =
  match !conf.debug_flag with
  | Debug { logc; _ } -> (
      match level with
      | Ldebug -> Printf.fprintf logc "%s\n" message
      | LWarning -> Printf.fprintf logc "[Warning] %s\n" message)
  | _ -> ()

let event_time_ message f =
  match !conf.debug_flag with
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
  match !conf.debug_flag with
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
