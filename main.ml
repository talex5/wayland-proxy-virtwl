open Eio.Std

(* Connect to socket at [path] (and then close it), to see if anyone's already listening there. *)
let is_listening path =
  let s = Unix.(socket PF_UNIX SOCK_STREAM 0) in
  Fun.protect ~finally:(fun () -> Unix.close s) @@ fun () ->
  match Unix.connect s (Unix.ADDR_UNIX path) with
  | () -> true
  | exception Unix.Unix_error(Unix.ECONNREFUSED, _, _) -> false
  | exception ex ->
    Log.warn (fun f -> f "Error testing socket %S: %a" path Fmt.exn ex);
    false

let on_error ex = Log.warn (fun f -> f "Error handling client: %a" Fmt.exn ex)

let create_wayland_socket ~sw ~net wayland_display =
  let socket_path = Wayland.Unix_transport.socket_path ~wayland_display () in
  let existing_socket = Sys.file_exists socket_path in
  if existing_socket && is_listening socket_path then (
    Fmt.error "A server is already listening on %S!" socket_path
  ) else (
    if existing_socket then Unix.unlink socket_path;
    Ok (Eio.Net.listen ~backlog:5 ~sw net (`Unix socket_path))
  )

(* Start a daemon fiber listening for connections to [listening_socket]. *)
let listen_wayland ~sw ~connect_host ~config listening_socket =
  Log.info (fun f -> f "Listening for Wayland clients");
  Fiber.fork_daemon ~sw (fun () ->
      Eio.Net.run_server listening_socket ~on_error (fun conn addr ->
          Log.info (fun f -> f "New connection from %a" Eio.Net.Sockaddr.pp addr);
          try
            Switch.run @@ fun sw ->
                          let host = connect_host ~sw in
                          Relay.run ~config host conn;
                          (* The virtio transport doesn't support shutdown,
                             so force host listen fiber to be cancelled now. *)
                          Switch.fail sw Exit
          with Exit -> ()
        )
    )

let create_x11_socket ~sw ~net x_display =
  let path = Printf.sprintf "\x00/tmp/.X11-unix/X%d" x_display in
  Eio.Net.listen ~sw net (`Unix path) ~backlog:5

(* Start a daemon fiber listening for connections to [xwayland_listening_socket] and set $DISPLAY. *)
let listen_x11 ~sw ~proc_mgr ~config ~connect_host x_display xwayland_listening_socket =
  Log.info (fun f -> f "Listening for X clients");
  Fiber.fork_daemon ~sw (fun () ->
      Xwayland.listen ~proc_mgr ~config ~connect_host ~display:x_display xwayland_listening_socket
    );
  Unix.putenv "DISPLAY" (Printf.sprintf ":%d" x_display)

let listening_socket_from_fd ~sw fd =
  Unix.clear_nonblock fd;
  Unix.set_close_on_exec fd;
  (Eio_unix.Net.import_socket_listening ~sw ~close_unix:true fd :
    [ Eio_unix.Net.listening_socket_ty | `Unix_fd ] Eio.Std.r :>
    [< Eio_unix.Net.listening_socket_ty ] Eio.Std.r)

let main ~env setup_tracing use_virtio_gpu wayland_display x_display config args =
  let proc_mgr = env#process_mgr in
  let net = env#net in
  Switch.run @@ fun sw ->
  let connect_host ~sw =
    if use_virtio_gpu then (
      let dri_dir = Virtio_gpu.default_dri_dir env#fs in
      match Virtio_gpu.find_device ~sw dri_dir with
      | Ok virtio_gpu ->
        let transport = Virtio_gpu.wayland_transport virtio_gpu in
        Host.connect ~virtio_gpu ~sw transport
      | Error (`Msg m) ->
        Fmt.epr "No virtio-gpu device: %s@." m;
        exit 1
    ) else (
      let transport = Wayland.Unix_transport.connect ~sw ~net () in
      Host.connect ~sw transport
    )
  in
  (* Listen for incoming Wayland client connections: *)
  let socket_activate =
    try
      string_of_int (Unix.getpid ()) = Unix.getenv "LISTEN_PID"
    with Not_found -> false
  in
  let (let*) = Result.bind in
  let* (wayland_display, wayland, x11) = if socket_activate then (
    let* () =
      match wayland_display with
      | Some _ -> Error "--wayland-display cannot be used with socket activation"
      | None -> Ok ()
    in
    let* total_fds =
      try
        Ok (int_of_string (Unix.getenv "LISTEN_FDS"))
      with
      | Not_found -> Error "LISTEN_FDS not set"
      | _ -> Error "LISTEN_FDS not an integer"
    in
    let fds = List.of_seq (Seq.take total_fds (Seq.ints 3)) in
    let listen_fdnames = try Unix.getenv "LISTEN_FDNAMES" with Not_found -> "" in
    let names =
      if listen_fdnames = "" then [] else (String.split_on_char ':' listen_fdnames)
    in
    let* named_fds =
      try
        Ok (List.combine names fds)
      with _ -> Error "Wrong number of LISTEN_FDNAMES provided"
    in
    let fds_with_name name: Unix.file_descr list =
      List.filter_map (fun (n, f) -> if n = name then Some (Obj.magic f) else None) named_fds
    in
    let wayland_fds = fds_with_name "wayland" in
    let x11_fds = fds_with_name "x11" in
    let wayland_fds_len = List.length wayland_fds in
    let x11_fds_len = List.length x11_fds in
    let wayland_fds = List.map (listening_socket_from_fd ~sw) wayland_fds in
    let x11_fds = List.map (listening_socket_from_fd ~sw) x11_fds in
    if wayland_fds_len + x11_fds_len < total_fds then (
      Error "activated with unrecognized fds"
    ) else (
      let* () =
        match compare wayland_fds_len 1 with
        | -1 -> Error "\"wayland\" fd not provided"
        | 1 -> Error "too many \"wayland\" fds provided"
        | _ -> Ok ()
      in
      let wayland = List.hd wayland_fds in
      let* wayland_addr =
        match Eio.Net.listening_addr wayland with
        | `Tcp _ -> Error "activated with non-unix \"wayland\" socket"
        | `Unix path -> Ok path
      in
      match (compare x11_fds_len 1, x_display) with
      | (-1, None) -> Ok (wayland_addr, wayland, None)
      | (-1, Some _) -> Error "socket activated with no \"x11\" socket, but --x-display given"
      | (0, Some _) -> Ok (wayland_addr, wayland, List.nth_opt x11_fds 0)
      | (0, None) -> Error "socket activated with \"x11\" socket, but --x-display not given"
      | _ -> Error "too many \"x11\" fds provided"
    )
  ) else (
    let wayland_display = Option.value wayland_display ~default:"wayland-1" in
    let* wayland = create_wayland_socket ~sw ~net wayland_display in
    let x11 = Option.map (create_x11_socket ~sw ~net) x_display in
    Ok (wayland_display, wayland, x11)
  ) in
  setup_tracing ~wayland_display;
  listen_wayland ~sw ~config ~connect_host wayland;
  (* Listen for incoming X11 client connections, if configured: *)
  Option.iter (fun s -> listen_x11 ~sw ~proc_mgr ~config ~connect_host (Option.get x_display) s) x11;
  (* Run the application (if any), or just wait (if not): *)
  match args with
  | [] -> Fiber.await_cancel ()
  | args ->
     let env =
       Unix.environment ()
       |> Array.to_list
       |> Unix_env.replace "WAYLAND_DISPLAY" wayland_display
       |> Array.of_list
     in
     let status = Eio.Process.spawn ~sw proc_mgr args ~env |> Eio.Process.await in
     Log.info (fun f -> f "Application process ended (%a)" Eio.Process.pp_status status);
     Ok ()

open Cmdliner

let x_display =
  Arg.value @@
  Arg.(opt (some int)) None @@
  Arg.info
    ~doc:"Number of X display to listen on (e.g. 2 for DISPLAY=:2)"
    ["x-display"]

let wayland_display =
  Arg.value @@
  Arg.(opt (some string)) None @@
  Arg.info
    ~doc:"Name or path of socket to listen on"
    ["wayland-display"]

let virtio_gpu =
  Arg.value @@
  Arg.flag @@
  Arg.info
    ~doc:"Use virtio-gpu to connect to compositor on host"
    ["virtio-gpu"]

let args =
  Arg.value @@
  Arg.(pos_all string) [] @@
  Arg.info
    ~doc:"Sub-command to execute"
    []

let () =
  Printexc.record_backtrace true;
  Eio_main.run @@ fun env ->
  let fs = fst env#fs in
  Switch.run @@ fun sw ->
  let virtwl_proxy =
    let info = Cmd.info "wayland-proxy-virtwl" in
    Cmd.v info Term.(const (main ~env) $ Trace.cmdliner ~sw ~fs $ virtio_gpu $ wayland_display $ x_display $ Config.cmdliner $ args)
  in
  exit @@ Cmd.eval_result virtwl_proxy
