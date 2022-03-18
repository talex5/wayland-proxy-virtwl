open Lwt.Syntax
open Lwt.Infix

let ( let*! ) x f =
  x >>= function
  | `Ok x -> f x
  | `Error _ as e -> Lwt.return e

let rec listen ~config socket =
  let* (client, _addr) = Lwt_unix.accept socket in
  Lwt_unix.set_close_on_exec client;
  Log.info (fun f -> f "New connection");
  Lwt.async (fun () ->
      Lwt.finalize
        (fun () ->
           let* r = Relay.create config in
           Relay.accept r client
        )
        (fun () ->
           Lwt_unix.close client
        )
    );
  listen ~config socket

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

(* Start listening for connections to [wayland_display]. *)
let listen_wayland ~config wayland_display = 
  let socket_path = Wayland.Unix_transport.socket_path ~wayland_display () in
  let existing_socket = Sys.file_exists socket_path in
  if existing_socket && is_listening socket_path then (
    Lwt.return (`Error (false, Fmt.str "A server is already listening on %S!" socket_path))
  ) else (
    if existing_socket then Unix.unlink socket_path;
    let listening_socket = Unix.(socket PF_UNIX SOCK_STREAM 0) in
    Unix.set_close_on_exec listening_socket;
    Unix.bind listening_socket (Unix.ADDR_UNIX socket_path);
    Unix.listen listening_socket 5;
    Log.info (fun f -> f "Listening on %S for Wayland clients" socket_path);
    Lwt.async (fun () -> listen ~config (Lwt_unix.of_unix_file_descr listening_socket));
    Lwt.return (`Ok ())
  )

(* Start listening for connections to [x_display] and set $DISPLAY. *)
let listen_x11 ~config x_display = 
  let xwayland_listening_socket =
    let path = Printf.sprintf "\x00/tmp/.X11-unix/X%d" x_display in
    let sock = Unix.(socket PF_UNIX SOCK_STREAM 0) in
    Unix.set_close_on_exec sock;
    Unix.bind sock (Unix.ADDR_UNIX path);
    Unix.listen sock 5;
    Log.info (fun f -> f "Listening on %S for X clients" path);
    sock
  in
  Lwt.async (fun () -> Xwayland.listen ~config ~display:x_display (Lwt_unix.of_unix_file_descr xwayland_listening_socket));
  Unix.putenv "DISPLAY" (Printf.sprintf ":%d" x_display)

let env_replace k v l =
  let prefix = k ^ "=" in
  (prefix ^ v) :: List.filter (fun x -> not (Config.starts_with ~prefix x)) l

let main setup_tracing wayland_display x_display config args =
  Lwt_main.run begin
    let* () = setup_tracing ~wayland_display in
    (* Listen for incoming Wayland client connections: *)
    let*! () = listen_wayland ~config wayland_display in
    (* Listen for incoming X11 client connections, if configured: *)
    Option.iter (listen_x11 ~config) x_display;
    (* Run the application (if any), or just wait (if not): *)
    match args with
    | [] ->
      fst (Lwt.wait ())
    | args ->
      let env =
        Unix.environment ()
        |> Array.to_list
        |> env_replace "WAYLAND_DISPLAY" wayland_display
        |> Array.of_list
      in
      let child = Lwt_process.open_process_none ("", Array.of_list args) ~env in
      let* status = child#status in
      Log.info (fun f -> f "Application process ended (%a)" Trace.pp_status status);
      Lwt.return (`Ok ())
  end

open Cmdliner

let x_display =
  Arg.value @@
  Arg.(opt (some int)) None @@
  Arg.info
    ~doc:"Number of X display to listen on (e.g. 2 for DISPLAY=:2)"
    ["x-display"]

let wayland_display =
  Arg.value @@
  Arg.(opt string) "wayland-1" @@
  Arg.info
    ~doc:"Name or path of socket to listen on"
    ["wayland-display"]

let args =
  Arg.value @@
  Arg.(pos_all string) [] @@
  Arg.info
    ~doc:"Sub-command to execute"
    []

let virtwl_proxy =
  let info = Cmd.info "wayland-proxy-virtwl" in
  Cmd.v info Term.(ret (const main $ Trace.cmdliner $ wayland_display $ x_display $ Config.cmdliner $ args))

let () =
  exit @@ Cmd.eval virtwl_proxy
