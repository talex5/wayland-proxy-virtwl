open Lwt.Syntax

let rec listen ~config socket =
  let* (client, _addr) = Lwt_unix.accept socket in
  Log.info (fun f -> f "New connection");
  Lwt.async (fun () -> Relay.handle ~config client);
  listen ~config socket

let () =
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.(set_level (Some Info));
  Printexc.record_backtrace true

let is_listening path =
  let s = Unix.(socket PF_UNIX SOCK_STREAM 0) in
  Fun.protect ~finally:(fun () -> Unix.close s) @@ fun () ->
  match Unix.connect s (Unix.ADDR_UNIX path) with
  | () -> true
  | exception Unix.Unix_error(Unix.ECONNREFUSED, _, _) -> false
  | exception ex ->
    Log.warn (fun f -> f "Error testing socket %S: %a" path Fmt.exn ex);
    false


let setup_logging ~verbose ~log_suppress =
  let log_level = if verbose then Logs.Info else Logs.Warning in
  Logs.(set_level (Some log_level));
  let wayland_env, wayland =
    match Sys.getenv_opt "WAYLAND_DEBUG_PROXY" with
    | Some x -> "WAYLAND_DEBUG_PROXY", x
    | None -> "WAYLAND_DEBUG", (Sys.getenv_opt "WAYLAND_DEBUG" |> Option.value ~default:"")
  in
  let wayland =
    if wayland = "1" then ["client"; "server"]
    else String.split_on_char ',' wayland
  in
  wayland |> List.iter (function
      | "client" -> Logs.Src.set_level Trace.Client.src (Some Logs.Info)
      | "server" -> Logs.Src.set_level Trace.Host.src (Some Logs.Info)
      | "" -> ()
      | x -> Log.warn (fun f -> f "Unknown $%s item %S" wayland_env x)
    );
  log_suppress |> List.iter (function
      | `Motion -> Trace.motion := false
      | `Shm -> Trace.shm := false
      | `Delete -> Trace.delete := false
      | `Region -> Trace.region := false
      | `Drawing -> Trace.drawing := false
      | `Hints -> Trace.hints := false
    )

let main tag verbose log_suppress wayland_display args =
  setup_logging ~verbose ~log_suppress;
  let config = { Config.tag } in
  let socket_path = Wayland.Unix_transport.socket_path ~wayland_display () in
  let existing_socket = Sys.file_exists socket_path in
  if existing_socket && is_listening socket_path then (
   `Error (false, Fmt.strf "A server is already listening on %S!" socket_path)
  ) else (
    if existing_socket then Unix.unlink socket_path;
    Lwt_main.run begin
      let listening_socket = Unix.(socket PF_UNIX SOCK_STREAM 0) in
      Unix.bind listening_socket (Unix.ADDR_UNIX socket_path);
      Unix.listen listening_socket 5;
      Log.info (fun f -> f "Listening on %S" socket_path);
      Lwt.async (fun () -> listen ~config (Lwt_unix.of_unix_file_descr listening_socket));
      Unix.putenv "WAYLAND_DISPLAY" wayland_display;
      match args with
      | [] ->
        fst (Lwt.wait ())
      | args ->
        let child = Lwt_process.open_process ("", Array.of_list args) in
        let* _ = child#status in
        Lwt.return (`Ok ())
    end
  )

open Cmdliner

let tag =
  Arg.value @@
  Arg.(opt string) "" @@
  Arg.info
    ~doc:"Tag to prefix to window titles"
    ["tag"]

let verbose =
  Arg.value @@
  Arg.flag @@
  Arg.info
    ~doc:"Verbose logging"
    ["v"; "verbose"]

let suppress = Arg.enum [
    "motion", `Motion;
    "shm", `Shm;
    "delete", `Delete;
    "region", `Region;
    "drawing", `Drawing;
    "hints", `Hints;
  ]

let log_suppress =
  Arg.value @@
  Arg.(opt (list suppress)) [] @@
  Arg.info
    ~doc:"Suppress some log messages"
    ["log-suppress"]

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

let virtwl_proxy = Term.(ret (const main $ tag $ verbose $ log_suppress $ wayland_display $ args))

let term_exit (x : unit Term.result) = Term.exit x

let () =
  term_exit @@ Term.eval (virtwl_proxy, Term.info "wayland-proxy-virtwl")
