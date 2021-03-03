open Lwt.Syntax

let rec listen ~config socket =
  let* (client, _addr) = Lwt_unix.accept socket in
  Log.info (fun f -> f "New connection");
  Lwt.async (fun () -> Relay.handle ~config client);
  listen ~config socket

let () =
  Logs.set_reporter (Logs_fmt.reporter ());
  (* Logs.(set_level (Some Info)); *)
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

let main tag wayland_display args =
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

let virtwl_proxy = Term.(ret (const main $ tag $ wayland_display $ args))

let term_exit (x : unit Term.result) = Term.exit x

let () =
  term_exit @@ Term.eval (virtwl_proxy, Term.info "wayland-proxy-virtwl")
