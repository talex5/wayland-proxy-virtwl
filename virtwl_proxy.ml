open Lwt.Syntax

let socket_path = "/run/user/1000/wayland-1"

let rec listen ~config socket =
  let* (client, _addr) = Lwt_unix.accept socket in
  Log.info (fun f -> f "New connection");
  Lwt.async (fun () -> Relay.handle ~config client);
  listen ~config socket

let () =
  Logs.set_reporter (Logs_fmt.reporter ());
  (* Logs.(set_level (Some Info)); *)
  Printexc.record_backtrace true

let main tag args =
  let config = { Config.tag } in
  Lwt_main.run begin
    let listening_socket = Unix.(socket PF_UNIX SOCK_STREAM 0) in
    if Sys.file_exists socket_path then Unix.unlink socket_path;
    Unix.bind listening_socket (Unix.ADDR_UNIX socket_path);
    Unix.listen listening_socket 5;
    Log.info (fun f -> f "Listening on %S" socket_path);
    Lwt.async (fun () -> listen ~config (Lwt_unix.of_unix_file_descr listening_socket));
    Unix.putenv "WAYLAND_DISPLAY" socket_path;
    let child = Lwt_process.open_process ("", Array.of_list args) in
    let* _ = child#status in
    Lwt.return ()
  end

open Cmdliner

let tag =
  Arg.value @@
  Arg.(opt string) "" @@
  Arg.info
    ~doc:"Tag to prefix to window titles"
    ["tag"]

let args =
  Arg.value @@
  Arg.(pos_all string) ["evince"] @@
  Arg.info
    ~doc:"Sub-command to execute"
    []

let virtwl_proxy = Term.(const main $ tag $ args)

let term_exit (x : unit Term.result) = Term.exit x

let () =
  term_exit @@ Term.eval (virtwl_proxy, Term.info "wayland-proxy-virtwl")
