open Wayland

let motion = ref true
let shm = ref true
let delete = ref true
let region = ref true
let drawing = ref true
let hints = ref true

let trace iface name =
  match iface, name with
  | "wl_display", "delete_id" -> !delete
  (* Motion *)
  | "wl_pointer", ("frame" | "motion") -> !motion
  (* Drawing *)
  | "wl_surface", ("attach" | "frame" | "damage" | "damage_region") -> !drawing
  (* Regions *)
  | "wl_compositor", "create_region"
  | "wl_region", _
  | "wl_surface", "set_input_region" -> !region
  (* Shared memory *)
  | ("wl_shm" | "wl_shm_pool" | "wl_buffer"), _ -> !shm
  (* Hints *)
  | "xdg_toplevel", ("set_min_size" | "set_max_size")
  | "xdg_surface", "set_window_geometry"
  | "wl_surface", ("set_buffer_scale") -> !hints
  | _ -> true

module Host = struct
  let src = Logs.Src.create "wl-server" ~doc:"host-side of Wayland proxy"
  module Log = (val Logs.src_log src : Logs.LOG)

  type role = [`Client]

  let inbound (type a) (proxy : (a, _, _) Proxy.t) msg =
    Log.info (fun f ->
        let (module M : Metadata.S with type t = a) = Proxy.metadata proxy in
        let msg_name, arg_info = M.events (Msg.op msg) in
        if trace M.interface msg_name then (
          f "@[<h>          <- %a.%s %a@]"
            Proxy.pp proxy
            msg_name
            (Msg.pp_args arg_info) msg
        )
      )

  let outbound (type a) (proxy : (a, _, _) Proxy.t) msg =
    Log.info (fun f ->
        let (module M) = Proxy.metadata proxy in
        let msg_name, arg_info = M.requests (Msg.op msg) in
        if trace M.interface msg_name then (
          f "@[<h>          -> %a.%s %a@]"
            Proxy.pp proxy
            msg_name
            (Msg.pp_args arg_info) msg
        )
      )
end

module Client = struct
  let src = Logs.Src.create "wl-client" ~doc:"client-side of Wayland proxy"
  module Log = (val Logs.src_log src : Logs.LOG)

  type role = [`Server]

  let inbound (type a) (proxy : (a, _, _) Proxy.t) msg =
    Log.info (fun f ->
        let (module M : Metadata.S with type t = a) = Proxy.metadata proxy in
        let msg_name, arg_info = M.requests (Msg.op msg) in
        if trace M.interface msg_name then (
          f "%a -> @[<h>%a.%s %a@]"
            Proxy.pp_transport proxy
            Proxy.pp proxy
            msg_name
            (Msg.pp_args arg_info) msg
        )
      )

  let outbound (type a) (proxy : (a, _, _) Proxy.t) msg =
    Log.info (fun f ->
        let (module M) = Proxy.metadata proxy in
        let msg_name, arg_info = M.events (Msg.op msg) in
        if trace M.interface msg_name then (
          f "%a <- @[<h>%a.%s %a@]"
            Proxy.pp_transport proxy
            Proxy.pp proxy
            msg_name
            (Msg.pp_args arg_info) msg
        )
      )
end

let pp_status f = function
  | Unix.WEXITED x -> Fmt.pf f "exited with status %d" x
  | Unix.WSIGNALED x -> Fmt.pf f "killed by signal %d" x
  | Unix.WSTOPPED x -> Fmt.pf f "stopped by signal %d" x

let reporter =
  let report src level ~over k msgf =
    let src = Logs.Src.name src in
    msgf @@ fun ?header ?tags:_ fmt ->
    Fmt.kstr (fun line ->
        output_string stderr line;
        flush stderr;
        over ();
        k ()
      )
      ("%11s %a: @[" ^^ fmt ^^ "@]@.")
      src
      Logs_fmt.pp_header (level, header)
  in
  { Logs.report = report }

let handle_async_error ex =
  let bt = Printexc.get_raw_backtrace () in
  Log.err (fun f -> f "Uncaught async exception: %a" Fmt.exn_backtrace (ex, bt))

let () =
  Lwt.async_exception_hook := handle_async_error;
  Fmt_tty.setup_std_outputs ();
  Logs.set_reporter reporter;
  Printexc.record_backtrace true

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
      | "client" -> Logs.Src.set_level Client.src (Some Logs.Info)
      | "server" -> Logs.Src.set_level Host.src (Some Logs.Info)
      | "xwayland" ->
        Logs.Src.set_level X11.log_src (Some Logs.Info);
        Logs.Src.set_level Log.xwayland_src (Some Logs.Info)
      | "" -> ()
      | x -> Log.warn (fun f -> f "Unknown $%s item %S" wayland_env x)
    );
  log_suppress |> List.iter (function
      | `Motion -> motion := false
      | `Shm -> shm := false
      | `Delete -> delete := false
      | `Region -> region := false
      | `Drawing -> drawing := false
      | `Hints -> hints := false
    )

open Cmdliner

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

let cmdliner =
  let make verbose log_suppress () =
    setup_logging ~verbose ~log_suppress
  in
  Term.(const make $ verbose $ log_suppress)
