open Eio.Std

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

module Ring_buffer = struct
  type t = {
    path : string;
    buffer_capacity : int;
    mutable active : int;               (* Buffer currently being written to *)
    buffers : Buffer.t array;
  }

  let add t msg =
    let b = t.buffers.(t.active) in
    let b =
      if Buffer.length b + String.length msg <= t.buffer_capacity then b
      else (
        t.active <- (t.active + 1) mod Array.length t.buffers;
        let b = t.buffers.(t.active) in
        Buffer.reset b;
        b
      )
    in
    Buffer.add_string b msg

  let create ~log_ring_size path =
    let n = 4 in
    let buffer_capacity = max (log_ring_size / n) 100 in
    let buffers = Array.init n (fun _ -> Buffer.create buffer_capacity) in
    { path; active = 0; buffers; buffer_capacity }

  let flush_to_channel t ch =
    let rec show_after i =
      let i = (i + 1) mod Array.length t.buffers in
      Buffer.output_buffer ch t.buffers.(i);
      Buffer.reset t.buffers.(i);
      if i <> t.active then show_after i
    in
    show_after t.active

  let flush_to_file t =
    let ch = open_out_gen [Open_append; Open_creat] 0o666 t.path in
    Fun.protect ~finally:(fun () -> close_out ch) @@ fun () ->
    output_string ch "=== flushing log to file ===\n";
    flush_to_channel t ch;
    flush ch
end

let create_ring_control_socket ~sw ~fs ~wayland_display ring =
  let ring_buffer_log_ctl = Printf.sprintf "/run/user/%d/%s-ctl" (Unix.getuid ()) wayland_display in
  if Sys.file_exists ring_buffer_log_ctl then Unix.unlink ring_buffer_log_ctl;
  Unix.mkfifo ring_buffer_log_ctl 0o600;
  let ring_buffer_log_ctl = (fs, ring_buffer_log_ctl) in
  Fiber.fork_daemon ~sw (fun () ->
      while true do
        Eio.Path.with_open_in ring_buffer_log_ctl @@ fun pipe ->
        match Eio.Buf_read.(parse line) pipe ~max_size:1024 with
        | Ok cmd ->
          Log.warn (fun f -> f "Got command on %a: %S (dumping log)" Eio.Path.pp ring_buffer_log_ctl cmd);
          Ring_buffer.flush_to_file ring
        | Error (`Msg e) ->
          Log.warn (fun f -> f "No command on %a: %s" Eio.Path.pp ring_buffer_log_ctl e);
      done;
      assert false
    )

let ms_of_time x = (truncate (x *. 1000.)) mod 1000

let pp_timestamp f x =
  let open Unix in
  let tm = localtime x in
  Fmt.pf f "%04d-%02d-%02d %02d:%02d:%02d.%03d" (tm.tm_year + 1900) (tm.tm_mon + 1)
    tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec (ms_of_time x)

let reporter ring =
  let report src level ~over k msgf =
    let src = Logs.Src.name src in
    msgf @@ fun ?header ?tags:_ fmt ->
    Fmt.kstr (fun line ->
        begin match ring with
          | Some ring ->
            Ring_buffer.add ring line;
            if level < Logs.Info then (
              output_string stderr line;
              flush stderr;
            )
          | None ->
            output_string stderr line;
            flush stderr;
        end;
        over ();
        k ()
      )
      ("%a %11s %a: @[" ^^ fmt ^^ "@]@.")
      pp_timestamp (Unix.gettimeofday ())
      src
      Logs.pp_header (level, header)
  in
  { Logs.report = report }

let setup_logging ~sw ~fs ~verbose ~log_suppress ~log_ring_file ~log_ring_size ~wayland_display =
  let ring = Option.map (Ring_buffer.create ~log_ring_size) log_ring_file in
  Logs.set_reporter (reporter ring);
  Option.iter (create_ring_control_socket ~sw ~fs ~wayland_display) ring;
  Printexc.record_backtrace true;
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

let log_ring_file =
  Arg.value @@
  Arg.(opt (some string)) None @@
  Arg.info
    ~doc:"Where to dump the log-ring on error"
    ["log-ring-path"]

let log_ring_size =
  Arg.value @@
  Arg.(opt int) 0x80000 @@
  Arg.info
    ~doc:"Size of the log ring buffer (if used)"
    ["log-ring-size"]

let cmdliner ~sw ~fs =
  let make verbose log_suppress log_ring_file log_ring_size ~wayland_display =
    setup_logging ~sw ~fs ~verbose ~log_suppress ~log_ring_file ~log_ring_size ~wayland_display
  in
  Term.(const make $ verbose $ log_suppress $ log_ring_file $ log_ring_size)
