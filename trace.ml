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

let reporter =
  let report src level ~over k msgf =
    let src = Logs.Src.name src in
    msgf @@ fun ?header ?tags:_ fmt ->
    Fmt.kstrf (fun line ->
        print_endline line;
        over ();
        k ()
      )
      ("%11s %a: @[" ^^ fmt ^^ "@]")
      src
      Logs_fmt.pp_header (level, header)
  in
  { Logs.report = report }
