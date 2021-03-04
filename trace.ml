open Wayland

let trace = function
(*
  | "xdg_toplevel"
  | "xdg_surface"
  | "wl_compositor"
  | "wl_region"
  | "wl_buffer"
  | "wl_pointer"
  | "wl_callback"
  | "wl_keyboard"
  | "wl_surface" -> false
*)
  | _ -> true

module Host : Client.TRACE = struct
  type role = [`Client]

  let inbound (type a) (proxy : (a, _, _) Proxy.t) msg =
    Log.info (fun f ->
        let (module M : Metadata.S with type t = a) = Proxy.metadata proxy in
        if trace M.interface then (
          let msg_name, arg_info = M.events (Msg.op msg) in
          f "@[<h>          <- %a.%s %a@]"
            Proxy.pp proxy
            msg_name
            (Msg.pp_args arg_info) msg
        )
      )

  let outbound (type a) (proxy : (a, _, _) Proxy.t) msg =
    Log.info (fun f ->
        let (module M) = Proxy.metadata proxy in
        if trace M.interface then (
          let msg_name, arg_info = M.requests (Msg.op msg) in
          f "@[<h>          -> %a.%s %a@]"
            Proxy.pp proxy
            msg_name
            (Msg.pp_args arg_info) msg
        )
      )
end

module Client : Server.TRACE = struct
  type role = [`Server]

  let inbound (type a) (proxy : (a, _, _) Proxy.t) msg =
    Log.info (fun f ->
        let (module M : Metadata.S with type t = a) = Proxy.metadata proxy in
        if trace M.interface then (
          let msg_name, arg_info = M.requests (Msg.op msg) in
          f "-> @[<h>%a.%s %a@]"
            Proxy.pp proxy
            msg_name
            (Msg.pp_args arg_info) msg
        )
      )

  let outbound (type a) (proxy : (a, _, _) Proxy.t) msg =
    Log.info (fun f ->
        let (module M) = Proxy.metadata proxy in
        if trace M.interface then (
          let msg_name, arg_info = M.events (Msg.op msg) in
          f "<- @[<h>%a.%s %a@]"
            Proxy.pp proxy
            msg_name
            (Msg.pp_args arg_info) msg
        )
      )
end
