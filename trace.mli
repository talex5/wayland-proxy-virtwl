module Client : Wayland.Server.TRACE
module Host : Wayland.Client.TRACE

val pp_status : Unix.process_status Fmt.t

val cmdliner : (wayland_display:string -> unit Lwt.t) Cmdliner.Term.t
