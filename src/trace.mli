module Client : Wayland.Server.TRACE
module Host : Wayland.Client.TRACE

val pp_status : Unix.process_status Fmt.t

val cmdliner : sw:Eio.Switch.t -> fs:_ Eio.Fs.dir -> (wayland_display:string -> unit) Cmdliner.Term.t
