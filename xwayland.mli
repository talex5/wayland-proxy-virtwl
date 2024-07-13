val listen :
  proc_mgr:_ Eio_unix.Process.mgr ->
  config:Config.t ->
  connect_host:(sw:Eio.Switch.t -> Host.t) ->
  display:int ->
  _ Eio_unix.Net.listening_socket ->
  'a
(** [listen ~proc_mgr ~config ~connect_host ~display fd] waits for a connection on [fd].
    If one arrives, it spawns Xwayland to handle it (and any further connections).
    It also plays the role of an X11 window manager. *)
