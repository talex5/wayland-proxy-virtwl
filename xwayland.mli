val listen :
  proc_mgr:#Eio_unix.Process.mgr ->
  net:#Eio_unix.Net.t ->
  config:Config.t ->
  virtio_gpu:Virtio_gpu.t option ->
  display:int ->
  #Eio_unix.Net.listening_socket ->
  'a
(** [listen ~config ~virtio_gpu ~display fd] waits for a connection on [fd].
    If one arrives, it spawns Xwayland to handle it (and any further connections).
    It also plays the role of an X11 window manager. *)
