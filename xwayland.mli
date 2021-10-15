val listen : config:Config.t -> display:int -> Lwt_unix.file_descr -> 'a Lwt.t
(** [listen ~config ~display fd] waits for a connection on [fd].
    If one arrives, it spawns Xwayland to handle it (and any further connections).
    It also plays the role of an X11 window manager. *)
