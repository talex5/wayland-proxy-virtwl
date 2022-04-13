module Dev = Dev

type transport = < Wayland.S.transport; close : unit Lwt.t >

type t

val find_device : ?dri_dir:string -> unit -> (t, [> `Msg of string]) Lwt_result.t
(** [find_device ()] searches for a virtio-gpu device. *)

val connect_wayland : t -> transport Lwt.t
(** [connect_wayland t] creates a new Wayland transport from [t]. *)

val close : t -> unit Lwt.t

val alloc : t -> size:int -> Unix.file_descr
(** [alloc t ~size] allocates a buffer of [size] bytes on the host and returns a handle to it.
    Use {!Utils.safe_map_file} to map it. *)

module Utils = Utils
