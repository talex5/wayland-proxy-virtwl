module Drm_format = Drm_format
module Dev = Dev
module Wayland_dmabuf = Wayland_dmabuf

type transport = < Wayland.S.transport; close : unit >

type t

val device_string : t -> string
(** [device_string t] returns the [dev_t] corresponding to the backing file descriptor. *)

val default_dri_dir : 'a Eio.Path.t -> 'a Eio.Path.t
(** [default_dri_dir fs] is [fs / "/dev/dri"]. *)

val find_device : sw:Eio.Switch.t -> _ Eio.Path.t -> (t, [> `Msg of string]) result
(** [find_device ~sw dri_dir] searches for a virtio-gpu device in [dri_dir] (see {!default_dri_dir}). *)

val close : t -> unit

val wayland_transport : t -> transport

val alloc : t -> gpu:bool -> Dev.query -> Dev.image
(** [alloc t query] allocates a buffer matching [query] on the host and returns a handle to it.
    Use {!Utils.safe_map_file} to map it. *)

val get_dev : t -> Eio_unix.Fd.t
(** [fd t] returns the underlying file descriptor *)

module Utils = Utils
