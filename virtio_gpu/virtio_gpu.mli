module Drm_format = Drm_format
module Dev = Dev
module Wayland_dmabuf = Wayland_dmabuf

type transport = < Wayland.S.transport; close : unit >

type t

val default_dri_dir : 'a Eio.Path.t -> 'a Eio.Path.t
(** [default_dri_dir fs] is [fs / "/dev/dri"]. *)

val find_device : sw:Eio.Switch.t -> _ Eio.Path.t -> (t, [> `Msg of string]) result
(** [find_device ~sw dri_dir] searches for a virtio-gpu device in [dri_dir] (see {!default_dri_dir}). *)

val connect_wayland : sw:Eio.Switch.t -> t -> transport
(** [connect_wayland t] creates a new Wayland transport from [t]. *)

val close : t -> unit

val alloc : t -> Dev.query -> Dev.image
(** [alloc t query] allocates a buffer matching [query] on the host and returns a handle to it.
    Use {!Utils.safe_map_file} to map it. *)

val probe_drm : t -> Wayland_dmabuf.t -> bool
(** [probe_drm t dma] tries to create a buffer backed by video memory.
    Returns [true] if this works.
    Caches the result on [t] for future calls. *)

module Utils = Utils
