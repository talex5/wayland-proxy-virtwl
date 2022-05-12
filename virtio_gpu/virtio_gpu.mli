module Drm_format = Drm_format
module Dev = Dev
module Wayland_dmabuf = Wayland_dmabuf

type transport = < Wayland.S.transport; close : unit Lwt.t >

type t

val find_device : ?dri_dir:string -> unit -> (t, [> `Msg of string]) Lwt_result.t
(** [find_device ()] searches for a virtio-gpu device. *)

val connect_wayland : t -> transport Lwt.t
(** [connect_wayland t] creates a new Wayland transport from [t]. *)

val close : t -> unit Lwt.t

val alloc : t -> Dev.query -> Dev.image
(** [alloc t query] allocates a buffer matching [query] on the host and returns a handle to it.
    Use {!Utils.safe_map_file} to map it. *)

val probe_drm : t -> Wayland_dmabuf.t -> bool Lwt.t
(** [probe_drm t dma] tries to create a buffer backed by video memory.
    Returns [true] if this works.
    Caches the result on [t] for future calls. *)

module Utils = Utils
