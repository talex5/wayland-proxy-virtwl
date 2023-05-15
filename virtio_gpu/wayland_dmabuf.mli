(** Using Linux_dmabuf_unstable_v1 to create Wayland buffers backed by video memory. *)

type modifiers = { hi : int32; lo : int32 }
type fmt = Drm_format.t * modifiers

type t

val create : Wayland.Client.t -> Wayland.Registry.t -> t option
(** [create wayland reg] gets a Linux_dmabuf_unstable_v1 from the registry and
    collects the supported formats. Returns [None] if this interface is missing. *)

val probe_drm : t -> Dev.image -> bool
(** [probe_drm t img] tries to create a test Wl_buffer from [img].
    Returns [true] iff this succeeds, indicating that we have access to video memory. *)

val get_format : t -> Drm_format.t -> fmt option
(** [get_format t fmt] returns the full format (with modifiers) for [fmt], if supported by [t]. *)

val create_immed :
  t ->
  fmt -> Dev.image ->
  ([`Wl_buffer], [`V1], [ `Client ]) #Wayland.Proxy.Handler.t ->
  [`V1] Wayland.Wayland_client.Wl_buffer.t
(** [create_immed t fmt img] returns a function for creating a Wl_buffer from [img]. *)
