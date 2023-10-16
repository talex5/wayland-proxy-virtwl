type surface_data = ..
(** Extra data that the Xwayland support may add to a surface. *)

type xwayland_hooks = <
  on_create_surface :
    'v. ([< `V1 | `V2 | `V3 | `V4 | `V5 | `V6] as 'v) H.Wl_surface.t -> 'v C.Wl_surface.t ->
    set_configured:([`Show | `Hide | `Unmanaged] -> unit) ->
    unit;
  (** Called when a new client wl_surface is created, along with the new host surface.
      Delivery of requests to the host surface is paused at this point; call [set_configured] to release the queue.
      Pass [`Hide] to suppress attaching any buffers; this will keep the window invisible (hack for GTK DnD windows). *)

  on_destroy_surface :
    'v. ([< `V1 | `V2 | `V3 | `V4 | `V5 | `V6] as 'v) H.Wl_surface.t ->
    unit;
  (** Called when the client destroys the surface, just before forwarding the destroy to the host surface.
      Use this opportunity to destroy any role attached to the host surface. *)

  on_pointer_entry : 'v.
    surface:([< `V1 | `V2 | `V3 | `V4 | `V5 | `V6] as 'v) H.Wl_surface.t ->
    forward_event:(unit -> unit) ->
    unit;
  (** Called when the pointer enters a surface. Call [forward_event] to forward the enter event to the client. *)

  on_keyboard_entry : 'v.
    surface:([< `V1 | `V2 | `V3 | `V4 | `V5 | `V6] as 'v) H.Wl_surface.t ->
    forward_event:(unit -> unit) ->
    unit;
  (** Called when the keyboard enters a surface. Call [forward_event] to forward the enter event to the client. *)

  on_keyboard_leave : 'v.
    surface:([< `V1 | `V2 | `V3 | `V4 | `V5 | `V6] as 'v) H.Wl_surface.t ->
    unit;
  (** Called when the keyboard leaves a surface. *)

  set_ping : (unit -> unit) -> unit;
  (** When/if Xwayland creates an xdg_wm_base object, this is called to provide a ping function.
      This does a round-trip to the client, ensuring that all previously sent events have been delivered. *)

  scale : int32;
  (** The buffer scale to set and then use to transform coordinates (for HiDPI screens). *)
>

val run : ?xwayland:xwayland_hooks -> config:Config.t -> Host.t -> _ Eio_unix.Net.stream_socket -> unit
(** [run ~config host client] talks the Wayland protocol to [client], relaying messages to [host].
    Returns when the client disconnects gracefully. Raises an exception on protocol error. *)

val set_surface_data : _ H.Wl_surface.t -> surface_data -> unit
(** [set_surface_data surface data] attaches [surface_data] to [surface]. *)

val get_surface_data : _ H.Wl_surface.t -> surface_data
(** [get_surface_data surface] returns data previously set with [set_surface_data].
    If none is set, returns a private value of type [surface_data]. *)
