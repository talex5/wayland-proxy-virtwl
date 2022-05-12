(** OCaml wrapper around /dev/dri/ virtio-gpu devices. *)

type 'a t
  constraint 'a = [< `Wayland | `Alloc ]

type query = {
  width : int32;
  height : int32;
  drm_format : Drm_format.t;
}

type image = {
  query : query;
  fd : Unix.file_descr;
  host_size : int64;
  offset : int32;
  stride : int32;
}

val of_fd : Lwt_unix.file_descr -> 'a t option
(** [of_fd x] checks that [x] is a virtio-gpu device and
    initialises it. Returns [None] if it's not a virtio-device.
    The result can be used either as a Wayland channel or
    for allocating host buffers, but not both (due to a race in the protocol).
    If you need to do both, open the device file twice and call this on both FDs. *)

val alloc : [`Alloc] t -> query -> image
(** [alloc t query] allocates a buffer matching [query] on the host and returns a handle to it.
    Use {!Utils.safe_map_file} to map it. *)

val poll : [`Wayland] t -> unit
(** [poll t] indicates that we are ready for the host to write the next event to the shared page. *)

val send : [`Wayland] t -> Cstruct.t -> Unix.file_descr list -> unit
(** [send t msg fds] sends a Wayland message to the host. *)

val handle_event : [`Wayland] t -> bytes -> [
    | `Recv of Cstruct.t * Unix.file_descr list  (** Note: data is a view onto the ring *)
    | `Again
  ] Lwt.t
(** [handle_event t buf] processes data read from the device FD.
    [buf] is expected to be an 8-byte message saying to look in the shared page. *)

val close : _ t -> unit Lwt.t
(** [close t] closes the underlying FD. *)

val is_closed : _ t -> bool
(** [is_closed t] is [true] after [close t] has been called. *)
