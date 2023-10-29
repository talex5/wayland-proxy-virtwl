(** This module knows the layout of the C types and structures, wrapping them in safer OCaml types. *)

type gem_handle
(** A handle assigned by the guest Linux kernel to refer to a resource.
    Local to the open device file. *)

type 'a to_host
(** A cross-domain message, sent from the guest to the host. *)

module Res_handle : sig
  type t
  (** A handle assigned by the host. *)

  val pp : t Fmt.t

  module Map : Map.S with type key = t

  val pipe_read_start : t
  (** Read pipe IDs start at this value *)

  val next_pipe_id : t -> t
  (** The next value of the pipe ID counter. *)
end

module Capabilities : sig
  type t = {
    version : int32;
    supported_channels : int32;
    supports_dmabuf : bool;
    supports_external_gpu_memory : bool;
  }

  val create_buffer : unit -> Cstruct.buffer

  val of_buffer : Cstruct.buffer -> t
end

module Init_context : sig
  (** Tell Linux that we want to use the context feature, and set the parameters. *)

  type t = Cstruct.buffer

  type ring = [ `Channel ]

  type param = [
    | `Capset_id of [`Cross_domain]
    | `Num_rings of int
    | `Poll_rings_mask of ring list
  ]

  val create : param list -> t
end

module Create_blob : sig
  (** Message creating a new resource shared with the host. *)
  type t

  val request :
    [< `Guest | `Host3D of Res_handle.t ] ->
    mappable:bool ->
    shareable:bool ->
    size:int64 ->
    t

  val parse : t -> gem_handle * Res_handle.t
end

module Cross_domain_init : sig
  type t = [`Init] to_host

  val create :
    query_ring:Res_handle.t ->
    channel_ring:Res_handle.t ->
    channel_type:[< `Camera | `Wayland ] -> t
  (** [create ~query_ring ~channel_ring ~channel_type] asks the host
      to use [query_ring] and [channel_ring] for the [channel_type] protocol. *)
end

module Cross_domain_poll : sig
  (** A cross-domain poll tells the host that the shared ring is ready for the next event. *)

  type t = [`Poll] to_host

  val v : t
end

module Cross_domain_send_recv : sig
  type t = [`Send] to_host

  val create : Cstruct.t -> (Res_handle.t * [`Blob | `Read_pipe]) list -> t
  (** Send a chunk of Wayland data (with FDs) to the host compositor. *)
end

module Cross_domain_read_write : sig
  type t = [`Write_pipe] to_host

  val create : id:Res_handle.t -> Cstruct.t -> t
  (** [create ~id buf] is a message telling the host to write [buf] to host pipe [id]. *)
end

module Cross_domain_image_requirements : sig
  (** To share image data with the host, we tell it the details of the image and it
      returns some metadata and a resource ID which we can use to create the blob.
      The returned ID can be used multiple times, so it seems to be a kind of template. *)

  type t = [`Image_req] to_host

  val create :
    linear:bool ->
    scanout:bool ->
    drm_format:Drm_format.t ->
    width:int32 ->
    height:int32 ->
    t

  val parse :
    Cstruct.t ->
    (stride0:int32 ->
     offset0:int32 ->
     host_size:int64 ->
     blob_id:Res_handle.t -> 'a) ->
    'a
end

module Wayland_ring : sig
  (** Parse the ring as a Wayland channel message. *)

  val parse :
    Cstruct.t ->
    recv:(Cstruct.t -> (Res_handle.t * [`Blob | `Write_pipe] * int64) list -> 'a) ->
    read_pipe:(id:Res_handle.t -> hang_up:bool -> string -> 'a) ->
    'a
  (** @param recv
        The returned Cstruct is a view onto the ring, and will therefore need to be copied before
        the ring is reused. For each attached FD, it returns the resource ID, type and size.
      @param read_pipe
        This asks us to write data to a guest pipe. If [hang_up = true] then the pipe should be closed. *)
end

module Event : sig
  val check : Cstruct.t -> unit
end
