(* Relay Wayland messages between a client and a virtio_gpu host compositor.
   When sending a file descriptor, we create a virtio_gpu descriptor of the appropriate type and send that instead.
   For streams, we copy the data.
   For buffers we copy the contents when the surface is committed (todo: copy just the damaged region).
   We generally ignore the version part of the ocaml-wayland types and just cast as necessary.
   Since we're relaying, we know that both sides are using the same version, so if we get e.g. a
   version 5 request from the client then we know it's safe to send it to the host. *)

open Eio.Std
open Wayland

(* Since we're just relaying messages, we mostly don't care about checking version compatibility.
   e.g. if a client sends us a v5 message, then we can assume the corresponding server object
   supports v5 too (otherwise the client shouldn't have sent it).
   So we just cast away version contraints using [cv]. *)
let cv = Proxy.cast_version

type surface_data = ..

type surface_data += No_surface_data

type xwayland_hooks = <
  on_create_surface :
    'v. ([< `V1 | `V2 | `V3 | `V4 | `V5 | `V6] as 'v) H.Wl_surface.t -> 'v C.Wl_surface.t ->
    set_configured:([`Show | `Hide | `Unmanaged] -> unit) ->
    unit;

  on_destroy_surface :
    'v. ([< `V1 | `V2 | `V3 | `V4 | `V5 | `V6] as 'v) H.Wl_surface.t ->
    unit;

  on_pointer_entry : 'v.
    surface:([< `V1 | `V2 | `V3 | `V4 | `V5 | `V6] as 'v) H.Wl_surface.t ->
    forward_event:(unit -> unit) ->
    unit;

  on_keyboard_entry : 'v.
    surface:([< `V1 | `V2 | `V3 | `V4 | `V5 | `V6] as 'v) H.Wl_surface.t ->
    forward_event:(unit -> unit) ->
    unit;

  on_keyboard_leave : 'v.
    surface:([< `V1 | `V2 | `V3 | `V4 | `V5 | `V6] as 'v) H.Wl_surface.t ->
    unit;

  set_ping : (unit -> unit) -> unit;

  scale : int32;
>

let scale_to_client ~xwayland (x, y) =
  match xwayland with
  | None -> (x, y)
  | Some xw ->
    let scale = xw#scale in
    (Int32.mul x scale, Int32.mul y scale)

let scale_to_host ~xwayland (x, y) =
  match xwayland with
  | None -> (x, y)
  | Some xw ->
    let scale = xw#scale in
    (Int32.div x scale, Int32.div y scale)

let point_to_client ~xwayland (x, y) =
  match xwayland with
  | None -> (x, y)
  | Some xw ->
    let scale = xw#scale in
    if scale = 1l then (x, y)
    else (
      Fixed.of_bits (Int32.mul (Fixed.to_bits x) scale),
      Fixed.of_bits (Int32.mul (Fixed.to_bits y) scale)
    )

type t = {
  host : Host.t;
  config : Config.t;
}

let update_serial t serial = t.host.last_serial <- serial

(* Data attached to host objects (e.g. the corresponding client object).
   Host and client versions are assumed to match. *)
module HD = struct
  type 'v surface = {
    client : 'v C.Wl_surface.t;
    mutable data : surface_data;
  }

  type 'a t =
    | Surface        : 'v surface                            -> [`Wl_surface]                     t
    | Data_offer     : 'v C.Wl_data_offer.t                  -> [`Wl_data_offer]                  t
    | Gtk_data_offer : 'v C.Gtk_primary_selection_offer.t    -> [`Zwp_primary_selection_offer_v1] t
    | Zwp_data_offer : 'v C.Zwp_primary_selection_offer_v1.t -> [`Zwp_primary_selection_offer_v1] t
    | Output         : 'v C.Wl_output.t                      -> [`Wl_output]                      t
end

(* Data attached to client objects (e.g. the corresponding host object).
   Host and client versions are assumed to match. *)
module CD = struct
  type 'v virtwl_buffer = {
    host_buffer : 'v H.Wl_buffer.t;
    host_memory : Cstruct.t;
    client_memory : Cstruct.t;
  }

  type 'v buffer = [
    | `Virtwl of 'v virtwl_buffer Lazy.t
    | `Direct of 'v H.Wl_buffer.t (* This also includes dma buffers,
                                     whether direct or otherwise!
                                     All of the complexity is handled entirely by
                                     the guest kernel. *)
  ]

  type surface_state =
    | Ready
    | Unconfigured of (unit -> unit) Queue.t      (* Events to forward once configured *)
    | Destroyed

  type 'v surface = {
    host_surface : 'v H.Wl_surface.t;
    mutable state : surface_state;
    mutable host_memory : Cstruct.t;
    mutable client_memory : Cstruct.t;
  }

  type 'a t =
    | Region               : 'v H.Wl_region.t                       -> [`Wl_region]                       t
    | Surface              : 'v surface                             -> [`Wl_surface]                      t
    | Buffer               : 'v buffer                              -> [`Wl_buffer]                       t
    | Seat                 : 'v H.Wl_seat.t                         -> [`Wl_seat]                         t
    | Output               : 'v H.Wl_output.t                       -> [`Wl_output]                       t
    | Toplevel             : 'v H.Xdg_toplevel.t                    -> [`Xdg_toplevel]                    t
    | Xdg_surface          : 'v H.Xdg_surface.t                     -> [`Xdg_surface]                     t
    | Xdg_positioner       : 'v H.Xdg_positioner.t                  -> [`Xdg_positioner]                  t
    | Data_source          : 'v H.Wl_data_source.t                  -> [`Wl_data_source]                  t
    | Gtk_source           : 'v H.Zwp_primary_selection_source_v1.t -> [`Gtk_primary_selection_source]    t
    | Pointer              : 'v H.Wl_pointer.t                      -> [`Wl_pointer]                      t
    | Zwp_source           : 'v H.Zwp_primary_selection_source_v1.t -> [`Zwp_primary_selection_source_v1] t
end

(* Note: the role here is our role: [`Server] data is attached to proxies to
 our clients (where we are the server), while [`Client] data is attached to host objects. *)
type ('a, 'role) user_data =
  | Client_data      : 'a CD.t -> ('a, [`Server]) user_data
  | Host_data        : 'a HD.t -> ('a, [`Client]) user_data

type ('a, 'role) Wayland.S.user_data += Relay of ('a, 'role) user_data

let host_data x = Relay (Host_data x)
let client_data x = Relay (Client_data x)

let user_data (proxy : ('a, _, 'role) Proxy.t) : ('a, 'role) user_data =
  match Wayland.Proxy.user_data proxy with
  | Relay x -> x
  | S.No_data -> Fmt.failwith "No data attached to %a!" Proxy.pp proxy
  | _ -> Fmt.failwith "Unexpected data attached to %a!" Proxy.pp proxy

let to_client (type a) (h : (a, 'v, [`Client]) Proxy.t) : (a, 'v, [`Server]) Proxy.t =
  let cv = Proxy.cast_version in
  let Host_data data = user_data h in
  let open HD in
  match data with
  | Output c -> cv c
  | Surface c -> cv c.client
  | Data_offer c -> cv c
  | Zwp_data_offer c -> cv c
  | Gtk_data_offer _ ->
    (* Here, a client Gtk corresponds to a host Zwp, so the types aren't right. *)
    failwith "Can't use to_client with GTK translation"

let to_host (type a) (c : (a, 'v, [`Server]) Proxy.t) : (a, 'v, [`Client]) Proxy.t =
  let cv = Proxy.cast_version in
  let Client_data data = user_data c in
  match data with
  | Surface x -> cv x.host_surface
  | Seat x -> cv x
  | Output x -> cv x
  | Region x -> cv x
  | Toplevel x -> cv x
  | Xdg_surface x -> cv x
  | Xdg_positioner x -> cv x
  | Data_source x -> cv x
  | Zwp_source x -> cv x
  | Buffer (`Virtwl x) -> cv (Lazy.force x).host_buffer
  | Buffer (`Direct x) -> cv x
  | Pointer c -> cv c
  | Gtk_source _ ->
    (* Here, a client Gtk corresponds to a host Zwp, so the types aren't right. *)
    failwith "Can't use to_host with GTK translation"

(* Validation *)
module V = struct
  let max_window_width_int32 = 16384l
  let max_window_height_int32 = 6144l
  let check_width_height_int32 t raise ~untrusted_width ~untrusted_height =
    if untrusted_width <= 0l then (
      raise t ~message:"Width is negative or 0"
    ) else if untrusted_width > max_window_width_int32 then (
      raise t ~message:"Width is excessive"
    ) else if untrusted_height <= 0l then (
      raise t ~message:"Height is negative or 0"
    ) else if untrusted_height > max_window_height_int32 then (
      raise t ~message:"Height is excessive"
    ) else (
      ()
    )
  let check_max_width_height t raise ~untrusted_width ~untrusted_height =
    if untrusted_width < 0l then (
      raise t ~message:(Format.asprintf "Width %ld is negative" untrusted_width)
    ) else if untrusted_width > max_window_width_int32 then (
      raise t ~message:(Format.asprintf "Width %ld is excessive" untrusted_width)
    ) else if untrusted_height < 0l then (
      raise t ~message:(Format.asprintf "Height %ld is negative" untrusted_height)
    ) else if untrusted_height > max_window_height_int32 then (
      raise t ~message:(Format.asprintf "Height %ld is excessive" untrusted_height)
    ) else (
      ()
    )
  let check_x_y t raise ~untrusted_x ~untrusted_y =
    if untrusted_x < Int32.sub 0l max_window_width_int32 then (
      raise t ~message:(Format.asprintf "X %ld is too negative" untrusted_x)
    ) else if untrusted_x > max_window_width_int32 then (
      raise t ~message:(Format.asprintf "X %ld is excessive" untrusted_x)
    ) else if untrusted_y < Int32.sub 0l max_window_height_int32 then (
      raise t ~message:(Format.asprintf "Y %ld is too negative" untrusted_y)
    ) else if untrusted_y > max_window_height_int32 then (
      raise t ~message:(Format.asprintf "Y %ld is excessive" untrusted_y)
    ) else (
      ()
    )
end

(* When the client asks to destroy something, delay the ack until the host object is destroyed.
   This means the client sees events in the usual order, and means we can continue forwarding
   any events the host sends before hearing about the deletion. *)
let delete_with fn host client =
  Proxy.on_delete host (fun () -> if Proxy.transport_up client then Proxy.delete client);
  fn host

let bad_impl _ ~message = Msg.bad_implementation message

let make_region ~host_region r =
  let h = host_region @@ new H.Wl_region.v1 in
  let user_data = client_data (Region h) in
  Proxy.Handler.attach r @@ object
    inherit [_] C.Wl_region.v1
    method! user_data = user_data
    method on_add t ~untrusted_x ~untrusted_y ~untrusted_width ~untrusted_height =
      V.check_x_y t bad_impl ~untrusted_x ~untrusted_y;
      V.check_width_height_int32 t bad_impl ~untrusted_width ~untrusted_height;
      let (x, y, width, height) = (untrusted_x, untrusted_y, untrusted_width, untrusted_height) in
      (* sanitize end *)
      H.Wl_region.add h ~x ~y ~width ~height
    method on_subtract t ~untrusted_x ~untrusted_y ~untrusted_width ~untrusted_height =
      V.check_x_y t bad_impl ~untrusted_x ~untrusted_y;
      V.check_width_height_int32 t bad_impl ~untrusted_width ~untrusted_height;
      let (x, y, width, height) = (untrusted_x, untrusted_y, untrusted_width, untrusted_height) in
      (* sanitize end *)
      H.Wl_region.subtract h ~x ~y ~width ~height
    method on_destroy = delete_with H.Wl_region.destroy h
  end

let shm_pool_invalid_stride (proxy:_ C.Wl_shm_pool.t) =
  Format.kasprintf (fun message -> Proxy.post_error proxy ~code:1l ~message)
  [@@ocaml.inline never]
(** [shm_pool_invalid_stride] disconnects the client with an invalid_stride error *)

let shm_pool_invalid_stride_str proxy =
  Proxy.post_error proxy ~code:1l
  [@@ocaml.inline never]
(** [shm_pool_invalid_stride_str] disconnects the client with an invalid_stride error *)

let validate_shm_parameters
      ~(buffer_size:int32)
      ~(untrusted_offset:int32)
      ~(untrusted_width:int32)
      ~(untrusted_height:int32)
      ~(untrusted_stride:int32)
      ~(untrusted_format:int32)
      (shm: _ C.Wl_shm_pool.t) =
  V.check_width_height_int32 shm shm_pool_invalid_stride_str ~untrusted_width ~untrusted_height;
  (if untrusted_offset < 0l then
    shm_pool_invalid_stride shm "Negative offset %ld" untrusted_offset
  else if untrusted_stride < 1l then
    shm_pool_invalid_stride shm "Negative or zero stride %ld" untrusted_stride
  else if buffer_size < 0l then
    assert false (* bug *));
  (* Overflow impossible: [Int32.max_int * Int32.max_int + Int32.max_int < Int64.max_int] *)
  let area = Int64.mul (Int64.of_int32 untrusted_height) (Int64.of_int32 untrusted_stride) in
  let end_pointer = Int64.add area (Int64.of_int32 untrusted_offset) in
  if end_pointer > Int64.of_int32 buffer_size then (
    shm_pool_invalid_stride shm "Buffer extends beyond end of pool: size %ldx%ld, \
                                 stride %ld, offset %ld, area %Ld, end pointer %Ld, buffer size %ld"
      untrusted_width untrusted_height untrusted_stride untrusted_offset
      area end_pointer buffer_size
  ) else (
    if not (Drm_format.validate_shm ~untrusted_offset ~untrusted_width ~untrusted_height
            ~untrusted_stride ~untrusted_format) then
      shm_pool_invalid_stride shm "Invalid buffer parameters for format"
    else
      ()
  )

(* wl_shm memory buffers are allocated by the client inside the guest and
   cannot be shared directly with the host. Instead, we allocate some host
   memory of the same size, map that into the guest, and copy the data across
   as needed.

   Xwayland likes to create huge numbers of mappings and then destroy them
   without ever using the buffers for anything, so to avoid the expense of
   mapping and unmapping pools that are never used, we map them lazily.

   We assume that when a pool is resized the client will recreate all the
   buffers, which might not always be true, but seems to be working so far. *)
module Shm : sig
  type t
  (** A proxy for a pair of memory pools. *)

  type buffer
  (** A region within the pools. *)

  val create :
    host_shm:([< `V1|`V2] as 'v) H.Wl_shm.t ->
    virtio_gpu:Virtio_gpu.t ->
    client_fd:Unix.file_descr ->
    size:int32 ->
    'v C.Wl_shm_pool.t ->
    t
  (** [create ~host_shm ~virtio_gpu ~client_fd ~size proxy] is a pool proxy that creates a host pool of size [size],
      and maps that and [client_fd] into our address space.
      @param virtio_gpu Used to create a memory region that can be shared with the host.
      @param host_shm Used to notify the host compositor about the new region.
      @param client_fd Used to map the client's memory. Will be closed when the ref-count reaches zero.
      @param proxy [client_fd] is closed when this and all buffers have been destroyed. *)

  val resize : t -> int32 -> unit

  val create_buffer : t ->
    untrusted_offset:int32 ->
    untrusted_width :int32 ->
    untrusted_height:int32 ->
    untrusted_stride:int32 ->
    untrusted_format:int32 ->
    [>`V1] C.Wl_shm_pool.t ->
    [`V1] C.Wl_buffer.t ->
    buffer
  (** [create_buffer t ... proxy] allocates a region of [t].
      @param proxy Will receive [release] events from the compositor if attached. *)

  val destroy_buffer : buffer -> unit
  (** [destroy_buffer b] destroys the host buffer (if any), and notifies the client proxy of the deletion. *)

  val user_data : buffer -> [`V1] CD.buffer
  (** [user_data b] is some data to attach to the client proxy so the surface can find it. *)

  val map_buffer : 'v CD.virtwl_buffer Lazy.t -> 'v CD.virtwl_buffer
  (** [map_buffer user_data] is used by the surface when attaching the buffer. *)
end = struct
  type mapping = {
    host_pool : [`V1|`V2] H.Wl_shm_pool.t;
    client_memory_pool : Cstruct.buffer;   (* The client's memory mapped into our address space *)
    host_memory_pool : Cstruct.buffer;     (* The host's memory mapped into our address space *)
  }


  type t = {
    host_shm : [`V1|`V2] H.Wl_shm.t;
    virtio_gpu : Virtio_gpu.t;
    mutable size : int32;
    mutable client_fd : Unix.file_descr option; (* [client_fd = None <=> ref_count = 0 *)
    mutable ref_count : int;                    (* The number of client proxies (pool + buffers) active *)
    mutable mapping : mapping option;           (* If [None] then map when needed *)
  }

  let with_memory_fd t ~size ~(gpu:bool) fn =
    let query = {
      Virtio_gpu.Dev.
      width = Int32.of_int size;
      height = 1l;
      drm_format = Virtio_gpu.Drm_format.r8;
    } in
    let image = Virtio_gpu.alloc t.virtio_gpu ~gpu query in
    match fn image with
    | x -> Unix.close image.fd; x
    | exception ex -> Unix.close image.fd; raise ex

  (* This is called when we attach a buffer to a surface
     (so the client-side buffer proxy must still exist). *)
  let get_mapping t =
    assert (t.ref_count > 0);
    match t.mapping with
    | Some m -> m
    | None ->
      let client_fd = Option.get t.client_fd in (* OK because ref_count > 0 *)
      let size = Int32.to_int t.size in
      let client_memory_pool = Unix.map_file client_fd Bigarray.Char Bigarray.c_layout true [| size |] in
      let host_pool, host_memory_pool =
        with_memory_fd t ~size ~gpu:false (fun { Virtio_gpu.Dev.fd; host_size; offset; _ } ->
            let host_pool = H.Wl_shm.create_pool t.host_shm ~fd ~size:t.size @@ new H.Wl_shm_pool.v1 in
            let host_memory = Virtio_gpu.Utils.safe_map_file fd
                ~kind:Bigarray.Char
                ~len:size
                ~host_size:(Int64.to_int host_size)
                ~pos:(Int64.of_int32 offset)
            in
            host_pool, host_memory
          )
      in
      let client_memory_pool = Bigarray.array1_of_genarray client_memory_pool in
      let m = { host_pool; client_memory_pool; host_memory_pool } in
      t.mapping <- Some m;
      m

  type buffer = {
    data : [`V1] CD.virtwl_buffer Lazy.t;      (* Forced when buffer is attached to a surface *)
    on_destroy : unit Lazy.t;                  (* Forced when client buffer proxy is destroyed *)
  }

  let user_data b : _ CD.buffer = `Virtwl b.data

  let clear_mapping t =
    t.mapping |> Option.iter (fun m ->
        if Proxy.transport_up m.host_pool then
          H.Wl_shm_pool.destroy m.host_pool;
        t.mapping <- None
      )

  let resize t new_size =
    if t.size <> new_size then (
      t.size <- new_size;
      clear_mapping t           (* Will force a new mapping if used in future *)
    )

  let dec_ref t =
    let ref_count = t.ref_count in
    if ref_count < 1 then
      assert false (* bug! *)
    else (
      let ref_count = ref_count - 1 in
      t.ref_count <- ref_count;
      if ref_count = 0 then (
        Unix.close (Option.get t.client_fd);
        t.client_fd <- None;
        clear_mapping t
      )
    )

  let create_buffer (t: t) ~untrusted_offset ~untrusted_width ~untrusted_height
        ~untrusted_stride ~untrusted_format
        (shm: _ C.Wl_shm_pool.t)
        (buffer: _ C.Wl_buffer.t) : buffer =
    validate_shm_parameters ~buffer_size:t.size ~untrusted_offset ~untrusted_width ~untrusted_height
        ~untrusted_stride ~untrusted_format shm;
    let (offset, width, height, stride, format) =
      (untrusted_offset, untrusted_width, untrusted_height, untrusted_stride, untrusted_format) in
    assert (t.ref_count > 0);
    let () =
      let ref_count = t.ref_count in
      if ref_count < 1 then (
        assert false (* The shm_pool proxy must exist to call this. *)
      ) else if ref_count > Int.max_int - 1 then (
        shm_pool_invalid_stride_str shm
          ~message:"Reference count overflow" (* TODO: better error for refcount overflow *)
      ) else (
        t.ref_count <- ref_count + 1
      )
    in
    Proxy.on_delete buffer (fun () -> dec_ref t);
    let data =
      lazy (
        (* Forced by [map_buffer] when the the buffer is attached to a surface,
           so buffer proxy still exists.  Overflow is impossible because
           [Drm_format.validate_shm] checks for it.
         *)
        let len = Int32.to_int height * Int32.to_int stride in
        let mapping = get_mapping t in
        let host_memory = Cstruct.of_bigarray mapping.host_memory_pool ~off:(Int32.to_int offset) ~len in
        let client_memory = Cstruct.of_bigarray mapping.client_memory_pool ~off:(Int32.to_int offset) ~len in
        let host_buffer =
          H.Wl_shm_pool.create_buffer mapping.host_pool ~offset ~width ~height ~stride ~format
          @@ object
            inherit [_] H.Wl_buffer.v1
            method on_release _ = C.Wl_buffer.release buffer
          end
        in
        { CD.host_memory; client_memory; host_buffer }
      )
    in
    let on_destroy = lazy (
      if Lazy.is_val data then (
        delete_with H.Wl_buffer.destroy (Lazy.force data).host_buffer buffer
      ) else (
        Proxy.delete buffer
      )
    ) in
    { on_destroy; data }

  (* Client-side buffer proxy must still exist when this is called. *)
  let map_buffer : _ -> _ CD.virtwl_buffer = Lazy.force

  let destroy_buffer b =
    Lazy.force b.on_destroy

  let create ~host_shm ~virtio_gpu ~client_fd ~size client_shm =
    let t = {
      host_shm = (host_shm :> [`V1|`V2] H.Wl_shm.t);
      virtio_gpu;
      size;
      client_fd = Some client_fd;
      ref_count = 1;
      mapping = None;
    } in
    Proxy.on_delete client_shm (fun () -> dec_ref t);
    t
end


let unsafe_no_sanitize a = a
(** This is [id] *)

let make_surface ~xwayland ~host_surface c =
  let c = cv c in
  let h =
    let user_data = host_data (HD.Surface { HD.client = c; data = No_surface_data }) in
    host_surface @@ object
      inherit [_] H.Wl_surface.v1
      method! user_data = user_data
      method on_enter _ ~output = C.Wl_surface.enter c ~output:(to_client output)
      method on_leave _ ~output = C.Wl_surface.leave c ~output:(to_client output)
      method on_preferred_buffer_scale _ = C.Wl_surface.preferred_buffer_scale c
      method on_preferred_buffer_transform _ ~transform =
        C.Wl_surface.preferred_buffer_transform c ~transform
    end
  in
  let h = Proxy.cast_version h in
  let data =
    let state = if xwayland = None then CD.Ready else Unconfigured (Queue.create ()) in
    { CD.host_surface = h; host_memory = Cstruct.empty; client_memory = Cstruct.empty; state }
  in
  let when_configured fn =
    match data.state with
    | Ready -> fn ()
    | Unconfigured q -> Queue.add fn q
    | Destroyed -> ()
  in
  let state = ref `Show in      (* X11 hidden windows get [`Hide] here *)
  Proxy.Handler.attach c @@ object
    val mutable buffer_x = Int.min_int;
    val mutable buffer_y = Int.min_int;
    inherit [_] C.Wl_surface.v1
    method! user_data = client_data (Surface data)

    method on_attach p ~buffer ~untrusted_x ~untrusted_y =
      (* sanitize start *)
      if Proxy.version p >= 5l then (
        if untrusted_x <> 0l then
          C.Wl_surface.Errors.invalid_size p ~message:"Nonzero x passed to wl_surface.attach"
        else if untrusted_y <> 0l then
          C.Wl_surface.Errors.invalid_size p ~message:"Nonzero y passed to wl_surface.attach"
        else
          ()
      ) else (
        (V.check_x_y p (fun _ -> assert false) ~untrusted_x ~untrusted_y);
      );
      let (x, y) = (untrusted_x, untrusted_y) in
      (* sanitize end *)
      buffer_x <- Int32.to_int x;
      buffer_y <- Int32.to_int y;
      let (x, y) = scale_to_host ~xwayland (x, y) in
      when_configured @@ fun () ->
      match buffer with
      | Some buffer when !state <> `Hide ->
        let Client_data (Buffer buffer) = user_data buffer in
        let host_buffer =
          match buffer with
          | `Direct host_buffer -> host_buffer
          | `Virtwl buffer ->
            let buffer = Shm.map_buffer buffer in
            data.host_memory <- buffer.host_memory;
            data.client_memory <- buffer.client_memory;
            buffer.host_buffer
        in
        H.Wl_surface.attach h ~buffer:(Some host_buffer) ~x ~y
      | _ ->
        data.host_memory <- Cstruct.empty;
        data.client_memory <- Cstruct.empty;
        H.Wl_surface.attach h ~buffer:None ~x ~y

    method on_commit _ =
      when_configured @@ fun () ->
      (* todo: only copy the bit that changed *)
      Cstruct.blit data.client_memory 0 data.host_memory 0 (Cstruct.length data.client_memory);
      H.Wl_surface.commit h

    method on_damage p ~untrusted_x ~untrusted_y ~untrusted_width ~untrusted_height =
      (* sanitize start *)
      V.check_x_y p C.Wl_surface.Errors.invalid_offset ~untrusted_x ~untrusted_y;
      V.check_width_height_int32 p C.Wl_surface.Errors.invalid_offset ~untrusted_width ~untrusted_height;
      let (x, y, width, height) = (untrusted_x, untrusted_y, untrusted_width, untrusted_height) in
      (* sanitize end *)
      (* FIXME: bounds check properly by knowing the actual dimensions *)
      when_configured @@ fun () ->
      let (x, y) = scale_to_host ~xwayland (x, y) in
      let (width, height) = scale_to_host ~xwayland (width, height) in
      H.Wl_surface.damage h ~x ~y ~width ~height

    method on_damage_buffer p ~untrusted_x ~untrusted_y ~untrusted_width ~untrusted_height =
      (* sanitize start *)
      V.check_x_y p C.Wl_surface.Errors.invalid_offset ~untrusted_x ~untrusted_y;
      V.check_width_height_int32 p C.Wl_surface.Errors.invalid_offset ~untrusted_width ~untrusted_height;
      let (x, y, width, height) = (untrusted_x, untrusted_y, untrusted_width, untrusted_height) in
      (* sanitize end *)
      (* FIXME: bounds check properly by knowing the actual dimensions *)
      when_configured @@ fun () ->
      H.Wl_surface.damage_buffer h ~x ~y ~width ~height

    method on_destroy =
      data.state <- Destroyed;
      xwayland |> Option.iter (fun (x:xwayland_hooks) -> x#on_destroy_surface h);
      delete_with H.Wl_surface.destroy h

    method on_frame _ callback =
      when_configured @@ fun () ->
      let _ : _ Proxy.t = H.Wl_surface.frame h @@ Wayland.callback @@ fun callback_data ->
        C.Wl_callback.done_ callback ~callback_data;
        Proxy.delete callback
      in
      Proxy.Handler.attach callback @@ new C.Wl_callback.v1

    method on_set_input_region _ ~region =
      when_configured @@ fun () ->
      H.Wl_surface.set_input_region h ~region:(Option.map to_host region)

    method on_set_opaque_region _ ~region =
      when_configured @@ fun () ->
      H.Wl_surface.set_opaque_region h ~region:(Option.map to_host region)

    method on_set_buffer_scale c ~untrusted_scale =
      let scale =
        if untrusted_scale < 1l then
          C.Wl_surface.Errors.invalid_scale c ~message:"Scale cannot be negative"
        else if untrusted_scale > 1024l then
          (* Assume nobody needs a scale this large.  TODO: is this actually permitted? *)
          Msg.bad_implementation "Scale greater than 1024 isn't allowed"
        else
          untrusted_scale
      in
      (* FIXME: defer request until surface commit *)
      when_configured @@ fun () ->
      H.Wl_surface.set_buffer_scale h ~scale

    method on_set_buffer_transform _ ~untrusted_transform =
      (* UNSAFE *)
      let transform = unsafe_no_sanitize untrusted_transform in
      (* NO-sanitize end *)
      when_configured @@ fun () ->
      H.Wl_surface.set_buffer_transform h ~transform

    method on_offset p ~untrusted_x ~untrusted_y =
      (* sanitize start *)
      V.check_x_y p C.Wl_surface.Errors.invalid_offset ~untrusted_x ~untrusted_y;
      let (x, y) = (untrusted_x, untrusted_y) in
      (* sanitize end *)
      (* FIXME: bounds check properly by knowing the actual dimensions *)
      when_configured @@ fun () ->
      let (x, y) = scale_to_host ~xwayland (x, y) in
      H.Wl_surface.offset h ~x ~y
  end;
  xwayland |> Option.iter (fun (x:xwayland_hooks) ->
      if x#scale <> 1l then
        H.Wl_surface.set_buffer_scale h ~scale:x#scale;       (* Xwayland will be a new enough version *)
      let set_configured s =
        if s = `Unmanaged && x#scale <> 1l then (
          (* For pointer cursors we want them at the normal size, even if low-res.
             Also, Vim tries to hide the pointer by setting a 1x1 cursor, which confuses things
             when unscaled. Ideally we would stop doing transforms in this case, but it doesn't
             seem to matter. *)
          H.Wl_surface.set_buffer_scale h ~scale:1l;
        );
        state := s;
        match data.state with
        | Ready | Destroyed -> ()
        | Unconfigured q ->
          data.state <- Ready;
          Queue.iter (fun f -> f ()) q
      in
      x#on_create_surface h c ~set_configured
    )

let set_surface_data surface data =
  let Host_data (HD.Surface x) = user_data surface in
  x.data <- data

let get_surface_data surface =
  let Host_data (HD.Surface x) = user_data surface in
  x.data

let make_compositor ~xwayland bind proxy =
  let h = bind @@ new H.Wl_compositor.v1 in
  Proxy.Handler.attach proxy @@ object
    inherit [_] C.Wl_compositor.v1
    method on_create_region _ = make_region ~host_region:(H.Wl_compositor.create_region h)
    method on_create_surface _ = make_surface ~xwayland ~host_surface:(H.Wl_compositor.create_surface h)
  end

let make_subsurface ~xwayland ~host_subsurface c =
  let h = host_subsurface @@ new H.Wl_subsurface.v1 in
  Proxy.Handler.attach c @@ object
    inherit [_] C.Wl_subsurface.v1
    method on_destroy = delete_with H.Wl_subsurface.destroy h
    method on_place_above _ ~sibling = H.Wl_subsurface.place_above h ~sibling:(to_host sibling)
    method on_place_below _ ~sibling = H.Wl_subsurface.place_below h ~sibling:(to_host sibling)
    method on_set_desync _ = H.Wl_subsurface.set_desync h

    method on_set_position p ~untrusted_x ~untrusted_y =
      (* sanitize start *)
      V.check_x_y p (fun _ -> assert false) ~untrusted_x ~untrusted_y;
      let (x, y) = (untrusted_x, untrusted_y) in
      (* sanitize end *)
      let (x, y) = scale_to_host ~xwayland (x, y) in
      H.Wl_subsurface.set_position h ~x ~y

    method on_set_sync _ = H.Wl_subsurface.set_sync h
  end

let make_subcompositor ~xwayland bind proxy =
  let h = bind @@ new H.Wl_subcompositor.v1 in
  Proxy.Handler.attach proxy @@ object
    inherit [_] C.Wl_subcompositor.v1
    method on_destroy = delete_with H.Wl_subcompositor.destroy h

    method on_get_subsurface _ subsurface ~surface ~parent =
      let surface = to_host surface in
      let parent = to_host parent in
      let host_subsurface = H.Wl_subcompositor.get_subsurface h ~surface ~parent in
      make_subsurface ~xwayland ~host_subsurface subsurface
  end

let make_shm_buffer b proxy =
  let user_data = client_data (Buffer (Shm.user_data b)) in
  Proxy.Handler.attach proxy @@ object
    inherit [_] C.Wl_buffer.v1
    method! user_data = user_data
    method on_destroy _ = Shm.destroy_buffer b
  end

(* todo: this all needs to be more robust.
   Also, sealing? *)
let make_shm_pool_virtwl ~virtio_gpu ~host_shm proxy ~fd:client_fd ~size:orig_size =
  let mapping = Shm.create ~host_shm ~virtio_gpu ~client_fd ~size:orig_size proxy in
  Proxy.Handler.attach proxy @@ object
    inherit [_] C.Wl_shm_pool.v1

    method on_create_buffer proxy buffer ~untrusted_offset ~untrusted_width ~untrusted_height ~untrusted_stride ~untrusted_format =
      let b = Shm.create_buffer mapping ~untrusted_offset ~untrusted_width ~untrusted_height ~untrusted_stride ~untrusted_format proxy buffer in
      make_shm_buffer b buffer

    method on_destroy t = Proxy.delete t

    (* UNSAFE *)
    method on_resize _ ~untrusted_size = Shm.resize mapping untrusted_size
  end

let make_shm_pool_direct size host_pool proxy =
  Proxy.Handler.attach proxy @@ object
    val mutable buffer_size = size
    inherit [_] C.Wl_shm_pool.v1

    method on_create_buffer shm buffer ~untrusted_offset ~untrusted_width ~untrusted_height ~untrusted_stride ~untrusted_format =
      validate_shm_parameters ~buffer_size ~untrusted_offset ~untrusted_width ~untrusted_height
        ~untrusted_stride ~untrusted_format shm;
      let (offset, width, height, stride, format) =
        (untrusted_offset, untrusted_width, untrusted_height, untrusted_stride, untrusted_format) in
      let host_buffer = H.Wl_shm_pool.create_buffer host_pool ~offset ~width ~height ~stride ~format @@ object
          inherit [_] H.Wl_buffer.v1
          method on_release _ = C.Wl_buffer.release buffer
        end
      in
      Proxy.Handler.attach buffer @@ object
        inherit [_] C.Wl_buffer.v1
        method! user_data = client_data (Buffer (`Direct host_buffer))
        method on_destroy = delete_with H.Wl_buffer.destroy host_buffer
      end

    method on_destroy _ = H.Wl_shm_pool.destroy host_pool

    method on_resize c ~untrusted_size =
      (if untrusted_size < buffer_size then
        shm_pool_invalid_stride_str c ~message:"Attempt to shrink buffer");
      let size = unsafe_no_sanitize untrusted_size in
      H.Wl_shm_pool.resize host_pool ~size;
      buffer_size <- size
  end

let make_output ~xwayland bind c =
  let c = Proxy.cast_version c in
  let h =
    let user_data = host_data (HD.Output c) in
    bind @@ object
      inherit [_] H.Wl_output.v1
      method! user_data = user_data
      method on_done _ = C.Wl_output.done_ (Proxy.cast_version c)
      method on_geometry _ ~x ~y ~physical_width ~physical_height ~subpixel ~make ~model ~transform =
        C.Wl_output.geometry c ~x ~y ~physical_width ~physical_height ~subpixel ~make ~model ~transform
      method on_mode _ = C.Wl_output.mode c
      method on_name _ ~name = C.Wl_output.name c ~name
      method on_description _ ~description = C.Wl_output.description c ~description

      method on_scale _ ~factor =
        let factor =
          match xwayland with
          | Some x -> Int32.div factor x#scale
          | None -> factor
        in
        C.Wl_output.scale (Proxy.cast_version c) ~factor
    end
  in
  let user_data = client_data (Output h) in
  Proxy.Handler.attach c @@ object
    inherit [_] C.Wl_output.v1
    method! user_data = user_data
    method on_release = delete_with H.Wl_output.release (cv h)
  end

let make_pointer t ~xwayland ~host_seat c =
  let c = cv c in
  let h : _ Proxy.t = H.Wl_seat.get_pointer host_seat @@ object
      inherit [_] H.Wl_pointer.v1
      method on_axis _ = C.Wl_pointer.axis c
      method on_axis_discrete _ = C.Wl_pointer.axis_discrete c
      method on_axis_source _ = C.Wl_pointer.axis_source c
      method on_axis_stop _ = C.Wl_pointer.axis_stop c
      method on_axis_value120 _ = C.Wl_pointer.axis_value120 c

      method on_button _ ~serial ~time ~button ~state =
        update_serial t serial;
        C.Wl_pointer.button c ~serial ~time ~button ~state

      method on_enter _ ~serial ~surface ~surface_x ~surface_y =
        update_serial t serial;
        let (surface_x, surface_y) = point_to_client ~xwayland (surface_x, surface_y) in
        let forward_event () =
          C.Wl_pointer.enter c ~serial ~surface:(to_client surface) ~surface_x ~surface_y
        in
        match xwayland with
        | None -> forward_event ()
        | Some (xwayland:xwayland_hooks) ->
          xwayland#on_pointer_entry ~surface ~forward_event

      method on_leave _ ~serial ~surface =
        update_serial t serial;
        C.Wl_pointer.leave c ~serial ~surface:(to_client surface)

      method on_motion _ ~time ~surface_x ~surface_y =
        let (surface_x, surface_y) = point_to_client ~xwayland (surface_x, surface_y) in
        C.Wl_pointer.motion c ~time ~surface_x ~surface_y

      method on_frame _ = C.Wl_pointer.frame c

      method on_axis_relative_direction _ = C.Wl_pointer.axis_relative_direction c
    end
  in
  let user_data = client_data (CD.Pointer h) in
  Proxy.Handler.attach c @@ object
    inherit [_] C.Wl_pointer.v1
    method! user_data = user_data

    method on_set_cursor _ ~untrusted_serial ~surface ~untrusted_hotspot_x ~untrusted_hotspot_y =
      (* FIXME: sanitize! *)
      let (serial, hotspot_x, hotspot_y) = (untrusted_serial, untrusted_hotspot_x, untrusted_hotspot_y) in
      (* Cursors are not unscaled, so no need to transform here. *)
      H.Wl_pointer.set_cursor h ~serial ~surface:(Option.map to_host surface) ~hotspot_x ~hotspot_y

    method on_release t =
      delete_with H.Wl_pointer.release h t
  end

let make_keyboard t ~xwayland ~host_seat c =
  let h : _ Proxy.t = H.Wl_seat.get_keyboard host_seat @@ object
      inherit [_] H.Wl_keyboard.v1

      method on_keymap _ ~format ~fd ~size =
        C.Wl_keyboard.keymap c ~format ~fd ~size;
        Unix.close fd

      method on_enter _ ~serial ~surface ~keys =
        update_serial t serial;
        let forward_event () =
          C.Wl_keyboard.enter c ~serial ~surface:(to_client surface) ~keys
        in
        match xwayland with
        | None -> forward_event ()
        | Some (xwayland:xwayland_hooks) ->
          xwayland#on_keyboard_entry ~surface ~forward_event

      method on_leave _ ~serial ~surface =
        update_serial t serial;
        C.Wl_keyboard.leave c ~serial ~surface:(to_client surface);
        xwayland |> Option.iter (fun (xwayland : xwayland_hooks) ->
            xwayland#on_keyboard_leave ~surface
          )

      method on_key _ ~serial ~time ~key ~state =
        update_serial t serial;
        C.Wl_keyboard.key c ~serial ~time ~key ~state

      method on_modifiers _ ~serial ~mods_depressed ~mods_latched ~mods_locked ~group =
        update_serial t serial;
        C.Wl_keyboard.modifiers c ~serial ~mods_depressed ~mods_latched ~mods_locked ~group

      method on_repeat_info _ = C.Wl_keyboard.repeat_info (cv c)
    end
  in
  Proxy.Handler.attach c @@ object
    inherit [_] C.Wl_keyboard.v1
    method on_release = delete_with H.Wl_keyboard.release h
  end

let make_seat ~xwayland t bind c =
  let c = Proxy.cast_version c in
  let cap_mask = C.Wl_seat.Capability.(Int32.logor keyboard pointer) in
  let host = bind @@ object
      inherit [_] H.Wl_seat.v1

      method on_capabilities _ ~capabilities =
        C.Wl_seat.capabilities c ~capabilities:(Int32.logand capabilities cap_mask)
      method on_name _ = C.Wl_seat.name (cv c)
    end
  in
  let host = cv host in
  let user_data = client_data (Seat host) in
  Proxy.Handler.attach c @@ object
    inherit [_] C.Wl_seat.v1
    method! user_data = user_data
    method on_get_keyboard _ keyboard = make_keyboard ~xwayland t ~host_seat:host keyboard
    method on_get_pointer _ c = make_pointer ~xwayland t ~host_seat:host c
    method on_get_touch _ = Fmt.failwith "TODO: on_get_touch"
    method on_release = delete_with H.Wl_seat.release host
  end

let make_shm ~virtio_gpu bind c =
  let c = Proxy.cast_version c in
  let h = bind @@ object
      inherit [_] H.Wl_shm.v1
      method on_format p ~format =
        (* Check that the format is one that can be supported. *)
        if format = 0l || format = 1l then (
          (* Do nothing.  This message is redundant. *)
        ) else if Relay_stubs.validate_shm_format ~untrusted_format:format then (
          (* Format can be used and won't trigger spurious protocol errors. *)
          match ((C.Wl_shm.Format.of_int32 ~_proxy:p ~value:format): Wayland_proto.Wl_shm.Format.t) with
          | _ -> C.Wl_shm.format c ~format
          | exception Msg.Error _ -> (
          (* Format will be rejected during message unmarshalling. *)
          )
        ) else (
          (* Format cannot be used and attempts to use it will trigger a protocol error.
           * Do not inform the client of this format. *)
        )
    end
  in
  Proxy.Handler.attach c @@ object
    inherit [_] C.Wl_shm.v1
    method on_create_pool _ proxy ~untrusted_fd ~untrusted_size =
      (* FIXME: sanitize! *)
      let (fd, size) = (untrusted_fd, untrusted_size) in
      (* FIXME end *)
      match virtio_gpu with
      | Some virtio_gpu -> make_shm_pool_virtwl ~virtio_gpu ~host_shm:h proxy ~fd ~size
      | None ->
        let host_pool = H.Wl_shm.create_pool h ~fd ~size @@ new H.Wl_shm_pool.v1 in
        Unix.close fd;
        make_shm_pool_direct size host_pool proxy

    method on_release = delete_with H.Wl_shm.release h
  end

module Linux_dmabuf = struct
  let make_linux_buffer_params ~host_linux_buffer_params c =
    let proxy = cv c in
    let make_dma_buffer ~client_buffer h =
      let c = client_buffer @@ object
        inherit [_] C.Wl_buffer.v1
        method on_destroy c = delete_with H.Wl_buffer.destroy h c
      end in
      Proxy.Handler.attach h @@ object
        inherit [_] H.Wl_buffer.v1
        method on_release _ = C.Wl_buffer.release c
      end
    in
    let h = host_linux_buffer_params @@ object
      inherit [_] H.Zwp_linux_buffer_params_v1.v1
      method on_failed _ = C.Zwp_linux_buffer_params_v1.failed proxy
      method on_created _ buffer = make_dma_buffer ~client_buffer:(C.Zwp_linux_buffer_params_v1.created proxy) buffer
    end
    in
    let check_used t created =
      if created then
        C.Zwp_linux_buffer_params_v1.Errors.already_used ~message:"Already created" t
    in
    let check_complete t complete =
      if not complete then C.Zwp_linux_buffer_params_v1.Errors.incomplete ~message:"No planes" t
    in
    Proxy.Handler.attach proxy @@ object (self)
      val mutable created = false
      val mutable have_modifiers = false
      val mutable modifier_lo = 0l
      val mutable modifier_hi = 0l
      val buffer_data = [|None; None; None; None|];
      inherit ['a] C.Zwp_linux_buffer_params_v1.v1
      (* FIXME: close the file descriptors *)
      method on_destroy = delete_with H.Zwp_linux_buffer_params_v1.destroy h
      method on_create_immed t buffer ~untrusted_width ~untrusted_height ~untrusted_format ~untrusted_flags =
        self#add t ~untrusted_format ~untrusted_width ~untrusted_height;
        (* TODO: check format, flags, etc *)
        let (width, height, format, flags) =
          (untrusted_width, untrusted_height, untrusted_format, untrusted_flags) in
        let host_buffer = H.Zwp_linux_buffer_params_v1.create_immed h ~width ~height ~format ~flags @@ object
          inherit [_] H.Wl_buffer.v1
          method on_release _ = C.Wl_buffer.release buffer
        end in
        Proxy.Handler.attach buffer @@ object
          inherit [_] C.Wl_buffer.v1
          method! user_data = client_data (Buffer (`Direct host_buffer))
          method on_destroy = delete_with H.Wl_buffer.destroy host_buffer
        end
      method private add t ~untrusted_format ~untrusted_width ~untrusted_height =
        check_used t created;
        check_complete t have_modifiers;
        V.check_width_height_int32 t C.Zwp_linux_buffer_params_v1.Errors.invalid_dimensions
          ~untrusted_width ~untrusted_height;
        if not (Relay_stubs.validate_format ~untrusted_format ~untrusted_width ~untrusted_height) then (
          C.Zwp_linux_buffer_params_v1.Errors.invalid_dimensions t
            ~message:"Invalid format or invalid dimensions for format"
        );
        if Option.is_none buffer_data.(0) then (
          C.Zwp_linux_buffer_params_v1.Errors.incomplete t ~message:"Plane 0 not used"
        );
        let max =
          if Option.is_none buffer_data.(2) then (
            if Option.is_some buffer_data.(3) then
              C.Zwp_linux_buffer_params_v1.Errors.incomplete t ~message:"Plane 3 is set but plane 2 is not"
            else if Option.is_some buffer_data.(1) then 1 else 0
          ) else (
            if Option.is_none buffer_data.(1) then
              C.Zwp_linux_buffer_params_v1.Errors.incomplete t ~message:"Plane 2 is set but plane 1 is not"
            else if Option.is_some buffer_data.(3) then 3 else 2
          ) in
        for plane_idx = 0 to max do
          match buffer_data.(plane_idx) with
          | None -> assert false
          | Some (fd, offset, stride) -> (
            (* Avoid double FD close *)
            buffer_data.(plane_idx) <- None;
            (* FIXME: does this take ownership of the FD? *)
            H.Zwp_linux_buffer_params_v1.add h ~plane_idx:(Int32.of_int plane_idx) ~fd ~offset ~stride ~modifier_hi ~modifier_lo
          )
        done
      method on_create t ~untrusted_width ~untrusted_height ~untrusted_format ~untrusted_flags =
        (* TODO: check format, flags, etc *)
        self#add t ~untrusted_format ~untrusted_width ~untrusted_height;
        let (width, height, format, flags) =
          (untrusted_width, untrusted_height, untrusted_format, untrusted_flags) in
        created <- true;
        H.Zwp_linux_buffer_params_v1.create h ~width ~height ~format ~flags
      method on_add t ~untrusted_fd
               ~(untrusted_plane_idx:int32)
               ~(untrusted_offset:int32)
               ~(untrusted_stride:int32)
               ~(untrusted_modifier_hi:int32)
               ~(untrusted_modifier_lo:int32) =
        (* FIXME: close the file descriptor on errors! *)
        check_used t created;
        if have_modifiers then begin
          let (lsl) = Int64.shift_left in
          let (lor) = Int64.logor in
          let of_int32 = Int64.of_int32 in
          let old_modifier = (of_int32 modifier_hi lsl 32) lor (of_int32 modifier_lo) in
          let new_modifier = (of_int32 untrusted_modifier_hi lsl 32) lor (of_int32 untrusted_modifier_lo) in
          if old_modifier <> new_modifier then
            let message = Format.asprintf "Modifier mismatch: previous modifier is 0x%016Lx, but current modifier is 0x%016LX" old_modifier new_modifier in
            C.Zwp_linux_buffer_params_v1.Errors.invalid_format t ~message
        end else begin
          modifier_lo <- untrusted_modifier_lo;
          modifier_hi <- untrusted_modifier_hi;
          have_modifiers <- true
        end;
        if untrusted_plane_idx < 0l then (
          let message = Format.asprintf "Negative plane index %ld" untrusted_plane_idx in
          C.Zwp_linux_buffer_params_v1.Errors.plane_idx ~message t
        );
        if untrusted_offset < 0l then (
          let message = Format.asprintf "Negative offset %ld" untrusted_plane_idx in
          C.Zwp_linux_buffer_params_v1.Errors.out_of_bounds ~message t
        );
        if untrusted_plane_idx >= 4l then (
          let message = Format.asprintf "Excessive plane index %ld" untrusted_plane_idx in
          C.Zwp_linux_buffer_params_v1.Errors.plane_idx ~message t
        );
        if untrusted_stride <= 0l then (
          let message = Format.asprintf "Negative or zero stride %ld" untrusted_stride in
          C.Zwp_linux_buffer_params_v1.Errors.invalid_dimensions ~message t
        );
        let size = Relay_stubs.check_fd_offset untrusted_fd in
        if Int64.of_int32 untrusted_offset >= size then (
          if size < 0L then (
            C.Zwp_linux_buffer_params_v1.Errors.invalid_wl_buffer t
              ~message:"lseek() failed or file size exceeds INT64_MAX"
          ) else (
            let message = Format.asprintf "Buffer size is %Ld, but offset is %ld" size untrusted_offset in
            C.Zwp_linux_buffer_params_v1.Errors.out_of_bounds t ~message
          )
        );
        let plane_idx = Int32.to_int untrusted_plane_idx in
        match buffer_data.(plane_idx) with
        | None -> buffer_data.(plane_idx) <- Some (untrusted_fd, untrusted_offset, untrusted_stride)
        | Some _ ->
          let message = Format.asprintf "Plane index %d already used" plane_idx in
          C.Zwp_linux_buffer_params_v1.Errors.already_used ~message t
    end

  let make_linux_dmabuf_feedback ?override_dev ~host_dmabuf_feedback c =
    let proxy = cv c in
    let h = host_dmabuf_feedback @@ object
      inherit [_] H.Zwp_linux_dmabuf_feedback_v1.v1
      method on_done _ = C.Zwp_linux_dmabuf_feedback_v1.done_ c
      method on_format_table _ = C.Zwp_linux_dmabuf_feedback_v1.format_table c
      method on_main_device _ ~(device:string) =
        C.Zwp_linux_dmabuf_feedback_v1.main_device c ~device:(
          match override_dev with Some dev -> dev | None -> device)
      method on_tranche_done _ = C.Zwp_linux_dmabuf_feedback_v1.tranche_done c
      method on_tranche_target_device _ ~device =
        C.Zwp_linux_dmabuf_feedback_v1.tranche_target_device c ~device:(
          match override_dev with Some dev -> dev | None -> device)
      method on_tranche_formats _ = C.Zwp_linux_dmabuf_feedback_v1.tranche_formats c
      method on_tranche_flags _ = C.Zwp_linux_dmabuf_feedback_v1.tranche_flags c
    end
    in Proxy.Handler.attach proxy @@ object
      inherit [_] C.Zwp_linux_dmabuf_feedback_v1.v1
      method on_destroy = delete_with H.Zwp_linux_dmabuf_feedback_v1.destroy h
    end

  let make_linux_dmabuf ~(virtio_gpu:Virtio_gpu.t option) bind c =
    let h = bind @@ object
        inherit [_] H.Zwp_linux_dmabuf_v1.v1
        method on_format _ ~format = C.Zwp_linux_dmabuf_v1.format c ~format
        method on_modifier _ ~format ~modifier_hi ~modifier_lo =
          C.Zwp_linux_dmabuf_v1.modifier c ~format ~modifier_hi ~modifier_lo
      end
    in
    Proxy.Handler.attach c @@ object
      val override_dev = Option.map Virtio_gpu.device_string virtio_gpu
      val h = h
      inherit [_] C.Zwp_linux_dmabuf_v1.v1
      method on_destroy = delete_with H.Zwp_linux_dmabuf_v1.destroy h
      method on_create_params _ params_id =
        let host_linux_buffer_params = H.Zwp_linux_dmabuf_v1.create_params h in
        make_linux_buffer_params ~host_linux_buffer_params params_id
      method on_get_surface_feedback _ id ~surface =
        let host_dmabuf_feedback = H.Zwp_linux_dmabuf_v1.get_surface_feedback h ~surface:(to_host surface) in
        make_linux_dmabuf_feedback ?override_dev ~host_dmabuf_feedback id
      method on_get_default_feedback _ id =
        let host_dmabuf_feedback = H.Zwp_linux_dmabuf_v1.get_default_feedback h in
        make_linux_dmabuf_feedback ?override_dev ~host_dmabuf_feedback id
    end
end

let validate_serial ~(untrusted_serial: int32) = untrusted_serial
(** TODO: do actual validation here! *)

let make_popup ~host_popup c =
  let h = host_popup @@ object
      inherit [_] H.Xdg_popup.v1
      method on_popup_done _ = C.Xdg_popup.popup_done c
      method on_configure _ = C.Xdg_popup.configure c
      method on_repositioned _ = C.Xdg_popup.repositioned c
    end
  in
  Proxy.Handler.attach c @@ object
    inherit [_] C.Xdg_popup.v1
    method on_destroy = delete_with H.Xdg_popup.destroy h
    method on_grab _ ~seat ~(untrusted_serial:int32): unit =
      let serial = validate_serial ~untrusted_serial in
      H.Xdg_popup.grab h ~seat:(to_host seat) ~serial
    method on_reposition _ ~positioner ~(untrusted_token:int32): unit =
      (* FIXME: validate token *)
      H.Xdg_popup.reposition h ~positioner:(to_host positioner) ~token:untrusted_token
  end

let make_toplevel ~tag ~host_toplevel c =
  let h = host_toplevel @@ object
      inherit [_] H.Xdg_toplevel.v1
      method on_close _ = C.Xdg_toplevel.close c
      method on_configure _ = C.Xdg_toplevel.configure c
      method on_configure_bounds _ = C.Xdg_toplevel.configure_bounds c
      method on_wm_capabilities _ = C.Xdg_toplevel.wm_capabilities c
    end
  in
  let user_data = client_data (Toplevel h) in
  Proxy.Handler.attach c @@ object
    val h = h
    val mutable min_width = 0
    val mutable min_height = 0
    val mutable max_width = 0
    val mutable max_height = 0
    inherit [_] C.Xdg_toplevel.v1
    method! user_data = user_data
    method on_destroy = delete_with H.Xdg_toplevel.destroy h
    method on_move _ ~seat ~(untrusted_serial:int32): unit =
      let serial = validate_serial ~untrusted_serial in
      H.Xdg_toplevel.move h ~seat:(to_host seat) ~serial
    method on_resize _ ~seat ~(untrusted_serial:int32) ~(untrusted_edges): unit =
      let serial = validate_serial ~untrusted_serial in
      (* FIXME: validate edges *)
      let edges = unsafe_no_sanitize untrusted_edges in
      H.Xdg_toplevel.resize h ~seat:(to_host seat) ~serial ~edges
    method on_set_app_id _ ~(untrusted_app_id:string): unit =
      (* TODO: validate app ID *)
      H.Xdg_toplevel.set_app_id h ~app_id:untrusted_app_id
    method on_set_fullscreen _ ~output =
      if false then
        H.Xdg_toplevel.set_fullscreen h ~output:(Option.map to_host output)
      else
        H.Xdg_toplevel.set_maximized h
    method on_set_max_size p ~(untrusted_width:int32) ~(untrusted_height:int32): unit =
      V.check_max_width_height p C.Xdg_toplevel.Errors.invalid_size ~untrusted_width ~untrusted_height;
      let untrusted_width_int = Int32.to_int untrusted_width in
      let untrusted_height_int = Int32.to_int untrusted_height in
      (if min_width <> 0 && untrusted_width_int <> 0 && min_width > untrusted_width_int then
        C.Xdg_toplevel.Errors.invalid_size p ~message:(
            Format.asprintf "Max width %d less than min width %d" untrusted_width_int min_width));
      (if min_height <> 0 && untrusted_height_int <> 0 && min_height > untrusted_height_int then
        C.Xdg_toplevel.Errors.invalid_size p ~message:(
            Format.asprintf "Max height %d less than min height %d" untrusted_height_int min_height));
      max_width <- untrusted_width_int;
      max_height <- untrusted_height_int;
      H.Xdg_toplevel.set_min_size h ~width:untrusted_width ~height:untrusted_height;
    method on_set_maximized _ = H.Xdg_toplevel.set_maximized h
    method on_set_min_size p ~(untrusted_width:int32) ~(untrusted_height:int32): unit =
      V.check_max_width_height p C.Xdg_toplevel.Errors.invalid_size ~untrusted_width ~untrusted_height;
      let untrusted_width_int = Int32.to_int untrusted_width in
      let untrusted_height_int = Int32.to_int untrusted_height in
      (if max_width <> 0 && untrusted_width_int <> 0 && max_width < untrusted_width_int then
        C.Xdg_toplevel.Errors.invalid_size p ~message:(
            Format.asprintf "Max width %d less than min width %d" max_width untrusted_width_int));
      (if max_height <> 0 && untrusted_height_int <> 0 && max_height < untrusted_height_int then
        C.Xdg_toplevel.Errors.invalid_size p ~message:(
            Format.asprintf "Max height %d less than min height %d" max_height untrusted_height_int));
      min_width <- untrusted_width_int;
      min_height <- untrusted_height_int;
      H.Xdg_toplevel.set_min_size h ~width:untrusted_width ~height:untrusted_height
    method on_set_minimized _ = H.Xdg_toplevel.set_minimized h
    method on_set_parent _ ~parent = H.Xdg_toplevel.set_parent h ~parent:(Option.map to_host parent)
    method on_set_title _ ~(untrusted_title:string) : unit =
      (* TODO: sanitize title using a C library. *)
      H.Xdg_toplevel.set_title h ~title:(tag ^ untrusted_title)
    method on_show_window_menu p ~seat ~(untrusted_serial:int32)
             ~(untrusted_x:int32) ~(untrusted_y:int32): unit =
      V.check_x_y p C.Xdg_toplevel.Errors.invalid_size ~untrusted_x ~untrusted_y;
      let serial = validate_serial ~untrusted_serial in
      H.Xdg_toplevel.show_window_menu h ~seat:(to_host seat) ~serial ~x:untrusted_x ~y:untrusted_y
    method on_unset_fullscreen _ = H.Xdg_toplevel.unset_fullscreen h
    method on_unset_maximized _ = H.Xdg_toplevel.unset_maximized h
  end

let make_xdg_surface ~tag ~host_xdg_surface c =
  let c = cv c in
  let h = host_xdg_surface @@ object
      inherit [_] H.Xdg_surface.v1
      method on_configure _ = C.Xdg_surface.configure c
    end
  in
  let user_data = client_data (Xdg_surface h) in
  Proxy.Handler.attach c @@ object
    inherit [_] C.Xdg_surface.v1
    method! user_data = user_data
    method on_destroy = delete_with H.Xdg_surface.destroy h
    method on_ack_configure _ ~(untrusted_serial:int32): unit =
      H.Xdg_surface.ack_configure h ~serial:(validate_serial ~untrusted_serial)
    method on_set_window_geometry p
             ~(untrusted_x:int32)
             ~(untrusted_y:int32)
             ~(untrusted_width:int32)
             ~(untrusted_height:int32): unit =
      V.check_width_height_int32 p C.Xdg_surface.Errors.invalid_size ~untrusted_width ~untrusted_height;
      V.check_x_y p C.Xdg_surface.Errors.invalid_size ~untrusted_x ~untrusted_y;

      let (x, y, width, height) = (untrusted_x, untrusted_y, untrusted_width, untrusted_height) in
      H.Xdg_surface.set_window_geometry h ~x ~y ~width ~height

    method on_get_toplevel _ = make_toplevel ~tag ~host_toplevel:(H.Xdg_surface.get_toplevel h)

    method on_get_popup _ popup ~parent ~positioner =
      let parent = Option.map to_host parent in
      let positioner = to_host positioner in
      make_popup ~host_popup:(H.Xdg_surface.get_popup h ~parent ~positioner) popup
  end

let make_positioner ~host_positioner c =
  let h = host_positioner @@ new H.Xdg_positioner.v1 in
  let user_data = client_data (Xdg_positioner h) in
  Proxy.Handler.attach c @@ object
    inherit [_] C.Xdg_positioner.v1
    method! user_data = user_data
    method on_destroy = delete_with H.Xdg_positioner.destroy h
    method on_set_anchor _ ~(untrusted_anchor:Protocols.Xdg_positioner.Anchor.t): unit =
      (* TODO: validate anchor *)
      let anchor = unsafe_no_sanitize untrusted_anchor in
      H.Xdg_positioner.set_anchor h ~anchor
    method on_set_anchor_rect p
             ~(untrusted_x:int32)
             ~(untrusted_y:int32)
             ~(untrusted_width:int32)
             ~(untrusted_height:int32): unit =
      V.check_width_height_int32 p C.Xdg_positioner.Errors.invalid_input ~untrusted_width ~untrusted_height;
      V.check_x_y p C.Xdg_positioner.Errors.invalid_input ~untrusted_x ~untrusted_y;
      let (x, y, width, height) = (untrusted_x, untrusted_y, untrusted_width, untrusted_height) in
      H.Xdg_positioner.set_anchor_rect h ~x ~y ~width ~height
    method on_set_constraint_adjustment _ ~(untrusted_constraint_adjustment:int32): unit =
      (* Validated by generated code.  FIXME: better error. *)
      H.Xdg_positioner.set_constraint_adjustment h ~constraint_adjustment:untrusted_constraint_adjustment
    method on_set_gravity _ ~(untrusted_gravity:Protocols.Xdg_positioner.Gravity.t): unit =
      (* TODO: validate gravity *)
      let gravity = unsafe_no_sanitize untrusted_gravity in
      H.Xdg_positioner.set_gravity h ~gravity
    method on_set_offset p ~(untrusted_x:int32) ~(untrusted_y:int32): unit =
      (* UNSAFE *)
      V.check_x_y p (fun _ -> assert false) ~untrusted_x ~untrusted_y;
      H.Xdg_positioner.set_offset h ~x:untrusted_x ~y:untrusted_y
    method on_set_size t ~(untrusted_width:int32) ~(untrusted_height:int32): unit =
      (* sanitize start *)
      V.check_width_height_int32 t (fun _ -> assert false) ~untrusted_width ~untrusted_height;
      let (width, height) = (untrusted_width, untrusted_height) in
      (* sanitize end.  FIXME: is this enough sanitization? *)
      H.Xdg_positioner.set_size h ~width ~height
    method on_set_reactive _ = H.Xdg_positioner.set_reactive h
    method on_set_parent_size t ~(untrusted_parent_width:int32) ~(untrusted_parent_height:int32) =
      (* sanitize start *)
      V.check_width_height_int32 t (fun _ -> assert false)
        ~untrusted_width:untrusted_parent_width
        ~untrusted_height:untrusted_parent_height;
      let (parent_width, parent_height) = (untrusted_parent_width, untrusted_parent_width) in
      (* sanitize end.  FIXME: is this enough sanitization? *)
      H.Xdg_positioner.set_parent_size h ~parent_width ~parent_height
    method on_set_parent_configure _ ~(untrusted_serial:int32): unit =
      let serial = validate_serial ~untrusted_serial in
      H.Xdg_positioner.set_parent_configure h ~serial
  end

let make_xdg_wm_base ~xwayland ~tag bind proxy =
  let pong_handlers = Queue.create () in
  let h = bind @@ object
      inherit [_] H.Xdg_wm_base.v1
      method on_ping h ~serial =
        Queue.add (H.Xdg_wm_base.pong h) pong_handlers;
        C.Xdg_wm_base.ping proxy ~serial
    end
  in
  let h = Proxy.cast_version h in
  Proxy.Handler.attach proxy @@ object
    inherit [_] C.Xdg_wm_base.v1

    method on_destroy = delete_with H.Xdg_wm_base.destroy h

    method on_pong _ ~(untrusted_serial:int32): unit =
      (* TODO: validate that the serial number is correct! *)
      match Queue.take_opt pong_handlers with
      | Some h -> h ~serial:untrusted_serial
      | None -> Log.warn (fun f -> f "Ignoring unexpected pong from client!")

    method on_create_positioner _ = make_positioner ~host_positioner:(H.Xdg_wm_base.create_positioner h)

    method on_get_xdg_surface _ xdg_surface ~surface =
      let host_xdg_surface = H.Xdg_wm_base.get_xdg_surface h ~surface:(to_host surface) in
      make_xdg_surface ~tag ~host_xdg_surface xdg_surface
  end;
  xwayland |> Option.iter (fun (x:xwayland_hooks) ->
      x#set_ping (fun () ->
          let serial = 0l in
          let pong, set_pong = Promise.create () in
          Queue.add (fun ~serial:_ -> Promise.resolve set_pong ()) pong_handlers;
          C.Xdg_wm_base.ping proxy ~serial;
          Promise.await pong
        )
    )

let make_zxdg_output ~xwayland ~host_xdg_output c =
  let c = cv c in
  let h = host_xdg_output @@ object
      inherit [_] H.Zxdg_output_v1.v1
      method on_description _ = C.Zxdg_output_v1.description c
      method on_done _ = C.Zxdg_output_v1.done_ c

      method on_logical_position _ ~x ~y =
        let (x, y) = scale_to_client ~xwayland (x, y) in
        C.Zxdg_output_v1.logical_position c ~x ~y

      method on_logical_size _ ~width ~height =
        let (width, height) = scale_to_client ~xwayland (width, height) in
        C.Zxdg_output_v1.logical_size c ~width ~height

      method on_name _ = C.Zxdg_output_v1.name c
    end in
  Proxy.Handler.attach c @@ object
    inherit [_] C.Zxdg_output_v1.v1
    method on_destroy = delete_with H.Zxdg_output_v1.destroy h
  end

let make_zxdg_output_manager_v1 ~xwayland bind proxy =
  let proxy = Proxy.cast_version proxy in
  let h = bind @@ new H.Zxdg_output_manager_v1.v1 in
  Proxy.Handler.attach proxy @@ object
    inherit [_] C.Zxdg_output_manager_v1.v1

    method on_destroy = delete_with H.Zxdg_output_manager_v1.destroy h

    method on_get_xdg_output _ c ~output =
      let output = to_host output in
      make_zxdg_output ~xwayland ~host_xdg_output:(H.Zxdg_output_manager_v1.get_xdg_output h ~output) c
  end

let make_kde_decoration ~host_decoration c =
  let h = host_decoration @@ object
      inherit [_] H.Org_kde_kwin_server_decoration.v1
      method on_mode _ = C.Org_kde_kwin_server_decoration.mode c
    end
  in
  Proxy.Handler.attach c @@ object
    inherit [_] C.Org_kde_kwin_server_decoration.v1
    method on_release = delete_with H.Org_kde_kwin_server_decoration.release h
    method on_request_mode _ ~untrusted_mode =
      begin match untrusted_mode with
      | 0l | 1l | 2l -> ()
      | _bad -> assert false (* TODO: proper protocol error *)
      end;
      H.Org_kde_kwin_server_decoration.request_mode h ~mode:untrusted_mode
  end

let make_kde_decoration_manager bind c =
  let h = bind @@ object
      inherit [_] H.Org_kde_kwin_server_decoration_manager.v1
      method on_default_mode _ = C.Org_kde_kwin_server_decoration_manager.default_mode c
    end
  in
  Proxy.Handler.attach c @@ object
    inherit [_] C.Org_kde_kwin_server_decoration_manager.v1
    method on_create _ decoration ~surface =
      let surface = to_host surface in
      make_kde_decoration ~host_decoration:(H.Org_kde_kwin_server_decoration_manager.create h ~surface) decoration
  end

let make_xdg_decoration ~host_decoration c =
  let h = host_decoration @@ object
      inherit [_] H.Zxdg_toplevel_decoration_v1.v1
      method on_configure _ = C.Zxdg_toplevel_decoration_v1.configure c
    end
  in
  Proxy.Handler.attach c @@ object
    inherit [_] C.Zxdg_toplevel_decoration_v1.v1
    method on_destroy = delete_with H.Zxdg_toplevel_decoration_v1.destroy h
    method on_set_mode _ ~(untrusted_mode:_): unit =
      (* TODO: better error for bad mode *)
      H.Zxdg_toplevel_decoration_v1.set_mode h ~mode:untrusted_mode
    method on_unset_mode _ = H.Zxdg_toplevel_decoration_v1.unset_mode h
  end

let make_xdg_decoration_manager bind c =
  let h = bind @@ object
      inherit [_] H.Zxdg_decoration_manager_v1.v1
    end
  in
  Proxy.Handler.attach c @@ object
    inherit [_] C.Zxdg_decoration_manager_v1.v1
    method on_destroy = delete_with H.Zxdg_decoration_manager_v1.destroy h
    method on_get_toplevel_decoration _ decoration ~toplevel =
      (* TODO: check that no buffers have been attached or committed *)
      (* TODO: check if buffers are attached or manipulated before configure event *)
      let toplevel = to_host toplevel in
      make_xdg_decoration ~host_decoration:(H.Zxdg_decoration_manager_v1.get_toplevel_decoration h ~toplevel) decoration
end

let make_relative_pointer ~host_relative_pointer c =
  let h =
    host_relative_pointer @@ object
      inherit [_] H.Zwp_relative_pointer_v1.v1
      method on_relative_motion _ = C.Zwp_relative_pointer_v1.relative_motion c
    end
  in
  Proxy.Handler.attach c @@ object
    inherit [_] C.Zwp_relative_pointer_v1.v1
    method on_destroy = delete_with H.Zwp_relative_pointer_v1.destroy h
  end

let make_relative_pointer_manager bind proxy =
  let proxy = Proxy.cast_version proxy in
  let h = bind @@ new H.Zwp_relative_pointer_manager_v1.v1 in
  Proxy.Handler.attach proxy @@ object
    inherit [_] C.Zwp_relative_pointer_manager_v1.v1
    method on_destroy = delete_with H.Zwp_relative_pointer_manager_v1.destroy h
    method on_get_relative_pointer _ relative_pointer ~pointer =
      let host_relative_pointer = H.Zwp_relative_pointer_manager_v1.get_relative_pointer h ~pointer:(to_host pointer) in
      make_relative_pointer ~host_relative_pointer relative_pointer
  end

let validate_mime_type _ ~(untrusted_mime_type:string): string =
  (* FIXME: validate! *)
  untrusted_mime_type


let make_data_offer ~client_offer h =
  let c = client_offer @@ object
      inherit [_] C.Wl_data_offer.v1
      method on_accept c ~(untrusted_serial:int32) ~(untrusted_mime_type:string option): unit =
        (* TODO: validate the serial number & the MIME type *)
        let serial = validate_serial ~untrusted_serial in
        let mime_type = match untrusted_mime_type with
          | Some untrusted_mime_type -> Some (validate_mime_type c ~untrusted_mime_type)
          | None -> None
        in
        H.Wl_data_offer.accept h ~serial ~mime_type
      method on_destroy c =
        delete_with H.Wl_data_offer.destroy h c;
        (* Effectively, the "selection" event is the destructor of the previous selection,
           and this is the confirmation. The server doesn't send a delete event, so just do it manually. *)
        Proxy.delete h
      method on_finish _ = H.Wl_data_offer.finish h
      method on_receive _ ~(untrusted_mime_type:string) ~(untrusted_fd:Unix.file_descr): unit =
        let fd = match Relay_stubs.validate_pipe untrusted_fd with
        (* FIXME: better logging *)
        | exception Unix.Unix_error _ -> Wayland.Msg.bad_implementation "problem validating pipe"
        | Success -> untrusted_fd
        | Non_AF_UNIX_Socket -> Wayland.Msg.bad_implementation "File descriptor is a socket, but not AF_UNIX socket"
        | Non_stream_AF_UNIX_Socket -> Wayland.Msg.bad_implementation "File descriptor is an AF_UNIX socket, but not a stream socket"
        | Neither_pipe_nor_socket -> Wayland.Msg.bad_implementation "File descriptor is neither a pipe nor a socket"
        in
        (* TODO: check that the MIME type is valid *)
        Fun.protect
          ~finally:(fun () -> Unix.close fd)
          (fun () -> (H.Wl_data_offer.receive h ~mime_type:untrusted_mime_type ~fd))
      method on_set_actions p ~(untrusted_dnd_actions:int32) ~(untrusted_preferred_action:int32) =
        (* sanitize start*)
        match untrusted_preferred_action with
        | _ when untrusted_dnd_actions < 0l || untrusted_dnd_actions > 7l -> (
          C.Wl_data_offer.Errors.invalid_action_mask p ~message:"Invalid action mask")
        | 1l | 2l | 4l when Int32.logand untrusted_preferred_action untrusted_dnd_actions = 0l -> (
          C.Wl_data_offer.Errors.invalid_action_mask p ~message:"Preferred action not in mask")
        | 1l | 2l | 4l -> (
          (* sanitized end *)
          H.Wl_data_offer.set_actions h
            ~dnd_actions:untrusted_dnd_actions
            ~preferred_action:untrusted_preferred_action)
        | _ -> C.Wl_data_offer.Errors.invalid_action_mask p ~message:"Invalid preferred action"
    end in
  let user_data = host_data (HD.Data_offer c) in
  Proxy.Handler.attach h @@ object
    inherit [_] H.Wl_data_offer.v1
    method! user_data = user_data
    method on_action _ = C.Wl_data_offer.action c
    method on_offer _ = C.Wl_data_offer.offer c
    method on_source_actions _ = C.Wl_data_offer.source_actions c
  end

let make_data_source ~host_source c =
  let c = cv c in
  let h =
    host_source @@ object
      inherit [_] H.Wl_data_source.v1
      method on_action _ = C.Wl_data_source.action c
      method on_cancelled _ = C.Wl_data_source.cancelled c
      method on_dnd_drop_performed _ = C.Wl_data_source.dnd_drop_performed c
      method on_dnd_finished _ = C.Wl_data_source.dnd_finished c
      method on_send _ ~mime_type ~fd =
        C.Wl_data_source.send c ~mime_type ~fd;
        Unix.close fd
      method on_target _ = C.Wl_data_source.target c
    end in
  let user_data = client_data (Data_source h) in
  Proxy.Handler.attach c @@ object
    inherit [_] C.Wl_data_source.v1
    method! user_data = user_data
    method on_destroy = delete_with H.Wl_data_source.destroy h
    method on_offer p ~untrusted_mime_type =
      H.Wl_data_source.offer h ~mime_type:(validate_mime_type p ~untrusted_mime_type)
    method on_set_actions _ ~(untrusted_dnd_actions:int32): unit =
      (* TODO: validate that this source will only be used for drag-and-drop. *)
      (* TODO: validate that this request is made only once. *)
      (if untrusted_dnd_actions < 0l || untrusted_dnd_actions > 7l then
        (* FIXME: protocol error *) assert false);
      H.Wl_data_source.set_actions h ~dnd_actions:untrusted_dnd_actions
  end

let make_data_device ~xwayland ~host_device c =
  let c = cv c in
  let h = host_device @@ object
      inherit [_] H.Wl_data_device.v1
      method on_data_offer _ offer = make_data_offer ~client_offer:(C.Wl_data_device.data_offer c) offer
      method on_drop _ = C.Wl_data_device.drop c

      method on_enter _ ~serial ~surface ~x ~y offer =
        let (x, y) = point_to_client ~xwayland (x, y) in
        C.Wl_data_device.enter c ~serial ~surface:(to_client surface) ~x ~y (Option.map to_client offer)

      method on_leave _ = C.Wl_data_device.leave c

      method on_motion _ ~time ~x ~y =
        let (x, y) = point_to_client ~xwayland (x, y) in
        C.Wl_data_device.motion c ~time ~x ~y

      method on_selection _ offer = C.Wl_data_device.selection c (Option.map to_client offer)
    end in
  Proxy.Handler.attach c @@ object
    inherit [_] C.Wl_data_device.v1
    method on_release = delete_with H.Wl_data_device.release h
    method on_set_selection _ ~source ~(untrusted_serial:int32) =
      (* TODO: validate serial *)
      H.Wl_data_device.set_selection h ~source:(Option.map to_host source) ~serial:untrusted_serial
    method on_start_drag _ ~source ~origin ~icon ~(untrusted_serial:int32): unit =
      (* TODO: validate serial *)
      H.Wl_data_device.start_drag h
        ~source:(Option.map to_host source)
        ~origin:(to_host origin)
        ~icon:(Option.map to_host icon)
        ~serial:untrusted_serial
  end

let make_data_device_manager ~xwayland bind proxy =
  let proxy = Proxy.cast_version proxy in
  let h = cv @@ bind @@ new H.Wl_data_device_manager.v1 in
  Proxy.Handler.attach proxy @@ object
    inherit [_] C.Wl_data_device_manager.v1
    method on_create_data_source _ c =
      make_data_source c ~host_source:(H.Wl_data_device_manager.create_data_source h)
    method on_get_data_device _ c ~seat =
      let seat = to_host seat in
      make_data_device ~xwayland c ~host_device:(H.Wl_data_device_manager.get_data_device h ~seat)
  end

let validate_pipe c ~(untrusted_fd:Unix.file_descr): Unix.file_descr =
  (* TODO: validate *)
  let _ = c in untrusted_fd

module Gtk_primary = struct
  let make_gtk_data_offer ~client_offer h =
    let c = client_offer @@ object
        inherit [_] C.Gtk_primary_selection_offer.v1

        method on_destroy c =
          delete_with H.Zwp_primary_selection_offer_v1.destroy h c;
          (* Effectively, the "selection" event is the destructor of the previous selection,
             and this is the confirmation. The server doesn't send a delete event, so just do it manually. *)
          Proxy.delete h

        method on_receive c ~(untrusted_mime_type:string) ~(untrusted_fd:Unix.file_descr) =
          Fun.protect ~finally:(fun () -> Unix.close untrusted_fd)
            (fun () ->
              let mime_type = validate_mime_type c ~untrusted_mime_type in
              let fd = validate_pipe c ~untrusted_fd in
              H.Zwp_primary_selection_offer_v1.receive h ~mime_type ~fd)
      end in
    let user_data = host_data (HD.Gtk_data_offer c) in
    Proxy.Handler.attach h @@ object
      inherit [_] H.Zwp_primary_selection_offer_v1.v1
      method! user_data = user_data
      method on_offer _ = C.Gtk_primary_selection_offer.offer c
    end

  let make_gtk_primary_selection_source ~host_source c =
    let h =
      host_source @@ object
        inherit [_] H.Zwp_primary_selection_source_v1.v1
        method on_cancelled _ = C.Gtk_primary_selection_source.cancelled c
        method on_send _ ~mime_type ~fd =
          C.Gtk_primary_selection_source.send c ~mime_type ~fd;
          Unix.close fd
      end in
    let user_data = client_data (Gtk_source h) in
    Proxy.Handler.attach c @@ object
      inherit [_] C.Gtk_primary_selection_source.v1
      method! user_data = user_data
      method on_destroy = delete_with H.Zwp_primary_selection_source_v1.destroy h
      method on_offer c ~(untrusted_mime_type: string): unit =
        let mime_type = validate_mime_type c ~untrusted_mime_type in
        H.Zwp_primary_selection_source_v1.offer h ~mime_type
    end

  let make_gtk_primary_selection_device ~host_device c =
    let h = host_device @@ object
        inherit [_] H.Zwp_primary_selection_device_v1.v1
        method on_data_offer _ offer = make_gtk_data_offer ~client_offer:(C.Gtk_primary_selection_device.data_offer c) offer
        method on_selection _ offer =
          let to_client x =
            let Host_data data = user_data x in
            match data with
            | HD.Gtk_data_offer c -> cv c
            | HD.Zwp_data_offer _ -> failwith "Can't mix Zwp and Gtk selection protocols!"
          in
          C.Gtk_primary_selection_device.selection c (Option.map to_client offer)
      end in
    Proxy.Handler.attach c @@ object
      inherit [_] C.Gtk_primary_selection_device.v1
      method on_destroy = delete_with H.Zwp_primary_selection_device_v1.destroy h
      method on_set_selection _ ~source ~(untrusted_serial:int32): unit =
        let to_host x =
          let Client_data (CD.Gtk_source data) = user_data x in
          cv data
        in
        let source = Option.map to_host source in
        (* TODO: validate serial *)
        let serial = untrusted_serial in
        H.Zwp_primary_selection_device_v1.set_selection h ~source ~serial
    end

  let make_device_manager bind proxy =
    let proxy = Proxy.cast_version proxy in
    let h = bind @@ new H.Zwp_primary_selection_device_manager_v1.v1 in
    Proxy.Handler.attach proxy @@ object
      inherit [_] C.Gtk_primary_selection_device_manager.v1
      method on_create_source _ source =
        let host_source = H.Zwp_primary_selection_device_manager_v1.create_source h in
        make_gtk_primary_selection_source ~host_source source
      method on_destroy = delete_with H.Zwp_primary_selection_device_manager_v1.destroy h
      method on_get_device _ dev ~seat =
        let seat = to_host seat in
        let host_device = H.Zwp_primary_selection_device_manager_v1.get_device h ~seat in
        make_gtk_primary_selection_device ~host_device dev
    end
end

let make_locked_pointer ~host_pointer c =
  let h = host_pointer @@ object
      inherit [_] H.Zwp_locked_pointer_v1.v1
      method on_locked _ = C.Zwp_locked_pointer_v1.locked c
      method on_unlocked _ = C.Zwp_locked_pointer_v1.unlocked c
    end in
  Proxy.Handler.attach c @@ object
    inherit [_] C.Zwp_locked_pointer_v1.v1
    method on_destroy = delete_with H.Zwp_locked_pointer_v1.destroy h
    method on_set_region _ ~region = H.Zwp_locked_pointer_v1.set_region h ~region:(Option.map to_host region)
    method on_set_cursor_position_hint _ ~(untrusted_surface_x:Fixed.t) ~(untrusted_surface_y:Fixed.t): unit =
      (* TODO: validate stuff *)
      let (surface_x, surface_y) = (untrusted_surface_x, untrusted_surface_y) in
      H.Zwp_locked_pointer_v1.set_cursor_position_hint h ~surface_x:surface_x ~surface_y:surface_y
  end

let make_confined_pointer ~host_pointer c =
  let h = host_pointer @@ object
      inherit [_] H.Zwp_confined_pointer_v1.v1
      method on_confined _ = C.Zwp_confined_pointer_v1.confined c
      method on_unconfined _ = C.Zwp_confined_pointer_v1.unconfined c
    end in
  Proxy.Handler.attach c @@ object
    inherit [_] C.Zwp_confined_pointer_v1.v1
    method on_destroy = delete_with H.Zwp_confined_pointer_v1.destroy h
    method on_set_region _ ~region = H.Zwp_confined_pointer_v1.set_region h ~region:(Option.map to_host region)
  end

let make_pointer_constraints bind proxy =
  let proxy = Proxy.cast_version proxy in
  let h = bind @@ new H.Zwp_pointer_constraints_v1.v1 in
  Proxy.Handler.attach proxy @@ object
    inherit [_] C.Zwp_pointer_constraints_v1.v1
    method on_destroy = delete_with H.Zwp_pointer_constraints_v1.destroy h

    method on_lock_pointer _ locked_pointer ~surface ~pointer ~region ~untrusted_lifetime =
      (* validated by generated code *)
      let lifetime = untrusted_lifetime in
      let surface = to_host surface in
      let pointer = to_host pointer in
      let region = Option.map to_host region in
      let host_pointer = H.Zwp_pointer_constraints_v1.lock_pointer h ~surface ~pointer ~region ~lifetime in
      make_locked_pointer ~host_pointer locked_pointer

    method on_confine_pointer _ locked_pointer ~surface ~pointer ~region ~untrusted_lifetime =
      (* validated by generated code *)
      let lifetime = untrusted_lifetime in
      let surface = to_host surface in
      let pointer = to_host pointer in
      let region = Option.map to_host region in
      let host_pointer = H.Zwp_pointer_constraints_v1.confine_pointer h ~surface ~pointer ~region ~lifetime in
      make_confined_pointer ~host_pointer locked_pointer
  end

(* This is basically the same as [Gtk_primary], but with things renamed a bit. *)
module Zwp_primary = struct
  let make_data_offer ~client_offer h =
    let c = client_offer @@ object
        inherit [_] C.Zwp_primary_selection_offer_v1.v1

        method on_destroy c =
          delete_with H.Zwp_primary_selection_offer_v1.destroy h c;
          (* Effectively, the "selection" event is the destructor of the previous selection,
             and this is the confirmation. The server doesn't send a delete event, so just do it manually. *)
          Proxy.delete h

        method on_receive c ~(untrusted_mime_type:string) ~(untrusted_fd): unit =
          Fun.protect
            ~finally:(fun () -> Unix.close untrusted_fd)
            (fun () ->
              let mime_type = validate_mime_type c ~untrusted_mime_type in
              let fd = validate_pipe c ~untrusted_fd in
              H.Zwp_primary_selection_offer_v1.receive h ~mime_type ~fd)
      end in
    let user_data = host_data (HD.Zwp_data_offer c) in
    Proxy.Handler.attach h @@ object
      inherit [_] H.Zwp_primary_selection_offer_v1.v1
      method! user_data = user_data
      method on_offer _ = C.Zwp_primary_selection_offer_v1.offer c
    end

  let make_primary_selection_source ~host_source c =
    let h =
      host_source @@ object
        inherit [_] H.Zwp_primary_selection_source_v1.v1
        method on_cancelled _ = C.Zwp_primary_selection_source_v1.cancelled c
        method on_send _ ~mime_type ~fd =
          C.Zwp_primary_selection_source_v1.send c ~mime_type ~fd;
          Unix.close fd
      end in
    let user_data = client_data (Zwp_source h) in
    Proxy.Handler.attach c @@ object
      inherit [_] C.Zwp_primary_selection_source_v1.v1
      method! user_data = user_data
      method on_destroy = delete_with H.Zwp_primary_selection_source_v1.destroy h
      method on_offer p ~(untrusted_mime_type:string): unit =
        let mime_type = validate_mime_type p ~untrusted_mime_type in
        H.Zwp_primary_selection_source_v1.offer h ~mime_type
    end

  let make_primary_selection_device ~host_device c =
    let h = host_device @@ object
        inherit [_] H.Zwp_primary_selection_device_v1.v1
        method on_data_offer _ offer = make_data_offer ~client_offer:(C.Zwp_primary_selection_device_v1.data_offer c) offer
        method on_selection _ offer = C.Zwp_primary_selection_device_v1.selection c (Option.map to_client offer)
      end in
    Proxy.Handler.attach c @@ object
      inherit [_] C.Zwp_primary_selection_device_v1.v1
      method on_destroy = delete_with H.Zwp_primary_selection_device_v1.destroy h
      method on_set_selection _ ~source ~(untrusted_serial:int32): unit =
        let serial = validate_serial ~untrusted_serial in
        let source = Option.map to_host source in
        H.Zwp_primary_selection_device_v1.set_selection h ~source ~serial
    end

  let make_device_manager bind proxy =
    let proxy = Proxy.cast_version proxy in
    let h = bind @@ new H.Zwp_primary_selection_device_manager_v1.v1 in
    Proxy.Handler.attach proxy @@ object
      inherit [_] C.Zwp_primary_selection_device_manager_v1.v1
      method on_create_source _ source =
        let host_source = H.Zwp_primary_selection_device_manager_v1.create_source h in
        make_primary_selection_source ~host_source source
      method on_destroy = delete_with H.Zwp_primary_selection_device_manager_v1.destroy h
      method on_get_device _ dev ~seat =
        let seat = to_host seat in
        let host_device = H.Zwp_primary_selection_device_manager_v1.get_device h ~seat in
        make_primary_selection_device ~host_device dev
    end
end

type entry = Entry : int32 * (module Metadata.S) -> entry

let registry =
  let open Protocols in
  [
    (module Wl_shm : Metadata.S);
    (module Wl_compositor);
    (module Wl_subcompositor);
    (module Xdg_wm_base);
    (module Wl_data_device_manager);
    (module Zxdg_output_manager_v1);
    (module Zwp_primary_selection_device_manager_v1);
    (module Wl_seat); (* Must come after primary selection device, or evince crashes *)
    (module Wl_output);
    (module Org_kde_kwin_server_decoration_manager);
    (module Zxdg_decoration_manager_v1);
    (module Zwp_relative_pointer_manager_v1);
    (module Zwp_pointer_constraints_v1);
    (module Zwp_linux_dmabuf_v1);
  ]

let make_registry ~xwayland t reg =
  let registry =
    registry |> List.concat_map (fun (module M : Metadata.S) ->
        match Registry.get t.host.registry M.interface with
        | [] ->
          Log.info (fun f -> f "Host doesn't support service %s, so skipping" M.interface);
          []
        | { Registry.name; version = host_version } :: _ ->
          let max_version = min M.version host_version in
          let item = (name, Entry (max_version, (module M))) in
          if M.interface = Protocols.Zwp_primary_selection_device_manager_v1.interface then (
            let compat = (name, Entry (max_version, (module Protocols.Gtk_primary_selection_device_manager))) in
            [item; compat]
          ) else (
            [item]
          )
      )
    |> Array.of_list
  in
  Proxy.Handler.attach reg @@ object
    inherit [_] C.Wl_registry.v1

    method on_bind : type a. _ -> untrusted_name:int32 -> (a, [`Unknown], _) Proxy.t -> unit =
      fun _ ~untrusted_name proxy ->
      if untrusted_name < 0l || untrusted_name >= Int32.of_int (Array.length registry) then
        Fmt.failwith "Bad registry entry name %lu" untrusted_name;
      let name = Int32.to_int untrusted_name in
      let host_name, Entry (max_version, (module M)) = registry.(name) in
      let requested_version = Proxy.version proxy in
      if requested_version > max_version then
        Fmt.failwith "Client asked for %S v%lu, but we only support up to %lu" M.interface requested_version max_version;
      let client_interface = Proxy.interface proxy in
      if client_interface <> M.interface then
        Fmt.failwith "Entry %d has type %S, client expected %S!" name M.interface client_interface;
      let bind x =
        assert (x#min_version = 1l);
        H.Wl_registry.bind (Registry.wl_registry t.host.registry) ~name:host_name (x, Proxy.version proxy)
      in
      let open Protocols in
      let proxy = Proxy.cast_version proxy in
      match Proxy.ty proxy with
      | Wl_compositor.T -> make_compositor ~xwayland bind proxy
      | Wl_subcompositor.T -> make_subcompositor ~xwayland bind proxy
      | Wl_shm.T -> make_shm ~virtio_gpu:t.host.virtio_gpu bind proxy
      | Wl_seat.T -> make_seat ~xwayland t bind proxy
      | Wl_output.T -> make_output ~xwayland bind proxy
      | Wl_data_device_manager.T -> make_data_device_manager ~xwayland bind proxy
      | Gtk_primary_selection_device_manager.T -> Gtk_primary.make_device_manager bind proxy
      | Zwp_primary_selection_device_manager_v1.T -> Zwp_primary.make_device_manager bind proxy
      | Xdg_wm_base.T -> make_xdg_wm_base ~xwayland ~tag:t.config.tag bind proxy
      | Zxdg_output_manager_v1.T -> make_zxdg_output_manager_v1 ~xwayland bind proxy
      | Org_kde_kwin_server_decoration_manager.T -> make_kde_decoration_manager bind proxy
      | Zxdg_decoration_manager_v1.T -> make_xdg_decoration_manager bind proxy
      | Zwp_relative_pointer_manager_v1.T -> make_relative_pointer_manager bind proxy
      | Zwp_pointer_constraints_v1.T -> make_pointer_constraints bind proxy
      | Zwp_linux_dmabuf_v1.T -> Linux_dmabuf.make_linux_dmabuf ~virtio_gpu:t.host.virtio_gpu bind proxy
      | _ -> Fmt.failwith "Invalid service name for %a" Proxy.pp proxy
  end;
  registry |> Array.iteri (fun name (_, entry) ->
      let Entry (version, (module M)) = entry in
      C.Wl_registry.global reg ~name:(Int32.of_int name) ~interface:M.interface ~version
    )

let run ?xwayland ~(error_callback:Wayland.Client.error_callback option ref) ~config host client =
  let t = { host; config } in
  let client_transport = Wayland.Unix_transport.of_socket client in
  Switch.run (fun sw ->
      let s =
        Server.connect ~sw client_transport ~trace:(module Trace.Client) @@ object
          inherit [_] C.Wl_display.v1
          method on_get_registry _ ref = make_registry ~xwayland t ref
          method on_sync _ cb =
            Proxy.Handler.attach cb @@ new C.Wl_callback.v1;
            let h : _ Proxy.t = H.Wl_display.sync (Client.wl_display host.display) @@ object
                inherit [_] H.Wl_callback.v1
                method on_done ~callback_data =
                  C.Wl_callback.done_ cb ~callback_data
              end
            in
            Proxy.on_delete h (fun () -> Proxy.delete cb)
        end
      in
      let cb ~object_id ~code ~message =
          Server.implementation_error s @@ Format.asprintf
            "Host compositor posted error: object %d, code %d, message %s"
            (Int32.to_int object_id)
            (Int32.to_int code)
            message in
      error_callback := Some cb

    );
  Log.info (fun f -> f "Client finished; closing host connection");
  Client.stop host.display
