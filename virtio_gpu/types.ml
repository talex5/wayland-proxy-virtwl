module type ENDIAN = (module type of Cstruct.BE)

let ne =
  if Sys.big_endian then (module Cstruct.BE : ENDIAN)
  else (module Cstruct.LE : ENDIAN)

(* Native endian *)
module NE = (val ne)

type gem_handle = int32         (* AKA bo_handle *)

type 'a to_host = Cstruct.t

module Res_handle = struct
  type t = int32

  let pp = Fmt.int32

  module Map = Map.Make(Int32)

  let init = 1l
  let next = Int32.add 2l
end

module Capabilities = struct
  type t = {
    version : int32;
    supported_channels : int32;
    supports_dmabuf : bool;
    supports_external_gpu_memory : bool;
  }

  let create_buffer () = (Cstruct.create (4 * 4)).buffer

  let of_buffer buffer =
    let x = Cstruct.of_bigarray buffer in
    let version = NE.get_uint32 x 0 in
    let supported_channels = NE.get_uint32 x 1 in
    let supports_dmabuf = NE.get_uint32 x 2 <> 0l in
    let supports_external_gpu_memory = NE.get_uint32 x 3 <> 0l in
    { version; supported_channels; supports_dmabuf; supports_external_gpu_memory }
end

module Init_context = struct
  type t = Cstruct.buffer

  type ring = [ `Channel ]

  type param = [
    | `Capset_id of [`Cross_domain]
    | `Num_rings of int
    | `Poll_rings_mask of ring list
  ]

  let encode_capset = function
    | `Cross_domain -> 5L

  let ring_id = function
    | `Channel -> 1

  let rec encode_mask = function
    | [] -> 0L
    | x :: xs ->
      Int64.logor
        (Int64.(shift_left one) (ring_id x))
        (encode_mask xs)

  let encode = function
    | `Capset_id id      -> 0x0001L, (encode_capset id)
    | `Num_rings n       -> 0x0002L, Int64.of_int n
    | `Poll_rings_mask m -> 0x0003L, (encode_mask m)

  let create items =
    let rec init cs = function
      | [] -> ()
      | item :: items ->
        let k, v = encode item in
        NE.set_uint64 cs 0 k;
        NE.set_uint64 cs 8 v;
        init (Cstruct.shift cs 16) items
    in
    let n = List.length items in
    let buffer = Bigarray.(Array1.create char c_layout (n * 16)) in
    let cs = Cstruct.of_bigarray buffer in
    init cs items;
    buffer
end

module Create_blob = struct
  type t = Cstruct.buffer

  let v_MEM_GUEST        = 0x0001l
  let v_MEM_HOST3D       = 0x0002l
  let _v_MEM_HOST3D_GUEST = 0x0003l

  let v_FLAG_USE_MAPPABLE     = 0x0001l
  let v_FLAG_USE_SHAREABLE    = 0x0002l
  let _v_FLAG_USE_CROSS_DEVICE = 0x0004l

  let len = 48

  let request mem ~mappable ~shareable ~size =
    let cs = Cstruct.create len in
    NE.set_uint32 cs 0 (
      match mem with
      | `Guest -> v_MEM_GUEST
      | `Host3D id ->
        NE.set_uint64 cs 40 (Int64.of_int32 id);
        v_MEM_HOST3D
    );
    NE.set_uint32 cs 4 (
      Int32.logor
        (if mappable then v_FLAG_USE_MAPPABLE else 0l)
        (if shareable then v_FLAG_USE_SHAREABLE else 0l)
    );
    NE.set_uint64 cs 16 size;
    cs.buffer

  let parse t =
    let cs = Cstruct.of_bigarray t in
    let gem_handle = NE.get_uint32 cs 8 in
    let res_handle = NE.get_uint32 cs 12 in
    (gem_handle, res_handle)
end

module Cross_domain_header = struct
  let cmd_code = function
    | `INIT -> 1
    | `GET_IMAGE_REQUIREMENTS -> 2
    | `POLL -> 3
    | `SEND -> 4
    | `RECEIVE -> 5
    | `READ -> 6
    | `WRITE -> 7

  let header_size = 8

  let create ?(fence_ctx_idx=0) cmd body_size write_body =
    let t = Cstruct.create (header_size + body_size) in
    Cstruct.set_uint8 t 0 (cmd_code cmd);
    Cstruct.set_uint8 t 1 fence_ctx_idx;
    NE.set_uint16 t 2 (Cstruct.length t);
    write_body (Cstruct.shift t header_size);
    t
end

module Cross_domain_init = struct
  type t = [`Init] to_host

  let create ~ring ~channel_type =
    Cross_domain_header.create `INIT 8 @@ fun body ->
    NE.set_uint32 body 0 ring;
    NE.set_uint32 body 4 (match channel_type with
        | `Wayland -> 0x0001l
        | `Camera  -> 0x0002l
      )
end

module Cross_domain_read_write = struct
  type t = [`Read_write] to_host

  let create ~id buf ~len =
    let size = 16 + len in
    Cross_domain_header.create `WRITE size @@ fun body ->
    NE.set_uint32 body 0 id;
    NE.set_uint32 body 4 (if len = 0 then 1l else 0l);
    NE.set_uint32 body 8 (Int32.of_int len);
    Cstruct.blit_from_bytes buf 0 body 16 len;
    ()

  let parse ring fn =
    let body = Cstruct.shift ring Cross_domain_header.header_size in
    let id = NE.get_uint32 body 0 in
    let hang_up =
      match NE.get_uint32 body 4 with
      | 0l -> false
      | 1l -> true
      | x -> Fmt.failwith "Invalid hang_up bool %ld" x
    in
    let opaque_data_size = NE.get_uint32 body 8 in
    let data = Cstruct.to_string body ~off:16 ~len:(Int32.to_int opaque_data_size) in
    fn ~id ~hang_up data
end

module Cross_domain_send_recv = struct
  type t = [`Send] to_host

  let max_ids = 4

  let header_size = 8 + max_ids * 12

  let encode_type = function
    | `Blob -> 1l
    | `Read_pipe -> 3l

  let decode_type = function
    | 1l -> `Blob
    | 4l -> `Write_pipe
    | x -> Fmt.failwith "Unknown ID type %ld" x

  let create data fds =
    let n_ids = List.length fds in
    assert (n_ids <= max_ids);
    Cross_domain_header.create `SEND (header_size + Cstruct.length data) @@ fun body ->
    NE.set_uint32 body 0 (Int32.of_int n_ids);
    NE.set_uint32 body 4 (Int32.of_int (Cstruct.length data));
    let ids_array = Cstruct.shift body 8 in
    let tys_array = Cstruct.shift ids_array (4 * max_ids) in
    fds |> List.iteri (fun i (res_handle, ty) ->
        let ty = encode_type ty in
        NE.set_uint32 ids_array (i * 4) res_handle;
        NE.set_uint32 tys_array (i * 4) ty
      );
    Cstruct.blit data 0 body header_size (Cstruct.length data)

  let parse ring fn =
    let body = Cstruct.shift ring Cross_domain_header.header_size in
    let n_ids = Int32.to_int (NE.get_uint32 body 0) in
    let len = Int32.to_int (NE.get_uint32 body 4) in
    let ids_array = Cstruct.shift body 8 in
    let tys_array = Cstruct.shift ids_array (4 * max_ids) in
    let sizes_array = Cstruct.shift tys_array (4 * max_ids) in
    let rec aux acc = function
      | 0 -> acc
      | i ->
        let i = i - 1 in
        let id = NE.get_uint32 ids_array (i * 4) in
        let ty = NE.get_uint32 tys_array (i * 4) in
        let size = Int64.of_int32 (NE.get_uint32 sizes_array (i * 4)) in
        let info = (id, decode_type ty, size) in
        aux (info :: acc) i
    in
    let ids = aux [] n_ids in
    fn (Cstruct.sub body header_size len) ids
end

module Cross_domain_poll = struct
  type t = [`Poll] to_host

  let v = Cross_domain_header.create `POLL 8 ignore
end

module Gbm = struct
  let v_BO_USE_SCANOUT = Int32.of_int @@ 1 lsl 0        (* image will be shown on screen *)
  let v_BO_USE_LINEAR  = Int32.of_int @@ 1 lsl 4        (* image is not tiled *)
end

module Cross_domain_image_requirements = struct
  type t = [`Image_req] to_host

  let create ~linear ~scanout ~drm_format ~width ~height =
    Cross_domain_header.create `GET_IMAGE_REQUIREMENTS 16 @@ fun x ->
    NE.set_uint32 x 0 width;
    NE.set_uint32 x 4 height;
    NE.set_uint32 x 8 (drm_format : Drm_format.t :> int32);
    NE.set_uint32 x 12 (
      Int32.logor
        (if linear then Gbm.v_BO_USE_LINEAR else 0l)
        (if scanout then Gbm.v_BO_USE_SCANOUT else 0l)
    )

  let parse t fn =
    let stride0 = NE.get_uint32 t 0 in
    let offset0 = NE.get_uint32 t 16 in
    let host_size = NE.get_uint64 t 40 in
    let blob_id = NE.get_uint32 t 48 in
    fn ~stride0 ~offset0 ~host_size ~blob_id
end

module Wayland_ring = struct
  let parse t ~recv ~read_pipe =
    Log.debug (fun f -> f "handle_event %ld" (NE.get_uint32 t 0));
    match NE.get_uint32 t 0 with
    | 5l -> Cross_domain_send_recv.parse t recv
    | 6l -> Cross_domain_read_write.parse t read_pipe
    | x -> Fmt.failwith "Unknown virtio_gpu event code %ld!" x
end
