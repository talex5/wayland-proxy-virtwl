open Eio.Std

open Types

type pipe = Eio_unix.sink_ty r

type image_template = {
  template_id : Res_handle.t;
  host_size : int64;
  stride0 : int32;
  offset0 : int32;
}

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

type ring = {
  id : Res_handle.t;
  handle : gem_handle;
  data : Cstruct.t;
}

type t = {
  sw : Switch.t;
  fd : Eio_unix.Fd.t;
  query_ring : ring;            (* Invalid if [is_closed] *)
  channel_ring : ring;          (* Invalid if [is_closed] *)
  unmap_rings : unit Lazy.t;    (* Force to close [fd] and unmap rings. *)
  mutable pipe_of_id : pipe Res_handle.Map.t;
  mutable last_pipe_id : Res_handle.t;
  alloc_cache : (query, image_template) Hashtbl.t;
}

let is_closed t =
  Eio_unix.Fd.use t.fd (Fun.const false) ~if_closed:(Fun.const true)

let get_dev t =
  if is_closed t then failwith "virtio-gpu device has been closed!"
  else t.fd

type version

external drm_get_version : Unix.file_descr -> version = "ocaml_drm_get_version"
external drm_get_caps : Unix.file_descr -> Cstruct.buffer -> unit = "ocaml_drm_get_caps"
external drm_version_name : version -> string = "ocaml_drm_version_name"
external drm_context_init : Unix.file_descr -> int -> Cstruct.buffer -> unit = "ocaml_drm_context_init"
external drm_create_blob : Unix.file_descr -> Create_blob.t -> unit = "ocaml_drm_create_blob"
external drm_map : Unix.file_descr -> gem_handle -> int64 = "ocaml_drm_map"
external get_page_size : unit -> int = "ocaml_get_page_size" [@@noalloc]
external ocaml_drm_exec_buffer : Unix.file_descr -> _ to_host -> int32 option -> gem_handle array -> unit = "ocaml_drm_exec_buffer"
external drm_prime_handle_to_fd : Unix.file_descr -> gem_handle -> Unix.file_descr = "ocaml_drm_prime_handle_to_fd"
external drm_prime_fd_to_handle : Unix.file_descr -> Unix.file_descr -> gem_handle = "ocaml_drm_prime_fd_to_handle"
external drm_resource_info : Unix.file_descr -> gem_handle -> Res_handle.t = "ocaml_drm_resource_info"
external drm_wait : Unix.file_descr -> gem_handle -> unit = "ocaml_drm_wait"
external close_gem_handle : Unix.file_descr -> gem_handle -> unit = "ocaml_close_gem_handle"

let drm_exec_buffer ?ring ?(handles=[| |]) fd data =
  let ring = ring |> Option.map (function
      | `Query -> 0l
      | `Channel -> 1l
    )
  in
  ocaml_drm_exec_buffer fd data ring handles

let page_size = get_page_size ()

let create_blob t mem ~mappable ~shareable ~size =
  let msg = Create_blob.request mem ~mappable ~shareable ~size in
  drm_create_blob t msg;
  Create_blob.parse msg

let init_context t items =
  let n = List.length items in
  drm_context_init t n (Init_context.create items)

let poll t =
  let dev = get_dev t in
  Eio_unix.Fd.use_exn "poll" dev @@ fun dev ->
  drm_exec_buffer dev Cross_domain_poll.v ~ring:`Channel ~handles:[| t.channel_ring.handle |]

let check_caps fd =
  let caps = Capabilities.create_buffer () in
  drm_get_caps fd caps;
  let { Capabilities.
        version;
        supported_channels;
        supports_dmabuf;
        supports_external_gpu_memory;
      } = Capabilities.of_buffer caps in 
  Log.debug (fun f -> f "Capabilities: version=%ld, supported_channels=0x%lx, dmabuf=%b, external_gpu_memory=%b"
                version
                supported_channels
                supports_dmabuf
                supports_external_gpu_memory
            )

let create_ring unix_fd =
  (* Set up shared ring *)
  let handle, id = create_blob unix_fd ~size:(Int64.of_int page_size) `Guest ~mappable:true ~shareable:false in
  (* Map it into our address space *)
  let offset = drm_map unix_fd handle in
  let data = Utils.safe_map_file unix_fd ~pos:offset ~len:page_size ~host_size:page_size ~kind:Bigarray.char in
  { id; handle; data = Cstruct.of_bigarray data }

let of_fd ~sw fd =
  Eio_unix.Fd.use_exn "of_fd" fd @@ fun unix_fd ->
  let version = drm_get_version unix_fd in
  if drm_version_name version <> "virtio_gpu" then None
  else (
    check_caps unix_fd;
    (* todo: Get parameters, check it supports Wayland *)
    init_context unix_fd [
      `Capset_id `Cross_domain;
      `Num_rings 2;
      `Poll_rings_mask [`Channel];
    ];
    let query_ring = create_ring unix_fd in
    let channel_ring = create_ring unix_fd in
    let unmap_rings = lazy (
      Log.info (fun f -> f "Closing %a and unmapping device" Eio_unix.Fd.pp fd);
      let closed = Eio_unix.Fd.use fd (Fun.const false) ~if_closed:(Fun.const true) in
      if not closed then Eio_unix.Fd.close fd;  (* Prevents further ring use *)
      Utils.unmap (Bigarray.genarray_of_array1 query_ring.data.buffer);
      Utils.unmap (Bigarray.genarray_of_array1 channel_ring.data.buffer)
    ) in
    Switch.on_release sw (fun () -> Lazy.force unmap_rings);
    (* Tell Linux to use it for Wayland *)
    let init =
      Cross_domain_init.create
        ~channel_type:`Wayland
        ~query_ring:query_ring.id
        ~channel_ring:channel_ring.id
    in
    drm_exec_buffer unix_fd init;
    Some {
      sw;
      fd;
      query_ring;
      channel_ring;
      unmap_rings;
      pipe_of_id = Res_handle.Map.empty;
      last_pipe_id = Res_handle.pipe_read_start;
      alloc_cache = Hashtbl.create 100;
    }
  )

(* Each time we query crosvm, it allocates a new resource which is never freed (while the device is open).
   So cache every response. *)
let query_image t query =
  match Hashtbl.find_opt t.alloc_cache query with
  | Some x -> x
  | None ->
    let cs = Cross_domain_image_requirements.create
        ~linear:true
        ~scanout:true
        ~width:query.width
        ~height:query.height
        ~drm_format:query.drm_format
    in
    let dev = get_dev t in
    Eio_unix.Fd.use_exn "query_image" dev @@ fun dev ->
    drm_exec_buffer dev cs ~ring:`Query ~handles:[| t.query_ring.handle |];
    drm_wait dev t.query_ring.handle;
    Cross_domain_image_requirements.parse t.query_ring.data @@ fun ~stride0 ~offset0 ~host_size ~blob_id ->
    let cached = { host_size; template_id = blob_id; stride0; offset0 } in
    Hashtbl.add t.alloc_cache query cached;
    cached

let alloc t query =
  let dev = get_dev t in
  Eio_unix.Fd.use_exn "alloc" dev @@ fun dev ->
  let details = query_image t query in 
  Log.info (fun f -> f "alloc: strides = %ld, offsets = %ld, host_size = %Ld, blob_id = %a"
               details.stride0 details.offset0 details.host_size Res_handle.pp details.template_id);
  let bo_handle, _ = create_blob dev (`Host3D details.template_id) ~size:details.host_size ~mappable:true ~shareable:true in
  let fd = drm_prime_handle_to_fd dev bo_handle in
  close_gem_handle dev bo_handle;
  { query; fd; host_size = details.host_size; stride = details.stride0; offset = details.offset0 }

let create_send t data fds =
  let handles = ref [] in
  let dev = get_dev t in
  Eio_unix.Fd.use_exn "create_send" dev @@ fun dev ->
  let to_id fd =
    match Unix.fstat fd with
    | Unix.{ st_kind = S_FIFO; _ } ->
      (* Send a pipe *)
      let fd = Unix.dup ~cloexec:true fd in
      let pipe = (Eio_unix.Net.import_socket_stream ~sw:t.sw ~close_unix:true fd :> Eio_unix.sink_ty r) in
      let id = Res_handle.next_pipe_id t.last_pipe_id in
      t.pipe_of_id <- Res_handle.Map.add id pipe t.pipe_of_id;
      t.last_pipe_id <- id;
      (id, `Read_pipe)          (* We read; the host writes *)
    | _ ->
      (* Send a buffer *)
      let gem_handle = drm_prime_fd_to_handle dev fd in
      let res_handle = drm_resource_info dev gem_handle in
      (* Closing gem_handle here seems to invalidate res_handle too. *)
      handles := gem_handle :: !handles;
      (res_handle, `Blob)
  in
  let ids = List.map to_id fds in
  Cross_domain_send_recv.create data ids, !handles

let send t data fds =
  let cmd_send, to_close = create_send t data fds in
  let dev = get_dev t in
  Eio_unix.Fd.use_exn "send" dev @@ fun dev ->
  drm_exec_buffer dev cmd_send;
  List.iter (close_gem_handle dev) to_close

(* Processing data that the host wrote to the shared ring. *)
module Recv = struct

  let make_blob_fd ~sw t ~id ~size =
    let dev = get_dev t in
    Eio_unix.Fd.use_exn "make_blob_fd" dev @@ fun dev ->
    let bo_handle, _ = create_blob dev (`Host3D id) ~mappable:true ~shareable:true ~size in
    let fd = drm_prime_handle_to_fd dev bo_handle in
    close_gem_handle dev bo_handle;
    Eio_unix.Fd.of_unix ~sw ~close_unix:true fd

  let make_write_pipe ~sw t ~id =
    let r, w = Eio_unix.pipe sw in
    Fiber.fork ~sw
      (fun () ->
         try
           let buf = Cstruct.create 4096 in
           let rec loop () =
             let got =
               try Eio.Flow.single_read r buf
               with End_of_file -> 0
             in
             Log.info (fun f -> f "Read %d bytes from local pipe" got);
             let msg = Cross_domain_read_write.create ~id (Cstruct.sub buf 0 got) in
             Eio_unix.Fd.use_exn "make_write_pipe" (get_dev t) (fun dev ->
                 drm_exec_buffer dev msg
               );
             if got = 0 then (
               Eio.Flow.close r
             ) else (
               loop ()
             )
           in
           loop ()
         with ex -> Log.err (fun f -> f "Error copying from host pipe: %a" Fmt.exn ex)
      );
    Eio_unix.Resource.fd w

  let wayland ~sw t data ids =
    let rec to_fds acc = function
      | [] -> List.rev acc
      | (id, ty, size) :: ids ->
        match
          match ty with
          | `Blob -> make_blob_fd ~sw t ~id ~size
          | `Write_pipe -> make_write_pipe ~sw t ~id
        with
        | fd -> to_fds (fd :: acc) ids
        | exception ex ->
          List.iter Eio_unix.Fd.close acc;
          raise ex
    in
    data, to_fds [] ids

  let pipe_host_to_guest t ~id ~hang_up data =
    match Res_handle.Map.find_opt id t.pipe_of_id with
    | None -> Fmt.failwith "Unknown pipe with ID %a" Res_handle.pp id
    | Some pipe ->
      Log.info (fun f -> f "Got %d bytes of pipe data for ID %a" (String.length data) Res_handle.pp id);
      if data <> "" then Eio.Flow.copy_string data pipe;
      if hang_up then (
        t.pipe_of_id <- Res_handle.Map.remove id t.pipe_of_id;
        Eio.Flow.close pipe
      )
end

let handle_event ~sw t buf =
  assert (not (is_closed t));
  let got = Cstruct.length buf in
  if got < 8 then Fmt.failwith "Expected to read an 8-byte drm_event (got %d bytes)" got;
  Types.Event.check buf;
  Wayland_ring.parse t.channel_ring.data
    ~recv:(fun data ids -> `Recv (Recv.wayland ~sw t data ids))
    ~read_pipe:(fun ~id ~hang_up data -> Recv.pipe_host_to_guest t ~id ~hang_up data; `Again)

let close t =
  Lazy.force t.unmap_rings
