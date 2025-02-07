open Wayland_protocols.Xdg_shell_client
open Wayland.Wayland_client

open Eio.Std

type image_data = (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array2.t

type t = {
  shm : [`V1] Wl_shm.t;
  linux_dmabuf : (Virtio_gpu.Wayland_dmabuf.t * Virtio_gpu.Wayland_dmabuf.fmt) option;
  surface : [`V4] Wl_surface.t;
  gpu : Virtio_gpu.t;
  fg : int32;
  mutable width : int;
  mutable height : int;
  mutable scroll : int;
  mutable recycled_image : ([`V1] Wl_buffer.t * image_data) option;   (* A free buffer of size [width, height] *)
}

let with_memory_fd (gpu:Virtio_gpu.t) ~width ~height ~(dmabuf:bool) f =
  let query = {
    Virtio_gpu.Dev.
    width = Int32.of_int width;
    height = Int32.of_int height;
    drm_format = Virtio_gpu.Drm_format.xr24;
  } in
  let image = Virtio_gpu.alloc gpu ~gpu:dmabuf query in
  (* Fmt.pr "Got memory FD: %d@." (Obj.magic fd : int); *)
  Fun.protect
    (fun () -> f image)
    ~finally:(fun () -> Unix.close image.fd)

let clear_recycle t =
  match t.recycled_image with
  | None -> ()
  | Some (buffer, _) ->
    Wl_buffer.destroy buffer;
    t.recycled_image <- None

let alloc_shm t ~width ~height =
  let stride = width * 4 in
  let size = height * stride in
  let pool, data = with_memory_fd t.gpu ~width ~height ~dmabuf:false (fun { Virtio_gpu.Dev.fd; host_size; offset; _ } ->
      let pool = Wl_shm.create_pool t.shm (new Wl_shm_pool.v1) ~fd ~size:(Int32.of_int size) in
      let ba = Virtio_gpu.Utils.safe_map_file fd
          ~kind:Bigarray.Int32
          ~len:(width * height)
          ~host_size:(Int64.to_int host_size)
          ~pos:(Int64.of_int32 offset)
      in
      let ba2 = Bigarray.reshape_2 (Bigarray.genarray_of_array1 ba) height width in
      pool, ba2
    ) in
  let buffer =
    Wl_shm_pool.create_buffer pool
      ~offset:0l
      ~width:(Int32.of_int width)
      ~height:(Int32.of_int height)
      ~stride:(Int32.of_int stride)
      ~format:(Wl_shm.Format.to_int32 Wl_shm.Format.Xrgb8888)
    @@ object
      inherit [_] Wl_buffer.v1
      method on_release buffer =
        if t.width = width && t.height = height then (
          clear_recycle t;
          t.recycled_image <- Some (buffer, data)
        ) else Wl_buffer.destroy buffer
    end
  in
  Wl_shm_pool.destroy pool;
  (buffer, data)

let alloc_drm t (dma, fmt) ~width ~height =
  let make_buffer, data = with_memory_fd t.gpu ~width ~height ~dmabuf:true (fun image ->
      let buffer = Virtio_gpu.Wayland_dmabuf.create_immed dma fmt image in
      let stride = Int32.to_int image.stride / 4 in
      let ba = Virtio_gpu.Utils.safe_map_file image.fd
          ~kind:Bigarray.Int32
          ~len:(height * stride)
          ~host_size:(Int64.to_int image.host_size)
          ~pos:(Int64.of_int32 image.offset)
      in
      let ba2 = Bigarray.reshape_2 (Bigarray.genarray_of_array1 ba) height stride in
      buffer, ba2
    ) in
  let buffer = make_buffer @@ object
      inherit [_] Wl_buffer.v1
      method on_release buffer =
        if t.width = width && t.height = height then (
          clear_recycle t;
          t.recycled_image <- Some (buffer, data)
        ) else Wl_buffer.destroy buffer
    end in
  (buffer, data)

(* Draw the content to [t.surface]. *)
let draw_frame t =
  let width = t.width in
  let height = t.height in
  let buffer, data =
    match t.recycled_image with
    | Some saved -> t.recycled_image <- None; saved
    | None ->
      Logs.info (fun f -> f "Allocate a new %dx%d buffer" width height);
      match t.linux_dmabuf with
      | None -> alloc_shm t ~width ~height
      | Some dma -> alloc_drm t dma ~width ~height
  in
  Wl_surface.attach t.surface ~buffer:(Some buffer) ~x:0l ~y:0l;
  let scroll = t.scroll in
  for row = 0 to height - 1 do
    for col = 0 to width - 1 do
      if (col + (row + scroll) land -16) land 31 < 16 then
        data.{row, col} <- 0xFF666666l
      else
        data.{row, col} <- t.fg
    done
  done;
  Wl_surface.damage t.surface ~x:0l ~y:0l ~width:Int32.max_int ~height:Int32.max_int;
  Wl_surface.commit t.surface

let or_die = function
  | Ok x -> x
  | Error (`Msg m) -> output_string stderr (m ^ "\n"); flush stderr; exit 1

let get_dmabuf _gpu = function
  | None -> None
  | Some dma ->
    match Virtio_gpu.Wayland_dmabuf.get_format dma Virtio_gpu.Drm_format.xr24 with
    | None -> None
    | Some fmt -> Some (dma, fmt)

let () =
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.(set_level (Some Info));
  Eio_main.run @@ fun env ->
  Switch.run @@ fun sw ->
  let dri_dir = Virtio_gpu.default_dri_dir env#fs in
  let gpu = Virtio_gpu.find_device ~sw dri_dir |> or_die in
  let transport = Virtio_gpu.wayland_transport gpu in
  let display = Wayland.Client.connect ~sw transport in
  let r = Wayland.Registry.of_display display in
  let comp = Wayland.Registry.bind r @@ new Wl_compositor.v4 in
  let xdg_wm_base = Wayland.Registry.bind r @@ object
      inherit [_] Xdg_wm_base.v1
      method on_ping = Xdg_wm_base.pong
    end in
  let shm = Wayland.Registry.bind r @@ object
      inherit [_] Wl_shm.v1
      method on_format _ ~format:_ = ()
    end in
  let linux_dmabuf = Virtio_gpu.Wayland_dmabuf.create display r in
  let seat = Wayland.Registry.bind r @@ object
      inherit [_] Wl_seat.v1
      method on_capabilities _ ~capabilities:_ = ()
      method on_name _ ~name:_ = ()
    end in
  let _keyboard = Wl_seat.get_keyboard seat @@ object
      inherit [_] Wl_keyboard.v1
      method on_enter _ ~serial:_ ~surface:_ ~keys:_ = ()
      method on_key _ ~serial:_ ~time:_ ~key:_ ~state:_ = ()
      method on_keymap _ ~format:_ ~fd ~size:_ = Unix.close fd
      method on_leave _ ~serial:_ ~surface:_ = ()
      method on_modifiers _ ~serial:_ ~mods_depressed:_ ~mods_latched:_ ~mods_locked:_ ~group:_ = ()
      method on_repeat_info _ ~rate:_ ~delay:_ = ()
    end in
  let surface = Wl_compositor.create_surface comp @@ object
      inherit [_] Wl_surface.v1
      method on_enter _ ~output:_ = ()
      method on_leave _ ~output:_ = ()
      method on_preferred_buffer_scale _ ~factor:_ = ()
      method on_preferred_buffer_transform _ ~transform:_ = ()
    end in
  let linux_dmabuf = get_dmabuf gpu linux_dmabuf in
  let t = { gpu; shm; linux_dmabuf; surface; scroll = 0; width = 0; height = 0; fg = 0xFFEEEEEEl; recycled_image = None } in
  let closed, set_closed = Promise.create () in
  let configured, set_configured = Promise.create () in
  let xdg_surface = Xdg_wm_base.get_xdg_surface xdg_wm_base ~surface @@ object
      inherit [_] Xdg_surface.v1
      method on_configure p ~serial =
        Xdg_surface.ack_configure p ~serial;
        if not (Promise.is_resolved configured) then Promise.resolve set_configured ()
    end in
  let toplevel = Xdg_surface.get_toplevel xdg_surface @@ object
      inherit [_] Xdg_toplevel.v1
      method on_close _ = Promise.resolve set_closed ()
      method on_configure _ ~width ~height ~states:_ =
        let width = if width = 0l then 640 else Int32.to_int width in
        let height = if height = 0l then 480 else Int32.to_int height in
        if width <> t.width || height <> t.height then (
          clear_recycle t;
          t.width <- width;
          t.height <- height;
        )
      method on_configure_bounds _ ~width:_ ~height:_ = ()
      method on_wm_capabilities _ ~capabilities:_ = ()
    end in
  Xdg_toplevel.set_title toplevel ~title:"virtio-gpu-proxy";
  Wl_surface.commit surface;
  Promise.await configured;
  Fiber.fork_daemon ~sw (fun () ->
      let trigger = Eio.Condition.create () in
      Eio.Condition.loop_no_mutex trigger (fun () ->
          let _frame = Wl_surface.frame surface (Wayland.callback (fun _ -> Eio.Condition.broadcast trigger)) in
          draw_frame t;
          t.scroll <- t.scroll + 1;
          None
        )
    );
  Promise.await closed;
  clear_recycle t;
  Wayland.Client.stop display
