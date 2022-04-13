open Wayland_protocols.Xdg_shell_client
open Wayland.Wayland_client

open Lwt.Syntax
open Lwt.Infix

type t = {
  shm : [`V1] Wl_shm.t;
  surface : [`V4] Wl_surface.t;
  gpu : Virtio_gpu.t;
  mutable fg : int32;
  mutable width : int;
  mutable height : int;
  mutable scroll : int;
}

let with_memory_fd gpu ~size f =
  let fd = Virtio_gpu.alloc gpu ~size in
  (* Fmt.pr "Got memory FD: %d@." (Obj.magic fd : int); *)
  Fun.protect
    (fun () -> f fd)
    ~finally:(fun () -> Unix.close fd)

(* Draw the content to [t.surface]. *)
let draw_frame t =
  let stride = t.width * 4 in
  let size = t.height * stride in
  let pool, data = with_memory_fd t.gpu ~size (fun fd ->
      let pool = Wl_shm.create_pool t.shm (new Wl_shm_pool.v1) ~fd ~size:(Int32.of_int size) in
      let ba = Virtio_gpu.Utils.safe_map_file fd ~kind:Bigarray.Int32 ~len:(t.height * t.width) in
      let ba2 = Bigarray.reshape_2 (Bigarray.genarray_of_array1 ba) t.height t.width in
      pool, ba2
    ) in
  let buffer =
    Wl_shm_pool.create_buffer pool
      ~offset:0l
      ~width:(Int32.of_int t.width)
      ~height:(Int32.of_int t.height)
      ~stride:(Int32.of_int stride)
      ~format:Wl_shm.Format.Xrgb8888
    @@ object
      inherit [_] Wl_buffer.v1
      method on_release = Wl_buffer.destroy
    end
  in
  Wl_shm_pool.destroy pool;
  Wl_surface.attach t.surface ~buffer:(Some buffer) ~x:0l ~y:0l;
  let scroll = t.scroll in
  for row = 0 to t.height - 1 do
    for col = 0 to t.width - 1 do
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

let () =
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.(set_level (Some Info));
  Lwt_main.run @@ begin
    let* gpu = Virtio_gpu.find_device () >|= or_die in
    let* transport = Virtio_gpu.connect_wayland gpu in
    let display, conn_closed = Wayland.Client.connect transport in
    Lwt.on_success conn_closed (fun r ->
        Lwt.async (fun () ->
            let+ () = Virtio_gpu.close gpu in
            match r with
            | Ok () -> ()
            | Error ex -> raise ex
          )
      );
    let* r = Wayland.Registry.of_display display in
    let comp = Wayland.Registry.bind r @@ new Wl_compositor.v4 in
    let xdg_wm_base = Wayland.Registry.bind r @@ object
        inherit [_] Xdg_wm_base.v1
        method on_ping = Xdg_wm_base.pong
      end in
    let shm = Wayland.Registry.bind r @@ object
        inherit [_] Wl_shm.v1
        method on_format _ ~format:_ = ()
      end in
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
      end in
    let t = { gpu; shm; surface; scroll = 0; width = 0; height = 0; fg = 0xFFEEEEEEl } in
    let closed, set_closed = Lwt.wait () in
    let configured, set_configured = Lwt.wait () in
    let xdg_surface = Xdg_wm_base.get_xdg_surface xdg_wm_base ~surface @@ object
        inherit [_] Xdg_surface.v1
        method on_configure p ~serial =
          Xdg_surface.ack_configure p ~serial;
          if Lwt.is_sleeping configured then Lwt.wakeup set_configured ()
    end in
    let toplevel = Xdg_surface.get_toplevel xdg_surface @@ object
        inherit [_] Xdg_toplevel.v1
        method on_close _ = Lwt.wakeup set_closed ()
        method on_configure _ ~width ~height ~states:_ =
          t.width <- if width = 0l then 640 else Int32.to_int width;
          t.height <- if height = 0l then 480 else Int32.to_int height
        method on_configure_bounds _ ~width:_ ~height:_ = ()
      end in
    Xdg_toplevel.set_title toplevel ~title:"virtio-gpu-proxy";
    Wl_surface.commit surface;
    let* () = configured in
    let trigger = Lwt_condition.create () in
    let rec animate () =
      let next = Lwt_condition.wait trigger in
      let _frame = Wl_surface.frame surface (Wayland.callback (fun _ -> Lwt_condition.broadcast trigger ())) in
      draw_frame t;
      let* () = next in
      t.scroll <- t.scroll + 1;
      animate ()
    in
    let* () =
      Lwt.pick [
        animate ();
        closed;
      ]
    in
    transport#shutdown
  end
