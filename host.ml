type t = {
  virtio_gpu : Virtio_gpu.t option;
  closed : unit Eio.Promise.or_exn;
  display : Wayland.Client.t;
  registry : Wayland.Registry.t;
  mutable last_serial : int32;
}

let connect ?virtio_gpu ~sw transport =
  let display, closed = Wayland.Client.connect ~sw ~trace:(module Trace.Host) transport in
  let registry = Wayland.Registry.of_display display in
  let _dma =
    match virtio_gpu with
    | None -> None
    | Some virtio_gpu ->
      let dma = Virtio_gpu.Wayland_dmabuf.create display registry in
      match dma with
      | None ->
        Log.info (fun f -> f "Host does not support dmabuf");
        None
      | Some dma ->
        let supported = Virtio_gpu.probe_drm virtio_gpu dma in
        if supported then (
          Log.warn (fun f -> f "Host is using dmabuf - this probably won't work yet");
          Some dma
        ) else None
  in
  {
    virtio_gpu;
    closed;
    display;
    registry;
    last_serial = 0l;
  }

let last_serial t = t.last_serial

let dump f t =
  Wayland.Client.dump f t.display
