(* A connection to the host compositor, to which we act as a client. *)

type t = {
  display : Wayland.Client.t;
  registry : Wayland.Registry.t;
  virtio_gpu : Virtio_gpu.t option;     (* [None] if this isn't a virtio-gpu connection. *)
  mutable last_serial : int32;          (* Last Wayland event serial number received. *)
}

(* Spawn a fiber talking the Wayland protocol over [transport].
   [sw] will fail if a protocol error occurs. *)
let connect ?virtio_gpu ~sw transport =
  let display = Wayland.Client.connect ~sw ~trace:(module Trace.Host) transport in
  let registry = Wayland.Registry.of_display display in
  let _dma =
    virtio_gpu |> Option.map @@ fun virtio_gpu ->
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
    display;
    registry;
    last_serial = 0l;
  }

let last_serial t = t.last_serial

let dump f t =
  Wayland.Client.dump f t.display
