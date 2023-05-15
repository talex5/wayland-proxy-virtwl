open Wayland_protocols.Linux_dmabuf_unstable_v1_client

open Eio.Std

type modifiers = { hi : int32; lo : int32 }
type fmt = Drm_format.t * modifiers

type t = {
  proxy : [`V3] Zwp_linux_dmabuf_v1.t;
  formats : (Drm_format.t, modifiers) Hashtbl.t;
}

let create wayland r =
  let formats = Hashtbl.create 32 in
  match Wayland.Registry.bind r @@ object
      inherit [_] Zwp_linux_dmabuf_v1.v3
      method on_format _ ~format:_ = ()
      method on_modifier _ ~format ~modifier_hi:hi ~modifier_lo:lo =
        let format = Drm_format.of_int32 format in
        Logs.debug (fun f -> f "Format: %a hi:%lx lo:%lx"
                       Drm_format.pp format
                       hi lo);
        Hashtbl.add formats format {hi; lo}
    end
  with
  | proxy ->
    Wayland.Client.sync wayland;
    Some { proxy; formats }
  | exception ex ->
    Log.info (fun f -> f "Can't find dmabuf: %a" Fmt.exn ex);
    None

let get_format t fmt =
  match Hashtbl.find_opt t.formats fmt with
  | None -> None
  | Some mods -> Some (fmt, mods)

let probe_drm t (image : Dev.image) =
  let fmt = Drm_format.xr24 in
  match Hashtbl.find_opt t.formats fmt with
  | None ->
    Log.info (fun f -> f "probe_drm: doesn't support test format %a, so can't probe" Drm_format.pp fmt);
    false
  | Some mods ->
    let result, set_result = Promise.create () in
    let params = Zwp_linux_dmabuf_v1.create_params t.proxy @@ object
        inherit [_] Zwp_linux_buffer_params_v1.v1
        method on_created self buffer =
          Log.info (fun f -> f "probe_drm: succeeded - we can use video memory");
          Wayland.Wayland_client.Wl_buffer.destroy (Wayland.Proxy.cast_version buffer);
          Promise.resolve set_result true;
          Zwp_linux_buffer_params_v1.destroy self
        method on_failed self =
          Log.info (fun f -> f "probe_drm: FAILED - we cannot use video memory");
          Promise.resolve set_result false;
          Zwp_linux_buffer_params_v1.destroy self
      end in
    Zwp_linux_buffer_params_v1.add params
      ~fd:image.fd
      ~offset:image.offset
      ~stride:image.stride
      ~modifier_lo:mods.lo
      ~modifier_hi:mods.hi
      ~plane_idx:0l;
    Zwp_linux_buffer_params_v1.create params
      ~width:1l
      ~height:1l
      ~format:(fmt :> int32)
      ~flags:0l;
    Promise.await result

let create_immed t (fmt, mods) (image : Dev.image) =
  let params = Zwp_linux_dmabuf_v1.create_params t.proxy @@ object
      inherit [_] Zwp_linux_buffer_params_v1.v1
      method on_created _ _buffer = failwith "Shouldn't get called with create_immed!"
      method on_failed _ = failwith "Shouldn't get called with create_immed!"
    end in
  Zwp_linux_buffer_params_v1.add params
    ~fd:image.fd
    ~offset:image.offset
    ~stride:image.stride
    ~modifier_lo:mods.lo
    ~modifier_hi:mods.hi
    ~plane_idx:0l;
  fun buffer_handler ->
    Zwp_linux_buffer_params_v1.create_immed params
      ~width:image.query.width
      ~height:image.query.height
      ~format:(fmt : Drm_format.t :> int32)
      ~flags:0l
      (Wayland.Proxy.Handler.cast_version buffer_handler)
    |> Wayland.Proxy.cast_version
