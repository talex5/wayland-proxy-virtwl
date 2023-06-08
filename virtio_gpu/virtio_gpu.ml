module Drm_format = Drm_format
module Dev = Dev
module Wayland_dmabuf = Wayland_dmabuf
module Utils = Utils

type transport = < Wayland.S.transport; close : unit >

type t = {
  device_path : Eio.Fs.dir_ty Eio.Path.t;
  alloc : [`Alloc] Dev.t;
  mutable have_dmabuf : bool option;    (* None if we haven't checked yet *)
}

let wayland_transport dev conn : #Wayland.S.transport =
  object
    val mutable up = true
    val mutable pending = Cstruct.empty

    method send data fds =
      Eio_unix.Fd.use_exn_list "send" fds @@ fun fds ->
      if up then Dev.send dev data fds

    method recv ~sw result_buf =
      (* Read into [pending] if it's empty *)
      let fds =
        if Cstruct.is_empty pending then (
          let buf = Cstruct.create 8 in
          let rec loop () =
            if not up then []
            else (
              Dev.poll dev;
              let got = Eio.Flow.single_read conn buf in
              if got = 0 then []
              else (
                match Dev.handle_event ~sw dev (Cstruct.sub buf 0 got) with
                | `Again -> loop ()
                | `Recv (data, fds) ->
                  pending <- data;
                  fds
              )
            )
          in
          loop ()
        ) else []
      in
      if Dev.is_closed dev then (0, fds)
      else (
        (* Return as much of [pending] as we can *)
        let len = min (Cstruct.length result_buf) (Cstruct.length pending) in
        Cstruct.blit pending 0 result_buf 0 len;
        pending <- Cstruct.shift pending len;
        (len, fds)
      )

    (* The ioctl interface doesn't seem to have shutdown, so try close instead: *)
    method shutdown =
      up <- false;
      Dev.close dev

    method up = up

    method close =
      if up then (
        Dev.close dev;
        up <- false
      )

    method pp f = Fmt.string f "virtio-gpu"
  end

(* Just until NixOS has OCaml 4.13 *)
let starts_with ~prefix x =
  String.length x >= String.length prefix &&
  String.sub x 0 (String.length prefix) = prefix

let is_device_name x =
  starts_with x ~prefix:"card" ||
  starts_with x ~prefix:"render"

let ( / ) = Eio.Path.( / )

let default_dri_dir fs = (fs / "/dev/dri")

let find_device_gen ~dri_dir init =
  match Eio.Path.read_dir dri_dir with
  | [] -> Fmt.error_msg "Device directory %a is empty!" Eio.Path.pp dri_dir
  | exception (Eio.Io _ as ex) -> Fmt.error_msg "%a" Eio.Exn.pp ex
  | items ->
    match List.filter is_device_name items with
    | [] -> Fmt.error_msg "No card* or render* devices found (got %a)" Fmt.Dump.(list string) items
    | items ->
      items
      |> List.find_map (fun name -> init (dri_dir / name))
      |> function
      | None -> Fmt.error_msg "No virtio-gpu device found (checked %a)" Fmt.Dump.(list string) items
      | Some x -> Ok x

let find_device ~sw dri_dir =
  let init device_path =
    let device_path = (device_path :> Eio.Fs.dir_ty Eio.Path.t) in
    let conn = Eio.Path.open_out ~sw ~create:`Never device_path in
    match Dev.of_fd ~sw (Eio_unix.Resource.fd_opt conn |> Option.get) with
    | None -> Eio.Flow.close conn; None
    | Some alloc -> Some { device_path; alloc; have_dmabuf = None }
  in
  find_device_gen ~dri_dir init

let close t = Dev.close t.alloc

let alloc t = Dev.alloc t.alloc

let connect_wayland ~sw t =
  let dev = Eio.Path.open_out ~sw t.device_path ~create:`Never in
  match Dev.of_fd ~sw (Eio_unix.Resource.fd_opt dev |> Option.get) with
  | Some wayland -> wayland_transport wayland dev
  | None ->
    Eio.Flow.close dev;
    Fmt.failwith "%a is no longer a virtio-gpu device!" Eio.Path.pp t.device_path

let with_memory_fd gpu ~width ~height f =
  let query = {
    Dev.
    width;
    height;
    drm_format = Drm_format.xr24;
  } in
  let image = alloc gpu query in
  (* Fmt.pr "Got memory FD: %d@." (Obj.magic fd : int); *)
  Fun.protect
    (fun () -> f image)
    ~finally:(fun () -> Unix.close image.fd)

let probe_drm t drm =
  match t.have_dmabuf with
  | Some x -> x
  | None ->
    let x =
      with_memory_fd t ~width:1l ~height:1l (fun image ->
          Wayland_dmabuf.probe_drm drm image
        )
    in
    t.have_dmabuf <- Some x;
    x
