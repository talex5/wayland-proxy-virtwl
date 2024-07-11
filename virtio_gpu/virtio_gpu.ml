module Drm_format = Drm_format
module Dev = Dev
module Wayland_dmabuf = Wayland_dmabuf
module Utils = Utils

type transport = < Wayland.S.transport; close : unit >

type t = {
  dev : Dev.t;
  transport : transport;
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
              let got =
                try Eio.Flow.single_read conn buf
                with End_of_file -> 0
              in
              if got = 0 || not up then []
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
    | Some dev ->
      let transport = wayland_transport dev conn in
      Some { dev; transport }
  in
  find_device_gen ~dri_dir init

let close t = Dev.close t.dev

let alloc t = Dev.alloc t.dev

let wayland_transport t = t.transport

let device_string t = Dev.get_dev_string t.dev
