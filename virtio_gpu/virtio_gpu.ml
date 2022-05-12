open Lwt.Syntax
open Lwt.Infix

module Drm_format = Drm_format
module Dev = Dev
module Wayland_dmabuf = Wayland_dmabuf
module Utils = Utils

type transport = < Wayland.S.transport; close : unit Lwt.t >

type t = {
  device_path : string;
  alloc : [`Alloc] Dev.t;
  mutable have_dmabuf : bool option;    (* None if we haven't checked yet *)
}

let wayland_transport dev fd : #Wayland.S.transport =
  object
    val mutable up = true
    val mutable pending = Cstruct.empty

    method send data fds =
      if up then Dev.send dev data fds;
      Lwt.return_unit

    method recv result_buf =
      (* Read into [pending] if it's empty *)
      let* fds =
        if Cstruct.is_empty pending then (
          let buf = Bytes.create 8 in
          let rec loop () =
            if not up then Lwt.return []
            else (
              Dev.poll dev;
              let* got = Lwt_unix.read fd buf 0 (Bytes.length buf) in
              if got = 0 then Lwt.return []
              else (
                Dev.handle_event dev (Bytes.sub buf 0 got) >>= function
                | `Again -> loop ()
                | `Recv (data, fds) ->
                  pending <- data;
                  Lwt.return fds
              )
            )
          in
          loop ()
        ) else Lwt.return []
      in
      if Dev.is_closed dev then Lwt.return (0, fds)
      else (
        (* Return as much of [pending] as we can *)
        let len = min (Cstruct.length result_buf) (Cstruct.length pending) in
        Cstruct.blit pending 0 result_buf 0 len;
        pending <- Cstruct.shift pending len;
        Lwt.return (len, fds)
      )

    (* The ioctl interface doesn't seem to have shutdown, so try close instead: *)
    method shutdown =
      up <- false;
      Dev.close dev

    method up = up

    method close =
      if up then (
        let+ () = Dev.close dev in
        up <- false
      ) else (
        Lwt.return_unit
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

let rec find_map_s f = function
  | [] -> Lwt.return_none
  | x :: xs ->
    f x >>= function
    | Some _ as y -> Lwt.return y
    | None -> find_map_s f xs

let find_device_gen ?(dri_dir="/dev/dri") init =
  match Sys.readdir dri_dir with
  | [| |] -> Lwt.return @@ Fmt.error_msg "Device directory %S is empty!" dri_dir
  | exception Sys_error x -> Lwt.return @@ Error (`Msg x)
  | items ->
    Array.sort String.compare items;
    let items = Array.to_list items in
    match List.filter is_device_name items with
    | [] -> Lwt.return @@ Fmt.error_msg "No card* or render* devices found (got %a)" Fmt.Dump.(list string) items
    | items ->
      items
      |> find_map_s (fun name -> init (Filename.concat dri_dir name))
      >>= function
      | None -> Lwt.return @@ Fmt.error_msg "No virtio-gpu device found (checked %a)" Fmt.Dump.(list string) items
      | Some x -> Lwt.return_ok x

let find_device ?dri_dir () =
  let init device_path =
    let* fd = Lwt_unix.(openfile device_path [O_RDWR; O_CLOEXEC] 0) in
    match Dev.of_fd fd with
    | None -> let+ () = Lwt_unix.close fd in None
    | Some alloc -> Lwt.return_some { device_path; alloc; have_dmabuf = None }
  in
  find_device_gen ?dri_dir init

let close t = Dev.close t.alloc

let alloc t = Dev.alloc t.alloc

let connect_wayland t =
  let* fd = Lwt_unix.(openfile t.device_path [O_RDWR; O_CLOEXEC] 0) in
  match Dev.of_fd fd with
  | Some wayland -> Lwt.return (wayland_transport wayland fd)
  | None ->
    let+ () = Lwt_unix.close fd in
    Fmt.failwith "%S is no longer a virtio-gpu device!" t.device_path

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
  | Some x -> Lwt.return x
  | None ->
    let+ x =
      with_memory_fd t ~width:1l ~height:1l (fun image ->
          Wayland_dmabuf.probe_drm drm image
        )
    in
    t.have_dmabuf <- Some x;
    x
