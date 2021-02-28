open Lwt.Syntax

let rec write_all fd buf off len =
  if len = 0 then Lwt.return_unit
  else (
    let* sent = Lwt_unix.write fd buf off len in
    assert (sent > 0);
    write_all fd buf (off + sent) (len - sent)
  )

let copy_stream ~src ~dst =
  let b = Bytes.create 4096 in
  let rec aux () =
    let* got = Lwt_unix.read src b 0 (Bytes.length b) in
    if got = 0 then Lwt.return_unit
    else (
      let* () = write_all dst b 0 got in
      aux ()
    )
  in
  aux ()

(** [with_wrapped_writeable virtwl client_fd f] creates a new pipe from the host,
    calls [f pipe_fd], and spawns a background thread that copies from the host
    to [client_fd]. [pipe_fd] can be passed to virtwl. Takes ownership of [client_fd]. *)
let with_wrapped_writeable ~virtwl fd f =
  let host_fd = Wayland_virtwl.pipe_read virtwl in
  Lwt.dont_wait (fun () ->
      Lwt.finalize
        (fun () ->
           f host_fd;
           let host_fd = Lwt_unix.of_unix_file_descr host_fd in
           let client_fd = Lwt_unix.of_unix_file_descr fd in
           copy_stream ~src:host_fd ~dst:client_fd)
        (fun () ->
           Unix.close host_fd;
           Unix.close fd;
           Lwt.return_unit
        )
    )
    (fun ex -> Log.warn (fun f -> f "Error copying data stream: %a" Fmt.exn ex))
