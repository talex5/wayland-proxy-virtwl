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
