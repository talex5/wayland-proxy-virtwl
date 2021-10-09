open Lwt.Syntax

let read_exactly n x =
  let resp = Cstruct.create n in
  let+ () = Lwt_io.read_into_exactly_bigstring x resp.buffer resp.off resp.len in
  resp

let round_up4 x = (x + 3) land -4

(* Cstruct lets us specify signed values, but then treats them as unsigned anyway. Fix that. *)
let signed16 x =
  if x >= 0x8000 then x - 0x10000
  else x
