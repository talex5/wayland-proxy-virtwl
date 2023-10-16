module Read = Eio.Buf_read

let read_exactly n x =
  Read.ensure x n;
  let resp = Cstruct.sub_copy (Read.peek x) 0 n in
  Read.consume x n;
  resp

let round_up4 x = (x + 3) land -4

(* Cstruct lets us specify signed values, but then treats them as unsigned anyway. Fix that. *)
let signed16 x =
  if x >= 0x8000 then x - 0x10000
  else x
