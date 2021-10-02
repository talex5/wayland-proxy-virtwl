type 'a t = int32

let pp f xid =
  Fmt.pf f "0x%lx" xid

let of_int x = x
