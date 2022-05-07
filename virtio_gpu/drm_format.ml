type t = int32

let of_str x =
  assert (String.length x = 4);
  let (+) = Int32.logor in
  let char i shift = Int32.shift_left (Int32.of_int (Char.code x.[i])) shift in
  (char 0 0) +
  (char 1 8) +
  (char 2 16) +
  (char 3 24)

let r8 = of_str "R8  "
let bgr888 = of_str "BG24"
let xr24 = of_str "XR24"
let of_int32 x = x

let pp f x =
  let char i =
    Char.chr (Int32.to_int (Int32.logand (Int32.shift_right x (i * 8)) 0xffl))
  in
  Fmt.pf f "%c%c%c%c (%lx)" (char 0) (char 1) (char 2) (char 3) x
