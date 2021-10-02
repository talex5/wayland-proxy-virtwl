type t = {
  x : int;
  y : int;
  width : int;
  height : int;
}

let pp f { x; y; width; height } =
  Fmt.pf f "%dx%d+%d+%d" width height x y
