type t = private int32

val of_str : string -> t
(** Parse a 4-character string as a format. *)

val r8 : t
val bgr888 : t
val xr24 : t

val of_int32 : int32 -> t
val pp : t Fmt.t
