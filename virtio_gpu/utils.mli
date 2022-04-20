val safe_map_file :
  ?pos:int64 ->
  Unix.file_descr ->
  kind:('a, 'b) Bigarray.kind ->
  len:int ->
  ('a, 'b, Bigarray.c_layout) Bigarray.Array1.t
(** [safe_map_file] is like {!Unix.map_file}, but it doesn't try to enlarge the file
    (which fails for special files such as shared buffers). *)

val pp_fd : Unix.file_descr Fmt.t

val unmap : _ Bigarray.Genarray.t -> unit
(** [unmap ba] immediately unmaps an array previously mapped with {!safe_map_file},
    without waiting for the garbage collector. You must not access [ba] after this
    (it will probably give an error if you try, but the compiler may optimise this out). *)
