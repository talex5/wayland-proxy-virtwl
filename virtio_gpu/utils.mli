val safe_map_file :
  ?pos:int64 ->
  Unix.file_descr ->
  kind:('a, 'b) Bigarray.kind ->
  len:int ->
  ('a, 'b, Bigarray.c_layout) Bigarray.Array1.t
(** [safe_map_file] is like {!Unix.map_file}, but it doesn't try to enlarge the file
    (which fails for special files such as shared buffers). *)

val pp_fd : Unix.file_descr Fmt.t
