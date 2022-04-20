external ocaml_safe_map_file :
  Unix.file_descr ->
  ('a, 'b) Bigarray.kind ->
  int64 ->
  int ->
  ('a, 'b, Bigarray.c_layout) Bigarray.Array1.t
  = "ocaml_safe_map_file"

external unmap : _ Bigarray.Genarray.t -> unit = "ocaml_ba_unmap"

let safe_map_file ?(pos=0L) fd ~kind ~len = ocaml_safe_map_file fd kind pos len

let pp_fd f (x : Unix.file_descr) = Fmt.int f (Obj.magic x : int)
