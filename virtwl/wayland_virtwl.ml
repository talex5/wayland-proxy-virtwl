type t = Unix.file_descr

external virtwl_new_context : Unix.file_descr -> Unix.file_descr = "ocaml_virtwl_new_context"
external virtwl_send : Unix.file_descr -> Lwt_bytes.t -> int -> int -> Unix.file_descr list -> unit = "ocaml_virtwl_send"
external virtwl_recv : Unix.file_descr -> Lwt_bytes.t -> int -> int -> int * Unix.file_descr list = "ocaml_virtwl_recv"
external virtwl_alloc : Unix.file_descr -> int -> Unix.file_descr = "ocaml_virtwl_alloc"
external virtwl_pipe_read : Unix.file_descr -> Unix.file_descr = "ocaml_virtwl_pipe_read"
external virtwl_pipe_write : Unix.file_descr -> Unix.file_descr = "ocaml_virtwl_pipe_write"
external virtwl_map_file : Unix.file_descr -> ('a, 'b) Stdlib.Bigarray.kind -> int -> ('a, 'b, 'c) Stdlib.Bigarray.Genarray.t
  = "ocaml_virtwl_map_file"

let of_context_fd fd : #Wayland.S.transport =
  let fd = Lwt_unix.of_unix_file_descr ~blocking:false fd in
  object
    val mutable up = true

    method send data fds =
      let { Cstruct.buffer; off; len } = data in
      virtwl_send (Lwt_unix.unix_file_descr fd) buffer off len fds;
      Lwt.return_unit

    method recv { Cstruct.buffer; off; len } =
      Lwt_unix.(wrap_syscall Read) fd (fun () ->
          assert (len > 0);             (* Or Linux panics *)
          let (got, fds) = virtwl_recv (Lwt_unix.unix_file_descr fd) buffer off len in
          if got = 0 then up <- false;
          (got, fds)
        )

    (* The ioctl interface doesn't seem to have shutdown, so try close instead: *)
    method shutdown =
      up <- false;
      Lwt_unix.close fd

    method up = up

    method close =
      up <- false;
      Lwt_unix.close fd

  method pp f = Fmt.string f "virtwl"
  end

let of_fd fd = fd

let new_context t =
  of_context_fd (virtwl_new_context t)

let map_file fd kind ~n_elements = virtwl_map_file fd kind n_elements

let alloc t ~size =
  virtwl_alloc t size

let pipe_read t =
  virtwl_pipe_read t

let pipe_write t =
  virtwl_pipe_write t

let with_memory_fd t ~size f =
  let fd = alloc t ~size in
  Fun.protect
    (fun () -> f fd)
    ~finally:(fun () -> Unix.close fd)
