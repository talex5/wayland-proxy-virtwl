external check_fd_offset : Unix.file_descr -> (int64[@unboxed]) =
  "wayland_proxy_virtwl_check_fd_offset_byte" "wayland_proxy_virtwl_check_fd_offset_native"
  [@@noalloc]
(** Check that the provided offset is in bounds.  Returns 0 on failure, or the remaining
    bytes in the file otherwise.

    @param fd The file descriptor to chedk.  lseek() will be called on this.
    @param offset The offset in bytes.  It is safe to pass unvalidated and untrusted input
      for this parameter.
    @return Zero if the offset is out of bounds or if lseek() fails.
      Otherwise, returns the number of bytes remaining after the provided offset. *)

external validate_pipe : Unix.file_descr -> int = "wayland_proxy_virtwl_validate_pipe"
(** Check that the provided file descriptor is either a pipe or an
    [AF_UNIX] stream socket.  Returns 0 on success, -1 for non-[AF_UNIX] sockets,
    -2 for [AF_UNIX] sockets that are not of type [SOCK_STREAM], and -3 for
    something that is neither a pipe nor a socket.  Raises [Unix.Unix_error]
    if something went wrong, *)
