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
