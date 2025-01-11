external check_fd_offset : Unix.file_descr -> (int64[@unboxed]) =
  "wayland_proxy_virtwl_check_fd_offset_byte" "wayland_proxy_virtwl_check_fd_offset_native"
  [@@noalloc]
(** Check that the provided offset is in bounds.  Returns 0 on failure, or the remaining
    bytes in the file otherwise.

    @param fd The file descriptor to check.  lseek() will be called on this.
    @return -1 if lseek() fails or if the file is bigger than INT64_MAX.
    Otherwise, returns the offset of the end of the file. *)

external validate_pipe : Unix.file_descr -> int = "wayland_proxy_virtwl_validate_pipe"
type pipe_info = Success
               | Non_AF_UNIX_Socket
               | Non_stream_AF_UNIX_Socket
               | Neither_pipe_nor_socket
let validate_pipe x =
  match validate_pipe x with
  | 0 -> Success
  | 1 -> Non_AF_UNIX_Socket
  | 2 -> Non_stream_AF_UNIX_Socket
  | 3 -> Neither_pipe_nor_socket
  | _ -> assert false
(** Check that the provided file descriptor is either a pipe or an
    [AF_UNIX] stream socket.  Returns 0 on success, -1 for non-[AF_UNIX] sockets,
    -2 for [AF_UNIX] sockets that are not of type [SOCK_STREAM], and -3 for
    something that is neither a pipe nor a socket.  Raises [Unix.Unix_error]
    if something went wrong, *)

external validate_shm_format : untrusted_format:(int32[@unboxed])
                             -> bool =
  "wayland_proxy_virtwl_validate_shm_format_byte" "wayland_proxy_virtwl_validate_shm_format_native"
  [@@noalloc]
(** Validate that the given shm format can be validly used and its uses properly validated.
    Returns [true] on success and [false] on failure. *)

external validate_format : untrusted_format:(int32[@unboxed])
                         -> untrusted_width:(int32[@unboxed])
                         -> untrusted_height:(int32[@unboxed])
                         -> bool =
  "wayland_proxy_virtwl_validate_format_byte" "wayland_proxy_virtwl_validate_format_native"
  [@@noalloc]
(** Validate that the given format is valid for the provided width and height.
    Returns [true] on success and [false] on failure. *)
