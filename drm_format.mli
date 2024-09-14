(** Validation of DRM formats.

    The canonical database of format specifiers lives in the Linux kernel,
    and all of the associated code is written in C.  Therefore, this code is
    also written in C.  The OCaml code is just a wrapper. *)

external validate_shm : untrusted_offset:(int32[@unboxed])
                      -> untrusted_width:(int32[@unboxed])
                      -> untrusted_height:(int32[@unboxed])
                      -> untrusted_stride:(int32[@unboxed])
                      -> untrusted_format:(int32[@unboxed])
                      -> bool
  = "validate_shm_byte" "validate_shm_native" [@@noalloc]
(** Validate the that the provided arguments (from [Wl_shm_pool.create_buffer])
    are valid.  This does not attempt to memory-map the buffer.
    This function can also be used to validate linear modifiers.

    It is safe to pass untrusted input to this function.
    The parameters are labeled [untrusted_*] to indicate this.

    @return [true] on success or [false] on failure.

    @param untrusted_offset The offset, see [Wl_shm_pool.create_buffer].
    @param untrusted_width The width, see [Wl_shm_pool.create_buffer].
    @param untrusted_height The height, see [Wl_shm_pool.create_buffer].
    @param untrusted_stride The stride, see [Wl_shm_pool.create_buffer].
    @param untrusted_format The pixel format, see [Wl_shm_pool.create_buffer].
 *)

