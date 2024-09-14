
external validate_shm : untrusted_offset : (int32[@unboxed])
                      -> untrusted_width  : (int32[@unboxed])
                      -> untrusted_height : (int32[@unboxed])
                      -> untrusted_stride : (int32[@unboxed])
                      -> untrusted_format : (int32[@unboxed])
                      -> bool
  = "validate_shm_byte" "validate_shm_native" [@@noalloc]
(** Validate offset, width, height, stride, and format for a linear modifier.

    Does not work for non-linear modifiers. *)
