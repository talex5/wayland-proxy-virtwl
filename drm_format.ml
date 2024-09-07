
external validate_shm : buffer_size:(nativeint[@unboxed])
                     -> untrusted_offset:(int32[@unboxed])
                     -> untrusted_width:(int32[@unboxed])
                     -> untrusted_height:(int32[@unboxed])
                     -> untrusted_stride:(int32[@unboxed])
                     -> untrusted_format:(int32[@unboxed])
                     -> bool
  = "validate_shm_byte" "validate_shm_native" [@@noalloc]
