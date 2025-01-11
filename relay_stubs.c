#include <stdint.h>
#include <stdbool.h>
#include <errno.h>

#include <sys/stat.h>
#include <sys/socket.h>
#include <unistd.h>
#include "drm_fourcc.h"
#include <drm/drm_fourcc.h>

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/unixsupport.h>

/**
 * Get the number of bytes remaining in a file descriptor after the provided offset.
 *
 * @param fd The file descriptor cast to a long.
 * @param untrusted_offset The offset claimed by the client.
 * @return The number of bytes after untrusted_offset, or -1 on any error.
 *         Wayland buffers cannot have zero pixels, so 0 bytes remaining
 *         is itself an error.
 */
static int64_t
get_fd_size(long fd)
{
   if (fd < 0 || fd > INT_MAX) {
      caml_fatal_error("Junk file descriptor passed to validate_fd");
   }

   off_t untrusted_raw_size = lseek((int)fd, 0, SEEK_END);
   if (untrusted_raw_size < 1 || untrusted_raw_size > INT64_MAX) {
      /* lseek() failed, size too large for int64_t, or size zero */
      return -1;
   }

   return (int64_t)untrusted_raw_size;
}

CAMLprim int64_t
wayland_proxy_virtwl_check_fd_offset_native(value fd)
{
   return get_fd_size(Long_val(fd));
}

CAMLprim value
wayland_proxy_virtwl_check_fd_offset_byte(value fd)
{
   return caml_copy_int64(get_fd_size(Long_val(fd)));
}

static const struct drm_format_info *
get_format(uint32_t untrusted_format)
{
   switch (untrusted_format) {
   case DRM_FORMAT_ARGB8888:
   case DRM_FORMAT_XRGB8888:
      return NULL;
   case 0:
      untrusted_format = DRM_FORMAT_ARGB8888;
      break;
   case 1:
      untrusted_format = DRM_FORMAT_XRGB8888;
      break;
   default:
      break;
   }
   const struct drm_format_info *const info = drm_format_info(untrusted_format);
   if (info == NULL) {
      /* Unknown format, cannot validate. */
      return NULL;
   }

   if (info->hsub < 1 || info->vsub < 1 || info->num_planes > 4) {
      caml_fatal_error("Corrupt description for format");
   }

   if (info->is_color_indexed) {
      return NULL; /* Color index cannot be used. */
   }

   return info;
}

static bool
validate_format_static(uint32_t const untrusted_format)
{
   const struct drm_format_info *const info = get_format(untrusted_format);
   if (info == NULL) {
      return false;
   }

   /* Check that the block width is valid. */
   const unsigned int block_width = drm_format_info_block_width(info, 0);
   if (block_width < 1) {
      /* Invalid block width. */
      return false;
   }

   /* Check that the block height is valid. */
   const unsigned int block_height = drm_format_info_block_height(info, 0);
   if (block_height != 1) {
      /* Multiple pixels per vertical block.  It is unclear whether the
       * stride should be multiplied by the block height or not. */
      return false;
   }
   return true;
}

CAMLprim value
wayland_proxy_virtwl_validate_shm_format_native(int32_t untrusted_format)
{
   return Val_bool(validate_format_static(untrusted_format));
}

CAMLprim value
wayland_proxy_virtwl_validate_shm_format_byte(value v)
{
   return wayland_proxy_virtwl_validate_shm_format_native(Int32_val(v));
}

static const struct drm_format_info *
validate_format(uint32_t const untrusted_format,
                int32_t const untrusted_width,
                int32_t const untrusted_height)
{
   if (untrusted_width < 1 || untrusted_height < 1) {
      return NULL; /* No pixels. */
   }

   if (untrusted_width > 16384 || untrusted_height > 6144) {
      return NULL; /* Too wide, too tall, or both. */
   }

   const struct drm_format_info *const info = get_format(untrusted_format);
   if (info == NULL) {
      return NULL;
   }

   if (untrusted_width % info->hsub) {
      return NULL; /* Width not multiple of subsampled block size, violating Vulkan valid usage. */
   }

   if (untrusted_height % info->vsub) {
      return NULL; /* Height not multiple of subsampled block size, violating Vulkan valid usage. */
   }

   /* Check that the block width is valid. */
   const unsigned int block_width = drm_format_info_block_width(info, 0);
   if (block_width < 1) {
      /* Invalid block width. */
      return NULL;
   }

   if ((untrusted_width % block_width) != 0) {
      /* Image does not have a whole number of horizontal blocks. */
      return NULL;
   }

   /* Check that the block height is valid. */
   const unsigned int block_height = drm_format_info_block_height(info, 0);
   if (true) {
      if (block_height != 1) {
         /* Multiple pixels per vertical block.  It is unclear whether the
          * stride should be multiplied by the block height or not. */
         return NULL;
      }
   } else {
      if (block_height < 1) {
         /* Invalid block height */
         return NULL;
      }

      if (untrusted_height % block_height) {
         /* Image does not have a whole number of vertical blocks. */
         return NULL;
      }
   }

   return info;
}

CAMLprim value
wayland_proxy_virtwl_validate_pipe(value arg)
{
   long v = Long_val(arg);
   if (v < 0 || v > INT_MAX)
      caml_fatal_error("invalid file descriptor %ld", v);
   struct stat stat_buf;
   int res = fstat((int)v, &stat_buf);
   if (res == -1) {
      if (errno == 0)
         caml_fatal_error("fstat() failed without setting errno");
      caml_unix_error(errno, "validate pipe: fstat()", Nothing);
   }
   if (res != 0)
      caml_fatal_error("fstat() returned %d, not 0 or -1 (kernel bug?)", res);
   switch (stat_buf.st_mode & S_IFMT) {
   case S_IFIFO:
      return Val_int(0);
   case S_IFSOCK: {
      int domain;
      socklen_t len = sizeof(domain);
      res = getsockopt(v, SOL_SOCKET, SO_DOMAIN, &domain, &len);
      if (res == -1)
         caml_unix_error(errno, "validate pipe: getsockopt(SOL_SOCKET, SO_DOMAIN)", Nothing);
      if (len != sizeof(domain))
         caml_fatal_error("bad socklen_t (%ju) set by getsockopt()", (uintmax_t)len);
      if (domain != AF_UNIX)
         return Val_int(1);
      res = getsockopt(v, SOL_SOCKET, SO_TYPE, &domain, &len);
      if (res == -1)
         caml_unix_error(errno, "validate pipe: getsockopt(SOL_SOCKET, SO_TYPE)", Nothing);
      if (len != sizeof(domain))
         caml_fatal_error("bad socklen_t (%ju) set by getsockopt()", (uintmax_t)len);
      if (domain != SOCK_STREAM)
         return Val_int(2);
      return Val_int(0);
   }
   default:
      return Val_int(3);
   }
}

static bool
validate_shm(int32_t untrusted_offset,
             int32_t const untrusted_width, int32_t const untrusted_height,
             int32_t const untrusted_stride,
             uint32_t untrusted_format)
{
   /* Offset can't be negative */
   if (untrusted_offset < 0) {
      return false;
   }

   /* This checks that untrusted_width and untrusted_height are both at least 1
    * and that untrusted_width * untrusted_height * 8 does not overflow.
    * If these checks fail, info will be NULL, which the functions below catch. */
   const struct drm_format_info *info = validate_format(untrusted_format, untrusted_width, untrusted_height);
   if (info == NULL) {
      return false;
   }

   /* Stride must not be less than width.  Since width must be in [1, 16384],
    * stride must be greater than 0. */
   if (untrusted_stride < untrusted_width) {
      return false;
   }

   /* Limit supported strides to 16384 * 8 until there is a need for more. */
   if (untrusted_stride > 16384L * 8) {
      return false;
   }

   /* Multi-plane formats are badly specified, so block them.
    * TODO: support them properly. */
   if (info->num_planes != 1) {
      return false;
   }

   if (info->char_per_block[0] < 1) {
      /* The format requires a non-linear modifier, and shm buffers always use
       * linear modifiers. This is an error. */
      return false;
   }

   if ((untrusted_stride % info->char_per_block[0]) != 0) {
      /* Row does not include a whole number of blocks.  This is an error. */
      return false;
   }

   if ((untrusted_offset % info->char_per_block[0]) != 0) {
      /* Offset is not a whole number of blocks.  This is an error. */
      return false;
   }

   uint64_t min_pitch = drm_format_info_min_pitch(info, 0, (uint32_t)untrusted_width);

   if (min_pitch < 1) {
      /* This means a bug in the DRM format handling code. */
      caml_fatal_error("BUG: invalid pitch returned by drm_format_info_min_pitch");
   }

   /* Check that the stride is sufficient. */
   return (uint32_t)untrusted_stride >= min_pitch;
}

CAMLprim value
validate_shm_native(int32_t offset, int32_t width, int32_t height, int32_t stride, int32_t format)
{
   return Val_bool(validate_shm(offset, width, height, stride, (uint32_t)format));
}

CAMLprim value
validate_shm_byte(value offset, value width, value height, value stride, value format)
{
   return Val_bool(validate_shm(Int32_val(offset), Int32_val(width), Int32_val(height),
                                Int32_val(stride), (uint32_t)Int32_val(format)));
}

CAMLprim value
wayland_proxy_virtwl_validate_format_native(int32_t untrusted_format, int32_t untrusted_width, int32_t untrusted_height)
{
   return Val_bool(validate_format((uint32_t)untrusted_format, untrusted_width, untrusted_height) != NULL);
}

CAMLprim value
wayland_proxy_virtwl_validate_format_byte(value untrusted_format, value untrusted_width, value untrusted_height)
{
   return Val_bool(validate_format((uint32_t)Int32_val(untrusted_format),
                                   Int32_val(untrusted_width),
                                   Int32_val(untrusted_height)) != NULL);
}

/*
 * Local Variables:
 * c-basic-offset: 3
 * indent-tabs-mode: nil
 * c-file-style: "linux"
 * End:
 */
