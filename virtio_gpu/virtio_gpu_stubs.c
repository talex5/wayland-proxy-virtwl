#include <sys/ioctl.h>
#include <string.h>
#include <unistd.h>
#include <sys/mman.h>
#include <errno.h>
#include <fcntl.h>

#define CAML_NAME_SPACE
#define CAML_INTERNALS
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/fail.h>
#include <caml/bigarray.h>
#include <caml/unixsupport.h>
#include <caml/io.h>

#include <xf86drm.h>
#include "virtgpu_drm.h"

#define CAPSET_CROSS_DOMAIN 5

#define Version_val(v) *((drmVersion**)Data_custom_val(v))

static void finalize_version(value v) {
  drmFreeVersion(Version_val(v));
  Version_val(v) = NULL;
}

static struct custom_operations version_ops = {
  "virtio_gpu.version",
  finalize_version,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

CAMLprim value ocaml_drm_get_version(value fd) {
  CAMLparam0();
  CAMLlocal1(v_version);

  v_version = caml_alloc_custom_mem(&version_ops, sizeof(drmVersion*), sizeof(drmVersion));
  Version_val(v_version) = drmGetVersion(Int_val(fd));
  if (!Version_val(v_version))
    caml_failwith("drmGetVersion returned NULL");

  CAMLreturn(v_version);
}

CAMLprim value ocaml_drm_get_caps(value fd, value v_caps) {
  CAMLparam1(v_caps);
  int ret;
  struct drm_virtgpu_get_caps args = {
    .cap_set_id = CAPSET_CROSS_DOMAIN,
    .size = Caml_ba_array_val(v_caps)->dim[0],
    .addr = (unsigned long long) Caml_ba_data_val(v_caps),
  };

  ret = drmIoctl(Int_val(fd), DRM_IOCTL_VIRTGPU_GET_CAPS, &args);
  if (ret) {
    unix_error(errno, "DRM_IOCTL_VIRTGPU_GET_CAPS", Nothing);
  }

  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_drm_version_name(value v_version) {
  CAMLparam1(v_version);
  drmVersion *v = Version_val(v_version);
  CAMLreturn(caml_alloc_initialized_string(v->name_len, v->name));
}

CAMLprim value ocaml_drm_context_init(value v_fd, value v_num_params, value v_params) {
  CAMLparam1(v_params);
  struct drm_virtgpu_context_init init = {0};
  int ret;
  init.ctx_set_params = (unsigned long long) Caml_ba_data_val(v_params);
  init.num_params = Int_val(v_num_params);
  //struct drm_virtgpu_context_set_param *ctx_set_params = (struct drm_virtgpu_context_set_param *) init.ctx_set_params;
  // for (int i = 0; i < init.num_params; i++) {
  //   printf("item %ld, %ld\n", ctx_set_params[i].param, ctx_set_params[i].value);
  // }
  ret = drmIoctl(Int_val(v_fd), DRM_IOCTL_VIRTGPU_CONTEXT_INIT, &init);
  if (ret) {
    unix_error(errno, "drmIoctl", Nothing);
  }
  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_drm_create_blob(value v_fd, value v_params) {
  CAMLparam1(v_params);
  int ret;
  ret = drmIoctl(Int_val(v_fd), DRM_IOCTL_VIRTGPU_RESOURCE_CREATE_BLOB, Caml_ba_data_val(v_params));
  if (ret < 0) {
    unix_error(errno, "DRM_IOCTL_VIRTGPU_RESOURCE_CREATE_BLOB", Nothing);
  }
  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_get_page_size(void) {
  return Val_int(getpagesize());
}

CAMLprim value ocaml_drm_map(value v_fd, value v_handle) {
  CAMLparam1(v_handle);
  int ret;
  struct drm_virtgpu_map map = {
    .handle = Int32_val(v_handle),
  };
  ret = drmIoctl(Int_val(v_fd), DRM_IOCTL_VIRTGPU_MAP, &map);
  if (ret < 0) {
    unix_error(errno, "DRM_IOCTL_VIRTGPU_MAP", Nothing);
  }
  CAMLreturn(caml_copy_int64(map.offset));
}

static void *cstruct_start(value v_cstruct) {
  value v_buffer = Field(v_cstruct, 0);
  value v_offset = Field(v_cstruct, 1);
  return Caml_ba_data_val(v_buffer) + Long_val(v_offset);
}

static long cstruct_len(value v_cstruct) {
  return Long_val(Field(v_cstruct, 2));
}

CAMLprim value ocaml_drm_exec_buffer(value v_fd, value v_cmd, value v_ring_idx, value v_handles) {
  CAMLparam3(v_cmd, v_ring_idx, v_handles);
  int ret;
  struct drm_virtgpu_execbuffer exec = {
    .command = (uint64_t) cstruct_start(v_cmd),
    .size = cstruct_len(v_cmd),
  };
  int n_handles = Wosize_val(v_handles);
  uint32_t handles[n_handles];
  if (Is_some(v_ring_idx)) {
    exec.flags = VIRTGPU_EXECBUF_RING_IDX;
    exec.ring_idx = Int32_val(Some_val(v_ring_idx));
  }
  if (n_handles > 0) {
    exec.bo_handles = (uint64_t) &handles[0];
    exec.num_bo_handles = n_handles;
    for (int i = 0; i < n_handles; i++) {
      handles[i] = Int32_val(Field(v_handles, i));
      // printf("handle[%d] = %d\n", i, handles[i]);
    }
  }
  ret = drmIoctl(Int_val(v_fd), DRM_IOCTL_VIRTGPU_EXECBUFFER, &exec);
  if (ret < 0) {
    unix_error(errno, "DRM_IOCTL_VIRTGPU_EXECBUFFER", Nothing);
  }
  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_drm_prime_handle_to_fd(value v_fd, value v_handle) {
  CAMLparam1(v_handle);
  int ret;
  int out_fd = -1;
  ret = drmPrimeHandleToFD(Int_val(v_fd), Int32_val(v_handle), DRM_CLOEXEC | DRM_RDWR, &out_fd);
  if (ret < 0) {
    unix_error(errno, "drmPrimeHandleToFD", Nothing);
  }
  CAMLreturn(Val_int(out_fd));
}

CAMLprim value ocaml_drm_prime_fd_to_handle(value v_fd, value v_prime_fd) {
  CAMLparam0();
  int ret;
  uint32_t out_handle = -1;
  ret = drmPrimeFDToHandle(Int_val(v_fd), Int_val(v_prime_fd), &out_handle);
  if (ret < 0) {
    unix_error(errno, "drmPrimeFDToHandle", Nothing);
  }
  CAMLreturn(caml_copy_int32(out_handle));

}

CAMLprim value ocaml_drm_resource_info(value v_fd, value v_gem_handle) {
  CAMLparam1(v_gem_handle);
  int ret;
  struct drm_virtgpu_resource_info drm_res_info = {0};
  drm_res_info.bo_handle = Int32_val(v_gem_handle);
  ret = drmIoctl(Int_val(v_fd), DRM_IOCTL_VIRTGPU_RESOURCE_INFO, &drm_res_info);
  if (ret < 0) {
    unix_error(errno, "DRM_IOCTL_VIRTGPU_RESOURCE_INFO", Nothing);
  }
  CAMLreturn(caml_copy_int32(drm_res_info.res_handle));
}

CAMLprim value ocaml_drm_wait(value v_fd, value v_gem_handle) {
  CAMLparam1(v_gem_handle);
  int ret;
  do {
    struct drm_virtgpu_3d_wait wait_3d = {
      .handle = Int32_val(v_gem_handle),
    };
    ret = drmIoctl(Int_val(v_fd), DRM_IOCTL_VIRTGPU_WAIT, &wait_3d);
  } while (ret == -EAGAIN);
  if (ret < 0) {
    unix_error(errno, "DRM_IOCTL_VIRTGPU_WAIT", Nothing);
  }
  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_close_gem_handle(value v_fd, value v_gem_handle) {
  CAMLparam1(v_gem_handle);
  int ret;
  struct drm_gem_close gem_close = {
    .handle = Int32_val(v_gem_handle),
  };
  ret = drmIoctl(Int_val(v_fd), DRM_IOCTL_GEM_CLOSE, &gem_close);
  if (ret < 0) {
    unix_error(errno, "DRM_IOCTL_GEM_CLOSE", Nothing);
  }
  CAMLreturn(Val_unit);
}

/* Memory-mapped files.
 *
 * This is based on [Unix.map_file], but:
 * 1. We don't try to extend the file's size (this fails for device files).
 * 2. We allow it to be manually unmapped.
 */

static void caml_ba_unmap_file2(void * addr, uintnat len)
{
  uintnat page = sysconf(_SC_PAGESIZE);
  uintnat delta = (uintnat) addr % page;
  if (len == 0) return;         /* PR#5463 */
  addr = (void *)((uintnat)addr - delta);
  len  = len + delta;
  munmap(addr, len);
}

static void caml_ba_mapped_finalize2(value v)
{
  struct caml_ba_array * b = Caml_ba_array_val(v);
  if ((b->flags & CAML_BA_MANAGED_MASK) == CAML_BA_MAPPED_FILE) {
    if (b->proxy == NULL) {
      // printf("proxy is NULL: unmap\n");
      caml_ba_unmap_file2(b->data, caml_ba_byte_size(b));
    } else {
      if (-- b->proxy->refcount == 0) {
	// printf("refcount is 0: unmap\n");
	caml_ba_unmap_file2(b->proxy->data, b->proxy->size);
	free(b->proxy);
      } else {
	// printf("non-zero refcount\n");
      }
    }
  } else {
    // printf("Already unmapped manually\n");
  }
}

static struct custom_operations caml_ba_mapped_ops2 = {
  "_bigarray",
  caml_ba_mapped_finalize2,
  caml_ba_compare,
  caml_ba_hash,
  caml_ba_serialize,
  caml_ba_deserialize,
  custom_compare_ext_default,
  custom_fixed_length_default
};

static value caml_unix_mapped_alloc2(int flags, int num_dims, void * data, intnat * dim)
{
  uintnat asize;
  int i;
  value res;
  struct caml_ba_array * b;
  intnat dimcopy[CAML_BA_MAX_NUM_DIMS];

  CAMLassert(num_dims >= 0 && num_dims <= CAML_BA_MAX_NUM_DIMS);
  CAMLassert((flags & CAML_BA_KIND_MASK) <= CAML_BA_CHAR);
  for (i = 0; i < num_dims; i++) dimcopy[i] = dim[i];
  asize = SIZEOF_BA_ARRAY + num_dims * sizeof(intnat);
  res = caml_alloc_custom(&caml_ba_mapped_ops2, asize, 0, 1);
  b = Caml_ba_array_val(res);
  b->data = data;
  b->num_dims = num_dims;
  b->flags = flags | CAML_BA_MAPPED_FILE;
  b->proxy = NULL;
  for (i = 0; i < num_dims; i++) b->dim[i] = dimcopy[i];
  return res;
}

/* Simplified version of the stdlib's map_file that doesn't try to call fstat or ftruncate. */
CAMLprim value ocaml_safe_map_file(value vfd, value vkind, value vstart, value vsize, value vhostsize)
{
  int fd, flags;
  intnat dim[1] = { Long_val(vsize) };
  uintnat array_size;
  void *addr = NULL;
  size_t hostsize = Long_val(vhostsize);

  fd = Int_val(vfd);
  flags = Caml_ba_kind_val(vkind) | CAML_BA_C_LAYOUT;
  if (dim[0] < 0)
    caml_invalid_argument("safe_map_file: negative dimension");
  array_size = dim[0] * caml_ba_element_size[flags & CAML_BA_KIND_MASK];
  if (array_size > hostsize)
    caml_invalid_argument("safe_map_file: size greater than hostsize!");
  if (array_size > 0)
    addr = mmap(NULL, hostsize, PROT_READ | PROT_WRITE, MAP_SHARED, fd, File_offset_val(vstart));
  if (addr == (void *) MAP_FAILED) uerror("map_file", Nothing);
  /* Build and return the OCaml bigarray */
  return caml_unix_mapped_alloc2(flags, 1, addr, dim);
}

CAMLprim value ocaml_ba_unmap(value v)
{
  struct caml_ba_array * b = Caml_ba_array_val(v);
  int i;

  /* Free data if we're the last user, or decr ref count if not */
  caml_ba_mapped_finalize2(v);

  /* Prevent later GC or free from doing anything more.
     Might need a lock here with multicore?
     This is best-efforts anyway, as the compiler is allowed to assume
     bigarrays don't resize when optimising. */
  b->flags = (b->flags & ~CAML_BA_MANAGED_MASK) | CAML_BA_EXTERNAL;
  /* Disallow all access via this bigarray */
  for (i = 0; i < b->num_dims; i++) b->dim[i] = 0;
  /* Tidy up (and let C users know that the data is gone). */
  b->data = NULL;
  b->proxy = NULL;

  return Val_unit;
}
