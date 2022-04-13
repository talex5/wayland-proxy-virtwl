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

extern value caml_unix_mapped_alloc(int, int, void *, intnat *);	/* XXX: Is this private? */

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
  CAMLparam0();
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

/* Simplified version of the stdlib's map_file that doesn't try to call fstat or ftruncate. */
CAMLprim value ocaml_safe_map_file(value vfd, value vkind, value vstart, value vsize)
{
  int fd, flags;
  intnat dim[1] = { Long_val(vsize) };
  uintnat array_size;
  void *addr = NULL;

  fd = Int_val(vfd);
  flags = Caml_ba_kind_val(vkind) | CAML_BA_C_LAYOUT;
  if (dim[0] < 0)
    caml_invalid_argument("safe_map_file: negative dimension");
  array_size = dim[0] * caml_ba_element_size[flags & CAML_BA_KIND_MASK];
  if (array_size > 0)
    addr = mmap(NULL, array_size, PROT_READ | PROT_WRITE, MAP_SHARED, fd, File_offset_val(vstart));
  if (addr == (void *) MAP_FAILED) uerror("map_file", Nothing);
  /* Build and return the OCaml bigarray */
  return caml_unix_mapped_alloc(flags, 1, addr, dim);
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
  CAMLparam2(v_cmd, v_handles);
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
  CAMLparam0();
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
  CAMLparam0();
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
  CAMLparam0();
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
  CAMLparam0();
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
