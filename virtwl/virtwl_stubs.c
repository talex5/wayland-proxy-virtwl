#include <sys/ioctl.h>
#include <string.h>
#include <unistd.h>
#include <sys/mman.h>

#define CAML_NAME_SPACE
#define CAML_INTERNALS
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/fail.h>
#include <caml/bigarray.h>
#include <caml/unixsupport.h>

#include "virtwl.h"

extern value caml_unix_mapped_alloc(int, int, void *, intnat *);	/* XXX: Is this private? */

CAMLprim value ocaml_virtwl_new_context(value control_fd) {
  CAMLparam1 (control_fd);
  struct virtwl_ioctl_new data = {
    .type = VIRTWL_IOCTL_NEW_CTX,
    .fd = -1,
    .flags = 0,
  };
  if (ioctl(Int_val(control_fd), VIRTWL_IOCTL_NEW, &data)) {
    uerror("VIRTWL_IOCTL_NEW", Nothing);
  };
  CAMLreturn (Val_int(data.fd));
}

CAMLprim value ocaml_virtwl_send(value val_fd, value val_buf, value val_off, value val_len, value val_fds) {
  CAMLparam5 (val_fd, val_buf, val_off, val_len, val_fds);
  CAMLlocal1 (val_next_fd);
  val_next_fd = val_fds;
  long off = Long_val(val_off);
  long len = Long_val(val_len);
  char* src = (char*)Caml_ba_data_val(val_buf) + Long_val(val_off);
  struct virtwl_ioctl_txn *tx;
  tx = alloca((sizeof(struct virtwl_ioctl_txn)) + len);
  for (int i = 0; i < VIRTWL_SEND_MAX_ALLOCS; i++) {
      if (val_next_fd == Val_int(0)) {
        /* val_next_fd = [] */
        tx->fds[i] = -1;
      } else {
        tx->fds[i] = Int_val(Field(val_next_fd, 0));
        val_next_fd = Field(val_next_fd, 1);
      }
  }
  tx->len = len;
  memcpy(&tx->data, src, len);
  if (ioctl(Int_val(val_fd), VIRTWL_IOCTL_SEND, tx)) {
    uerror("VIRTWL_IOCTL_SEND failed", Nothing);
  };
  CAMLreturn (Val_unit);
}

CAMLprim value ocaml_virtwl_recv(value val_fd, value val_buf, value val_off, value val_len) {
  CAMLparam4 (val_fd, val_buf, val_off, val_len);
  CAMLlocal2 (res, tmp);
  long off = Long_val(val_off);
  long len = Long_val(val_len);
  struct virtwl_ioctl_txn *tx;
  tx = alloca(sizeof(struct virtwl_ioctl_txn) + len);
  for (int i = 0; i < VIRTWL_SEND_MAX_ALLOCS; i++) {
      tx->fds[i] = -1;
  }
  tx->len = len;
  if (ioctl(Int_val(val_fd), VIRTWL_IOCTL_RECV, tx)) {
    uerror("VIRTWL_IOCTL_RECV failed", Nothing);
  };
  memcpy(Caml_ba_data_val(val_buf) + Long_val(off), &tx->data, tx->len);
  res = Val_int(0);
  for (int i = 0; i < VIRTWL_SEND_MAX_ALLOCS; i++) {
    if (tx->fds[i] == -1)
      break;
    tmp = res;
    res = caml_alloc_tuple(2);
    Store_field(res, 0, Val_int(tx->fds[i]));
    Store_field(res, 1, tmp);
  }
  tmp = res;
  res = caml_alloc_tuple(2);
  Store_field(res, 0, Val_long(tx->len));
  Store_field(res, 1, tmp);
  CAMLreturn (res);
}

CAMLprim value ocaml_virtwl_alloc(value val_control, value val_size) {
  CAMLparam2 (val_control, val_size);
  int control_fd = Int_val(val_control);
  __u32 size = Long_val(val_size);
  struct virtwl_ioctl_new data = { 0 };
  data.type = VIRTWL_IOCTL_NEW_ALLOC;
  data.fd = -1;
  data.flags = 0;
  data.size = size;
  if (ioctl(control_fd, VIRTWL_IOCTL_NEW, &data)) {
    uerror("VIRTWL_IOCTL_NEW_ALLOC", Nothing);
  };
  CAMLreturn (Val_int(data.fd));
}

CAMLprim value ocaml_virtwl_pipe_read(value val_control) {
  CAMLparam1 (val_control);
  int control_fd = Int_val(val_control);
  struct virtwl_ioctl_new data = { 0 };
  data.type = VIRTWL_IOCTL_NEW_PIPE_READ;
  data.fd = -1;
  data.flags = 0;
  if (ioctl(control_fd, VIRTWL_IOCTL_NEW, &data)) {
    uerror("VIRTWL_IOCTL_NEW_PIPE_READ", Nothing);
  };
  CAMLreturn (Val_int(data.fd));
}

CAMLprim value ocaml_virtwl_pipe_write(value val_control) {
  CAMLparam1 (val_control);
  int control_fd = Int_val(val_control);
  struct virtwl_ioctl_new data = { 0 };
  data.type = VIRTWL_IOCTL_NEW_PIPE_WRITE;
  data.fd = -1;
  data.flags = 0;
  if (ioctl(control_fd, VIRTWL_IOCTL_NEW, &data)) {
    uerror("VIRTWL_IOCTL_NEW_PIPE_WRITE", Nothing);
  };
  CAMLreturn (Val_int(data.fd));
}

/* Simplified version of the stdlib's map_file that doesn't try to call fstat or ftruncate. */
CAMLprim value ocaml_virtwl_map_file(value vfd, value vkind, value vsize)
{
  int fd, flags;
  intnat dim[1] = { Long_val(vsize) };
  uintnat array_size;
  void *addr = NULL;

  fd = Int_val(vfd);
  flags = Caml_ba_kind_val(vkind) | CAML_BA_C_LAYOUT;
  if (dim[0] < 0)
    caml_invalid_argument("virtwl_map_file: negative dimension");
  array_size = dim[0] * caml_ba_element_size[flags & CAML_BA_KIND_MASK];
  if (array_size > 0)
    addr = mmap(NULL, array_size, PROT_READ | PROT_WRITE, 1, fd, 0);
  if (addr == (void *) MAP_FAILED) uerror("map_file", Nothing);
  addr = (void *) ((uintnat) addr);
  /* Build and return the OCaml bigarray */
  return caml_unix_mapped_alloc(flags, 1, addr, dim);
}
