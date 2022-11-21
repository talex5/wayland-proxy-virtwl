When running in a VM, the proxy needs to be able to connect to the Wayland compositor running on the host and share chunks of memory with it.
Originally, this was done using "virtwl", a Linux kernel module that implemented a custom protocol to crosvm on the host.
This module never got upstreamed. Instead, Linux 5.16 added "context types" to virtio-gpu, allowing custom protocols to be implemented without any extra kernel modules.
crosvm added support for this in version 94.14150 (it continues to support virtwl too for now).

Most of this new system seems to be undocumented.
This file documents my attempt to understand it.

This is based on reading the source code to:
- [Sommelier](https://chromium.googlesource.com/chromiumos/platform2/+/refs/heads/main/vm_tools/sommelier/virtualization/virtgpu_channel.cc)
- [Linux](https://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git/tree/drivers/gpu/drm/virtio?h=v5.16&id=df0cc57e057f18e44dac8e6c18aba47ab53202f9)
- [crosvm](https://github.com/google/crosvm/tree/main/rutabaga_gfx/src/cross_domain)

For the proxy's code, see [dev.mli](./virtio_gpu/dev.mli) and [dev.ml](./virtio_gpu/dev.ml).

### Setup

Open device files in `/dev/dri/*` and call `drmGetVersion` on each one until you find one with the name "virtio_gpu".
The `card*` files give full access to the device, whereas the `render*` ones provide a safe subset for unprivileged applications.
However, on my system my default user account only had access to `card0`.

Optional: check whether Wayland contexts are supported.

Do `DRM_IOCTL_VIRTGPU_CONTEXT_INIT` to create a cross-domain context.
Sommelier sets `VIRTGPU_CONTEXT_PARAM_NUM_RINGS = 2`
and `VIRTGPU_CONTEXT_PARAM_POLL_RINGS_MASK = 1 << CROSS_DOMAIN_CHANNEL_RING`.

Use `DRM_IOCTL_VIRTGPU_RESOURCE_CREATE_BLOB` to allocate one page of memory shared with the host
(`VIRTGPU_BLOB_MEM_GUEST` with `VIRTGPU_BLOB_FLAG_USE_MAPPABLE`).
This gets you a gem handle in a field called `bo_handle` (I don't know what `bo` stands for).

The [GEM documentation](https://lwn.net/Articles/283798/) (an out-of-date mailing list post),
explains that gem handles are "for all intents and purposes, equivalent to file descriptors".
X uses `select`, which is limited to 1024 FDs, and rather than fix this they created gem handles,
which don't seem to be subject to any limits.
There are ioctls corresponding to the usual FD operations (`pread`, `pwrite`, `mmap`, `close`).
But unlike FDs, you can't share them over Unix domain sockets,
so there are also calls to convert between gem handles and FDs when necessary.

The `mmap` system described in the documentation seems to be obsolete.
Instead, use `DRM_IOCTL_GEM_MMAP` to get a "fake offset" and then perform an `mmap` on the device file,
quoting that offset to map the gem handle instead.

Finally, send a `CROSS_DOMAIN_CMD_INIT` command to the host to tell it about the memory page.
Everything refers to this page as a "ring", but it doesn't seem to be used as a ring.
The host just writes stuff to the start of it when it wants to tell the guest something.

### Sending data

Submit a `CrossDomainSendReceive` message containing the Wayland message.
For each FD attached to the Wayland message, you need to convert it to a crosvm resource ID.
Use `drmPrimeFDToHandle` to turn the FD into a gem handle, then `DRM_IOCTL_VIRTGPU_RESOURCE_INFO`
to get the resource ID.

### Receiving data

Submit a `CROSS_DOMAIN_CMD_POLL` using `CROSS_DOMAIN_CHANNEL_RING`.
Then read from the device FD, which will wait until data is available.
On the host, crosvm will write any data received from Wayland to the start of the shared page
and then send an event.
You read from the device FD, getting an 8-byte message containing no information,
then read the message in the shared page.

For Wayland messages, the shared page is a `CrossDomainSendReceive` structure
containing the binary data and a table of identifiers (corresponding to attached FDs).

For blob IDs, use `DRM_IOCTL_VIRTGPU_RESOURCE_CREATE_BLOB` to convert the crosvm resource ID to a gem handle,
then `drmPrimeHandleToFD` to turn that into a regular FD for Wayland.

When done, submit another `CROSS_DOMAIN_CMD_POLL` to request then next item
(which will overwrite the existing data).

### Creating a new shared buffer

Submit a `CROSS_DOMAIN_CMD_GET_IMAGE_REQUIREMENTS` using `CROSS_DOMAIN_QUERY_RING`.
The request gives the width, height, format and flags for the new buffer.
crosvm replies by writing a response to the start of the shared page.

Use `DRM_IOCTL_VIRTGPU_RESOURCE_CREATE_BLOB` with the `blob_id` from the query
to create a new blob. Oddly, the blob_id is not the ID of the blob.
It seems more like a template that can be used to create blobs, which are returned as gem handles.

Note that these template resources are never freed (until the device itself is closed),
and crosvm creates a fresh one for every query.
To avoid crosvm constantly using more memory,
it's best to cache the results and reuse for images with the same size.

### Pipes

The protocol allows a sender to send a writable pipe, which is sufficient for Wayland data transfers (selection, clipboard, etc).

To receive a pipe from the host:

1. The host will send a Wayland message with the pipe's resource ID.
2. Create a Unix pipe and give the write end to the application.
3. Run a loop reading data from the read end and performing a cross-domain write for each chunk of data.

To send a pipe to the host:

1. Send a Wayland message, quoting the new pipe's resource ID (see **Problems** below).
2. When data is available, you will read an event from the device FD. The shared page contains the data.

### Problems

The system appears to have a number of flaws:

- `CROSS_DOMAIN_CMD_GET_IMAGE_REQUIREMENTS` writes to the shared ring immediately,
  and thus races with any Wayland data being written to it.
  To avoid this, I open the device file twice, using one for images and the other for Wayland protocol data.

- When sending a pipe, the guest has to know what ID the host will assign to it.
  The guest guesses, by adding 2 to the last ID it knows about, and the host checks its guess.
  If it was wrong (e.g. because the host created a new ID at the same time) then you get a protocol error.

- crosvm's `CrossDomainWorker` handles all pending events on each poll, so if
  Wayland data and pipe data are available at the same time, one event gets overwritten.
  Reported at: https://issuetracker.google.com/issues/259268477
