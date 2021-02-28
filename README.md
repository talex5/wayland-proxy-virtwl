# A virtwl Wayland proxy for VMs

Status: **prototyping**

`wayland-proxy-virtwl` runs inside a VM, offering a Wayland socket to applications.
It proxies all requests to the host Wayland compositor using the virtwl Linux module.
See https://alyssa.is/using-virtio-wl/ for some background.

Since regular guest memory cannot be shared with the host, it allocates a shadow buffer from the host and copies the frame data into that.
Wayland can also use graphics memory directly, which should avoid the copy.

`tests/test.ml` is a simple test application that uses the virtwl kernel interface directly (without the proxy), avoiding any copying.

It can also be used to make changes to the protocol messages.
e.g. using `--tag` will add a prefix to window titles.

It is similar to the [sommelier][] proxy from ChromiumOS, but written in OCaml.
It uses the [ocaml-wayland][] library.

## TODO

So far, it has only been tested with `evince` (GNOME's PDF viewer) and provides only the exact protocol versions that it requires. Also:

- Find alternative to private `caml_unix_mapped_alloc`.

[sommelier]: https://chromium.googlesource.com/chromiumos/platform2/+/main/vm_tools/sommelier/
[ocaml-wayland]: https://github.com/talex5/ocaml-wayland
