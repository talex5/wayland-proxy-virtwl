# A virtwl Wayland proxy for VMs

Status: **prototyping**

`wayland-proxy-virtwl` runs inside a VM, offering a Wayland socket to applications.
It proxies all requests to the host Wayland compositor using the virtwl Linux module.
See https://alyssa.is/using-virtio-wl/ for some background.

Since regular guest memory cannot be shared with the host, it allocates a shadow buffer from the host and copies the frame data into that.
Wayland can also use graphics memory directly, which should avoid the copy, but this is not yet supported.

It can also be used to make changes to the protocol messages.
e.g. using `--tag` will add a prefix to window titles.

It is similar to the [sommelier][] proxy from ChromiumOS, but written in OCaml.
It adds support for the primary selection, and aims to be easier to modify and less segfaulty.
It uses the [ocaml-wayland][] library.

It is able to proxy Evince and Firefox at least.

## Installation

Clone the repository (with submodules):

```
git clone --recursive https://github.com/talex5/wayland-virtwl-proxy.git
```

Then build with either opam (`opam install .`) or Nix (`nix-build .`).

I use the following systemd file to run the proxy
(in `~/.local/share/systemd/user/wayland-virtlw-proxy.service`):

```
[Unit]
Description=Wayland-virtwl-proxy

[Service]
ExecStart=/path/to/wayland-proxy-virtwl --tag="[my-vm] " --wayland-display wayland-0

[Install]
WantedBy=default.target
```

## Using virtwl directly

`tests/test.ml` is a simple test application that uses the virtwl kernel interface directly
(without the proxy), avoiding any copying.

## TODO

- Find alternative to private `caml_unix_mapped_alloc`.
- Only copy the buffer regions that have changed.
- Fork a subprocess for each connection, like sommelier.

[sommelier]: https://chromium.googlesource.com/chromiumos/platform2/+/main/vm_tools/sommelier/
[ocaml-wayland]: https://github.com/talex5/ocaml-wayland
