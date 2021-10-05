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

Clone the repository:

```
git clone https://github.com/talex5/wayland-proxy-virtwl.git
```

Then build with either opam (`opam install .`) or Nix (`nix-build .`).

I use the following systemd file to run the proxy
(in `~/.local/share/systemd/user/wayland-proxy-virtwl.service`):

```
[Unit]
Description=Wayland-proxy-virtwl

[Service]
ExecStart=/path/to/wayland-proxy-virtwl --tag="[my-vm] " --wayland-display wayland-0

[Install]
WantedBy=default.target
```

## Logging

There are several ways to enable logging:

- Running with `--verbose` will turn on all info-level logging.
- Setting e.g. `WAYLAND_DEBUG_PROXY=client,server` will turn on info-level debugging for the listed components.
- If `WAYLAND_DEBUG_PROXY` isn't set, `WAYLAND_DEBUG` is used instead. Setting this will also affect child processes.

The available log sources are:

- `client` -- log Wayland messages between the relay and the client applications.
- `server` -- log Wayland messages between the relay and the host compositor.

You can also suppress certain classes of log messages using e.g. `--log-suppress motion,shm,delete,region,drawing,hints`.
The classes are:

- `motion` -- pointer motion/frame events
- `shm` -- managing shared memory pools
- `delete` -- confirmation of object deletion
- `region` -- setting up input regions
- `drawing` -- attaching buffers, marking damage
- `hints` -- window manager hints

See [trace.ml](./trace.ml) for details.

## Using virtwl directly

`tests/test.ml` is a simple test application that uses the virtwl kernel interface directly
(without the proxy), avoiding any copying.

## TODO

- Find alternative to private `caml_unix_mapped_alloc`.
- Only copy the buffer regions that have changed.
- Fork a subprocess for each connection, like sommelier.

[sommelier]: https://chromium.googlesource.com/chromiumos/platform2/+/main/vm_tools/sommelier/
[ocaml-wayland]: https://github.com/talex5/ocaml-wayland
