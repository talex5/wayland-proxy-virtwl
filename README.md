# A virtwl Wayland proxy for VMs

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

It is able to proxy Evince and Firefox at least, and also works with Xwayland well enough to run gvim.

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
ExecStart=/path/to/wayland-proxy-virtwl --tag="[my-vm] " --wayland-display wayland-0 --x-display=0 --xrdb Xft.dpi:150

[Install]
WantedBy=default.target
```

## Xwayland support

If you run with `--x-display=0` then it listens on the abstract socket `@/tmp/.X11-unix/X0` for X11 clients.
If one tries to connect, it spawns an Xwayland process to handle it (and any future X11 clients).

Xwayland is an X server that renders application window contents to Wayland surfaces.
wayland-proxy-virtwl acts as a window manager to integrate these surfaces into the Wayland desktop
(e.g. by reading the `WM_NAME` X11 property and setting it as the `xdg_toplevel`'s title).

This is rather complicated. The following features mostly work:

- Toplevel windows and dialogs are given the appropriate xdg-shell roles.
- The title is set from `WM_NAME`.
- Popup menus and tooltips work.
- The PRIMARY and CLIPBOARD selections are connected to the corresponding Wayland APIs.
- Pointer and keyboard events work.

You can use the `--xrdb 'KEY:VALUE'` option to set default settings in the xrdb database.
For example, `--xrdb Xft.dpi:150` is useful on high-DPI screens.

Limitations:

- It has only been tested with Sway so far, and might need some adjustments for a non-tiling compositor.

- Drag-and-drop will not work, even between X applications, as the (hidden) X11 window layout used by X11 applications does not match the Wayland compositor's layout. Wayland does not provide layout information to us, so there's not much that can be done here.

## Logging

There are several ways to enable logging:

- Running with `--verbose` will turn on all info-level logging.
- Setting e.g. `WAYLAND_DEBUG_PROXY=client,server,xwayland` will turn on info-level debugging for the listed components.
- If `WAYLAND_DEBUG_PROXY` isn't set, `WAYLAND_DEBUG` is used instead. Setting this will also affect child processes.

The available log sources are:

- `client` -- log Wayland messages between the relay and the client applications.
- `server` -- log Wayland messages between the relay and the host compositor.
- `xwayland` -- log X11 messages between the relay and Xwayland.

You can also suppress certain classes of log messages using e.g. `--log-suppress motion,shm,delete,region,drawing,hints`.
The classes are:

- `motion` -- pointer motion/frame events
- `shm` -- managing shared memory pools
- `delete` -- confirmation of object deletion
- `region` -- setting up input regions
- `drawing` -- attaching buffers, marking damage
- `hints` -- window manager hints

See [trace.ml](./trace.ml) for details.

You can configure the proxy to log to an in-memory ring-buffer, and then dump that whenever an error occurs.
Use e.g. `-v --log-ring-path ~/wayland.log` to enable this feature.
When an uncaught exception occurs, the proxy will flush (append) the log to the given path.
By default, it keeps about 512K of history; use `--log-ring-size` to change this.

You can also force it to flush the log by writing a line to the control pipe. e.g.

```
wayland-proxy-virtwl ... -v --log-ring-path ~/wayland.log &
echo dump-log > /run/user/1000/wayland-1-ctl
cat ~/wayland.log
```

This is useful if an application is misbehaving and you want to check its recent interactions.

## Using virtwl directly

`tests/test.ml` is a simple test application that uses the virtwl kernel interface directly
(without the proxy), avoiding any copying.

## TODO

- Find alternative to private `caml_unix_mapped_alloc`.
- Only copy the buffer regions that have changed.
- Fork a subprocess for each connection, like sommelier.

[sommelier]: https://chromium.googlesource.com/chromiumos/platform2/+/main/vm_tools/sommelier/
[ocaml-wayland]: https://github.com/talex5/ocaml-wayland
