# A Wayland proxy

`wayland-proxy-virtwl` accepts Wayland connections from client applications and proxies the messages to a host compositor.
Features:

- It can act as an X11 window manager, allowing Xwayland to work without needing support in the host compositor.

- It can run inside a VM, using the `virtwl` Linux kernel module to proxy to the host's compositor.
  This allows applications running in VMs to be used as if they were running on the host.

- It can also be used to make changes to protocol messages.
  e.g. using `--tag` will add a prefix to window titles.

It is similar to the [sommelier][] proxy from ChromiumOS, but written in OCaml.
It adds support for the primary selection, and aims to be easier to modify and less segfaulty.
It uses the [ocaml-wayland][] library.

It is able to proxy Evince and Firefox at least, and also works with Xwayland well enough to run gvim.

See [Qubes-lite With KVM and Wayland](https://roscidus.com/blog/blog/2021/03/07/qubes-lite-with-kvm-and-wayland/) for a setup using this.

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

## virtwl support

If `$WAYLAND_DISPLAY` is not set and `/dev/wl0` is present, the proxy will use that to connect to the host's compositor.
Since regular guest memory cannot be shared with the host, it allocates a shadow buffer from the host and copies the frame data into that.
Wayland can also use graphics memory directly, which should avoid the copy, but this is not yet supported.

See https://alyssa.is/using-virtio-wl/ for some background.

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

Xwayland doesn't support Wayland's HiDPI feature, which causes the compositor to double everything in size,
which is often blurry and unusable. To fix this, use e.g. `--x-unscale=2` to reverse this transformation
(and then just configure the X11 apps to use a larger font).

You can use the `--xrdb 'KEY:VALUE'` option to set default settings in the xrdb database.
For example, `--xrdb Xft.dpi:150` is useful on high-DPI screens.

Limitations:

- It has only been tested with Sway so far, and might need some adjustments for a non-tiling compositor.

- Drag-and-drop will not work, even between X applications, as the (hidden) X11 window layout used by X11 applications does not match the Wayland compositor's layout. Wayland does not provide layout information to us, so there's not much that can be done here.

For more details, see [Isolating Xwayland in a VM][xwayland-blog].

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

## Hacking

Execution starts in `main.ml`,
which parses the command-line arguments and then starts listening for Wayland and/or X connections.

Wayland connections are handled by `relay.ml`.
`Relay.create` connects to the host compositor and `Relay.accept` accepts the connection from the client
(each connection gets its own relay anyway, but this split simplifies the code a bit).
`make_registry` generates the `Wl_registry` object offered to clients.
When a client binds to one of the offered APIs,
the `on_bind` method binds the corresponding host API and calls an API-specific function to relay messages
between the new client-side object and the new host object.

There is one function for each Wayland interface.
For example, the `Xdg_popup` interface is handled like this:

```ocaml
let make_popup ~host_popup c =
  let h = host_popup @@ object
      inherit [_] H.Xdg_popup.v1
      method on_popup_done _ = C.Xdg_popup.popup_done c
      method on_configure _ = C.Xdg_popup.configure c
      method on_repositioned _ = C.Xdg_popup.repositioned c
    end
  in
  Proxy.Handler.attach c @@ object
    inherit [_] C.Xdg_popup.v1
    method on_destroy = delete_with H.Xdg_popup.destroy h
    method on_grab _ ~seat = H.Xdg_popup.grab h ~seat:(to_host seat)
    method on_reposition _ ~positioner = H.Xdg_popup.reposition h ~positioner:(to_host positioner)
  end
```

The `host_popup` function will create the host object, once you provide a set of event handlers for it.
`c` is the corresponding client-side object.
Whenever you get an event from the host, send the same event to the client.
The client-side APIs are accessible via the `C` module, and the host-side ones via `H`.

Then `Handler.attach c` sets up handlers for requests from the client.
When you get a request, make the corresponding request on the host object.
`on_destroy` can be handled using `delete_with` so that the generated `delete_id` event gets relayed too.

If the trailing arguments are the same, they are often omitted.
For example, `on_configure` above could have been written out in full as:

```ocaml
  let h = host_popup @@ object
      inherit [_] H.Xdg_popup.v1
      [...]
      method on_configure _ ~x ~y ~width ~height =
        C.Xdg_popup.configure c ~x ~y ~width ~height
```

If an argument is another object, you will need to convert it.
For example, when the client makes a `grab` request they pass a client-side `Wl_seat` object.
When calling the host compositor, we must pass the corresponding host-side object.
`to_host` and `to_client` perform these conversions.
Objects that require conversion in this way must provide a `user_data` method to get their peer.

Objects that need to do special things when running under virtwl take an extra `virtwl` option with a connection to `/dev/wl0`.
Objects that interact with Xwayland take an `xwayland` option with hooks for that.

### Updating a protocol

The proxy can only relay messages it knows about.
To add support for a newer version of protocol:

1. Update the XML file in <https://github.com/talex5/ocaml-wayland/tree/master/protocols>.
2. Build the proxy with the new version of ocaml-wayland,
   either by installing it with opam or by putting it inside the proxy's directory
   (note that the proxy might already contain a git-submodule of it; update that if so).
3. The compiler errors will take you to any places that need updating.

### Adding a new protocol

0. Run your test application with `WAYLAND_DEBUG=1` directly on the host to find out what protocol it needs.
1. Add the XML file to ocaml-wayland's `protocols` directory, and extend the `dune` file to build it.
2. Import it into `c.ml`, `h.ml` and `protocols.ml`.
   These just provide aliases so you can e.g. refer to a client `Wl_surface` as `C.Wl_surface`.
3. Extend `relay.ml`'s `registry` function: list the new interface and handle it in `on_bind`.
4. You can mostly just follow the compiler errors to implement it, copying the pattern of the other objects.

If your interface needs to do things with virtwl, it's probably easiest to get it working on the host, without virtwl, first.

## TODO

- Find alternative to private `caml_unix_mapped_alloc`.
- Only copy the buffer regions that have changed.

[sommelier]: https://chromium.googlesource.com/chromiumos/platform2/+/main/vm_tools/sommelier/
[ocaml-wayland]: https://github.com/talex5/ocaml-wayland
[xwayland-blog]: https://roscidus.com/blog/blog/2021/10/30/xwayland/
