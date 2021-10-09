type t = {
  tag : string;
  xwayland_binary : string;
  xrdb : string list;           (* Config lines for xrdb *)
}

open Cmdliner

let tag =
  Arg.value @@
  Arg.(opt string) "" @@
  Arg.info
    ~doc:"Tag to prefix to window titles"
    ["tag"]

let xwayland_binary =
  Arg.value @@
  Arg.(opt string) "Xwayland" @@
  Arg.info
    ~doc:"Xwayland binary to execute if an X11 application tries to connect"
    ["xwayland-binary"]

let xrdb =
  Arg.value @@
  Arg.(opt_all string) [] @@
  Arg.info
    ~doc:"Initial xrdb config (e.g. 'Xft.dpi:150')"
    ["xrdb"]

let make_config tag xwayland_binary xrdb =
  { tag; xwayland_binary; xrdb }

let cmdliner =
  Term.(const make_config $ tag $ xwayland_binary $ xrdb)
