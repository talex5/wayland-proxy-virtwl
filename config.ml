type t = {
  tag : string;
  xwayland_binary : string;
  xrdb : string list;           (* Config lines for xrdb *)
  xunscale : int;
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

let xunscale =
  Arg.value @@
  Arg.(opt int) 1 @@
  Arg.info
    ~doc:"Compensate for Wayland's attempts to scale X11 apps (e.g. 2 for a HiDPI display)"
    ["x-unscale"]

let make_config tag xwayland_binary xrdb xunscale =
  { tag; xwayland_binary; xrdb; xunscale }

let cmdliner =
  Term.(const make_config $ tag $ xwayland_binary $ xrdb $ xunscale)

(* Just until NixOS has OCaml 4.13 *)
let starts_with ~prefix x =
  String.length x >= String.length prefix &&
  String.sub x 0 (String.length prefix) = prefix
