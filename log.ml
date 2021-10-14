let src = Logs.Src.create "wl-proxy" ~doc:"Wayland proxy"
include (val Logs.src_log src : Logs.LOG)

let xwayland_src = Logs.Src.create "xwayland" ~doc:"Xwayland support"
module Xwayland = (val Logs.src_log xwayland_src : Logs.LOG)
