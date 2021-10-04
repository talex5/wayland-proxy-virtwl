let src = Logs.Src.create "wl-proxy" ~doc:"Wayland proxy"
include (val Logs.src_log src : Logs.LOG)
