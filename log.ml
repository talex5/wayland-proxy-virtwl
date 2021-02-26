let src = Logs.Src.create "virtwl-proxy" ~doc:"Wayland VM proxy"
include (val Logs.src_log src : Logs.LOG)
