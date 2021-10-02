let src = Logs.Src.create "x11" ~doc:"x11 protocol"
include (val Logs.src_log src : Logs.LOG)
