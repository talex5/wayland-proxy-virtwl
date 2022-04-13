let src = Logs.Src.create "virtio-gpu" ~doc:"virtio-gpu device"
include (val Logs.src_log src : Logs.LOG)
