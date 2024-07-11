(** Modules we use to interact with clients (to which we are a server). *)

include Wayland.Wayland_server
include Wayland_protocols.Xdg_shell_server
include Wayland_protocols.Xdg_output_unstable_v1_server
include Wayland_protocols.Gtk_primary_selection_server
include Wayland_protocols.Wp_primary_selection_unstable_v1_server
include Wayland_protocols.Server_decoration_server
include Wayland_protocols.Xdg_decoration_unstable_v1_server
include Wayland_protocols.Relative_pointer_unstable_v1_server
include Wayland_protocols.Pointer_constraints_unstable_v1_server
include Wayland_protocols.Linux_dmabuf_v1_server
