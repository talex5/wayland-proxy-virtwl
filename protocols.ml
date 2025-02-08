(** Metadata for the protocols we use. *)

include Wayland.Wayland_proto
include Wayland_protocols.Xdg_shell_proto
include Wayland_protocols.Xdg_output_unstable_v1_proto
include Wayland_protocols.Gtk_primary_selection_proto
include Wayland_protocols.Wp_primary_selection_unstable_v1_proto
include Wayland_protocols.Server_decoration_proto
include Wayland_protocols.Xdg_decoration_unstable_v1_proto
include Wayland_protocols.Relative_pointer_unstable_v1_proto
include Wayland_protocols.Pointer_constraints_unstable_v1_proto
include Wayland_protocols.Viewporter_proto
