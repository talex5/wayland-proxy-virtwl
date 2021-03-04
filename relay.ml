(* Relay Wayland messages between a client and a virtwl host compositor.
   When sending a file descriptor, we create a virtwl descriptor of the appropriate type and send that instead.
   For streams, we copy the data.
   For buffers we copy the contents when the surface is committed (todo: copy just the damaged region).
   We generally ignore the version part of the ocaml-wayland types and just cast as necessary.
   Since we're relaying, we know that both sides are using the same version, so if we get e.g. a
   version 5 request from the client then we know it's safe to send it to the host. *)

open Lwt.Syntax
open Wayland

(* Since we're just relaying messages, we mostly don't care about checking version compatibility.
   e.g. if a client sends us a v5 message, then we can assume the corresponding server object
   supports v5 too (otherwise the client shouldn't have sent it).
   So we just cast away version contraints using [cv]. *)
let cv = Proxy.cast_version

(* Modules we use to interact with the host (to which we are a client). *)
module H = struct
  include Wayland.Wayland_client
  include Wayland_protocols.Xdg_shell_client
  include Wayland_protocols.Xdg_output_unstable_v1_client
  include Wayland_protocols.Gtk_primary_selection_client
  include Wayland_protocols.Server_decoration_client
end

(* Modules we use to interact with clients (to which we are a server). *)
module C = struct
  include Wayland.Wayland_server
  include Wayland_protocols.Xdg_shell_server
  include Wayland_protocols.Xdg_output_unstable_v1_server
  include Wayland_protocols.Gtk_primary_selection_server
  include Wayland_protocols.Server_decoration_server
end

(* Metadata for the protocols we use. *)
module Protocols = struct
  include Wayland_proto
  include Wayland_protocols.Xdg_shell_proto
  include Wayland_protocols.Xdg_output_unstable_v1_proto
  include Wayland_protocols.Gtk_primary_selection_proto
  include Wayland_protocols.Server_decoration_proto
end

type t = {
  config : Config.t;
  virtwl : Wayland_virtwl.t;
  host_registry : Wayland.Registry.t;
}

(* Data attached to host objects (e.g. the corresponding client object).
   Host and client versions are assumed to match. *)
module HD = struct
  type 'a t = 
    | Surface        : 'v C.Wl_surface.t                   -> [`Wl_surface]                  t
    | Data_offer     : 'v C.Wl_data_offer.t                -> [`Wl_data_offer]               t
    | Gtk_data_offer : 'v C.Gtk_primary_selection_offer.t  -> [`Gtk_primary_selection_offer] t
    | Output         : 'v C.Wl_output.t                    -> [`Wl_output]                   t
end

(* Data attached to client objects (e.g. the corresponding host object).
   Host and client versions are assumed to match. *)
module CD = struct
  type 'v buffer = {
    host_buffer : 'v H.Wl_buffer.t;
    host_memory : Cstruct.t;
    client_memory : Cstruct.t;
  }

  type 'v surface = {
    host_surface : 'v H.Wl_surface.t;
    mutable host_memory : Cstruct.t;
    mutable client_memory : Cstruct.t;
  }

  type 'a t = 
    | Region           : 'v H.Wl_region.t                    -> [`Wl_region]                    t
    | Surface          : 'v surface                          -> [`Wl_surface]                   t
    | Buffer           : 'v buffer                           -> [`Wl_buffer]                    t
    | Seat             : 'v H.Wl_seat.t                      -> [`Wl_seat]                      t
    | Output           : 'v H.Wl_output.t                    -> [`Wl_output]                    t
    | Toplevel         : 'v H.Xdg_toplevel.t                 -> [`Xdg_toplevel]                 t
    | Xdg_surface      : 'v H.Xdg_surface.t                  -> [`Xdg_surface]                  t
    | Xdg_positioner   : 'v H.Xdg_positioner.t               -> [`Xdg_positioner]               t
    | Data_source      : 'v H.Wl_data_source.t               -> [`Wl_data_source]               t
    | Gtk_source       : 'v H.Gtk_primary_selection_source.t -> [`Gtk_primary_selection_source] t
end

(* Note: the role here is our role: [`Server] data is attached to proxies to
 our clients (where we are the server), while [`Client] data is attached to host objects. *)
type ('a, 'role) user_data = 
  | Client_data      : 'a CD.t -> ('a, [`Server]) user_data
  | Host_data        : 'a HD.t -> ('a, [`Client]) user_data

type ('a, 'role) Wayland.S.user_data += Relay of ('a, 'role) user_data

let host_data x = Relay (Host_data x)
let client_data x = Relay (Client_data x)

let user_data (proxy : ('a, _, 'role) Proxy.t) : ('a, 'role) user_data =
  match Wayland.Proxy.user_data proxy with
  | Relay x -> x
  | S.No_data -> Fmt.failwith "No data attached to %a!" Proxy.pp proxy
  | _ -> Fmt.failwith "Unexpected data attached to %a!" Proxy.pp proxy

let to_client (type a) (h : (a, 'v, [`Client]) Proxy.t) : (a, 'v, [`Server]) Proxy.t =
  let cv = Proxy.cast_version in
  let Host_data data = user_data h in
  let open HD in
  match data with
  | Output c -> cv c
  | Surface c -> cv c
  | Data_offer c -> cv c
  | Gtk_data_offer c -> cv c

let to_host (type a) (c : (a, 'v, [`Server]) Proxy.t) : (a, 'v, [`Client]) Proxy.t =
  let cv = Proxy.cast_version in
  let Client_data data = user_data c in
  match data with
  | Surface x -> cv x.host_surface
  | Seat x -> cv x
  | Output x -> cv x
  | Region x -> cv x
  | Toplevel x -> cv x
  | Xdg_surface x -> cv x
  | Xdg_positioner x -> cv x
  | Data_source x -> cv x
  | Gtk_source x -> cv x
  | Buffer x -> cv x.host_buffer

(* When the client asks to destroy something, delay the ack until the host object is destroyed.
   This means the client sees events in the usual order, and means we can continue forwarding
   any events the host sends before hearing about the deletion. *)
let delete_with fn host client =
  Proxy.on_delete host (fun () -> Proxy.delete client);
  fn host

let make_region ~host_region r =
  let h = host_region @@ new H.Wl_region.handlers in
  let user_data = client_data (Region h) in
  Proxy.Handler.attach r @@ object
    inherit [_] C.Wl_region.handlers
    method! user_data = user_data
    method on_add _ = H.Wl_region.add h
    method on_subtract _ = H.Wl_region.subtract h
    method on_destroy = delete_with H.Wl_region.destroy h
  end

let make_surface ~host_surface c =
  let h =
    let user_data = host_data (HD.Surface c) in
    host_surface @@ object
      inherit [_] H.Wl_surface.handlers
      method! user_data = user_data
      method on_enter _ ~output = C.Wl_surface.enter c ~output:(to_client output)
      method on_leave _ ~output = C.Wl_surface.leave c ~output:(to_client output)
    end
  in
  let h = Proxy.cast_version h in
  let data = { CD.host_surface = h; host_memory = Cstruct.empty; client_memory = Cstruct.empty } in
  Proxy.Handler.attach c @@ object
    inherit [_] C.Wl_surface.handlers
    method! user_data = client_data (Surface data)
    method on_attach _ ~buffer ~x ~y =
      match buffer with
      | Some buffer ->
        let Client_data (Buffer buffer) = user_data buffer in
        data.host_memory <- buffer.host_memory;
        data.client_memory <- buffer.client_memory;
        H.Wl_surface.attach h ~buffer:(Some buffer.host_buffer) ~x ~y
      | None ->
        data.host_memory <- Cstruct.empty;
        data.client_memory <- Cstruct.empty;
        H.Wl_surface.attach h ~buffer:None ~x ~y
    method on_commit _ =
      (* todo: only copy the bit that changed *)
      Cstruct.blit data.client_memory 0 data.host_memory 0 (Cstruct.len data.client_memory);
      H.Wl_surface.commit h
    method on_damage _ ~x ~y ~width ~height = H.Wl_surface.damage h ~x ~y ~width ~height
    method on_damage_buffer _ ~x ~y ~width ~height = H.Wl_surface.damage_buffer h ~x ~y ~width ~height
    method on_destroy = delete_with H.Wl_surface.destroy h
    method on_frame _ callback =
      let _ : _ Proxy.t = H.Wl_surface.frame h @@ Wayland.callback @@ fun callback_data ->
        C.Wl_callback.done_ callback ~callback_data;
        Proxy.delete callback
      in
      Proxy.Handler.attach callback @@ new C.Wl_callback.handlers
    method on_set_input_region _ ~region = H.Wl_surface.set_input_region h ~region:(Option.map to_host region)
    method on_set_opaque_region _ ~region = H.Wl_surface.set_opaque_region h ~region:(Option.map to_host region)
    method on_set_buffer_scale _ = H.Wl_surface.set_buffer_scale h
    method on_set_buffer_transform _ = H.Wl_surface.set_buffer_transform h
  end

let make_compositor bind proxy =
  let h = bind @@ new H.Wl_compositor.v1 in
  Proxy.Handler.attach proxy @@ object
    inherit [_] C.Wl_compositor.handlers
    method on_create_region _ = make_region ~host_region:(H.Wl_compositor.create_region h)
    method on_create_surface _ = make_surface ~host_surface:(H.Wl_compositor.create_surface h)
  end

let make_subsurface ~host_subsurface c =
  let h = host_subsurface @@ new H.Wl_subsurface.handlers in
  Proxy.Handler.attach c @@ object
    inherit [_] C.Wl_subsurface.handlers
    method on_destroy = delete_with H.Wl_subsurface.destroy h
    method on_place_above _ ~sibling = H.Wl_subsurface.place_above h ~sibling:(to_host sibling)
    method on_place_below _ ~sibling = H.Wl_subsurface.place_below h ~sibling:(to_host sibling)
    method on_set_desync _ = H.Wl_subsurface.set_desync h
    method on_set_position _ = H.Wl_subsurface.set_position h
    method on_set_sync _ = H.Wl_subsurface.set_sync h
  end

let make_subcompositor bind proxy =
  let h = bind @@ new H.Wl_subcompositor.v1 in
  Proxy.Handler.attach proxy @@ object
    inherit [_] C.Wl_subcompositor.handlers
    method on_destroy = delete_with H.Wl_subcompositor.destroy h

    method on_get_subsurface _ subsurface ~surface ~parent =
      let surface = to_host surface in
      let parent = to_host parent in
      let host_subsurface = H.Wl_subcompositor.get_subsurface h ~surface ~parent in
      make_subsurface ~host_subsurface subsurface
  end

let make_buffer ~host_buffer ~host_memory ~client_memory proxy =
  let user_data = client_data (Buffer {host_buffer; host_memory; client_memory}) in
  Proxy.Handler.attach proxy @@ object
    inherit [_] C.Wl_buffer.handlers
    method! user_data = user_data
    method on_destroy = delete_with H.Wl_buffer.destroy host_buffer
  end

type mapping = {
  host_pool : [`V1] H.Wl_shm_pool.t;
  client_memory_pool : Lwt_bytes.t;
  host_memory_pool : Lwt_bytes.t;
}

(* todo: this all needs to be more robust.
   Also, sealing? *)
let make_shm_pool ~virtwl ~host_shm proxy ~fd:client_fd ~size:orig_size =
  let alloc ~size =
    let client_memory_pool = Unix.map_file client_fd Bigarray.Char Bigarray.c_layout true [| Int32.to_int size |] in
    let host_pool, host_memory_pool =
      Wayland_virtwl.with_memory_fd virtwl ~size:(Int32.to_int size) (fun fd ->
          let host_pool = H.Wl_shm.create_pool host_shm ~fd ~size @@ new H.Wl_shm_pool.handlers in
          let host_memory = Wayland_virtwl.map_file fd Bigarray.Char ~n_elements:(Int32.to_int size) in
          host_pool, host_memory
        )
    in
    let host_memory_pool = Bigarray.array1_of_genarray host_memory_pool in
    let client_memory_pool = Bigarray.array1_of_genarray client_memory_pool in
    { host_pool; client_memory_pool; host_memory_pool }
  in
  let mapping = ref (alloc ~size:orig_size) in
  Proxy.Handler.attach proxy @@ object
    inherit [_] C.Wl_shm_pool.handlers

    method on_create_buffer _ buffer ~offset ~width ~height ~stride ~format =
      let len = Int32.to_int height * Int32.to_int stride in
      let host_memory = Cstruct.of_bigarray (!mapping).host_memory_pool ~off:(Int32.to_int offset) ~len in
      let client_memory = Cstruct.of_bigarray (!mapping).client_memory_pool ~off:(Int32.to_int offset) ~len in
      let host_buffer =
        H.Wl_shm_pool.create_buffer (!mapping).host_pool ~offset ~width ~height ~stride ~format
        @@ object
          inherit [_] H.Wl_buffer.handlers
          method on_release _ = C.Wl_buffer.release buffer
        end 
      in
      make_buffer ~host_buffer ~host_memory ~client_memory buffer

    method on_destroy t =
      Unix.close client_fd;
      delete_with H.Wl_shm_pool.destroy (!mapping).host_pool t

    method on_resize _ ~size =
      H.Wl_shm_pool.destroy (!mapping).host_pool;
      mapping := alloc ~size
  end

let make_output bind c =
  let c = Proxy.cast_version c in
  let h =
    let user_data = host_data (HD.Output c) in
    bind @@ object
      inherit H.Wl_output.v1
      method! user_data = user_data
      method on_done _ = C.Wl_output.done_ (Proxy.cast_version c)
      method on_geometry _ = C.Wl_output.geometry c
      method on_mode _ = C.Wl_output.mode c
      method on_scale  _ = C.Wl_output.scale (Proxy.cast_version c)
    end
  in
  let user_data = client_data (Output h) in
  Proxy.Handler.attach c @@ object
    inherit [_] C.Wl_output.handlers
    method! user_data = user_data
    method on_release = delete_with H.Wl_output.release (cv h)
  end

let make_seat bind c =
  let c = Proxy.cast_version c in
  let cap_mask = C.Wl_seat.Capability.(Int32.logor keyboard pointer) in
  let host = bind @@ object
      inherit H.Wl_seat.v1

      method on_capabilities _ ~capabilities =
        C.Wl_seat.capabilities c ~capabilities:(Int32.logand capabilities cap_mask)
      method on_name _ = C.Wl_seat.name (cv c)
    end
  in
  let host = cv host in
  let user_data = client_data (Seat host) in
  Proxy.Handler.attach c @@ object
    inherit [_] C.Wl_seat.handlers
    method! user_data = user_data
    method on_get_keyboard _ keyboard =
      let h : _ Proxy.t = H.Wl_seat.get_keyboard host @@ object
          inherit [_] H.Wl_keyboard.handlers
          method on_keymap    _ ~format ~fd ~size =
            C.Wl_keyboard.keymap keyboard ~format ~fd ~size;
            Unix.close fd
          method on_enter     _ ~serial ~surface = C.Wl_keyboard.enter keyboard ~serial ~surface:(to_client surface)
          method on_leave     _ ~serial ~surface = C.Wl_keyboard.leave keyboard ~serial ~surface:(to_client surface)
          method on_key       _ = C.Wl_keyboard.key keyboard
          method on_modifiers _ = C.Wl_keyboard.modifiers keyboard
          method on_repeat_info _ = C.Wl_keyboard.repeat_info (cv keyboard)
        end
      in
      Proxy.Handler.attach keyboard @@ object
        inherit [_] C.Wl_keyboard.handlers
        method on_release = delete_with H.Wl_keyboard.release h
      end

    method on_get_pointer _ c =
      let c = cv c in
      let h : _ Proxy.t = H.Wl_seat.get_pointer host @@ object
          inherit [_] H.Wl_pointer.handlers
          method on_axis _ = C.Wl_pointer.axis c
          method on_axis_discrete _ = C.Wl_pointer.axis_discrete c
          method on_axis_source _ = C.Wl_pointer.axis_source c
          method on_axis_stop _ = C.Wl_pointer.axis_stop c
          method on_button _ = C.Wl_pointer.button c
          method on_enter _ ~serial ~surface = C.Wl_pointer.enter c ~serial ~surface:(to_client surface)
          method on_leave _ ~serial ~surface = C.Wl_pointer.leave c ~serial ~surface:(to_client surface)
          method on_motion _ = C.Wl_pointer.motion c
          method on_frame _ = C.Wl_pointer.frame c
        end
      in
      Proxy.Handler.attach c @@ object
        inherit [_] C.Wl_pointer.handlers
        method on_set_cursor _ ~serial ~surface = H.Wl_pointer.set_cursor h ~serial ~surface:(Option.map to_host surface)
        method on_release = delete_with H.Wl_pointer.release h
      end

    method on_get_touch _ = Fmt.failwith "TODO: on_get_touch"
    method on_release = delete_with H.Wl_seat.release host
  end

let make_shm ~virtwl bind c =
  let c = Proxy.cast_version c in
  let h = bind @@ object
      inherit H.Wl_shm.v1
      method on_format _ = C.Wl_shm.format c
    end
  in
  Proxy.Handler.attach c @@ object
    inherit [_] C.Wl_shm.handlers
    method on_create_pool _ = make_shm_pool ~virtwl ~host_shm:h
  end

let make_popup ~host_popup c =
  let h = host_popup @@ object
      inherit [_] H.Xdg_popup.handlers
      method on_popup_done _ = C.Xdg_popup.popup_done c
      method on_configure _ = C.Xdg_popup.configure c
      method on_repositioned _ = C.Xdg_popup.repositioned c
    end
  in
  Proxy.Handler.attach c @@ object
    inherit [_] C.Xdg_popup.handlers
    method on_destroy = delete_with H.Xdg_popup.destroy h
    method on_grab _ ~seat = H.Xdg_popup.grab h ~seat:(to_host seat)
    method on_reposition _ ~positioner = H.Xdg_popup.reposition h ~positioner:(to_host positioner)
  end

let make_toplevel ~tag ~host_toplevel c =
  let h = host_toplevel @@ object
      inherit [_] H.Xdg_toplevel.handlers
      method on_close _ = C.Xdg_toplevel.close c
      method on_configure _ = C.Xdg_toplevel.configure c
    end
  in
  let user_data = client_data (Toplevel h) in
  Proxy.Handler.attach c @@ object
    inherit [_] C.Xdg_toplevel.handlers
    method! user_data = user_data
    method on_destroy = delete_with H.Xdg_toplevel.destroy h
    method on_move _ ~seat = H.Xdg_toplevel.move h ~seat:(to_host seat)
    method on_resize _ ~seat = H.Xdg_toplevel.resize h ~seat:(to_host seat)
    method on_set_app_id _ = H.Xdg_toplevel.set_app_id h
    method on_set_fullscreen _ ~output = H.Xdg_toplevel.set_fullscreen h ~output:(Option.map to_host output)
    method on_set_max_size _ = H.Xdg_toplevel.set_max_size h
    method on_set_maximized _ = H.Xdg_toplevel.set_maximized h
    method on_set_min_size _ = H.Xdg_toplevel.set_min_size h
    method on_set_minimized _ = H.Xdg_toplevel.set_minimized h
    method on_set_parent _ ~parent = H.Xdg_toplevel.set_parent h ~parent:(Option.map to_host parent)
    method on_set_title _ ~title = H.Xdg_toplevel.set_title h ~title:(tag ^ title)
    method on_show_window_menu _ ~seat = H.Xdg_toplevel.show_window_menu h ~seat:(to_host seat)
    method on_unset_fullscreen _ = H.Xdg_toplevel.unset_fullscreen h
    method on_unset_maximized _ = H.Xdg_toplevel.unset_maximized h
  end

let make_xdg_surface ~tag ~host_xdg_surface c =
  let c = cv c in
  let h = host_xdg_surface @@ object
      inherit [_] H.Xdg_surface.handlers
      method on_configure _ = C.Xdg_surface.configure c
    end
  in
  let user_data = client_data (Xdg_surface h) in
  Proxy.Handler.attach c @@ object
    inherit [_] C.Xdg_surface.handlers
    method! user_data = user_data
    method on_destroy = delete_with H.Xdg_surface.destroy h
    method on_ack_configure _ = H.Xdg_surface.ack_configure h
    method on_set_window_geometry _ = H.Xdg_surface.set_window_geometry h

    method on_get_toplevel _ = make_toplevel ~tag ~host_toplevel:(H.Xdg_surface.get_toplevel h)

    method on_get_popup _ popup ~parent ~positioner =
      let parent = Option.map to_host parent in
      let positioner = to_host positioner in
      make_popup ~host_popup:(H.Xdg_surface.get_popup h ~parent ~positioner) popup
  end

let make_positioner ~host_positioner c =
  let h = host_positioner @@ new H.Xdg_positioner.handlers in
  let user_data = client_data (Xdg_positioner h) in
  Proxy.Handler.attach c @@ object
    inherit [_] C.Xdg_positioner.handlers
    method! user_data = user_data
    method on_destroy = delete_with H.Xdg_positioner.destroy h
    method on_set_anchor _ = H.Xdg_positioner.set_anchor h
    method on_set_anchor_rect _ = H.Xdg_positioner.set_anchor_rect h
    method on_set_constraint_adjustment _ = H.Xdg_positioner.set_constraint_adjustment h
    method on_set_gravity _ = H.Xdg_positioner.set_gravity h
    method on_set_offset _ = H.Xdg_positioner.set_offset h
    method on_set_size _ = H.Xdg_positioner.set_size h
    method on_set_reactive _ = H.Xdg_positioner.set_reactive h
    method on_set_parent_size _ = H.Xdg_positioner.set_parent_size h
    method on_set_parent_configure _ = H.Xdg_positioner.set_parent_configure h
  end

let make_xdg_wm_base ~tag bind proxy =
  let h = bind @@ object
      inherit H.Xdg_wm_base.v1
      method on_ping _ = C.Xdg_wm_base.ping proxy
    end
  in
  let h = Proxy.cast_version h in
  Proxy.Handler.attach proxy @@ object
    inherit [_] C.Xdg_wm_base.handlers

    method on_destroy = delete_with H.Xdg_wm_base.destroy h
    method on_pong _ = H.Xdg_wm_base.pong h

    method on_create_positioner _ = make_positioner ~host_positioner:(H.Xdg_wm_base.create_positioner h)

    method on_get_xdg_surface _ xdg_surface ~surface =
      let host_xdg_surface = H.Xdg_wm_base.get_xdg_surface h ~surface:(to_host surface) in
      make_xdg_surface ~tag ~host_xdg_surface xdg_surface
  end

let make_zxdg_output ~host_xdg_output c =
  let c = cv c in
  let h = host_xdg_output @@ object
      inherit [_] H.Zxdg_output_v1.handlers
      method on_description _ = C.Zxdg_output_v1.description c
      method on_done _ = C.Zxdg_output_v1.done_ c
      method on_logical_position _ = C.Zxdg_output_v1.logical_position c
      method on_logical_size _ = C.Zxdg_output_v1.logical_size c
      method on_name _ = C.Zxdg_output_v1.name c
    end in
  Proxy.Handler.attach c @@ object
    inherit [_] C.Zxdg_output_v1.handlers
    method on_destroy = delete_with H.Zxdg_output_v1.destroy h
  end

let make_zxdg_output_manager_v1 bind proxy =
  let proxy = Proxy.cast_version proxy in
  let h = bind @@ new H.Zxdg_output_manager_v1.v3 in
  Proxy.Handler.attach proxy @@ object
    inherit [_] C.Zxdg_output_manager_v1.handlers

    method on_destroy = delete_with H.Zxdg_output_manager_v1.destroy h

    method on_get_xdg_output _ c ~output =
      let output = to_host output in
      make_zxdg_output ~host_xdg_output:(H.Zxdg_output_manager_v1.get_xdg_output h ~output) c
  end

let make_kde_decoration ~host_decoration c =
  let h = host_decoration @@ object
      inherit [_] H.Org_kde_kwin_server_decoration.handlers
      method on_mode _ = C.Org_kde_kwin_server_decoration.mode c
    end
  in
  Proxy.Handler.attach c @@ object
    inherit [_] C.Org_kde_kwin_server_decoration.handlers
    method on_release = delete_with H.Org_kde_kwin_server_decoration.release h
    method on_request_mode _ = H.Org_kde_kwin_server_decoration.request_mode h
  end

let make_kde_decoration_manager bind c =
  let h = bind @@ object
      inherit H.Org_kde_kwin_server_decoration_manager.v1
      method on_default_mode _ = C.Org_kde_kwin_server_decoration_manager.default_mode c
    end
  in
  Proxy.Handler.attach c @@ object
    inherit [_] C.Org_kde_kwin_server_decoration_manager.handlers
    method on_create _ decoration ~surface =
      let surface = to_host surface in
      make_kde_decoration ~host_decoration:(H.Org_kde_kwin_server_decoration_manager.create h ~surface) decoration
  end

let make_data_offer ~virtwl ~client_offer h =
  let c = client_offer @@ object
      inherit [_] C.Wl_data_offer.handlers
      method on_accept _ = H.Wl_data_offer.accept h
      method on_destroy c =
        delete_with H.Wl_data_offer.destroy h c;
        (* Effectively, the "selection" event is the destructor of the previous selection,
           and this is the confirmation. The server doesn't send a delete event, so just do it manually. *)
        Proxy.delete h
      method on_finish _ = H.Wl_data_offer.finish h
      method on_receive _ ~mime_type ~fd =
        Pipes.with_wrapped_writeable ~virtwl fd @@ fun fd ->
        H.Wl_data_offer.receive h ~mime_type ~fd
      method on_set_actions _ = H.Wl_data_offer.set_actions h
    end in
  let user_data = host_data (HD.Data_offer c) in
  Proxy.Handler.attach h @@ object
    inherit [_] H.Wl_data_offer.handlers
    method! user_data = user_data
    method on_action _ = C.Wl_data_offer.action c
    method on_offer _ = C.Wl_data_offer.offer c
    method on_source_actions _ = C.Wl_data_offer.source_actions c
  end

let make_data_source ~host_source c =
  let c = cv c in
  let h =
    host_source @@ object
      inherit [_] H.Wl_data_source.handlers
      method on_action _ = C.Wl_data_source.action c
      method on_cancelled _ = C.Wl_data_source.cancelled c
      method on_dnd_drop_performed _ = C.Wl_data_source.dnd_drop_performed c
      method on_dnd_finished _ = C.Wl_data_source.dnd_finished c
      method on_send _ ~mime_type ~fd =
        C.Wl_data_source.send c ~mime_type ~fd;
        Unix.close fd
      method on_target _ = C.Wl_data_source.target c
    end in
  let user_data = client_data (Data_source h) in
  Proxy.Handler.attach c @@ object
    inherit [_] C.Wl_data_source.handlers
    method! user_data = user_data
    method on_destroy = delete_with H.Wl_data_source.destroy h
    method on_offer _ = H.Wl_data_source.offer h
    method on_set_actions _ = H.Wl_data_source.set_actions h
  end

let make_data_device ~virtwl ~host_device c =
  let c = cv c in
  let h = host_device @@ object
      inherit [_] H.Wl_data_device.handlers
      method on_data_offer _ offer = make_data_offer ~virtwl ~client_offer:(C.Wl_data_device.data_offer c) offer
      method on_drop _ = C.Wl_data_device.drop c
      method on_enter _ ~serial ~surface ~x ~y offer =
        C.Wl_data_device.enter c ~serial ~surface:(to_client surface) ~x ~y (Option.map to_client offer)
      method on_leave _ = C.Wl_data_device.leave c
      method on_motion _ = C.Wl_data_device.motion c
      method on_selection _ offer = C.Wl_data_device.selection c (Option.map to_client offer)
    end in
  Proxy.Handler.attach c @@ object
    inherit [_] C.Wl_data_device.handlers
    method on_release = delete_with H.Wl_data_device.release h
    method on_set_selection _ ~source = H.Wl_data_device.set_selection h ~source:(Option.map to_host source)
    method on_start_drag _ ~source ~origin ~icon =
      H.Wl_data_device.start_drag h
        ~source:(Option.map to_host source)
        ~origin:(to_host origin)
        ~icon:(Option.map to_host icon)
  end

let make_data_device_manager ~virtwl bind proxy =
  let proxy = Proxy.cast_version proxy in
  let h = bind @@ new H.Wl_data_device_manager.v3 in
  Proxy.Handler.attach proxy @@ object
    inherit [_] C.Wl_data_device_manager.handlers
    method on_create_data_source _ c =
      make_data_source c ~host_source:(H.Wl_data_device_manager.create_data_source h)
    method on_get_data_device _ c ~seat =
      let seat = to_host seat in
      make_data_device ~virtwl c ~host_device:(H.Wl_data_device_manager.get_data_device h ~seat)
  end

let make_gtk_data_offer ~virtwl ~client_offer h =
  let c = client_offer @@ object
      inherit [_] C.Gtk_primary_selection_offer.handlers

      method on_destroy c =
        delete_with H.Gtk_primary_selection_offer.destroy h c;
        (* Effectively, the "selection" event is the destructor of the previous selection,
           and this is the confirmation. The server doesn't send a delete event, so just do it manually. *)
        Proxy.delete h

      method on_receive _ ~mime_type ~fd =
        Pipes.with_wrapped_writeable ~virtwl fd @@ fun fd ->
        H.Gtk_primary_selection_offer.receive h ~mime_type ~fd
    end in
  let user_data = host_data (HD.Gtk_data_offer c) in
  Proxy.Handler.attach h @@ object
    inherit [_] H.Gtk_primary_selection_offer.handlers
    method! user_data = user_data
    method on_offer _ = C.Gtk_primary_selection_offer.offer c
  end

let make_gtk_primary_selection_source ~host_source c =
  let h =
    host_source @@ object
      inherit [_] H.Gtk_primary_selection_source.handlers
      method on_cancelled _ = C.Gtk_primary_selection_source.cancelled c
      method on_send _ ~mime_type ~fd =
        C.Gtk_primary_selection_source.send c ~mime_type ~fd;
        Unix.close fd
    end in
  let user_data = client_data (Gtk_source h) in
  Proxy.Handler.attach c @@ object
    inherit [_] C.Gtk_primary_selection_source.handlers
    method! user_data = user_data
    method on_destroy = delete_with H.Gtk_primary_selection_source.destroy h
    method on_offer _ = H.Gtk_primary_selection_source.offer h
  end

let make_gtk_primary_selection_device ~virtwl ~host_device c =
  let h = host_device @@ object
      inherit [_] H.Gtk_primary_selection_device.handlers
      method on_data_offer _ offer = make_gtk_data_offer ~virtwl ~client_offer:(C.Gtk_primary_selection_device.data_offer c) offer
      method on_selection _ offer = C.Gtk_primary_selection_device.selection c (Option.map to_client offer)
    end in
  Proxy.Handler.attach c @@ object
    inherit [_] C.Gtk_primary_selection_device.handlers
    method on_destroy = delete_with H.Gtk_primary_selection_device.destroy h
    method on_set_selection _ ~source =
      let source = Option.map to_host source in
      H.Gtk_primary_selection_device.set_selection h ~source
  end

let make_gtk_primary_selection_device_manager ~virtwl bind proxy =
  let proxy = Proxy.cast_version proxy in
  let h = bind @@ new H.Gtk_primary_selection_device_manager.v1 in
  Proxy.Handler.attach proxy @@ object
    inherit [_] C.Gtk_primary_selection_device_manager.handlers
    method on_create_source _ source =
      let host_source = H.Gtk_primary_selection_device_manager.create_source h in
      make_gtk_primary_selection_source ~host_source source
    method on_destroy = delete_with H.Gtk_primary_selection_device_manager.destroy h
    method on_get_device _ dev ~seat =
      let seat = to_host seat in
      let host_device = H.Gtk_primary_selection_device_manager.get_device h ~seat in
      make_gtk_primary_selection_device ~virtwl ~host_device dev
  end

type entry = Entry : int32 * (module Metadata.S) -> entry

let pp_closed f = function
  | Ok () -> Fmt.string f "closed connection"
  | Error ex -> Fmt.pf f "connection failed: %a" Fmt.exn ex

let registry =
  let open Protocols in
  [
    (module Wl_compositor : Metadata.S);
    (module Wl_subcompositor);
    (module Wl_shm);
    (module Xdg_wm_base);
    (module Wl_output);
    (module Wl_data_device_manager);
    (module Zxdg_output_manager_v1);
    (module Gtk_primary_selection_device_manager);
    (module Wl_seat); (* Must come after gtk, or evince crashes *)
    (module Org_kde_kwin_server_decoration_manager);
  ]

let make_registry t reg =
  let registry =
    registry |> List.filter_map (fun (module M : Metadata.S) ->
        match Registry.get t.host_registry M.interface with
        | [] ->
          Log.info (fun f -> f "Host doesn't support service %s, so skipping" M.interface);
          None
        | { Registry.name; version = host_version } :: _ ->
          let max_version = min M.version host_version in
          Some (name, Entry (max_version, (module M)))
      )
    |> Array.of_list
  in
  Proxy.Handler.attach reg @@ object
    inherit [_] C.Wl_registry.handlers

    method on_bind : type a. _ -> name:int32 -> (a, [`Unknown], _) Proxy.t -> unit =
      fun _ ~name proxy ->
      let name = Int32.to_int name in
      if name < 0 || name >= Array.length registry then Fmt.failwith "Bad registry entry name %d" name;
      let host_name, Entry (max_version, (module M)) = registry.(name) in
      let requested_version = Proxy.version proxy in
      if requested_version > max_version then
        Fmt.failwith "Client asked for %S v%lu, but we only support up to %lu" M.interface requested_version max_version;
      let client_interface = Proxy.interface proxy in
      if client_interface <> M.interface then
        Fmt.failwith "Entry %d has type %S, client expected %S!" name M.interface client_interface;
      let bind x = H.Wl_registry.bind (Registry.wl_registry t.host_registry) ~name:host_name (x, Proxy.version proxy) in
      let open Protocols in
      let proxy = Proxy.cast_version proxy in
      match Proxy.ty proxy with
      | Wl_compositor.T -> make_compositor bind proxy
      | Wl_subcompositor.T -> make_subcompositor bind proxy
      | Wl_shm.T -> make_shm ~virtwl:t.virtwl bind proxy
      | Wl_seat.T -> make_seat bind proxy
      | Wl_output.T -> make_output bind proxy
      | Wl_data_device_manager.T -> make_data_device_manager ~virtwl:t.virtwl bind proxy
      | Gtk_primary_selection_device_manager.T -> make_gtk_primary_selection_device_manager ~virtwl:t.virtwl bind proxy
      | Xdg_wm_base.T -> make_xdg_wm_base ~tag:t.config.tag bind proxy
      | Zxdg_output_manager_v1.T -> make_zxdg_output_manager_v1 bind proxy
      | Org_kde_kwin_server_decoration_manager.T -> make_kde_decoration_manager bind proxy
      | _ -> Fmt.failwith "Invalid service name for %a" Proxy.pp proxy
  end;
  registry |> Array.iteri (fun name (_, entry) ->
      let Entry (version, (module M)) = entry in
      C.Wl_registry.global reg ~name:(Int32.of_int name) ~interface:M.interface ~version
    )

let handle ~config client =
  let client_transport = Wayland.Unix_transport.of_socket client in
  let fd = Unix.(openfile "/dev/wl0" [O_RDWR; O_CLOEXEC] 0x600) in
  let virtwl = Wayland_virtwl.of_fd fd in
  let host_transport = Wayland_virtwl.new_context virtwl in
  let display, host_closed = Wayland.Client.connect ~trace:(module Trace.Host) host_transport in
  let* host_registry = Wayland.Registry.of_display display in
  let t = {
    virtwl;
    host_registry;
    config;
  } in
  let s : Server.t =
    Server.connect client_transport ~trace:(module Trace.Client) @@ object
      inherit [_] C.Wl_display.handlers
      method on_get_registry _ ref = make_registry t ref
      method on_sync _ cb =
        Proxy.Handler.attach cb @@ new C.Wl_callback.handlers;
        let h : _ Proxy.t = H.Wl_display.sync (Client.wl_display display) @@ object
            inherit [_] H.Wl_callback.handlers
            method on_done ~callback_data =
              C.Wl_callback.done_ cb ~callback_data
          end
        in
        Proxy.on_delete h (fun () -> Proxy.delete cb)
    end
  in
  let is_active = ref true in
  let client_done =
    let* r = Server.closed s in
    if !is_active then (
      Log.info (fun f -> f "Client %a" pp_closed r);
      is_active := false
    );
    host_transport#close
  in
  let host_done =
    let* r = host_closed in
    if !is_active then (
      Log.info (fun f -> f "Host %a" pp_closed r);
      is_active := false
    );
    Lwt_unix.shutdown client Unix.SHUTDOWN_SEND;
    Lwt.return_unit
  in
  let* () = Lwt.choose [client_done; host_done] in
  Unix.close virtwl;
  Lwt_unix.close client
