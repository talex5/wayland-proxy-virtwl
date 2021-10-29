open Lwt.Syntax
open Lwt.Infix

let min_respawn_time = 10.0             (* Don't run Xwayland more than once per 10s *)

module Log = Log.Xwayland

module Proxy = Wayland.Proxy
module Wl_seat = Wayland.Wayland_client.Wl_seat
module Xdg_wm_base = Wayland_protocols.Xdg_shell_client.Xdg_wm_base
module Xdg_surface = Wayland_protocols.Xdg_shell_client.Xdg_surface
module Xdg_toplevel = Wayland_protocols.Xdg_shell_client.Xdg_toplevel
module Xdg_popup = Wayland_protocols.Xdg_shell_client.Xdg_popup
module Xdg_positioner = Wayland_protocols.Xdg_shell_client.Xdg_positioner
module Xdg_decor_mgr = Wayland_protocols.Xdg_decoration_unstable_v1_client.Zxdg_decoration_manager_v1
module Xdg_decoration = Wayland_protocols.Xdg_decoration_unstable_v1_client.Zxdg_toplevel_decoration_v1

type host_surface = [`V1 | `V2 | `V3 | `V4 ] Wayland.Wayland_client.Wl_surface.t
type client_surface = [`V1 | `V2 | `V3 | `V4 ] Wayland.Wayland_server.Wl_surface.t

(* When Xwayland notifies us of a new surface over the Wayland connection, we record the details.
   We don't yet know the X11 window ID, however.
   Later, if we get a pairing message over the X11 connection this will become a {!paired} window.
   We must stop Xwayland from attaching a buffer until the window is configured,
   so the relay queues events until [set_configured] is called.
   Note that the surfaces may get destroyed over the Wayland connection before we get the X11 message.
   We don't remove the entry from [t.unpaired] until we get the X11 message.
*)
type unpaired = {
  client_surface : client_surface;
  host_surface : host_surface;
  set_configured : [`Show | `Hide | `Unmanaged] -> unit;
}

type paired = {
  window : X11.Window.t;
  xdg_surface : [`V1] Xdg_surface.t;
  mutable xdg_role : [
    | `Toplevel of [`V1] Xdg_toplevel.t
    | `Popup of [`V1] Xdg_popup.t
    | `None
  ];
  mutable geometry : X11.Geometry.t;
  override_redirect : bool;
}

let pp_paired f { window; xdg_surface; xdg_role; geometry; override_redirect } =
  Fmt.pf f "%a@%a/%t=%a%s"
    Proxy.pp xdg_surface
    X11.Geometry.pp geometry
    (fun f -> match xdg_role with
       | `Toplevel x -> Proxy.pp f x
       | `Popup x -> Proxy.pp f x
       | `None -> Fmt.string f "(no role)"
    )
    X11.Xid.pp window
    (if override_redirect then "(override-redirect)" else "")

type t = {
  relay : Relay.t;
  x11 : X11.Display.t Lwt.t;
  config : Config.t;
  wm_base : [`V1] Xdg_wm_base.t;
  decor_mgr : [`V1] Xdg_decor_mgr.t option;
  unpaired : (int32, unpaired) Hashtbl.t;         (* Client-side Wayland ID -> details *)
  unpaired_added : unit Lwt_condition.t;          (* Fires when [unpaired] gets a new entry. *)
  paired : (X11.Window.t, paired) Hashtbl.t;      (* X11 ID -> details *)
  of_host_surface : (int32, paired) Hashtbl.t;    (* Host-side Wayland ID -> details *) 

  mutable pointer_surface : paired option;        (* A member of [of_host_surface] *)
  mutable keyboard_surface : paired option;       (* A member of [of_host_surface] *)
  mutable last_event_surface : [`Keyboard | `Pointer];  (* Which to prefer *)

  (* This is set when Xwayland creates the xdg_wm_base. We need it to sync the two connections.
     Xwayland doesn't actually need this in rootless mode, but luckily for us it creates it anyway. *)
  mutable wayland_ping : unit -> unit Lwt.t;
}

let scale_to_client t (x, y) =
  x * t.config.xunscale,
  y * t.config.xunscale

(* We round up, as otherwise popup menus can be one pixel too short and GTK adds scrollers *)
let scale_to_host t (x, y) =
  (x + t.config.xunscale - 1) / t.config.xunscale,
  (y + t.config.xunscale - 1) / t.config.xunscale

let intern ?only_if_exists t name =
  let* x11 = t.x11 in
  X11.Atom.intern ?only_if_exists x11 name

module Selection = struct
  (* There are two states here:

     - An X11 client owns the selection (and we own the Wayland selection).
     - A Wayland client owns the selection (and we own the X11 selection).

     When we own a selection we proxy requests for it to the matching selection on the other protocol.

     When we lose the X11 selection it means that an X11 client now owns it and we take the Wayland selection.
     When we lose the Wayland selection it means that a Wayland client now owns it and we take the X11 selection.

     At startup, we take ownership of the X11 selection, since there are no X11 apps running yet.

     As well as the PRIMARY selection, there is also the CLIPBOARD. This is handled in the same was for X, just
     using a different atom, but Wayland has a completely separate API for it so there's lots of near-duplication. *)

  module Primary = Wayland_protocols.Wp_primary_selection_unstable_v1_client
  module Primary_mgr = Primary.Zwp_primary_selection_device_manager_v1
  module Primary_device = Primary.Zwp_primary_selection_device_v1
  module Primary_offer = Primary.Zwp_primary_selection_offer_v1
  module Primary_source = Primary.Zwp_primary_selection_source_v1

  module Clipboard_mgr = Wayland.Wayland_client.Wl_data_device_manager
  module Clipboard_device = Wayland.Wayland_client.Wl_data_device
  module Clipboard_offer = Wayland.Wayland_client.Wl_data_offer
  module Clipboard_source = Wayland.Wayland_client.Wl_data_source

  type (_, _) Wayland.S.user_data += Offer of string list ref

  (* Note: the target in the notification always matches the one in the
     request, even if the actual data returned has a different type. *)
  type notify_key = {
    selection : X11.Atom.t;
    target : X11.Atom.t;
  }

  type t = {
    x11 : X11.Display.t Lwt.t;
    wayland_primary_offer : [`V1] Primary_offer.t option ref;
    wayland_clipboard_offer : [`V1|`V2|`V3] Clipboard_offer.t option ref;
    primary_selection_mgr : [`V1] Primary_mgr.t;
    clipboard_mgr : [`V1] Clipboard_mgr.t;
    primary_device : [`V1] Primary_device.t;
    clipboard_device : [`V1] Clipboard_device.t;
    selection_window : X11.Window.t Lwt.t;
    set_selection_window : X11.Window.t Lwt.u;
    clipboard_window : X11.Window.t Lwt.t;
    set_clipboard_window : X11.Window.t Lwt.u;
    virtwl : Wayland_virtwl.t option;
    relay : Relay.t;

    (* When we request an X11 selection, we add the callback for the response here. *)
    awaiting_notify : (notify_key, (X11.Display.timestamp * X11.Atom.t) option -> unit Lwt.t) Hashtbl.t;
  }

  (* Object to keep track of the current Wayland primary selection (updates [wayland_primary_offer]). *)
  let primary_device ~wayland_primary_offer =
    object
      inherit [_] Primary_device.v1

      method on_selection _ offer =
        !wayland_primary_offer |> Option.iter (fun old ->
            Primary_offer.destroy old;
            Proxy.delete old            (* Objects with IDs created by the server need to be deleted manually *)
          );
        wayland_primary_offer := offer

      method on_data_offer _ offer =
        let mime_types = ref [] in
        Proxy.Handler.attach offer @@ object
          inherit [_] Primary_offer.v1
          method! user_data = Offer mime_types
          method on_offer _ ~mime_type = mime_types := mime_type :: !mime_types
        end
    end

  (* Object to keep track of the current Wayland clipboard (updates [wayland_clipboard_offer]).
     This is similar to {!primary_device}, but there's also a load of DnD stuff we have to ignore. *)
  let clipboard_device ~wayland_clipboard_offer =
    object
      inherit [_] Clipboard_device.v1

      method on_selection _ offer =
        !wayland_clipboard_offer |> Option.iter (fun old ->
            Clipboard_offer.destroy old;
            Proxy.delete old            (* Objects with IDs created by the server need to be deleted manually *)
          );
        wayland_clipboard_offer := offer

      method on_data_offer _ offer =
        let mime_types = ref [] in
        Proxy.Handler.attach offer @@ object
          inherit [_] Clipboard_offer.v1
          method! user_data = Offer mime_types
          method on_offer _ ~mime_type = mime_types := mime_type :: !mime_types
          method on_source_actions _ ~source_actions:_ = ()
          method on_action _ ~dnd_action:_ = ()
        end

      method on_motion _ ~time:_ ~x:_ ~y:_ = ()
      method on_leave _ = ()
      method on_enter _ ~serial:_ ~surface:_ ~x:_ ~y:_ offer = Option.iter Clipboard_offer.destroy offer
      method on_drop _ = ()
    end

  let get_targets offer =
    match Proxy.user_data offer with
    | Offer targets -> !targets
    | _ ->
      Log.warn (fun f -> f "No targets attached to %a!" Proxy.pp offer);
      []

  (** An X application has requested [target]. The Wayland provider is offering [targets].
      Return the MIME type and X11 target to use.
      For example, if xterm asks for target TEXT then we might reply with (text/plain, UTF8_STRING). *)
  let mime_type_of_target ~targets target =
    if List.mem target targets then target, target
    else if target = "TEXT" || target = "UTF8_STRING" then (
      if List.mem "UTF8_STRING" targets then "UTF8_STRING", "UTF8_STRING"
      else (
        List.find_map (fun mime_type ->
            match mime_type with
            | "text/plain" -> Some (mime_type, "UTF8_STRING")
            | x when Config.starts_with ~prefix:"text/plain;" x -> Some (mime_type, "UTF8_STRING")
            | _ -> None
          ) targets
        |> Option.value ~default:(target, target)
      )
    ) else (
      target, target
    )

  (* An X application wants to get the Wayland selection/clipboard. *)
  let selection_request t ~time ~owner:_ ~requestor ~selection ~target ~property =
    Lwt.async (fun () ->
        let* x11 = t.x11 in
        let* primary = X11.Atom.intern x11 "PRIMARY" in
        let* clipboard = X11.Atom.intern x11 "CLIPBOARD" in
        let reply property = X11.Selection.notify x11 selection ~time:(`Time time) ~requestor ~target ~property in
        let property = Option.value property ~default:target in (* For old clients; see ICCCM *)
        let offer =
          let get_targets fn = Option.map (fun offer -> fn offer, get_targets offer) in
          if selection = primary then get_targets Primary_offer.receive !(t.wayland_primary_offer)
          else if selection = clipboard then get_targets Clipboard_offer.receive !(t.wayland_clipboard_offer)
          else None
        in
        match offer with
        | None ->
          Log.info (fun f -> f "No Wayland %a offer - rejecting request" (X11.Atom.pp x11) selection);
          reply None
        | Some (receive, targets) ->
          let* mime_type, reply_target = X11.Atom.get_name x11 target >|= mime_type_of_target ~targets in
          if List.mem mime_type targets then (
            let host_fd, need_close_host, r =
              match t.virtwl with
              | Some virtwl ->
                let host_fd = Wayland_virtwl.pipe_read virtwl in
                let r = Lwt_io.(of_unix_fd ~mode:input) host_fd in    (* Will close [host_fd] *)
                (host_fd, false, r)
              | None ->
                let r, w = Unix.pipe () in
                let r = Lwt_io.(of_unix_fd ~mode:input) r in    (* Will close [r] *)
                (w, true, r)
            in
            Lwt.try_bind
              (fun () ->
                 receive ~mime_type ~fd:host_fd;        (* Tell Wayland app to write to [host_fd]. *)
                 if need_close_host then Unix.close host_fd;
                 Lwt_io.read r
              )
              (fun data ->
                 let* () = Lwt_io.close r in
                 let* reply_target = X11.Atom.intern x11 reply_target in
                 let* () = X11.Property.set_string ~ty:reply_target x11 requestor property data in
                 reply (Some property)
              )
              (fun ex ->
                 let* () = Lwt_io.close r in
                 Log.warn (fun f -> f "Error reading selection data from host: %a" Fmt.exn ex);
                 reply None
              )
          ) else (
            Log.info (fun f -> f "Request for unavailable MIME type %S - rejecting" mime_type);
            reply None
          )
      );
    Lwt.return_unit

  (* Call [fn] when we get the next SelectionNotify for [selection].
     If we were already waiting for one, cancel it. *)
  let on_selection_notify t ~selection ~target fn =
    (* todo: timeout old requests after a while? *)
    let key = { selection; target } in
    let old = Hashtbl.find_opt t.awaiting_notify key in
    Hashtbl.replace t.awaiting_notify key fn;
    match old with
    | None -> Lwt.return_unit
    | Some fn ->
      let* x11 = t.x11 in
      Log.info (fun f -> f "Started a new transfer for %a; cancelling existing one" (X11.Atom.pp x11) selection);
      fn None

  (* We have been notified by an X application that the data we requested is now ready. *)
  let selection_notify t ~time ~requestor:_ ~selection ~target ~property =
    let key = { selection; target } in
    begin match Hashtbl.find_opt t.awaiting_notify key with
      | None -> Log.warn (fun f -> f "Unexpected SelectionNotify!")
      | Some fn ->
        Hashtbl.remove t.awaiting_notify key;
        Lwt.async (fun () -> property |> Option.map (fun p -> (time, p)) |> fn)
    end;
    Lwt.return_unit

  (* Request a selection, wait for the transfer, and return the data. *)
  let fetch_selection t ?(time=`CurrentTime) ~requestor ~property ~target selection =
    let* x11 = t.x11 in
    let data, set_data = Lwt.wait () in
    let* () = on_selection_notify t ~selection ~target (function
        | Some (_, p) when p = property ->
          let* d = X11.Property.get_all ~delete:true x11 requestor property in
          Lwt.wakeup set_data d;
          Lwt.return_unit
        | _ ->
          Log.warn (fun f -> f "X selection request rejected");
          Lwt.wakeup set_data None;
          Lwt.return_unit
      )
    in
    let* () = X11.Property.delete x11 requestor property in
    let* () = X11.Selection.convert x11 selection
        ~requestor
        ~target
        ~property:(Some property)
        ~time
    in
    data

  let parse_targets x11 value =
    let rec to_atoms data =
      if Cstruct.length data < 4 then []
      else (
        let atom = Cstruct.LE.get_uint32 data 0 |> X11.Xid.of_int in
        atom :: to_atoms (Cstruct.shift data 4)
      )
    in
    value |> to_atoms |> Lwt_list.map_p (X11.Atom.get_name x11)

  (* Fetch an X selection from an X client and write it to [dst].
     [dst] will be closed afterwards. *)
  let send_x_selection t selection ~via:requestor ~mime_type ~dst =
    let w = Lwt_io.(of_unix_fd ~mode:output) dst in        (* Will close dst *)
    Lwt.async (fun () ->
        Lwt.finalize
          (fun () ->
             let* x11 = t.x11 in
             let* target = X11.Atom.intern x11 mime_type in
             fetch_selection t ~requestor ~property:target ~target selection >>= function
             | Some data ->
               Lwt_io.write w (Cstruct.to_string data)
             | None ->
               Log.warn (fun f -> f "X selection property not available!");
               Lwt.return_unit
          )
          (fun () -> Lwt_io.close w)
      )

  (* We lost our ownership of the X selection. This means that an X client now owns the selection.
     Tell Wayland that we can now provide that selection to it. *)
  let set_x_owned_primary t =
    let* x11 = t.x11 in
    (* Find out what targets the X app is offering *)
    let* primary = X11.Atom.intern x11 "PRIMARY" in
    let* targets_atom = X11.Atom.intern x11 "TARGETS" in
    let* requestor = t.selection_window in
    let* targets =
      fetch_selection t primary
        ~requestor
        ~target:targets_atom
        ~property:targets_atom
      >>= function
      | Some x -> parse_targets x11 x
      | None -> Lwt.return []
    in
    (* Create a Wayland source offering those targets *)
    let source = Primary_mgr.create_source t.primary_selection_mgr @@ object
        inherit [_] Primary_source.v1

        method on_send _ ~mime_type ~fd =
          Log.info (fun f -> f "Sending X PRIMARY selection to Wayland (%S)" mime_type);
          send_x_selection t primary ~via:requestor ~mime_type ~dst:fd

        method on_cancelled self =
          Log.info (fun f -> f "X selection source cancelled by Wayland - X app no longer owns selection");
          Lwt.async (fun () ->
              X11.Selection.set_owner x11 ~owner:(Some requestor) ~timestamp:`CurrentTime primary
            );
          Primary_source.destroy self
      end
    in
    targets |> List.iter (fun mime_type -> Primary_source.offer source ~mime_type);
    Primary_device.set_selection t.primary_device ~source:(Some source) ~serial:(Relay.last_serial t.relay);
    Lwt.return_unit

  (* Similar to {!set_x_owned_primary}, but for Wayland's clipboard API. *)
  let set_x_owned_clipboard t =
    let* x11 = t.x11 in
    (* Find out what targets the X app is offering *)
    let* clipboard = X11.Atom.intern x11 "CLIPBOARD" in
    let* targets_atom = X11.Atom.intern x11 "TARGETS" in
    let* requestor = t.clipboard_window in
    let* targets =
      fetch_selection t clipboard
        ~requestor
        ~target:targets_atom
        ~property:targets_atom
      >>= function
      | Some x -> parse_targets x11 x
      | None -> Lwt.return []
    in
    (* Create a Wayland source offering those targets *)
    let source = Clipboard_mgr.create_data_source t.clipboard_mgr @@ object
        inherit [_] Clipboard_source.v1

        method on_send _ ~mime_type ~fd =
          Log.info (fun f -> f "Sending X CLIPBOARD selection to Wayland (%S)" mime_type);
          send_x_selection t clipboard ~via:requestor ~mime_type ~dst:fd

        method on_cancelled self =
          Log.info (fun f -> f "X selection source cancelled by Wayland - X app no longer owns clipboard");
          Lwt.async (fun () ->
              X11.Selection.set_owner x11 ~owner:(Some requestor) ~timestamp:`CurrentTime clipboard
            );
          Clipboard_source.destroy self

        (* Drag-and-drop stuff we don't care about. *)
        method on_target _ ~mime_type:_ = ()
        method on_dnd_finished _ = ()
        method on_dnd_drop_performed _ = ()
        method on_action _ ~dnd_action:_ = ()
      end
    in
    targets |> List.iter (fun mime_type -> Clipboard_source.offer source ~mime_type);
    Clipboard_device.set_selection t.clipboard_device ~source:(Some source) ~serial:(Relay.last_serial t.relay);
    Lwt.return_unit

  (* Handle a SelectionClear event from Xwayland. *)
  let selection_clear t ~time:_ ~owner:_ ~selection =
    Lwt.async (fun () ->
        let* x11 = t.x11 in
        let* primary = X11.Atom.intern x11 "PRIMARY"
        and* clipboard = X11.Atom.intern x11 "CLIPBOARD" in
        if selection = primary then (
          Log.info (fun f -> f "An Xwayland app now owns the PRIMARY selection");
          set_x_owned_primary t
        ) else if selection = clipboard then (
          Log.info (fun f -> f "An Xwayland app now owns the CLIPBOARD selection");
          set_x_owned_clipboard t
        ) else (
          Log.warn (fun f -> f "SelectionClear for unknown selection type %a" (X11.Atom.pp x11) selection);
          Lwt.return_unit
        )
      );
    Lwt.return_unit

  (* Create a dummy X window for receiving selection transfers. *)
  let create_transfer_window x11 ~parent =
    let geometry = { X11.Geometry.x = 0; y = 0; width = 1; height = 1 } in
    X11.Window.create_input_only x11 ~parent ~geometry (X11.Window.create_attributes ())

  (* Create windows for handling selections. Take ownership of the selections (no X clients are running yet,
     so any existing selection must be owned by a Wayland client). This is called once the X connection
     is initialised. *)
  let init_x11 t =
    let* x11 = t.x11 in
    match X11.Window.roots x11 with
    | [root] ->
      let* selection_window = create_transfer_window x11 ~parent:root
      and* clipboard_window = create_transfer_window x11 ~parent:root
      and* primary = X11.Atom.intern x11 "PRIMARY"
      and* clipboard = X11.Atom.intern x11 "CLIPBOARD" in
      let* () = X11.Selection.set_owner x11 ~owner:(Some selection_window) ~timestamp:`CurrentTime primary
      and* () = X11.Selection.set_owner x11 ~owner:(Some clipboard_window) ~timestamp:`CurrentTime clipboard in
      Lwt.wakeup t.set_selection_window selection_window;
      Lwt.wakeup t.set_clipboard_window clipboard_window;
      Lwt.return_unit
    | _ ->
      failwith "Expected exactly one X11 root window!"

  let create ~relay ~seat ~x11 ~virtwl ~registry =
    let primary_selection_mgr = Wayland.Registry.bind registry @@ new Primary_mgr.v1 in
    let clipboard_mgr = Wayland.Registry.bind registry @@ new Clipboard_mgr.v1 in
    let selection_window, set_selection_window = Lwt.wait () in
    let clipboard_window, set_clipboard_window = Lwt.wait () in
    let wayland_primary_offer = ref None in
    let wayland_clipboard_offer = ref None in
    let primary_device = Primary_mgr.get_device primary_selection_mgr ~seat @@ primary_device ~wayland_primary_offer in
    let clipboard_device = Clipboard_mgr.get_data_device clipboard_mgr ~seat @@ clipboard_device ~wayland_clipboard_offer in
    {
      x11;
      virtwl;
      relay;
      awaiting_notify = Hashtbl.create 1;
      (* Primary: *)
      wayland_primary_offer;
      selection_window;
      set_selection_window;
      primary_selection_mgr;
      primary_device;
      (* Clipboard: *)
      wayland_clipboard_offer;
      clipboard_window;
      set_clipboard_window;
      clipboard_mgr;
      clipboard_device;
    }
end

type window_info = {
  title : string;
  window_type : [`Normal | `Dialog | `DnD | `Popup | `Unknown];
  wm_normal_hints : X11.Icccm.Wm_normal_hints.t;
  transient_for : X11.Window.t option;
  win_attrs : X11.Window.attributes;
  geometry : X11.Geometry.t;
}

(* Collect information about a new window we've been asked to manage. *)
let examine_window t window : window_info Lwt.t =
  let* x11 = t.x11 in
  let* title =
    let* wm_name = intern t "WM_NAME" in
    X11.Property.get_string x11 window wm_name >|= Option.value ~default:"<untitled>"
  and* window_type =
    let* net_wm_window_type = intern t "_NET_WM_WINDOW_TYPE" in
    X11.Property.get_atoms x11 window net_wm_window_type
  and* wm_normal_hints = X11.Icccm.get_wm_normal_hints x11 window
  and* win_attrs = X11.Window.get_attributes x11 window
  and* type_normal = intern t "_NET_WM_WINDOW_TYPE_NORMAL"
  and* type_dialog = intern t "_NET_WM_WINDOW_TYPE_DIALOG"
  and* type_dnd = intern t "_NET_WM_WINDOW_TYPE_DND"
  and* type_dropdown_menu = intern t "_NET_WM_WINDOW_TYPE_DROPDOWN_MENU"
  and* type_popup_menu = intern t "_NET_WM_WINDOW_TYPE_POPUP_MENU"
  and* transient_for =
    let* transient_for = intern t "WM_TRANSIENT_FOR" in
    X11.Property.get x11 window transient_for ~long_offset:0l ~long_length:1l >|= Option.map (fun info ->
        Cstruct.LE.get_uint32 info.X11.Property.value 0 |> X11.Xid.of_int
      )
  and* geometry = X11.Window.get_geometry x11 window
  in
  let window_type =
    let rec aux = function
      | [] -> `Unknown
      | ty :: _ when ty = type_normal -> `Normal
      | ty :: _ when ty = type_dialog -> `Dialog
      | ty :: _ when ty = type_dropdown_menu -> `Popup
      | ty :: _ when ty = type_popup_menu -> `Popup
      | ty :: _ when ty = type_dnd -> `DnD
      | _ :: tys -> aux tys
    in
    if window_type = [] then `Normal else aux window_type
  in
  Lwt.return {
    title;
    window_type;
    wm_normal_hints;
    win_attrs;
    transient_for;
    geometry;
  }

(* Set the toplevel role for an xdg_surface. *)
let init_toplevel t ~x11 ~xdg_surface ~info ~paired window =
  let toplevel = Xdg_surface.get_toplevel xdg_surface @@ object
      inherit [_] Xdg_toplevel.v1

      method on_configure _ ~width ~height ~states:_ =
        let width = Int32.to_int width in
        let height = Int32.to_int height in
        if width > 0 && height > 0 then (
          Lwt.async (fun () ->
              let (width, height) = scale_to_client t (width, height) in
              (paired:paired).geometry <- { paired.geometry with width; height };
              X11.Window.configure x11 window ~width ~height ~border_width:0
            )
        )

      method on_close _ =
        Lwt.async (fun () ->
            let* x11 = t.x11 in
            let* wm_protocols = X11.Atom.intern x11 "WM_PROTOCOLS"
            and* wm_delete_window = X11.Atom.intern x11 "WM_DELETE_WINDOW" in
            let* protocols = X11.Property.get_atoms x11 window wm_protocols in
            if List.mem wm_delete_window protocols then (
              let data = Cstruct.create 8 in
              Cstruct.LE.set_uint32 data 0 (wm_delete_window :> int32);
              Cstruct.LE.set_uint32 data 4 0l;        (* TODO: timestamp *)
              X11.Window.send_client_message x11 window ~fmt:32 ~propagate:false ~event_mask:0l ~ty:wm_protocols data;
            ) else (
              Log.info (fun f -> f "DestroyWindow");
              X11.Window.destroy x11 window
            )
          )
    end
  in
  t.decor_mgr |> Option.iter (fun decor_mgr ->
      let decor = Xdg_decor_mgr.get_toplevel_decoration decor_mgr ~toplevel @@ object
          inherit [_] Xdg_decoration.v1
          method on_configure _ ~mode:_ = ()
        end
      in
      Xdg_decoration.set_mode decor ~mode:Xdg_decoration.Mode.Server_side;
    );
  Xdg_toplevel.set_title toplevel ~title:(t.config.tag ^ info.title);
  X11.Icccm.Wm_normal_hints.min_size info.wm_normal_hints |> Option.iter (fun (width, height) ->
      let scale = Int32.of_int t.config.xunscale in
      let width = Int32.div width scale in
      let height = Int32.div height scale in
      Xdg_toplevel.set_min_size toplevel ~width ~height
    );
  toplevel

(* Set the popup role for an xdg_surface. *)
let init_popup t ~x11 ~xdg_surface ~info ~parent ~paired window =
  let positioner = Xdg_wm_base.create_positioner t.wm_base @@ new Xdg_positioner.v1 in
  let geometry = info.geometry in
  Log.debug (fun f -> f "Parent geom: %a" X11.Geometry.pp (parent:paired).geometry);
  Log.debug (fun f -> f "Popup geom: %a" X11.Geometry.pp geometry);
  let x = (geometry.x - parent.geometry.x) in
  let y = (geometry.y - parent.geometry.y) in
  let (x, y) = scale_to_host t (x, y) in
  let (width, height) = scale_to_host t (geometry.width, geometry.height) in
  Xdg_positioner.set_size positioner ~width:(Int32.of_int width) ~height:(Int32.of_int height);
  Xdg_positioner.set_anchor_rect positioner ~x:(Int32.of_int x) ~y:(Int32.of_int y) ~width:1l ~height:1l;
  Xdg_positioner.set_anchor positioner ~anchor:Xdg_positioner.Anchor.Top_left;
  Xdg_positioner.set_gravity positioner ~gravity:Xdg_positioner.Gravity.Bottom_right;
  let popup = Xdg_surface.get_popup xdg_surface ~parent:(Some parent.xdg_surface) ~positioner @@ object
      inherit [_] Xdg_popup.v1
      method on_configure _ ~x:_ ~y:_ ~width ~height =
        let width = Int32.to_int width in
        let height = Int32.to_int height in
        (* For override_redirect windows, let them use their preferred size.
           This may violate the Wayland spec if unscaling is being used, but Sway allows it and it looks much better. *)
        if width > 0 && height > 0 && not info.win_attrs.override_redirect then (
          let (width, height) = scale_to_client t (width, height) in
          Lwt.async (fun () ->
              (paired:paired).geometry <- { paired.geometry with width; height };
              X11.Window.configure x11 window ~width ~height ~border_width:0
            )
        )
      method on_popup_done _ = ()               (* todo: maybe notify the X application about this? *)
      method on_repositioned _ ~token:_ = ()
    end
  in
  Xdg_positioner.destroy positioner;
  popup

let last_event_surface t =
  match t.last_event_surface with
  | `Pointer -> t.pointer_surface
  | `Keyboard -> t.keyboard_surface

(* X window [window] corresponds to Wayland surface [host_surface].
   Create the window frame and add to [paired] and [of_host_surface].
   Note that [host_surface] may have already been destroyed by the time we get here. *)
let pair t ~set_configured ~host_surface window =
  let* x11 = t.x11 in
  let* () =
    (* Get notified of title changes *)
    X11.Window.create_attributes ~event_mask:[X11.Window.PropertyChange] ()
    |> X11.Window.change_attributes x11 window
  in
  let+ info = examine_window t window in
  if Proxy.can_send host_surface then (
    let parent = Option.bind info.transient_for (Hashtbl.find_opt t.paired) in
    let xdg_surface = Xdg_wm_base.get_xdg_surface t.wm_base ~surface:host_surface @@ object
        inherit [_] Xdg_surface.v1
        method on_configure proxy ~serial =
          if Proxy.can_send proxy then Xdg_surface.ack_configure proxy ~serial;
          set_configured (
            if info.window_type = `Normal && info.win_attrs.override_redirect then `Hide else `Show
          )
      end in
    let paired = {
      window;
      xdg_surface;
      geometry = info.geometry;
      xdg_role = `None;
      override_redirect = info.win_attrs.override_redirect;
    } in
    Hashtbl.add t.paired window paired;
    Hashtbl.add t.of_host_surface (Wayland.Proxy.id host_surface) paired;
    let fallback_parent = if parent = None then last_event_surface t else parent in
    match info.window_type, fallback_parent with
    | (`Normal | `Dialog), _
    | _, None ->  (* (if we don't have a parent, then we must make it a top-level) *)
      Log.info (fun f -> f "Open %a as top-level" pp_paired paired);
      let toplevel = init_toplevel t ~x11 ~xdg_surface ~info ~paired window in
      paired.xdg_role <- `Toplevel toplevel;
      let parent = if info.window_type = `Normal then parent else fallback_parent in
      parent |> Option.iter (fun parent ->
          match parent.xdg_role with
          | `Toplevel parent -> Xdg_toplevel.set_parent toplevel ~parent:(Some parent)
          | _ -> Log.info (fun f -> f "Parent %a is not a toplevel!" pp_paired parent)
        );
      Wayland.Wayland_client.Wl_surface.commit host_surface
    | (`DnD | `Popup | `Unknown), Some parent ->
      Log.info (fun f -> f "Open %a as popup" pp_paired paired);
      let popup = init_popup t ~x11 ~xdg_surface ~info ~parent ~paired window in
      paired.xdg_role <- `Popup popup;
      Wayland.Wayland_client.Wl_surface.commit host_surface
  ) else (
    Log.info (fun f -> f "%a destroyed while we were examining the X11 window properties!" Proxy.pp host_surface)
  )

(* We got an X11 message saying X11 [window] corresponds to Wayland surface [wayland_id].
   Turn [wayland_id] into an xdg_surface. If we haven't seen that surface yet, wait until it appears
   on the Wayland socket. *)
let rec pair_when_ready ~x11 t window wayland_id =
  match Hashtbl.find_opt t.unpaired wayland_id with
  | None ->
    Log.info (fun f -> f "Unknown Wayland object %ld; waiting for surface to be created..." wayland_id);
    let* () = Lwt_condition.wait t.unpaired_added in
    pair_when_ready ~x11 t window wayland_id
  | Some { client_surface = _; host_surface; set_configured } ->
    Log.info (fun f -> f "Setting up Wayland surface %ld using X11 window %a" wayland_id X11.Xid.pp window);
    Hashtbl.remove t.unpaired wayland_id;
    Lwt.async (fun () -> pair t ~set_configured ~host_surface window);
    Lwt.return_unit

let unpair t ~host_surface paired =
  begin match paired.xdg_role with
    | `Popup role -> Xdg_popup.destroy role
    | `Toplevel role -> Xdg_toplevel.destroy role
    | `None -> ()
  end;
  Xdg_surface.destroy paired.xdg_surface;
  Hashtbl.remove t.of_host_surface (Proxy.id host_surface);
  Hashtbl.remove t.paired paired.window;
  begin match t.pointer_surface with
    | Some p when p == paired ->
      t.pointer_surface <- None;
      t.last_event_surface <- `Keyboard
    | _ -> ()
  end;
  begin match t.keyboard_surface with
    | Some p when p == paired ->
      t.keyboard_surface <- None;
      t.last_event_surface <- `Pointer
    | _ -> ()
  end

module Input = struct
  (* Before sending a pointer event to Xwayland, we must make sure that the window
     is at the top of the stack. The raise is sent over the X11 connection, so we
     need to wait to be sure it has arrived before sending any further Wayland
     events.

     Similarly, we need to send SetInputFocus requests on keyboard focus. *)

  type nonrec t = {
    xwayland : t;
    mutable top_window : paired option;
    mutable focus_window : paired option;
  }

  (* There is never more than one instance of this running at a time because
     events from the compositor are paused while it's running. *)
  let ensure_topmost ~x11 t paired =
    match t.top_window with
    | Some x when x.window = paired.window -> Lwt.return_unit            (* Already on top *)
    | _ ->
      (* Ensure any previous pointer events (for the old surface) have been delivered: *)
      let* () = t.xwayland.wayland_ping () in
      t.top_window <- None;
      (* let* () = Lwt_unix.sleep 2.0 in *)
      let+ e = X11.Window.configure_checked x11 paired.window ~stack_mode:`Above in
      match e with
      | Ok () ->
        t.top_window <- Some paired
      | Error err ->
        (* Probably the window got destroyed. That's fine. Xwayland should discard the events.
           We're really just using "_check" to ensure we did a round-trip. *)
        Log.info (fun f -> f "Error raising window: %a" X11.Error.pp_code err)

  (* There is never more than one instance of this running at a time because
     events from the compositor are paused while it's running. *)
  let ensure_focus ~x11 t paired =
    match t.focus_window with
    | Some x when x.window = paired.window -> Lwt.return_unit            (* Already has focus *)
    | _ ->
      (* Ensure any previous keyboard events (for the old surface) have been delivered: *)
      let* () = t.xwayland.wayland_ping () in
      t.focus_window <- None;
      (* let* () = Lwt_unix.sleep 2.0 in *)
      let+ e = X11.Window.set_input_focus_checked x11 (`Window paired.window)
          ~revert_to:`PointerRoot ~time:`CurrentTime in
      match e with
      | Ok () ->
        t.focus_window <- Some paired
      | Error err ->
        (* Probably the window got destroyed. That's fine. Xwayland should discard the events.
           We're really just using "_check" to ensure we did a round-trip. *)
        Log.info (fun f -> f "Error giving focus to window: %a" X11.Error.pp_code err)

  let surface_destroyed t paired =
    match t.top_window with
    | Some p when p == paired -> t.top_window <- None
    | _ -> ()

  let make xwayland =
    {
      xwayland;
      top_window = None;
      focus_window = None;
    }

  let on_pointer_entry t ~surface ~forward_event =
    (* Fmt.pr "Entry: %a@." Relay.dump t.xwayland.relay; *)
    let paired = Hashtbl.find_opt t.xwayland.of_host_surface (Proxy.id surface) in
    t.xwayland.pointer_surface <- paired;
    t.xwayland.last_event_surface <- `Pointer;
    match paired with
    | None ->
      Log.warn (fun f -> f "Pointer entered unknown surface %a" Proxy.pp surface);
      forward_event ()
    | Some paired ->
      Log.info (fun f -> f "Pausing to raise X11 window");
      Relay.set_from_host_paused t.xwayland.relay true;
      Lwt.async (fun () ->
          let* x11 = t.xwayland.x11 in
          (* Raise the target X11 window so that it will get the following events: *)
          let* () = ensure_topmost ~x11 t paired in
          (* Now resume event delivery, starting with the delayed pointer enter event: *)
          forward_event ();
          Log.info (fun f -> f "Window raised; unpausing");
          Relay.set_from_host_paused t.xwayland.relay false;
          Lwt.return_unit
        )

  let on_keyboard_entry t ~surface ~forward_event =
    let paired = Hashtbl.find_opt t.xwayland.of_host_surface (Proxy.id surface) in
    t.xwayland.keyboard_surface <- paired;
    t.xwayland.last_event_surface <- `Keyboard;
    match paired with
    | None ->
      Log.warn (fun f -> f "Keyboard entered unknown surface %a" Proxy.pp surface);
      forward_event ()
    | Some paired ->
      Log.info (fun f -> f "Pausing to focus X11 window");
      Relay.set_from_host_paused t.xwayland.relay true;
      Lwt.async (fun () ->
          let* x11 = t.xwayland.x11 in
          (* Focus the target X11 window so that it will get the following events: *)
          let* () = ensure_focus ~x11 t paired in
          (* Now resume event delivery, starting with the delayed keyboard enter event: *)
          forward_event ();
          Log.info (fun f -> f "Window has focus; unpausing");
          Relay.set_from_host_paused t.xwayland.relay false;
          Lwt.return_unit
        )
end

(* Get Xwayland ready to run and become the window manager on each screen
   (there will probably only ever be one).
   @param xrdb use this as the initial xrdb configuration.
   @param selection use this to initialise selection proxying *)
let initialise_x ~xrdb ~selection t =
  let* x11 = t.x11 in
  let* composite = X11.Composite.init x11 in
  (* Take ownership of the selections *)
  let* () = Selection.init_x11 selection in
  X11.Window.roots x11 |> Lwt_list.iteri_p (fun i root ->
      (* Enable the Composite extension.
         By default, X just asks clients to draw to the root window as needed, but we're not using a root window.
         Composite instead allocates a buffer to store each window's data, which can then be shared with the
         Wayland compositor. *)
      let* () = X11.Composite.redirect_subwindows composite ~window:root ~update:`Manual in
      (* Load the default cursor image *)
      let* cursor_font = X11.Font.open_font x11 "cursor" in
      let* default_cursor = X11.Font.create_glyph_cursor x11
          ~source_font:cursor_font ~mask_font:cursor_font
          ~source_char:68 ~mask_char:69
          ~bg:(0xffff, 0xffff, 0xffff)
          ~fg:(0, 0, 0)
      in
      (* Enable substructure redirects on the root.
         This means we get notified when new windows are mapped, and we receive a message telling
         us the corresponding Wayland surface for each X window. *)
      let event_mask = X11.Window.[SubstructureNotify; SubstructureRedirect] in
      let* () =
        X11.Window.create_attributes ~event_mask ~cursor:default_cursor ()
        |> X11.Window.change_attributes x11 root
      in
      (* Initialise xrdb *)
      let* atom_string = intern t "STRING"
      and* atom_resource_manager = intern t "RESOURCE_MANAGER" in
      let* () = X11.Property.set_string x11 root atom_resource_manager xrdb ~ty:atom_string in
      (* Become the window manager. This allows other clients to connect. *)
      let* wm_sn = intern t ~only_if_exists:false ("WM_S" ^ string_of_int i) in
      X11.Selection.set_owner x11 ~owner:(Some root) ~timestamp:`CurrentTime wm_sn
    )

(* We've just spawned the Xwayland process. Run the X event loop. *)
let listen_x11 ~selection t =
  let* x11 = t.x11 in
  let wl_surface_id = intern t "WL_SURFACE_ID" ~only_if_exists:false in
  (* The event handler is used to handle event message received from Xwayland. *)
  let event_handler = object (_ : X11.Event.handler)
    method client_message ~window ~ty body =
      let* wl_surface_id = wl_surface_id in
      if ty = wl_surface_id then (
        let wayland_id = Cstruct.LE.get_uint32 body 0 in
        Log.info (fun f -> f "X window %a corresponds to Wayland surface %ld" X11.Window.pp window wayland_id);
        (* Note: this blocks the X11 event loop until the corresponding Wayland event arrives: *)
        pair_when_ready ~x11 t window wayland_id
      ) else (
        Log.info (fun f -> f "ClientMessage on window %a (type=%a): %a" X11.Window.pp window (X11.Atom.pp x11) ty Cstruct.hexdump_pp body);
        Lwt.return_unit
      )

    method selection_request = Selection.selection_request selection
    method selection_clear = Selection.selection_clear selection
    method selection_notify = Selection.selection_notify selection

    method map_request ~window =
      (* Put new windows at the bottom of the stack so they don't interfere with the active window *)
      let* () = X11.Window.configure x11 window ~stack_mode:`Below in
      X11.Window.map x11 window

    method configure_request ~window ~width ~height =
      match Hashtbl.find_opt t.paired window with
      | None ->
        (* In theory, we must ensure the size is a multiple of the scale factor, as the Wayland spec requires this.
           However, this makes some windows look ugly, and Sway seems to allow any size. *)
        (* let (width, height) = scale_to_host t (width, height) |> scale_to_client t in *)
        X11.Window.configure x11 window ~width ~height ~border_width:0
        (* todo: send a synthetic ConfigureNotify event if nothing changed *)
      | Some p ->
        (* For now, don't allow apps to change their own size once mapped. *)
        Log.info (fun f -> f "Refusing ConfigureRequest for already-mapped window %a" pp_paired p);
        X11.Window.configure_notify x11 ~event:window ~window
          ~above_sibling:None
          ~geometry:p.geometry
          ~border_width:0
          ~override_redirect:p.override_redirect

    method property_notify ~window ~atom ~time:_ ~state =
      Log.info (fun f -> f "PropertyNotify: %a/%a %s" X11.Window.pp window (X11.Atom.pp x11) atom
                   (match state with `NewValue -> "has new value" | `Deleted -> "deleted"));
      Lwt.async (fun () ->
          let* wm_name = intern t "WM_NAME" in
          if atom = wm_name then (
            match Hashtbl.find_opt t.paired window with
            | Some { xdg_role = `Toplevel toplevel; _ } ->
              let* title = X11.Property.get_string x11 window wm_name >|= Option.value ~default:"<untitled>" in
              if Proxy.can_send toplevel then
                Xdg_toplevel.set_title toplevel ~title:(t.config.tag ^ title);
              Lwt.return_unit
            | _ ->
              Lwt.return_unit
          ) else (
            Lwt.return_unit
          )
        );
      Lwt.return_unit
  end in
  X11.Event.listen x11 event_handler

let no_wayland_ping_warning = lazy (
  Log.warn (fun f -> f "Xwayland didn't connect an xdg_wm_base - can't sync reliably!")
)

let no_wayland_ping () =
  Lazy.force no_wayland_ping_warning;
  Lwt_unix.sleep 0.01

let string_of_fd (fd : Lwt_unix.file_descr) =
  let fd : int = Obj.magic (Lwt_unix.unix_file_descr fd : Unix.file_descr) in
  string_of_int fd

(* Exec Xwayland (note: this function runs in a forked child process).
   @param display X11 display number
   @param remote_wayland Xwayland's end of the Wayland protocol socket
   @param remote_wm_socket Xwayland's end of the X11 protocol socket
   @param listen_socket the X11 socket where clients connect *)
let exec_xwayland config ~display ~remote_wayland ~remote_wm_socket ~listen_socket =
  try
    Lwt_main.Exit_hooks.remove_all ();
    let cmd = [|
      (* "rr"; "record"; *)
      config.Config.xwayland_binary;
      "-nolisten"; "tcp";
      "-rootless";
      "-shm";
      "-listen"; string_of_fd listen_socket;
      (* "-verbose"; "9"; *)
      "-wm"; string_of_fd remote_wm_socket;
      Printf.sprintf ":%d" display
    |] in
    Unix.putenv "WAYLAND_SOCKET" (string_of_fd remote_wayland);
    Lwt_unix.clear_close_on_exec remote_wayland;
    Lwt_unix.clear_close_on_exec listen_socket;
    Lwt_unix.clear_close_on_exec remote_wm_socket;
    Unix.execvp cmd.(0) cmd
  with ex ->
    Format.eprintf "Fork error: %a@." Fmt.exn ex;
    exit 1

let monitor name thread =
  Lwt.try_bind thread
    (fun () -> Log.info (fun f -> f "%s finished" name); Lwt.return_unit)
    (fun ex -> Log.warn (fun f -> f "%s failed: %a" name Fmt.exn ex); Lwt.return_unit)

(* We've just spawned an Xwayland process.
   Talk to it using the Wayland and X11 protocols. *)
let handle_xwayland ~config ~local_wayland ~local_wm_socket =
  let x11 = X11.Display.connect local_wm_socket in
  let* relay = Relay.create config in
  let virtwl = Relay.virtwl relay in
  let registry = Relay.registry relay in
  let wm_base = Wayland.Registry.bind registry @@ object
      inherit [_] Xdg_wm_base.v1
      method on_ping = Xdg_wm_base.pong
    end
  in
  let seat = Wayland.Registry.bind registry @@ object
      inherit [_] Wl_seat.v1
      method on_name _ ~name:_ = ()
      method on_capabilities _ ~capabilities:_ = ()
    end
  in
  let decor_mgr =
    try Some (Wayland.Registry.bind registry @@ new Xdg_decor_mgr.v1)
    with ex ->
      Log.warn (fun f -> f "Can't get decoration manager: %a" Fmt.exn ex);
      None
  in
  let selection = Selection.create ~seat ~x11 ~virtwl ~registry ~relay in
  let t = {
    relay;
    x11;
    config;
    wm_base;
    decor_mgr;
    unpaired = Hashtbl.create 5;
    unpaired_added = Lwt_condition.create ();
    paired = Hashtbl.create 5;
    of_host_surface = Hashtbl.create 5;
    pointer_surface = None;
    keyboard_surface = None;
    last_event_surface = `Keyboard;
    wayland_ping = no_wayland_ping;
  } in
  let input = Input.make t in
  let xwayland = object (_ : Relay.xwayland_hooks)
    method on_pointer_entry = Input.on_pointer_entry input
    method on_keyboard_entry = Input.on_keyboard_entry input

    method on_create_surface host_surface client_surface ~set_configured =
      Log.info (fun f -> f "%a created by Xwayland (host=%a)"
                   Proxy.pp client_surface
                   Proxy.pp host_surface);
      let client_surface = (client_surface :> client_surface) in
      let host_surface = (host_surface :> host_surface) in
      let client_surface_id = Proxy.id client_surface in
      Hashtbl.add t.unpaired client_surface_id { client_surface; host_surface; set_configured };
      Lwt_condition.broadcast t.unpaired_added ();
      Lwt.async (fun () ->
          let* x11 = t.x11 in
          let* () = X11.Display.sync x11 in
          (* If we haven't received a pairing message by now, then this isn't for us. *)
          if Hashtbl.mem t.unpaired client_surface_id then (
            Log.info (fun f -> f "%a doesn't correspond to an X11 window" Proxy.pp client_surface);
            Hashtbl.remove t.unpaired client_surface_id;
            set_configured `Unmanaged
          );
          Lwt.return_unit
        )

    method on_destroy_surface host_surface =
      match Hashtbl.find_opt t.of_host_surface (Proxy.id host_surface) with
      | None -> ()      (* If it's still in [unpaired], another thread will remove it later. *)
      | Some paired ->
        Input.surface_destroyed input paired;
        unpair t ~host_surface paired

    method set_ping fn =
      t.wayland_ping <- fn

    method scale = Int32.of_int t.config.xunscale
  end in
  let xrdb = String.concat "\n" config.xrdb in
  Lwt.join [
    monitor "Xwayland Wayland thread" (fun () -> Relay.accept relay ~xwayland local_wayland);
    monitor "Xwayland X11 thread"     (fun () -> listen_x11 ~selection t);
    monitor "Xwayland WM init thread" (fun () -> initialise_x ~xrdb ~selection t);
  ]

let with_socket_pair fn =
  let local, remote = Lwt_unix.(socketpair PF_UNIX SOCK_STREAM 0) in
  Lwt_unix.set_close_on_exec local;
  Lwt_unix.set_close_on_exec remote;
  Lwt.finalize
    (fun () -> fn ~local ~remote)
    (fun () ->
       let* () = Lwt_unix.close local in
       match Lwt_unix.state remote with
       | Opened -> Lwt_unix.close remote
       | Closed | Aborted _ -> Lwt.return_unit
    )

let spawn_and_run_xwayland ~config ~display listen_socket =
  (* Set up connections between us and Xwayland: *)
  with_socket_pair @@ fun ~local:local_wm_socket ~remote:remote_wm_socket ->
  with_socket_pair @@ fun ~local:local_wayland ~remote:remote_wayland ->
  (* Spawn Xwayland child process: *)
  match Lwt_unix.fork () with
  | 0 ->
    (* We are the child *)
    exec_xwayland config ~display ~remote_wayland ~listen_socket ~remote_wm_socket
  | xwayland_pid ->
    (* We are the parent *)
    Lwt.finalize
      (fun () ->
         let* () = Lwt_unix.close remote_wm_socket in
         let* () = Lwt_unix.close remote_wayland in
         Lwt.catch
           (fun () ->
             handle_xwayland ~config ~local_wayland ~local_wm_socket
           )
           (fun ex ->
              Log.warn (fun f -> f "X11 WM failed: %a" Fmt.exn ex);
              Lwt.return_unit
           )
      )
      (fun () ->
         let* (_, status) = Lwt_unix.waitpid [] xwayland_pid in
         Log.info (fun f -> f "Xwayland process ended (%a)" Trace.pp_status status);
         Lwt.return_unit
      )

let listen ~config ~display listen_socket =
  let rec aux () =
    Log.info (fun f -> f "Waiting for X11 clients");
    let* () = Lwt_unix.wait_read listen_socket in
    Log.info (fun f -> f "X client detected - launching %S..." config.Config.xwayland_binary);
    let t0 = Unix.gettimeofday () in
    let* () = spawn_and_run_xwayland  ~config ~display listen_socket in
    let delay = min_respawn_time -. (Unix.gettimeofday () -. t0) in
    if delay > 0.0 then (
      Log.info (fun f -> f "Xwayland died too quickly... waiting a bit before retrying...");
      let* () = Lwt_unix.sleep delay in
      aux ()
    ) else (
      aux ()
    )
  in
  aux ()
