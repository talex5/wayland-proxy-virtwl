open Eio.Std

module Read = Eio.Buf_read

open Types

[%%cenum
  type code =
    | Error [@id 0]
    | Reply
    | KeyPress
    | KeyRelease
    | ButtonPress
    | ButtonRelease
    | MotionNotify
    | EnterNotify
    | LeaveNotify
    | FocusIn
    | FocusOut
    | KeymapNotify
    | Expose
    | GraphicsExposure
    | NoExposure
    | VisibilityNotify
    | CreateNotify
    | DestroyNotify
    | UnmapNotify
    | MapNotify
    | MapRequest
    | ReparentNotify
    | ConfigureNotify
    | ConfigureRequest
    | GravityNotify
    | ResizeRequest
    | CirculateNotify
    | CirculateRequest
    | PropertyNotify
    | SelectionClear
    | SelectionRequest
    | SelectionNotify
    | ColormapNotify
    | ClientMessage
    | MappingNotify
  [@@uint8_t]
]

[%%cstruct
  type event = {
    code : uint8_t;
    (* All except KeymapNotify then have the following: *)
    detail : uint8_t;
    seq : uint16_t;
  } [@@little_endian]
]

[%%cstruct
  type reply = {
    code : uint8_t;
    detail : uint8_t;
    seq : uint16_t;
    length : uint32_t;
  } [@@little_endian]
]

exception X11_read_error of exn
let () =
  Printexc.register_printer (function
      | X11_read_error ex -> Some (Fmt.str "Error reading from X11 WM socket: %a" Fmt.exn ex)
      | _ -> None
    )

let read x =
  try
    let msg = Wire.read_exactly 32 x in
    match get_event_code msg with
    | 0 -> `Error msg
    | 1 ->
      let extra_len = Int32.to_int (get_reply_length msg) in
      let extra =
        if extra_len > 0 then Wire.read_exactly (extra_len * 4) x
        else Cstruct.empty
      in
      `Reply (Cstruct.append msg extra)
    | event ->
      match int_to_code (event land 0x7f) with
      | Some ev -> `Event (ev, msg)
      | None -> `Unknown_event event
  with ex -> raise (X11_read_error ex)

type handler = <
  map_request : window:window -> unit;

  configure_request :
    window:window -> width:int -> height:int -> unit;

  client_message :
    window:window -> ty:atom -> Cstruct.t -> unit;

  selection_request :
    time:timestamp -> owner:window -> requestor:window ->
    selection:atom -> target:atom -> property:atom option -> unit;

  selection_clear :
    time:timestamp -> owner:window -> selection:atom -> unit;

  selection_notify :
    time:timestamp -> requestor:window -> selection:atom -> target:atom -> property:atom option -> unit;

  property_notify :
    window:window -> atom:atom -> time:timestamp -> state:[`NewValue | `Deleted] -> unit;
>

module MapRequest = struct
  [%%cstruct
    type t = {
      opcode : uint8_t;
      pad0 : uint8_t;
      seq : uint16_t;
      parent : uint32_t;
      window : uint32_t;
      unused : uint8_t [@len 20];
    } [@@little_endian]
  ]
end

module PropertyNotify = struct
  [%%cstruct
    type t = {
      opcode : uint8_t;
      pad0 : uint8_t;
      seq : uint16_t;
      window : uint32_t;
      atom : uint32_t;
      time : uint32_t;
      state : uint8_t;
      unused : uint8_t [@len 15];
    } [@@little_endian]
  ]
end

module SelectionRequest = struct
  [%%cstruct
    type t = {
      major_opcode : uint8_t;
      unused : uint8_t;
      seq : uint16_t;
      time : uint32_t;
      owner : uint32_t;
      requestor : uint32_t;
      selection : uint32_t;
      target : uint32_t;
      property : uint32_t;
      unused2 : uint32_t;
    } [@@little_endian]
  ]
end

module SelectionClear = struct
  [%%cstruct
    type t = {
      major_opcode : uint8_t;
      unused : uint8_t;
      seq : uint16_t;
      time : uint32_t;
      owner : uint32_t;
      selection : uint32_t;
      unused2 : uint8_t [@len 16];
    } [@@little_endian]
  ]
end

(* Read the next event from X. If [need_sync] is set (or becomes set while waiting), send a sync. *)
let get_event t =
  let x_event = Fiber.fork_promise ~sw:t.sw (fun () -> read t.from_server) in
  let rec aux () =
    if t.need_sync then Atom.Intern.send_sync t;
    match
      Fiber.first
        (fun () -> Promise.await_exn x_event)
        (fun () -> Eio.Condition.await_no_mutex t.wake_input; `Check_sync_flag)
    with
    | `Check_sync_flag -> aux ()
    | `Event _ | `Error _ | `Reply _ | `Unknown_event _ as x -> x
  in
  aux ()

(* Find the waker for request with sequence number [seq].
   Wake any previous handlers with [`No_reply]. *)
let rec wake t seq msg =
  match Queue.take_opt t.pending with
  | None ->
    Log.warn (fun f -> f "Reply to unknown sequence code %d" seq)
  | Some (expected, resolver) ->
    if expected = seq then Promise.resolve resolver msg
    else (
      Log.debug (fun f -> f "No reply for message %d" expected);
      Promise.resolve resolver `No_reply;
      wake t seq msg
    )

let listen t (handler:handler) =
  let handle_event () =
    match get_event t with
    | `Reply msg ->
      let seq = get_reply_seq msg in
      Log.debug (fun f -> f "@[<v2>Reply to %d@,%a@]" seq Cstruct.hexdump_pp msg);
      wake t seq (`Reply msg)
    | `Error msg ->
      let seq = Error.get_error_seq msg in
      Log.debug (fun f -> f "@[<v2>Error for %d@,%a@]" seq Cstruct.hexdump_pp msg);
      wake t seq (`Error msg)
    | `Event (MapRequest, body) ->
      Log.info (fun f -> f "Got MapRequest");
      let window = MapRequest.get_t_window body in
      handler#map_request ~window
    | `Event (ConfigureRequest, body) ->
      Log.info (fun f -> f "Got ConfigureRequest");
      let window = Window.ConfigureRequest.get_t_window body in
      let width = Window.ConfigureRequest.get_t_width body in
      let height = Window.ConfigureRequest.get_t_height body in
      handler#configure_request ~window ~width ~height
    | `Event (SelectionRequest, body) ->
      let time = SelectionRequest.get_t_time body in
      let owner = SelectionRequest.get_t_owner body in
      let requestor = SelectionRequest.get_t_requestor body in
      let selection = SelectionRequest.get_t_selection body in
      let target = SelectionRequest.get_t_target body in
      let property = SelectionRequest.get_t_property body in
      Log.info (fun f -> f "Got SelectionRequest(%a) for target %a" (Atom.pp t) selection (Atom.pp t) target);
      let property = if property = 0l then None else Some property in
      handler#selection_request ~time ~owner ~requestor ~selection ~target ~property
    | `Event (SelectionNotify, body) ->
      let time = Selection.Notify.get_req_time body in
      let requestor = Selection.Notify.get_req_requestor body in
      let selection = Selection.Notify.get_req_selection body in
      let target = Selection.Notify.get_req_target body in
      let property = Selection.Notify.get_req_property body in
      let property = if property = 0l then None else Some property in
      Log.info (fun f -> f "Got SelectionNotify: %a/%a (%a)"
                   Xid.pp requestor
                   Fmt.(option ~none:(any "-") (Atom.pp t)) property
                   (Atom.pp t) target);
      handler#selection_notify ~time ~requestor ~selection ~target ~property
    | `Event (PropertyNotify, body) ->
      let window = PropertyNotify.get_t_window body in
      let atom = PropertyNotify.get_t_atom body in
      let time = PropertyNotify.get_t_time body in
      let state = if PropertyNotify.get_t_state body = 0 then `NewValue else `Deleted in
      handler#property_notify ~window ~atom ~time ~state
    | `Event (SelectionClear, body) ->
      let time = SelectionClear.get_t_time body in
      let owner = SelectionClear.get_t_owner body in
      let selection = SelectionClear.get_t_selection body in
      handler#selection_clear ~time ~owner ~selection
    | `Event (ClientMessage, body) ->
      let window = Window.ClientMessage.get_t_window body in
      let ty = Window.ClientMessage.get_t_ty body in
      handler#client_message ~window ~ty (Cstruct.shift body Window.ClientMessage.sizeof_t)
    | `Event (x, _body) ->
      Log.info (fun f -> f "Got event %s" (code_to_string x))
    | `Unknown_event x ->
      Log.info (fun f -> f "Skipping unknown X11 message type %d" x)
  in
  let rec aux () = handle_event (); aux () in
  aux ()
