type t = Types.window
type cursor = Types.cursor
type visual = Types.visual

let pp = Xid.pp

let roots t = t.Types.roots

[%%cenum
  type event =
    | KeyPress [@id 0x00000001]
    | KeyRelease [@id 0x00000002]
    | ButtonPress [@id 0x00000004]
    | ButtonRelease [@id 0x00000008]
    | EnterWindow [@id 0x00000010]
    | LeaveWindow [@id 0x00000020]
    | PointerMotion [@id 0x00000040]
    | PointerMotionHint [@id 0x00000080]
    | Button1Motion [@id 0x00000100]
    | Button2Motion [@id 0x00000200]
    | Button3Motion [@id 0x00000400]
    | Button4Motion [@id 0x00000800]
    | Button5Motion [@id 0x00001000]
    | ButtonMotion [@id 0x00002000]
    | KeymapState [@id 0x00004000]
    | Exposure [@id 0x00008000]
    | VisibilityChange [@id 0x00010000]
    | StructureNotify [@id 0x00020000]
    | ResizeRedirect [@id 0x00040000]
    | SubstructureNotify [@id 0x00080000]
    | SubstructureRedirect [@id 0x00100000]
    | FocusChange [@id 0x00200000]
    | PropertyChange [@id 0x00400000]
    | ColormapChange [@id 0x00800000]
    | OwnerGrabButton [@id 0x01000000]
  [@@uint32_t]
]

let mask_of_list = List.fold_left (fun acc x -> Int32.logor acc (event_to_int x)) 0l

type stack_mode = [
  | `Above
  | `Below
  | `TopIf
  | `BottomIf
  | `Opposite
]

type create_attributes = {
  bitmask : int32;
  attrs : int32 list;
}

let create_attributes ?cursor ?event_mask () =
  let bitmask = ref 0l in
  let opt32 mask = function
    | None -> []
    | Some v ->
      bitmask := Int32.logor !bitmask mask;
      [v]
  in
  let attrs = List.concat [
      opt32 0x800l (Option.map mask_of_list event_mask);
      opt32 0x4000l cursor;
    ]
  in
  { bitmask = !bitmask; attrs }

let add_attrs cs attrs =
  attrs.attrs |> List.iteri (fun i v ->
      Cstruct.LE.set_uint32 cs (i * 4) v
    )

module Create = struct
  [%%cstruct
    type req = {
      wid : uint32_t;
      parent : uint32_t;
      x : int16_t;
      y : int16_t;
      width : uint16_t;
      height : uint16_t;
      border_width : uint16_t;
      window_class : uint16_t;
      visual : uint32_t;
      bitmask : uint32_t;
      (* uint32_t values, one for each bit set in [bitmask]. *)
    } [@@little_endian]
  ]

  let send t ~parent ~geometry ~border_width ~window_class ?visual attrs =
    let id = Display.mint_id t in
    let depth = 0 in
    let n_attrs = List.length attrs.attrs in
    Log.info (fun f -> f "CreateWindow %a at %a" pp id Geometry.pp geometry);
    Request.send_only t ~major:1 ~minor:depth (sizeof_req + 4 * n_attrs) (fun r ->
        set_req_wid r id;
        set_req_parent r parent;
        set_req_x r geometry.x;
        set_req_y r geometry.y;
        set_req_width r geometry.width;
        set_req_height r geometry.height;
        set_req_border_width r border_width;
        set_req_window_class r
          (match window_class with
           | `CopyFromParent -> 0
           | `InputOutput -> 1
           | `InputOnly -> 2
          );
        set_req_visual r (Option.value visual ~default:0l);
        set_req_bitmask r attrs.bitmask
      );
    id
end

let create_input_only t ~parent ~geometry ?visual attrs =
  Create.send t ~parent ~geometry ~border_width:0 ~window_class:`InputOnly ?visual attrs

module ChangeAttributes = struct
  [%%cstruct
    type request = {
      window : uint32_t;
      bitmask : uint32_t;
      (* uint32_t values, one for each bit set in [bitmask]. *)
    } [@@little_endian]
  ]

  let send t window attrs =
    Log.info (fun f -> f "ChangeWindowAttributes on %a" pp window);
    let n_attrs = List.length attrs.attrs in
    Request.send_only t ~major:2 (sizeof_request + 4 * n_attrs) @@ fun r ->
    set_request_window r window;
    add_attrs (Cstruct.shift r sizeof_request) attrs;
    set_request_bitmask r attrs.bitmask
end

let change_attributes = ChangeAttributes.send

module GetGeometry = struct
  [%%cstruct
    type request = {
      window : uint32_t;
    } [@@little_endian]
  ]

  [%%cstruct
    type reply = {
      code : uint8_t;
      depth : uint8_t;
      seq : uint16_t;
      length : uint32_t;
      root : uint32_t;
      x : int16_t;
      y : int16_t;
      width : uint16_t;
      height : uint16_t;
      border_width : uint16_t;
      unused : uint8_t [@len 10];
    } [@@little_endian]
  ]

  let send t window =
    let r = Request.send_exn t ~major:14 sizeof_request (fun r -> set_request_window r window) in
    let g = { Geometry.
      x = get_reply_x r |> Wire.signed16;
      y = get_reply_y r |> Wire.signed16;
      width = get_reply_width r;
      height = get_reply_height r;
    } in
    Log.info (fun f -> f "GetGeometry %a => %a" pp window Geometry.pp g);
    g
end

let get_geometry = GetGeometry.send

type attributes = {
  window_class : [`InputOnly | `InputOutput];
  override_redirect : bool;
}

let pp_attrs f { window_class; override_redirect } =
  Fmt.pf f "class:%s override_redirect:%b"
    (match window_class with `InputOnly -> "InputOnly" | `InputOutput -> "InputOutput")
    override_redirect

module GetAttributes = struct
  [%%cstruct
    type request = {
      window : uint32_t;
    } [@@little_endian]
  ]

  [%%cstruct
    type reply = {
      code : uint8_t;
      backing_store : uint8_t;
      seq : uint16_t;
      length : uint32_t;
      visual : uint32_t;
      window_class : uint16_t;
      bit_gravity : uint8_t;
      win_gravity : uint8_t;
      backing_planes : uint32_t;
      backing_pixel : uint32_t;
      save_under : uint8_t;
      map_is_installed : uint8_t;
      map_state : uint8_t;
      override_redirect : uint8_t;
      colormap : uint32_t;
      all_event_masks : uint32_t;
      your_event_masks : uint32_t;
      do_not_propagage_mask : uint16_t;
      unused : uint16_t;
    } [@@little_endian]
  ]

  let send t window =
    let r = Request.send_exn t ~major:3 sizeof_request (fun r -> set_request_window r window) in
    let window_class =
      match get_reply_window_class r with
      | 1 -> `InputOutput
      | 2 -> `InputOnly
      | x -> Fmt.failwith "Unknown window class %d" x
    in
    let x = {
      window_class;
      override_redirect = get_reply_override_redirect r <> 0;
    } in
    Log.info (fun f -> f "GetWindowAttributes %a => %a" pp window pp_attrs x);
    x
end

let get_attributes = GetAttributes.send

module Map = struct
  [%%cstruct
    type request = {
      window : uint32_t;
    } [@@little_endian]
  ]

  let send t window =
    Log.info (fun f -> f "MapWindow %a" pp window);
    Request.send_only t ~major:8 sizeof_request @@ fun r ->
    set_request_window r window
end

let map = Map.send

module Destroy = struct
  [%%cstruct
    type request = {
      window : uint32_t;
    } [@@little_endian]
  ]

  let send t window =
    Log.info (fun f -> f "DestroyWindow %a" pp window);
    Request.send_only t ~major:4 sizeof_request @@ fun r ->
    set_request_window r window
end

let destroy = Destroy.send

module ClientMessage = struct
  [%%cstruct
    type t = {
      opcode : uint8_t;
      fmt : uint8_t;
      seq : uint16_t;
      window : uint32_t;
      ty : uint32_t;
      (* 20 bytes of data, according to [fmt] *)
    } [@@little_endian]
  ]

  let send t window ~propagate ~event_mask ~fmt ~ty data =
    Request.send_event t ~window ~propagate ~event_mask @@ fun r ->
    set_t_opcode r 33;
    set_t_fmt r fmt;
    set_t_window r window;
    set_t_ty r ty;
    Cstruct.blit data 0 r sizeof_t (Cstruct.length data)
end

let send_client_message = ClientMessage.send

module ConfigureRequest = struct
  [%%cstruct
    type t = {
      major_opcode : uint8_t;
      stack_mode : uint8_t;
      seq : uint16_t;
      parent : uint32_t;
      window : uint32_t;
      sibling : uint32_t;
      x : uint16_t;
      y : uint16_t;
      width : uint16_t;
      height : uint16_t;
      border_width : uint16_t;
      bitmask : uint16_t;
      (* List of values *)
    } [@@little_endian]
  ]
end

module ConfigureWindow = struct
  [%%cstruct
    type req = {
      window : uint32_t;
      bitmask : uint16_t;
      unused2: uint16_t;
      (* List of values *)
    } [@@little_endian]
  ]

  let pp_attrs ~bitmask f attrs =
    let attrs = ref attrs in
    let opt bit name =
      if bitmask land bit <> 0 then (
        match !attrs with
        | [] -> failwith "Bad bitmask!"
        | x :: xs ->
          Fmt.pf f " %s:%ld" name x;
          attrs := xs
      )
    in
    opt 0x01 "x";
    opt 0x02 "y";
    opt 0x04 "width";
    opt 0x08 "height";
    opt 0x10 "border_width";
    opt 0x20 "sibling";
    opt 0x40 "stack_mode"

  let int_of_stack_mode : stack_mode -> int = function
    | `Above    -> 0
    | `Below    -> 1     
    | `TopIf    -> 2     
    | `BottomIf -> 3     
    | `Opposite -> 4     

  let send checked ?x ?y ?width ?height ?border_width ?sibling ?stack_mode t window =
    let bitmask = ref 0 in
    let opt32 mask = function
      | None -> []
      | Some v ->
        bitmask := !bitmask lor mask;
        [v]
    in
    let opt mask v = opt32 mask (Option.map Int32.of_int v) in
    let attrs = List.concat [
        opt 0x01 x;
        opt 0x02 y;
        opt 0x04 width;
        opt 0x08 height;
        opt 0x10 border_width;
        opt32 0x20 sibling;
        opt 0x40 (Option.map int_of_stack_mode stack_mode);
      ]
    in
    let bitmask = !bitmask in
    let n_attrs = List.length attrs in
    Log.info (fun f -> f "ConfigureRequest %a%a" pp window (pp_attrs ~bitmask) attrs);
    Request.send_maybe_checked checked t ~major:12 (sizeof_req + 4 * n_attrs) @@ fun r ->
    set_req_window r window;
    attrs |> List.iteri (fun i v ->
        Cstruct.LE.set_uint32 r (sizeof_req + 4 * i) v
      );
    set_req_bitmask r bitmask
end

let configure = ConfigureWindow.send Request.Unchecked
let configure_checked = ConfigureWindow.send Request.Checked

module SetInputFocus = struct
  [%%cstruct
    type req = {
      focus : uint32_t;
      time : uint32_t;
    } [@@little_endian]
  ]

  let pp_focus f = function
    | `None -> Fmt.string f "None"
    | `PointerRoot -> Fmt.string f "PointerRoot"
    | `Window x -> pp f x

  let send_checked t ~revert_to ~time focus =
    Log.info (fun f -> f "SetInputFocus %a" pp_focus focus);
    let minor =
      match revert_to with
      | `None -> 0
      | `PointerRoot -> 1
      | `Parent -> 2
    in
    Request.send_checked t ~major:42 ~minor sizeof_req (fun r ->
        set_req_focus r (match focus with
            | `None -> 0l
            | `PointerRoot -> 1l
            | `Window x -> x
          );
        set_req_time r (match time with
            | `CurrentTime -> 0l
            | `Time time -> time
          )
      )
end

let set_input_focus_checked = SetInputFocus.send_checked

module ConfigureNotify = struct
  [%%cstruct
    type req = {
      code : uint8_t;
      unused : uint8_t;
      seq : uint16_t;   (* set by server *)
      event : uint32_t;
      window : uint32_t;
      above_sibling : uint32_t;
      x : int16_t;
      y : int16_t;
      width : uint16_t;
      height : uint16_t;
      border_width : uint16_t;
      override_redirect : uint8_t;
      unused2 : uint8_t [@len 5];
    } [@@little_endian]
  ]

  let send t ~event ~window ~above_sibling ~geometry ~border_width ~override_redirect =
    Log.info (fun f -> f "ConfigureNotify(%a/%a): above:%a %a"
                 pp event
                 pp window
                 (Fmt.Dump.option pp) above_sibling
                 Geometry.pp geometry);
    Request.send_event t ~window ~propagate:false ~event_mask:0l @@ fun r ->
    set_req_code r 22;
    set_req_event r event;
    set_req_window r window;
    set_req_above_sibling r (Option.value above_sibling ~default:0l);
    set_req_x r geometry.x;
    set_req_y r geometry.y;
    set_req_width r geometry.width;
    set_req_height r geometry.height;
    set_req_border_width r border_width;
    set_req_override_redirect r (Bool.to_int override_redirect)
end

let configure_notify = ConfigureNotify.send
