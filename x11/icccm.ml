open Lwt.Syntax
open Lwt.Infix

module Wm_normal_hints = struct
  [%%cenum
    type flags =
      | USPosition [@id 1]
      | USSize [@id 2]
      | PPosition [@id 4]
      | PSize [@id 8]
      | PMinSize [@id 16]
      | PMaxSize [@id 32]
      | PResizeInc [@id 64]
      | PAspect [@id 128]
      | PBaseSize [@id 256]
      | PWinGravity [@id 512]
    [@@uint32_t]
  ]

  [%%cstruct
    type t = {
      flags : uint32_t;
      pad : uint32_t [@len 4];
      min_width : uint32_t;
      min_height : uint32_t;
      max_width : uint32_t;
      max_height : uint32_t;
      width_inc : uint32_t;
      height_inc : uint32_t;
      min_aspect : uint32_t [@len 2];
      max_aspect : uint32_t [@len 2];
      base_width : uint32_t;
      base_height : uint32_t;
      win_gravity : uint32_t;
    } [@@little_endian]
  ]

  type t = Cstruct.t

  let default = Cstruct.create sizeof_t

  let get x11 window =
    let* wm_normal_hints = Atom.intern x11 "WM_NORMAL_HINTS" in
    let long_length = Int32.of_int (sizeof_t / 4) in
    Property.get x11 window wm_normal_hints ~long_offset:0l ~long_length >|= function
    | None -> default
    | Some info when Cstruct.length info.value >= sizeof_t -> info.value
    | Some _ ->
      Log.warn (fun f -> f "WM_NORMAL_HINTS on %a too short!" Window.pp window);
      default

  let if_set bit getter t =
    if Int32.logand (get_t_flags t) (flags_to_int bit) <> 0l then
      Some (getter t)
    else
      None

  let min_size =
    if_set PMinSize @@ fun t ->
    (get_t_min_width t, get_t_min_height t)
end

let get_wm_normal_hints = Wm_normal_hints.get
