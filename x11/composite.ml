type t = {
  x11 : Display.t;
  major_opcode : int;
}

module QueryVersion = struct
  [%%cstruct
    type req = {
      client_major_version : uint32_t;
      client_minor_version : uint32_t;
    } [@@little_endian]
  ]

  [%%cstruct
    type reply = {
      opcode : uint8_t;
      pad1 : uint8_t;
      seq : uint16_t;
      len : uint32_t;
      major_version : uint32_t;
      minor_version : uint32_t;
      pad : uint8_t [@len 16];
    } [@@little_endian]
  ]

  let send t ~major_opcode (client_major_version, client_minor_version) =
    let r = Request.send_exn t ~major:major_opcode ~minor:0 sizeof_req (fun r ->
        set_req_client_major_version r client_major_version;
        set_req_client_minor_version r client_minor_version
      )
    in
    get_reply_major_version r, get_reply_minor_version r
end

module Redirect_subwindows = struct
  [%%cstruct
    type req = {
      window : uint32_t;
      update : uint8_t;
      pad : uint8_t [@len 3];
    } [@@little_endian]
  ]

  let send t ~window ~update =
    Request.send_only t.x11 ~major:t.major_opcode ~minor:2 sizeof_req @@ fun r ->
    set_req_window r (window : Types.window :> int32);
    set_req_update r (match update with
        | `Automatic -> 0
        | `Manual -> 1
      )
end

let redirect_subwindows = Redirect_subwindows.send

let init t =
  let { Extension.major_opcode } = Extension.query_exn t "Composite" in
  Log.info (fun f -> f "Composite extension has major opcode %d" major_opcode);
  let (maj, min) = QueryVersion.send t ~major_opcode (0l, 3l) in
  Log.info (fun f -> f "Using Composite protocol version %ld.%ld" maj min);
  { x11 = t; major_opcode }
