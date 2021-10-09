open Lwt.Syntax
open Lwt.Infix

type info = {
  major_opcode : int;
}

module Query = struct
  [%%cstruct
    type request = {
      name_len : uint16_t;
      pad2 : uint8_t [@len 2];
      (* name *)
    } [@@little_endian]
  ]

  [%%cstruct
    type reply = {
      opcode : uint8_t;
      pad1 : uint8_t;
      seq : uint16_t;
      len : uint32_t;
      present : uint8_t;
      major_opcode : uint8_t;
      first_event : uint8_t;
      first_error : uint8_t;
    } [@@little_endian]
  ]

  let send t name =
    Log.info (fun f -> f "QueryExtension %S" name);
    let len = sizeof_request + (Wire.round_up4 (String.length name)) in
    let+ r = Request.send_exn t ~major:98 len (fun r ->
        set_request_name_len r (String.length name);
        Cstruct.blit_from_string name 0 r sizeof_request (String.length name)
      )
    in
    if get_reply_present r = 1 then (
      Some { major_opcode = get_reply_major_opcode r }
    ) else (
      None
    )
end

let query = Query.send

let query_exn t name =
  query t name >|= function
  | None -> Fmt.failwith "Extension %S not present" name
  | Some x -> x
