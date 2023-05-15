let max_transfer_longs = Int32.of_int (4 * 1024 * 1024)      (* 16 MB max chunk size; probably not important *)

type info = {
  fmt : int;          (* Bytes per item *)
  ty : Types.atom;
  value : Cstruct.t;
  bytes_after : int;
}

let pp_info t f { fmt; ty; value; bytes_after } =
  Fmt.pf f "{%a/%d:%a+%d}"
    (Atom.pp t) ty
    fmt
    Cstruct.hexdump_pp value
    bytes_after

module Get = struct
  [%%cstruct
    type req = {
      window : uint32_t;
      property : uint32_t;
      ty : uint32_t;
      long_offset: uint32_t;
      long_length: uint32_t;
    } [@@little_endian]
  ]

  [%%cstruct
    type reply = {
      reply : uint8_t;
      fmt : uint8_t;
      seq : uint16_t;
      len : uint32_t;
      ty : uint32_t;
      bytes_after : uint32_t;
      len_val : uint32_t;
      unused : uint8_t [@len 12];
      (* List of values *)
    } [@@little_endian]
  ]

  (* Warning: this can fail with ValueError if the property becomes shorter than [long_offset]! *)
  let send t ?(delete=false) ~window ?ty ~long_offset ~long_length property =
    let r = Request.send_exn t ~major:20 ~minor:(Bool.to_int delete) sizeof_req (fun r ->
        set_req_window r window;
        set_req_property r property;
        set_req_ty r (Option.value ty ~default:0l);
        set_req_long_offset r long_offset;
        set_req_long_length r long_length
      )
    in
    match get_reply_fmt r with
    | 0 -> None
    | fmt ->
      let len = Int32.to_int (get_reply_len_val r) in
      let bytes_after = Int32.to_int (get_reply_bytes_after r) in
      let ty = get_reply_ty r in
      let value = Cstruct.sub r sizeof_reply (len * (fmt / 8)) in
      Some { fmt; ty; value; bytes_after }
end

let get ?delete ?ty ~long_offset ~long_length t window property =
  let info = Get.send t ?delete ~window ?ty ~long_offset ~long_length property in
  Log.info (fun f -> f "GetProperty %a/%a => %a" Xid.pp window (Atom.pp t) property
               (Fmt.option ~none:Fmt.(any "(unset)") (pp_info t)) info
           );
  info

let get_all ?delete ?ty t window property =
  let rec aux ~long_offset =
    match Get.send ?delete ?ty t ~window property ~long_offset ~long_length:max_transfer_longs with
    | None -> None
    | Some d when d.bytes_after = 0 -> Some [d.value]
    | Some d ->
      let long_offset = Int32.add long_offset (Int32.of_int (Cstruct.length d.value / 4)) in
      aux ~long_offset |> Option.map (fun ds -> d.value :: ds)
  in
  aux ~long_offset:0l |> Option.map Cstruct.concat

let get_string ?delete t window property =
  let reply = get_all ?delete t window property in
  match reply with
  | None ->
    Log.info (fun f -> f "GetProperty %a/%a => (unset)" Xid.pp window (Atom.pp t) property);
    None
  | Some data ->
    (* todo: convert ty to UTF-8 *)
    let s = Cstruct.to_string data in
    Log.info (fun f -> f "GetProperty %a/%a => %S" Xid.pp window (Atom.pp t) property s);
    Some s

let get_atoms ?delete t window property =
  let data = get_all ?delete t window property in
  let atoms =
    match data with
    | None -> []
    | Some data ->
      List.init (Cstruct.length data / 4)
        (fun i -> Cstruct.LE.get_uint32 data (i * 4))
  in
  Log.info (fun f -> f "GetProperty %a/%a => %a"
               Xid.pp window
               (Atom.pp t) property
               (Fmt.Dump.list (Atom.pp t)) atoms);
  atoms

let get_atom ?delete t window property =
  match get_atoms ?delete t window property with
  | [] -> None
  | [x] -> Some x
  | atoms -> Fmt.failwith "Expected only a single atom in property %a, got %a"
               (Atom.pp t) property
               (Fmt.Dump.list (Atom.pp t)) atoms

let pp_length f cs = Fmt.pf f "%d bytes" (Cstruct.length cs)

let get_all ?delete ?ty t window property =
  let data = get_all ?delete ?ty t window property in
  Log.info (fun f -> f "GetProperty %a/%a => %a" Xid.pp window (Atom.pp t) property
               (Fmt.option ~none:Fmt.(any "(unset)") pp_length) data
           );
  data

module Change = struct
  let mode_to_int = function
    | `Replace -> 0
    | `Prepend -> 1
    | `Append  -> 2

  [%%cstruct
    type req = {
      window : uint32_t;
      property : uint32_t;
      ty : uint32_t;
      fmt: uint8_t;
      unused : uint8_t [@len 3];
      len_val : uint32_t;
      (* List of values *)
    } [@@little_endian]
  ]

  let send t ~mode ~window ~ty ~fmt property value =
    let bytes_per_value =
      match fmt with
      | 8 -> 1
      | 16 -> 2
      | 32 -> 4
      | _ -> Fmt.failwith "Invalid format %d" fmt
    in
    Request.send_only t ~major:18 ~minor:(mode_to_int mode) (sizeof_req + Wire.round_up4 (Cstruct.length value)) @@ fun r ->
    set_req_window r window;
    set_req_property r property;
    set_req_ty r ty;
    set_req_fmt r fmt;
    set_req_len_val r (Int32.of_int (Cstruct.length value / bytes_per_value));
    Cstruct.blit value 0 r sizeof_req (Cstruct.length value)
end

let set_string ~ty t window property value =
  Log.info (fun f -> f "ChangeProperty %a/%a to %S" Xid.pp window (Atom.pp t) property value);
  Change.send t ~mode:`Replace ~window ~ty ~fmt:8 property (Cstruct.of_string value)

let change ~mode ~ty ~fmt t window property value =
  Log.info (fun f -> f "ChangeProperty %a/%a to %a" Xid.pp window (Atom.pp t) property Cstruct.hexdump_pp value);
  Change.send t ~mode ~window ~ty ~fmt property value

module Delete = struct
  [%%cstruct
    type req = {
      window : uint32_t;
      property : uint32_t;
    } [@@little_endian]
  ]

  let send t window property =
    Request.send_only t ~major:19 sizeof_req @@ fun r ->
    set_req_window r window;
    set_req_property r property
end

let delete = Delete.send
