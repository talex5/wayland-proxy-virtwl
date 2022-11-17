let maybe_current_time = function
  | `CurrentTime -> 0l
  | `Time t -> t

module SetOwner = struct
  [%%cstruct
    type request = {
      owner : uint32_t;
      selection : uint32_t;
      timestamp : uint32_t;
    } [@@little_endian]
  ]

  let send t selection ~owner ~timestamp =
    Log.info (fun f -> f "SetSelectionOwner %a to be owned by %a"
                 (Atom.pp t) selection
                 (Fmt.option ~none:Fmt.(any "nobody") Xid.pp) owner);
    Request.send_only t ~major:22 sizeof_request @@ fun r ->
    set_request_owner r (Option.value owner ~default:0l);
    set_request_selection r selection;
    set_request_timestamp r (maybe_current_time timestamp)
end

let set_owner = SetOwner.send

module GetOwner = struct
  [%%cstruct
    type request = {
      selection : uint32_t;
    } [@@little_endian]
  ]

  [%%cstruct
    type reply = {
      code : uint8_t;
      _unused : uint8_t;
      seq : uint16_t;
      length : uint32_t;
      owner : uint32_t;
      unused : uint8_t [@len 20];
    } [@@little_endian]
  ]

  let send t selection =
    let open Lwt.Syntax in
    Log.info (fun f -> f "GetSelectionOwner of %a" (Atom.pp t) selection);
    let+ r = Request.send_exn t ~major:23 sizeof_request (fun r ->
        set_request_selection r selection
      )
    in
    let window = get_reply_owner r in
    Log.info (fun f -> f "GetSelectionOwner %a => %a" (Atom.pp t) selection Window.pp window);
    window
end

let get_owner = GetOwner.send

module Convert = struct
  [%%cstruct
    type req = {
      requestor : uint32_t;
      selection : uint32_t;
      target : uint32_t;
      property : uint32_t;
      time : uint32_t;
    } [@@little_endian]
  ]

  let send t selection ~requestor ~target ~property ~time =
    Log.info (fun f -> f "ConvertSelection(%a): %a/%a (target %a)"
                 (Atom.pp t) selection
                 Xid.pp requestor Fmt.(option ~none:(any "-") (Atom.pp t)) property
                 (Atom.pp t) target);
    Request.send_only t ~major:24 sizeof_req @@ fun r ->
    set_req_requestor r requestor;
    set_req_selection r selection;
    set_req_target r target;
    set_req_property r (Option.value property ~default:0l);
    set_req_time r (maybe_current_time time)
end

let convert = Convert.send

module Notify = struct
  [%%cstruct
    type req = {
      code : uint8_t;
      unused : uint8_t;
      seq : uint16_t;   (* set by server *)
      time : uint32_t;
      requestor : uint32_t;
      selection : uint32_t;
      target : uint32_t;
      property : uint32_t;
      unused2 : uint8_t [@len 8];
    } [@@little_endian]
  ]

  let send t selection ~time ~requestor ~target ~property =
    Log.info (fun f -> f "SelectionNotify(%a): %a/%a (target %a)"
                 (Atom.pp t) selection
                 Xid.pp requestor Fmt.(option ~none:(any "-") (Atom.pp t)) property
                 (Atom.pp t) target);
    Request.send_event t ~window:requestor ~propagate:false ~event_mask:0l @@ fun r ->
    set_req_code r 31;
    set_req_time r (maybe_current_time time);
    set_req_requestor r requestor;
    set_req_selection r selection;
    set_req_target r target;
    set_req_property r (Option.value property ~default:0l)
end

let notify = Notify.send
