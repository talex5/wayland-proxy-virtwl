open Eio.Std

type reply = Types.reply

type _ checked =
  | Unchecked : unit checked
  | Checked : (unit, Error.code) result checked

[%%cstruct
  type request_header = {
    major : uint8_t;
    minor : uint8_t;
    len : uint16_t;
  } [@@little_endian]
]

let send_aux (t:Types.display) ~major ?(minor=0) size fn =
  assert (size >= 0);
  Eio.Mutex.use_rw ~protect:false t.send_mutex @@ fun () ->
  let size = sizeof_request_header + Wire.round_up4 size in
  let msg = Cstruct.create size in
  set_request_header_major msg major;
  set_request_header_minor msg minor;
  set_request_header_len msg (size / 4);
  fn (Cstruct.shift msg sizeof_request_header);
  let reply, set_reply = Promise.create () in
  Queue.add ((t.next_seq land 0xffff), set_reply) t.pending;
  Eio.Flow.write t.socket [msg];
  Log.debug (fun f -> f "@[<v2>Sent request with sequence number %d@,%a@]" t.next_seq
                Cstruct.hexdump_pp msg);
  t.next_seq <- t.next_seq + 1;
  reply

let send (t:Types.display) ~major ?minor size fn =
  t.need_sync <- false;
  Promise.await (send_aux t ~major ?minor size fn)

let send_checked (t:Types.display) ~major ?minor size fn =
  t.need_sync <- true;
  Eio.Condition.broadcast t.wake_input;
  let reply = send_aux t ~major ?minor size fn in
  match Promise.await reply with
  | `Reply r -> Fmt.failwith "Unexpected reply %a" Cstruct.hexdump_pp r
  | `No_reply -> Ok ()
  | `Error msg ->
    let code = Error.get_error_code msg in
    match Error.int_to_code code with
    | Some code -> Error code
    | None -> Fmt.failwith "Unknown X11 error code %d" code

(* todo: if we send enough messages that don't expect a reply in a row, the
   sequence number would wrap. So in theory we should force a sync here. *)
let send_only (t:Types.display) ~major ?minor size fn =
  let r = send_aux t ~major ?minor size fn in
  Fiber.fork ~sw:t.sw
    (fun () ->
       match Promise.await r with
       | `Reply r -> Fmt.failwith "Unexpected reply %a" Cstruct.hexdump_pp r
       | `No_reply -> ()
       | `Error msg ->
         Log.warn (fun f -> f "@[<v2>ERROR from X server: %a (last-sent=%d)@]" Error.pp msg
                      (t.next_seq - 1))
    )

let send_maybe_checked (type a) (checked : a checked) t ~major ?minor size fn : a =
  match checked with
  | Unchecked -> send_only t ~major ?minor size fn
  | Checked -> send_checked t ~major ?minor size fn

let send_exn (t:Types.display) ~major ?minor size fn =
  let n = t.next_seq in
  match send t ~major ?minor size fn with
  | `Reply r -> r
  | `No_reply -> Fmt.failwith "Expected a reply to message %d" n
  | `Error msg -> raise (Error.to_exn msg)

let send_sync (t:Types.display) ~major ?minor size fn =
  Log.info (fun f -> f "Sending dummy message to check for previous errors");
  t.need_sync <- false;
  let r = send_aux t ~major ?minor size fn in
  Fiber.fork ~sw:t.sw
    (fun () ->
       match Promise.await r with
       | `Reply _ -> ()
       | `No_reply -> Log.warn (fun f -> f "No reply to sync!")
       | `Error msg ->
         Log.warn (fun f -> f "@[<v2>ERROR from X server sync: %a (last-sent=%d)@]" Error.pp msg
                      (t.next_seq - 1))
    )

module SendEvent = struct
  [%%cstruct
    type req = {
      window : uint32_t;
      event_mask : uint32_t;
      event : uint8_t [@len 32];
    } [@@little_endian]
  ]

  let send t ~window ~propagate ~event_mask fn =
    send_only t ~major:25 ~minor:(Bool.to_int propagate) sizeof_req @@ fun r ->
    set_req_window r window;
    set_req_event_mask r event_mask;
    fn (get_req_event r)
end
let send_event = SendEvent.send
