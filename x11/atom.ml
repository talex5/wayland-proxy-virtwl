open Lwt.Syntax

type t = Types.atom

module Intern = struct
  [%%cstruct
    type request = {
      name_len : uint16_t;
      pad2 : uint8_t [@len 2];
      (* name *)
    } [@@little_endian]
  ]

  [%%cstruct
    type reply = {
      code : uint8_t;
      pad0 : uint8_t;
      seq : uint16_t;
      length : uint32_t;
      atom : uint32_t;
      unused : uint8_t [@len 20];
    } [@@little_endian]
  ]

  let send t ~only_if_exists name =
    let len = sizeof_request + Wire.round_up4 (String.length name) in
    let+ r = Request.send_exn t ~major:16 ~minor:(Bool.to_int only_if_exists) len (fun r ->
        set_request_name_len r (String.length name);
        Cstruct.blit_from_string name 0 r sizeof_request (String.length name)
      )
    in
    get_reply_atom r

  (* Send a dummy message that generates a reply, but don't wait for it.
     When the reply arrives, it will mark all previous pending messages as successful. *)
  let send_sync (t:Types.display) =
    let name = "SYNC" in
    let only_if_exists = true in
    let len = sizeof_request + Wire.round_up4 (String.length name) in
    Request.send_sync t ~major:16 ~minor:(Bool.to_int only_if_exists) len (fun r ->
        set_request_name_len r (String.length name);
        Cstruct.blit_from_string name 0 r sizeof_request (String.length name)
      )
end

let intern (t:Types.display) ?(only_if_exists=false) name =
  match Hashtbl.find_opt t.atoms name with
  | Some x -> x
  | None ->
    Log.info (fun f -> f "Intern %S" name);
    let atom =
      let+ atom = Intern.send t ~only_if_exists name in
      Hashtbl.add t.atom_names atom name;
      atom
    in
    Hashtbl.add t.atoms name atom;
    atom

module GetName = struct
  [%%cstruct
    type req = {
      atom : uint32_t;
    } [@@little_endian]
  ]

  [%%cstruct
    type reply = {
      code : uint8_t;
      pad0 : uint8_t;
      seq : uint16_t;
      length : uint32_t;
      name_len : uint16_t;
      unused : uint8_t [@len 22];
    } [@@little_endian]
  ]

  let send t atom =
    let+ r = Request.send_exn t ~major:17 sizeof_req (fun r -> set_req_atom r atom) in
    let len = get_reply_name_len r in
    Cstruct.to_string r ~off:sizeof_reply ~len
end

let get_name (t:Types.display) atom =
  match Hashtbl.find_opt t.atom_names atom with
  | Some name -> Lwt.return name
  | None ->
    let+ name = GetName.send t atom in
    Hashtbl.replace t.atom_names atom name;
    name

let pp (t:Types.display) f atom =
  match Hashtbl.find_opt t.atom_names atom with
  | None -> Fmt.pf f "%lx" atom
  | Some name -> Fmt.pf f "%S" name
