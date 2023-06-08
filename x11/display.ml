open Eio.Std
open Types

module Read = Eio.Buf_read

type t = Types.display
type timestamp = Types.timestamp

(* We always increment by one and ignore the mask, as Xwayland does:
   [#define RESOURCE_ID_MASK ((1 << CLIENTOFFSET) - 1)]
   We also assume we won't run out, which is OK for the proxy as we only create
   a fixed number of things (two selection windows, a font, and a cursor). *)
let mint_id t =
  t.last_resource_id <- Int32.succ t.last_resource_id;
  t.last_resource_id

let card16 = Read.LE.uint16

module Connection_setup = struct
  [%%cstruct
    type request = {
      byte_order : uint8_t;         (* 'B' or 'l' *)
      unused : uint8_t;
      version_major: uint16_t;
      version_minor: uint16_t;
      authz_protocol_name_len : uint16_t;
      authz_protocol_data_len : uint16_t;
      unused2 : uint16_t;
    } [@@little_endian]]

  let request =
    let x = Cstruct.create sizeof_request in
    set_request_byte_order x (Char.code 'l');
    set_request_version_major x 11;
    set_request_version_minor x 0;
    set_request_authz_protocol_name_len x 0;
    set_request_authz_protocol_data_len x 0;
    x

  [%%cstruct
    type response = {
      release_number : uint32_t;
      resource_id_base : uint32_t;
      resource_id_mask : uint32_t;
      motion_buffer_size : uint32_t;
      vendor_length : uint16_t;
      max_request_len : uint16_t;
      n_screens : uint8_t;
      n_formats : uint8_t;
      image_byte_order : uint8_t;
      bitmap_bit_order : uint8_t;
      bitmap_scanline_unit : uint8_t;
      bitmap_scanline_pad : uint8_t;
      min_keycode : uint8_t;
      max_keycode : uint8_t;
      unused : uint8_t [@len 4];
      (* vendor ; pad(vendor_length) *)
      (* 8n: pixmap-formats *)
      (* m: screens *)
    } [@@little_endian]
  ]

  [%%cstruct
    type fmt = {
      depth : uint8_t;
      bits_per_pixel : uint8_t;
      scanline_pad : uint8_t;
      unused : uint8_t [@len 5];
    } [@@little_endian]
  ]

  [%%cstruct
    type screen = {
      window : uint32_t;
      colormap : uint32_t;
      white_pixel : uint32_t;
      black_pixel : uint32_t;
      current_input_masks : uint32_t;
      width_pixels : uint16_t;
      height_pixels : uint16_t;
      width_mm : uint16_t;
      height_mm : uint16_t;
      min_installed_maps : uint16_t;
      max_installed_maps : uint16_t;
      root_visual : uint32_t;
      backing_stores : uint8_t;
      save_unders : uint8_t;
      root_depth : uint8_t;
      depths : uint8_t;
      (* LISTofDEPTH *)
    } [@@little_endian]
  ]

  [%%cstruct
    type depth = {
      depth : uint8_t;
      unused : uint8_t;
      n_visuals : uint16_t;
      unused2 : uint8_t [@len 4];
      (* LISTofVISUALTYPE *)
    } [@@little_endian]
  ]

  let rec dump_depths data = function
    | 0 -> data
    | depths ->
      let n_visuals = get_depth_n_visuals data in
      Log.info (fun f -> f "Depth: %d with %d visuals" (get_depth_depth data) n_visuals);
      dump_depths (Cstruct.shift data (sizeof_depth + 24 * n_visuals)) (depths - 1)

  let rec dump_screens ~roots data = function
    | 0 -> data
    | screens ->
      let win = get_screen_window data in
      roots := win :: !roots;
      let depths = get_screen_depths data in
      Log.info (fun f -> f "Screen: %d x %d (%d depths) has root %lx"
                   (get_screen_width_pixels data)
                   (get_screen_height_pixels data)
                   (get_screen_depths data)
                   win);
      dump_screens ~roots (dump_depths (Cstruct.shift data sizeof_screen) depths) (screens - 1)

  type t = {
    roots : window list;
    resource_id_base : int32;
    resource_id_mask : int32;
  }

  let read_response from_wm =
    let r = Read.any_char from_wm in
    if Char.code r <> 1 then failwith "Xwayland handshake failed!";
    Read.skip 1 from_wm;  (* unused *)
    let version_major = card16 from_wm in
    let version_minor = card16 from_wm in
    let len4 = card16 from_wm in
    Read.ensure from_wm (len4 * 4);
    let resp = Read.peek from_wm in
    let from_vendor = Cstruct.shift resp sizeof_response in
    let vendor = Cstruct.to_string from_vendor ~len:(get_response_vendor_length resp) in
    let resource_id_base = get_response_resource_id_base resp in
    let resource_id_mask = get_response_resource_id_mask resp in
    Log.info (fun f -> f
                 "@[<v2>Connected WM over X11 to Xwayland:@,\
                 Server protocol version: %d.%d@,\
                 Server release: %S : %ld@,\
                 Resource base/mask: %lx/%lx@,\
                 Max request len: %d@,\
                 Screens: %d@,\
                 Keycode range: %d-%d@]"
                 version_major version_minor
                 vendor (get_response_release_number resp)
                 resource_id_base resource_id_mask
                 (get_response_max_request_len resp)
                 (get_response_n_screens resp)
                 (get_response_min_keycode resp) (get_response_max_keycode resp)
             );
    let from_formats = Cstruct.shift from_vendor (Wire.round_up4 (String.length vendor)) in
    let n_formats = get_response_n_formats resp in
    for i = 0 to n_formats - 1 do
      let fmt = Cstruct.shift from_formats (i * sizeof_fmt) in
      Log.info (fun f -> f "Format %d: depth=%d, bpp=%d, pad=%d"
                   i
                   (get_fmt_depth fmt)
                   (get_fmt_bits_per_pixel fmt)
                   (get_fmt_scanline_pad fmt))
    done;
    let from_screens = Cstruct.shift from_formats (n_formats * sizeof_fmt) in
    let roots = ref [] in
    let rest = dump_screens ~roots from_screens (get_response_n_screens resp) in
    assert (Cstruct.length rest = 0);
    Read.consume from_wm (len4 * 4);
    {
      roots = !roots;
      resource_id_base;
      resource_id_mask;
    }
end

let connect ~sw socket =
  let from_server = Read.of_flow socket ~max_size:max_int in
  Eio.Flow.write socket [Connection_setup.request];
  let reply = Connection_setup.read_response from_server in
  {
    sw;
    socket = (socket :> Eio.Net.stream_socket_ty r);
    from_server;
    next_seq = 1;
    pending = Queue.create ();
    roots = reply.roots;
    atoms = Hashtbl.create 10;
    atom_names = Hashtbl.create 10;
    last_resource_id = reply.resource_id_base;
    need_sync = false;
    wake_input = Eio.Condition.create ();
    send_mutex = Eio.Mutex.create ();
  }

let sync t =
  Log.info (fun f -> f "Sync");
  (* Confusingly, we want [send] not [send_sync] here,
     because we want to wait for a round-trip. *)
  let _ : timestamp = Atom.Intern.send t ~only_if_exists:true "SYNC" in
  Log.info (fun f -> f "Sync done")
