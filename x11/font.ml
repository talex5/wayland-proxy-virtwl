type t = Types.font

module OpenFont = struct
  [%%cstruct
    type req = {
      fid : uint32_t;
      name_len : uint16_t;
      unused2 : uint16_t;
      (* name *)
    } [@@little_endian]
  ]

  let send t name =
    let fid = Display.mint_id t in
    Log.info (fun f -> f "OpenFont %S as %a" name Xid.pp fid);
    Request.send_only t ~major:45 (sizeof_req + Wire.round_up4 (String.length name)) (fun r ->
        set_req_fid r fid;
        set_req_name_len r (String.length name);
        Cstruct.blit_from_string name 0 r sizeof_req (String.length name)
      );
    fid
end

let open_font = OpenFont.send

module CreateGlyphCursor = struct
  [%%cstruct
    type req = {
      cid : uint32_t;
      source_font : uint32_t;
      mask_font : uint32_t;
      source_char : uint16_t;
      mask_char : uint16_t;
      fg_r : uint16_t;
      fg_g : uint16_t;
      fg_b : uint16_t;
      bg_r : uint16_t;
      bg_g : uint16_t;
      bg_b : uint16_t;
    } [@@little_endian]
  ]

  let send t ~source_font ~mask_font ~source_char ~mask_char ~fg ~bg =
    let cid = Display.mint_id t in
    Log.info (fun f -> f "CreateGlyphCursor");
    Request.send_only t ~major:94 sizeof_req (fun r ->
        set_req_cid r cid;
        set_req_source_font r source_font;
        set_req_mask_font r mask_font;
        set_req_source_char r source_char;
        set_req_mask_char r mask_char;
        let (r1, g1, b1) = fg in
        set_req_fg_r r r1;
        set_req_fg_g r g1;
        set_req_fg_b r b1;
        let (r2, g2, b2) = bg in
        set_req_bg_r r r2;
        set_req_bg_g r g2;
        set_req_bg_b r b2
      );
    cid
end

let create_glyph_cursor = CreateGlyphCursor.send
