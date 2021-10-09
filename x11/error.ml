type t = Cstruct.t

[%%cenum
  type code =
    | Request [@id 1]
    | Value
    | Window
    | Pixmap
    | Atom
    | Cursor
    | Font
    | Match
    | Drawable
    | Access
    | Alloc
    | Colormap
    | GContext
    | IDChoice
    | Name
    | Length
    | Implementation
  [@@uint8_t]
]

[%%cstruct
  type error = {
    zero : uint8_t;
    code : uint8_t;
    seq : uint16_t;
    pad : uint8_t [@len 4];
    minor_opcode : uint16_t;
    major_opcode : uint8_t;
    unused : uint8_t [@len 21];
  } [@@little_endian]
]

let pp_code f x = Fmt.string f (code_to_string x)

let pp f e =
  Fmt.pf f "%a (op %d.%d, seq %d)"
    Fmt.(option ~none:(any "unknown") string)
    (Option.map code_to_string (int_to_code (get_error_code e)))
    (get_error_major_opcode e) (get_error_minor_opcode e)
    (get_error_seq e)

let to_exn e = Failure (Fmt.str "%a" pp e)
