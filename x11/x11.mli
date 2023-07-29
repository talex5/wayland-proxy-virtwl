(** A pure OCaml implementation of the X11 protocol.
    This is not complete; it has just enough features to implement an Xwayland window manager.

    The API corresponds fairly directly to the protocol description at
    https://www.x.org/releases/X11R7.7/doc/xproto/x11protocol.html *)

open Eio.Std

(** {2 Core protocol} *)

module Xid : sig
  type 'a t = private int32
  val pp : _ t Fmt.t
  val of_int : int32 -> 'a t
end

module Display : sig
  type t
  (** An X11 connection to a display server. *)

  type timestamp = private int32

  val connect : sw:Switch.t -> _ Eio.Net.stream_socket -> t
  (** [connect ~sw fd] performs an X11 handshake on [fd].
      You must call {!Event.listen} after this to start processing events. *)

  val sync : t -> unit
  (** [sync t] sends a dummy message and waits for the reply. *)
end

module Atom : sig
  type t = [`Atom] Xid.t

  val pp : Display.t -> t Fmt.t
  (** [pp display] is a formatter that prints the name of the atom if cached, or its number otherwise. *)

  val intern : Display.t -> ?only_if_exists:bool -> string -> t
  (** [intern display name] returns the atom corresponding to [name].
      The results are cached, so this is only slow the first time. *)

  val get_name : Display.t -> t -> string
  (** [get_name display t] returns the name of [t] using [display]'s local cache.
      If the name isn't in the cache, it queries the X server. *)
end

module Error : sig
  type t

  type code =
    | Request
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

  val pp_code : code Fmt.t
  val pp : t Fmt.t
  val to_exn : t -> exn

  type error =
    | X11_error of t

  type Eio.Exn.err += E of error
end

module Geometry : sig
  type t = { x : int; y : int; width : int; height : int; }

  val pp : t Fmt.t
end

module Window : sig
  type t = [`Window] Xid.t
  type cursor = [`Cursor] Xid.t
  type visual = [`Visual] Xid.t

  type event =
    | KeyPress
    | KeyRelease
    | ButtonPress
    | ButtonRelease
    | EnterWindow
    | LeaveWindow
    | PointerMotion
    | PointerMotionHint
    | Button1Motion
    | Button2Motion
    | Button3Motion
    | Button4Motion
    | Button5Motion
    | ButtonMotion
    | KeymapState
    | Exposure
    | VisibilityChange
    | StructureNotify
    | ResizeRedirect
    | SubstructureNotify
    | SubstructureRedirect
    | FocusChange
    | PropertyChange
    | ColormapChange
    | OwnerGrabButton

  val pp : t Fmt.t
  (** Format a window ID. *)

  val roots : Display.t -> t list
  (** [roots display] is the list of root windows (one per screen). *)

  type create_attributes

  val create_attributes :
    ?cursor:cursor ->
    ?event_mask:event list ->
    unit -> create_attributes

  val create_input_only :
    Display.t ->
    parent:t ->
    geometry:Geometry.t ->
    ?visual:visual ->
    create_attributes ->
    t
  (** Create an InputOnly window (which can be used for e.g. receiving selections, but is not visible). *)

  val change_attributes : Display.t -> t -> create_attributes -> unit

  val get_geometry : Display.t -> t -> Geometry.t

  type attributes = {
    window_class : [`InputOnly | `InputOutput];
    override_redirect : bool;
  }

  val get_attributes : Display.t -> t -> attributes

  val send_client_message :
    Display.t -> t ->
    propagate:bool ->
    event_mask:Cstruct.uint32 ->
    fmt:Cstruct.uint8 -> ty:Atom.t -> Cstruct.t -> unit

  val map : Display.t -> t -> unit

  val destroy : Display.t -> t -> unit

  type stack_mode = [
    | `Above
    | `Below
    | `TopIf
    | `BottomIf
    | `Opposite
  ]

  val configure :
    ?x:int ->
    ?y:int ->
    ?width:int ->
    ?height:int ->
    ?border_width:int ->
    ?sibling:t ->
    ?stack_mode:stack_mode ->
    Display.t -> t ->
    unit

  val configure_checked :
    ?x:int ->
    ?y:int ->
    ?width:int ->
    ?height:int ->
    ?border_width:int ->
    ?sibling:t ->
    ?stack_mode:stack_mode ->
    Display.t -> t ->
    (unit, Error.code) result
  (** Like {!configure}, but waits for the server to confirm that it worked. *)

  val configure_notify :
    Display.t ->
    event:t ->
    window:t ->
    above_sibling:t option ->
    geometry:Geometry.t ->
    border_width:int ->
    override_redirect:bool ->
    unit

  val set_input_focus_checked :
    Display.t ->
    revert_to:[< `None | `PointerRoot | `Parent] ->
    time:[< `CurrentTime | `Time of Display.timestamp ] ->
    [< `Window of t | `PointerRoot | `None] ->
    (unit, Error.code) result
end

module Selection : sig
  val set_owner :
    Display.t -> Atom.t ->
    owner:Window.t option ->
    timestamp:[< `CurrentTime | `Time of Display.timestamp ] ->
    unit

  val convert :
    Display.t -> Atom.t ->
    requestor:Window.t ->
    target:Atom.t ->
    property:Atom.t option ->
    time:[< `CurrentTime | `Time of Display.timestamp ] -> unit

  val notify :
    Display.t -> Atom.t ->
    time:[< `CurrentTime | `Time of Display.timestamp ] ->
    requestor:Window.t ->
    target:Atom.t ->
    property:Atom.t option ->
    unit
end

module Font : sig
  type t = [`Font] Xid.t

  val open_font : Display.t -> string -> t

  val create_glyph_cursor :
    Display.t ->
    source_font:t ->
    mask_font:t ->
    source_char:Cstruct.uint16 ->
    mask_char:Cstruct.uint16 ->
    fg:Cstruct.uint16 * Cstruct.uint16 * Cstruct.uint16 ->
    bg:Cstruct.uint16 * Cstruct.uint16 * Cstruct.uint16 ->
    Window.cursor
end

module Property : sig
  type info = {
    fmt : int;          (** Bytes per item *)
    ty : Atom.t;
    value : Cstruct.t;
    bytes_after : int;
  }

  val set_string : ty:Atom.t -> Display.t -> Window.t -> Atom.t -> string -> unit

  val get_string : ?delete:bool -> Display.t -> Window.t -> Atom.t -> string option
  val get_atoms  : ?delete:bool -> Display.t -> Window.t -> Atom.t -> Atom.t list
  val get_atom   : ?delete:bool -> Display.t -> Window.t -> Atom.t -> Atom.t option

  val get :
    ?delete:bool ->
    ?ty:Atom.t ->
    long_offset:Cstruct.uint32 ->
    long_length:Cstruct.uint32 ->
    Display.t -> Window.t -> Atom.t ->
    info option

  val get_all :
    ?delete:bool ->
    ?ty:Atom.t ->
    Display.t -> Window.t -> Atom.t ->
    Cstruct.t option
  (** [get_all] uses {!get} to read as many chunks as needed and concatenates them. *)

  val change :
    mode:[`Replace|`Prepend|`Append] ->
    ty:Atom.t ->
    fmt:Cstruct.uint8 ->
    Display.t -> Window.t -> Atom.t ->
    Cstruct.t -> unit

  val delete : Display.t -> Window.t -> Atom.t -> unit
end

module Event : sig
  (** Note: the X11 event handler loop is blocked until the callback returns. *)
  type handler = <
    map_request : window:Window.t -> unit;

    unmap_notify : window:Window.t -> unit;

    configure_request :
      window:Window.t -> width:int -> height:int -> unit;

    client_message :
      window:Window.t -> ty:Atom.t -> Cstruct.t -> unit;

    selection_request :
      time:Display.timestamp -> owner:Window.t -> requestor:Window.t ->
      selection:Atom.t -> target:Atom.t -> property:Atom.t option -> unit;

    selection_clear :
      time:Display.timestamp -> owner:Window.t -> selection:Atom.t -> unit;

    selection_notify :
      time:Display.timestamp -> requestor:Window.t -> selection:Atom.t -> target:Atom.t -> property:Atom.t option -> unit;

    property_notify :
      window:Window.t -> atom:Atom.t -> time:Display.timestamp -> state:[`NewValue | `Deleted] -> unit;
  >

  val listen : Display.t -> handler -> 'a
  (** [listen t handler] runs the main event loop, dispatching events with [handler]. *)
end

module Icccm : sig
  module Wm_normal_hints : sig
    type t

    val min_size : t -> (int32 * int32) option
  end

  val get_wm_normal_hints : Display.t -> Window.t -> Wm_normal_hints.t
end

(** {2 Extensions} *)

module Extension : sig
  type info = {
    major_opcode : int;
  }

  val query : Display.t -> string -> info option
  val query_exn : Display.t -> string -> info
end

module Composite : sig
  type t

  val redirect_subwindows :
    t ->
    window:Window.t ->
    update:[< `Automatic | `Manual ] -> unit

  val init : Display.t -> t
end

(** {2 Low-level API (for extensions)} *)

module Request : sig
  type reply = [
    | `Reply of Cstruct.t
    | `Error of Error.t
    | `No_reply
  ]

  type _ checked =
    | Unchecked : unit checked
    | Checked : (unit, Error.code) result checked

  val send_only : Display.t -> major:int -> ?minor:int -> int -> (Cstruct.t -> unit) -> unit
  (** [send_only display ~major size build] creates an X11 request message with body size [size].
      It fills in the header part (using [major] and [minor], plus a fresh sequence number and the length),
      and then uses [build body] to fill in the body part.
      Use this for sending an X11 message that doesn't produce a reply.
      It returns as soon as the message is sent, and if we get an error response, it just logs it. *)

  val send : Display.t -> major:int -> ?minor:int -> int -> (Cstruct.t -> unit) -> reply
  (** Like {!send_only}, but expect (and wait for) the reply. *)

  val send_checked : Display.t -> major:int -> ?minor:int -> int -> (Cstruct.t -> unit) -> (unit, Error.code) result
  (** Send an X11 message that doesn't have a reply, and wait until we know it succeeded. *)

  val send_maybe_checked : 'a checked -> Display.t -> major:int -> ?minor:int -> int -> (Cstruct.t -> unit) -> 'a
  (** [send_maybe_checked Checked] is [send_checked].
      [send_maybe_checked Unchecked] is [send_only]. *)

  val send_exn : Display.t -> major:int -> ?minor:int -> int -> (Cstruct.t -> unit) -> Cstruct.t
  (** Send an X11 message and wait for the reply. Raise an exception if we get an error. *)
end

val log_src : Logs.src
