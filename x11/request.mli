type reply = Types.reply

type _ checked =
  | Unchecked : unit checked
  | Checked : (unit, Error.code) result checked

val send_only : Types.display -> major:int -> ?minor:int -> int -> (Cstruct.t -> unit) -> unit Lwt.t
(** Send an X11 message that doesn't produce a reply.
    Return as soon as the message is sent, and if we get an error response, just log it. *)

val send : Types.display -> major:int -> ?minor:int -> int -> (Cstruct.t -> unit) -> Types.reply Lwt.t
(** Send an X11 message and wait for the reply. *)

val send_checked : Types.display -> major:int -> ?minor:int -> int -> (Cstruct.t -> unit) -> (unit, Error.code) Lwt_result.t
(** Send an X11 message that doesn't have a reply, and wait until we know it succeeded. *)

val send_maybe_checked : 'a checked -> Types.display -> major:int -> ?minor:int -> int -> (Cstruct.t -> unit) -> 'a Lwt.t
(** [send_maybe_checked Checked] is [send_checked].
    [send_maybe_checked Unchecked] is [send_only]. *)

val send_exn : Types.display -> major:int -> ?minor:int -> int -> (Cstruct.t -> unit) -> Cstruct.t Lwt.t
(** Send an X11 message and wait for the reply. Raise an exception if we get an error. *)

val send_sync : Types.display -> major:int -> ?minor:int -> int -> (Cstruct.t -> unit) -> unit Lwt.t
(** Send a dummy message that generates a reply, but don't wait for it.
    When the reply arrives, it will mark all previous pending messages as successful. *)

val send_event : Types.display -> window:int32 -> propagate:bool -> event_mask:int32 -> (Cstruct.t -> unit) -> unit Lwt.t
(** [send_event display ~window ~propagate ~event_mask build] sends a synthetic event.
    [build body] is used to fill in the event details. *)
