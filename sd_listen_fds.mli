(** Systemd-style socket activation.

    See {{:https://www.freedesktop.org/software/systemd/man/latest/sd_listen_fds.html}} *)

open Eio.Std

val import : sw:Switch.t -> string -> (Eio_unix.Net.listening_socket_ty r option, string) result
(** [import ~sw name] takes the socket named [name] from the set of available sockets
    received from the parent process.

    Returns [Ok (Some listening_socket)] if a single socket with that name is available.

    Returns [Ok None] if no socket with this name is available
    (which could be because socket activation is not being used, or because it has already been imported).

    Returns [Error msg] if there are multiple sockets with that name,
    or if the environment variables are set up inconsistently. *)

val ensure_all_consumed : unit -> (unit, string) result
(** [ensure_all_consumed ()] checks that every socket passed to the process has been imported (with {!import}). *)
