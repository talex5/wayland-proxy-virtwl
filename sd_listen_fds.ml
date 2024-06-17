open Eio.Std

module M = Map.Make(String)

let (let*) = Result.bind

let listen_fds_start = 3

let init () =
  match int_of_string (Sys.getenv "LISTEN_FDS") with
  | exception Not_found -> Error "LISTEN_FDS not set"
  | exception Failure _ -> Error "LISTEN_FDS not an integer"
  | total_fds ->
    let names =
      match Sys.getenv_opt "LISTEN_FDNAMES" |> Option.value ~default:"" with
      | "" -> []
      | fdnames -> String.split_on_char ':' fdnames
    in
    if List.length names <> total_fds then Error "Wrong number of LISTEN_FDNAMES provided"
    else (
      let v = ref M.empty in
      names |> List.iteri (fun i name ->
          v := M.add_to_list name (i + listen_fds_start) !v
        );
      Ok v
    )

(* Maps not-yet-imported socket names to FDs (in reverse order). *)
let available : (int list M.t ref, string) result =
  match Sys.getenv_opt "LISTEN_PID" with
  | Some p when p = string_of_int (Unix.getpid ()) -> init ()
  | _ -> Ok (ref M.empty)       (* Socket activation is not being used, so no sockets *)

let take name =
  let* available in
  match M.find_opt name !available with
  | None -> Ok []
  | Some fds ->
    available := M.remove name !available;
    Ok fds

let listening_socket_from_fd ~sw fd =
  let fd : Unix.file_descr = Obj.magic (fd : int) in
  Unix.clear_nonblock fd;
  Unix.set_close_on_exec fd;
  (Eio_unix.Net.import_socket_listening ~sw ~close_unix:true fd :> Eio_unix.Net.listening_socket_ty r)

let import ~sw name =
  let* fds = take name in
  match fds with
  | [] -> Ok None
  | [fd] -> Ok (Some (listening_socket_from_fd ~sw fd))
  | _ -> Fmt.error "Multiple sockets named %S from service manager!" name

let ensure_all_consumed () =
  let* available in
  match M.bindings !available with
  | [] -> Ok ()
  | fds ->
    Fmt.error "Unexpected extra sockets %a from service manager"
      Fmt.Dump.(list string) (List.map fst fds)
