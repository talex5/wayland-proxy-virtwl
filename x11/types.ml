type window = [`Window] Xid.t
type atom = [`Atom] Xid.t
type font = [`Font] Xid.t
type cursor = [`Cursor] Xid.t
type visual = [`Visual] Xid.t

type timestamp = int32

type reply = [
  | `Reply of Cstruct.t
  | `Error of Cstruct.t
  | `No_reply
]

type display = {
  socket : Lwt_unix.file_descr;
  from_server : Lwt_io.input Lwt_io.channel;
  roots : window list;
  mutable next_seq : int;
  atoms : (string, atom Lwt.t) Hashtbl.t;
  atom_names : (atom, string) Hashtbl.t;        (* Reverse of resolved [atoms] *)
  mutable last_resource_id : int32;             (* Note: we create a fixed number and assume we won't run out *)
  pending : (int * reply Lwt.u) Queue.t;        (* Expected sequence number and resolver *)
  mutable need_sync : bool;                     (* Send a sync before blocking *)
  wake_input : unit Lwt_condition.t;            (* Fires when the event loop might need to send a sync *)
  send_mutex : Lwt_mutex.t;                     (* Only one thread can send at a time *)
}
