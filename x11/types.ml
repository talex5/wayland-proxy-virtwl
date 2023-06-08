open Eio.Std

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
  sw : Eio.Switch.t;
  socket : Eio.Net.stream_socket_ty r;
  from_server : Eio.Buf_read.t;
  roots : window list;
  mutable next_seq : int;
  atoms : (string, atom Promise.or_exn) Hashtbl.t;
  atom_names : (atom, string) Hashtbl.t;        (* Reverse of resolved [atoms] *)
  mutable last_resource_id : int32;             (* Note: we create a fixed number and assume we won't run out *)
  pending : (int * reply Promise.u) Queue.t;    (* Expected sequence number and resolver *)
  mutable need_sync : bool;                     (* Send a sync before blocking *)
  wake_input : Eio.Condition.t;                 (* Fires when the event loop might need to send a sync *)
  send_mutex : Eio.Mutex.t;                     (* Only one thread can send at a time *)
}
