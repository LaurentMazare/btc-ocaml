open Core.Std
open Async.Std

module Status = struct
  type connected =
    { services : Int64.t
    ; send : Message.t -> unit
    ; port : int
    }
  type t =
    | Connected of connected
    | Pending
end

type t =
  { mutable last_seen : Time.t
  ; address : Address.t
  ; interrupt : unit Ivar.t
  ; mutable status : Status.t
  } with fields

let create ~address =
  { last_seen = Time.now ()
  ; address
  ; status = Pending
  ; interrupt = Ivar.create ()
  }

let send t msg =
  match t.status with
  | Pending -> ()
  | Connected connected -> connected.send msg
