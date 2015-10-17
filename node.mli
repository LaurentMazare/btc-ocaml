open Core.Std
open Async.Std

module Status : sig
  type connected =
    { services : Int64.t
    ; send : Message.t -> unit
    ; port : int
    }
  type t =
    | Connected of connected
    | Pending
end

type t

val create : address:Address.t -> t
val send : t -> Message.t -> unit
val address : t -> Address.t
val status : t -> Status.t
val set_status : t -> Status.t -> unit
val last_seen : t -> Time.t
val set_last_seen : t -> Time.t -> unit
val interrupt : t -> unit Ivar.t
