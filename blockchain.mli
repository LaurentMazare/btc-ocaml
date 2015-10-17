open Async.Std
type t

val create 
  :  blockchain_file : string
  -> network : Network.t
  -> t Deferred.t

val not_connected : t -> bool

val maybe_start_syncing : t -> Address.t -> [ `from_hash of Hash.t ] option

val close : t -> unit
