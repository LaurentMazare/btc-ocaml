open Async.Std
type t

val create 
  :  blockchain_file : string
  -> network : Network.t
  -> t Deferred.t

val close : t -> unit

val blockchain_length : t -> int
val verified_length : t -> int
