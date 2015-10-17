open Async.Std
type t

val create 
  :  blockchain_file : string
  -> network : Network.t
  -> t Deferred.t

val close : t -> unit
