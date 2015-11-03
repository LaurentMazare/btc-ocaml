open Async.Std
type t

val create 
  :  blockchain_file : string
  -> network : Network.t
  -> t Deferred.t

val close : t -> unit

val blockchain_length : t -> int
val tip_depth : t -> int
val tip_difficulty_sum : t -> float
val tip_hash : t -> Hash.t
