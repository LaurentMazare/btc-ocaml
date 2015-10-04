open Core.Std
open Async.Std

type t

val create
  :  blockchain_file : string
  -> t Deferred.t

val add_node
  :  t
  -> ipv4_address : string
  -> port : int
  -> unit

val close : t -> unit
