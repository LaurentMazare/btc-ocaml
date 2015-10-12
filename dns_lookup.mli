open Core.Std
open Async.Std

val query
  :  dns_server : string
  -> domain : string
  -> f:(string -> unit)
  -> unit Deferred.t
