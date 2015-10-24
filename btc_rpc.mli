open Core.Std
open Async.Std

module Protocol : sig
  module Stats : sig
    type response =
      { connected_nodes : int
      ; known_nodes : int
      ; blockchain_length : int
      ; verified_length : int
      } with sexp_of
  end
end

module Client : sig
  val get_stats
    :  rpc_port:int
    -> Protocol.Stats.response Or_error.t Deferred.t
end

module Server : sig
  val start
    :  network:Network.t
    -> blockchain:Blockchain.t
    -> rpc_port:int
    -> unit Deferred.t
end

