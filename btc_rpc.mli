open Core.Std
open Async.Std

module Protocol : sig
  module Stats : sig
    type response =
      { connected_nodes : Address.t list
      ; known_nodes : Address.t list
      ; blockchain_length : int
      ; tip_depth : int
      ; tip_difficulty_sum : float
      ; tip_hash : Hash.t
      } with sexp, bin_io
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

