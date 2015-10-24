open Core.Std
open Async.Std

module Protocol = struct
  module Stats = struct
    type query = unit with sexp, bin_io

    type response =
      { connected_nodes : int
      ; known_nodes : int
      ; blockchain_length : int
      ; verified_length : int
      } with sexp, bin_io

    let rpc =
      Rpc.Rpc.create
        ~name:"stats"
        ~version:1
        ~bin_query
        ~bin_response
  end
end

module Client = struct
  let get_stats ~rpc_port =
    Rpc.Connection.client
      ~host:"127.0.0.1"
      ~port:rpc_port
      ()
    >>= function
    | Error exn -> return (Or_error.of_exn exn)
    | Ok connection ->
      Rpc.Rpc.dispatch
        Protocol.Stats.rpc
        connection
        ()
end
