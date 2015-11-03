open Core.Std
open Async.Std

module Protocol = struct
  module Stats = struct
    type query = unit with sexp, bin_io

    type response =
      { connected_nodes : Address.t list
      ; known_nodes : Address.t list
      ; blockchain_length : int
      ; tip_depth : int
      ; tip_difficulty_sum : float
      ; tip_hash : Hash.t
      } with sexp, bin_io

    let rpc =
      Rpc.Rpc.create
        ~name:"stats"
        ~version:1
        ~bin_query
        ~bin_response

    let handle_query ~network ~blockchain =
      fun _connection_state () ->
        return
          { connected_nodes = Network.connected_nodes network |> List.map ~f:Node.address
          ; known_nodes = Network.known_nodes network |> List.map ~f:Node.address
          ; blockchain_length = Blockchain.blockchain_length blockchain
          ; tip_depth = Blockchain.tip_depth blockchain
          ; tip_difficulty_sum = Blockchain.tip_difficulty_sum blockchain
          ; tip_hash = Blockchain.tip_hash blockchain
          }
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

module Server = struct
  let start ~network ~blockchain ~rpc_port =
    let implementations =
      [ Rpc.Rpc.implement Protocol.Stats.rpc (Protocol.Stats.handle_query ~network ~blockchain)
      ]
    in
    let implementations =
      Rpc.Implementations.create_exn
        ~on_unknown_rpc:`Continue
        ~implementations
    in
    Rpc.Connection.serve
      ~implementations
      ~initial_connection_state:(fun _address _connection -> ())
      ~where_to_listen:(Tcp.on_port rpc_port)
      ()
    >>| fun _tcp_server ->
    ()
end
