(* corebuild -pkgs async test.native *)
(* dig seed.bitcoin.sipa.be *)
open Core.Std
open Async.Std

let dns_server = "8.8.8.8"

let dns_domains =
  [ "dnsseed.bluematt.me"
  ]

let default_rpc_port = 12314

let start_server =
  Command.async_basic ~summary:"start the btc-ocaml server"
    Command.Spec.(
      empty
      +> flag "-log-level" (optional_with_default `Info Log.Level.arg)
        ~doc:"LOG-LEVEL debug|info|error"
      +> flag "-blockchain" (optional_with_default "./blockchain.bin" string)
        ~doc:"FILE file to load/store the blockchain headers from."
      +> flag "-rpc-port" (optional_with_default default_rpc_port int)
        ~doc:"PORT port number for the RPC server."
    )
    (fun log_level blockchain_file rpc_port () ->
      Log.Global.set_level log_level;
      Network.create ()
      >>= fun network ->
      Blockchain.create ~blockchain_file ~network
      >>= fun blockchain ->
      Btc_rpc.Server.start ~network ~blockchain ~rpc_port
      >>= fun () ->
      Deferred.List.iter dns_domains ~f:(fun domain ->
        Dns_lookup.query ~dns_server ~domain ~f:(fun ipv4_address ->
          Network.add_node network ~ipv4_address ~port:Network.Hardcoded.port)
      )
      >>= fun () ->
      Deferred.never ()
      >>| fun () -> Blockchain.close blockchain
    )

let stats =
  Command.async_basic ~summary:"get some stats on the current server status"
    Command.Spec.(
      empty
      +> flag "-rpc-port" (optional_with_default default_rpc_port int)
        ~doc:"PORT port number for the RPC server."
    )
    (fun rpc_port () ->
      Btc_rpc.Client.get_stats ~rpc_port
      >>| fun stats ->
      let { Btc_rpc.Protocol.Stats.connected_nodes
          ; known_nodes
          ; blockchain_length
          ; verified_length } = Or_error.ok_exn stats
      in
      printf "Connected nodes: %d\n" connected_nodes;
      printf "Known nodes: %d\n" known_nodes;
      printf "Blockchain length: %d\n" blockchain_length;
      printf "Verified length: %d\n" verified_length
    )


let client_group =
  Command.group ~summary:"btc-ocaml client"
    [ "stats", stats
    ]

let server_group =
  Command.group ~summary:"btc-ocaml server"
    [ "start", start_server
    ]

let () =
  Command.run (Command.group ~summary:"btc-ocaml"
    [ "client", client_group
    ; "server", server_group
    ])

