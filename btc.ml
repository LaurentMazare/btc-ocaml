(* corebuild -pkgs async test.native *)
(* dig seed.bitcoin.sipa.be *)
open Core.Std
open Async.Std

let dns_server = "8.8.8.8"

let dns_domains =
  [ "dnsseed.bluematt.me"
  ]

let () =
  Command.async_basic
    ~summary:"test"
    Command.Spec.(
      empty
      +> flag "-log-level" (optional_with_default `Info Log.Level.arg)
        ~doc:"LOG-LEVEL debug|info|error"
      +> flag "-blockchain" (optional_with_default "./blockchain.bin" string)
        ~doc:"FILE file to load/store the blockchain headers from."
    )
    (fun log_level blockchain_file () ->
      Log.Global.set_level log_level;
      Network.create ()
      >>= fun network ->
      Blockchain.create ~blockchain_file ~network
      >>= fun blockchain ->
      Deferred.List.iter dns_domains ~f:(fun domain ->
        Dns_lookup.query ~dns_server ~domain ~f:(fun ipv4_address ->
          Network.add_node network ~ipv4_address ~port:8333)
      )
      >>= fun () ->
      Deferred.never ()
      >>| fun () -> Blockchain.close blockchain
    )
  |> fun cmd -> Command.run cmd
