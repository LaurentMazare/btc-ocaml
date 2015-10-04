(* corebuild -pkgs async test.native *)
(* dig seed.bitcoin.sipa.be *)
open Core.Std
open Async.Std

let () =
  Command.async_basic
    ~summary:"test"
    Command.Spec.(
      empty
      +> flag "-blockchain" (optional_with_default "./blockchain.bin" string)
        ~doc:"FILE file to load/store the blockchain headers from."
      +> anon ("address" %: string)
    )
    (fun blockchain_file ipv4_address () ->
      Network.create ~blockchain_file
      >>= fun network ->
      Network.add_node network ~ipv4_address ~port:8333;
      Deferred.never ()
      >>| fun () -> Network.close network
    )
  |> fun cmd -> Command.run cmd
