open Core.Std
open Async.Std

let consume_domain buffer =
  let rec loop () =
    let len = Iobuf.Consume.uint8 buffer in
    if len = 0 then ()
    else if len land 192 > 0 then begin
      let _ptr = Iobuf.Consume.uint8 buffer in
      ()
    end else begin
      let _query = Iobuf.Consume.string ~len buffer in
      loop ()
    end
  in
  loop ();
  let _qtype = Iobuf.Consume.uint16_be buffer in
  let _qclass = Iobuf.Consume.uint16_be buffer in
  ()

let query ~dns_server ~domain ~f =
  let udp_sendto = Udp.sendto () |> Or_error.ok_exn in
  Udp.bind_any ()
  >>= fun udp ->
  let iobuf = Iobuf.create ~len:8192 in
  Iobuf.Fill.uint16_be iobuf (Random.int 65536);
  Iobuf.Fill.uint8 iobuf 1;
  Iobuf.Fill.uint8 iobuf 0;
  Iobuf.Fill.uint16_be iobuf 1; (* QDCOUNT *)
  Iobuf.Fill.uint16_be iobuf 0; (* ANCOUNT *)
  Iobuf.Fill.uint16_be iobuf 0; (* NSCOUNT *)
  Iobuf.Fill.uint16_be iobuf 0; (* ARCOUNT *)
  List.iter (String.split domain ~on:'.') ~f:(fun domain ->
    Iobuf.Fill.uint8 iobuf (String.length domain);
    Iobuf.Fill.string iobuf domain;
  );
  Iobuf.Fill.uint8 iobuf 0;
  Iobuf.Fill.uint16_be iobuf 1; (* QTYPE *)
  Iobuf.Fill.uint16_be iobuf 1; (* QCLASS *)
  Iobuf.flip_lo iobuf;
  Unix.Inet_addr.of_string_or_getbyname dns_server
  >>= fun dns_server ->
  udp_sendto (Socket.fd udp) iobuf (Socket.Address.Inet.create dns_server ~port:53)
  >>= fun () ->
    Udp.read_loop (Socket.fd udp) (fun buffer ->
      let _nonce = Iobuf.Consume.uint16_be buffer in
      let _flags = Iobuf.Consume.uint16_be buffer in
      let qdcount = Iobuf.Consume.uint16_be buffer in
      let ancount = Iobuf.Consume.uint16_be buffer in
      let _nscount = Iobuf.Consume.uint16_be buffer in
      let _arcount = Iobuf.Consume.uint16_be buffer in
      for _i = 0 to qdcount - 1 do
        consume_domain buffer
      done;
      for _i = 0 to ancount - 1 do
        consume_domain buffer;
        let _time = Iobuf.Consume.uint32_be buffer in
        let len = Iobuf.Consume.uint16_be buffer in
        let rdata = Iobuf.Consume.string ~len buffer in
        if len = 4 then
          f (String.to_list rdata
            |> List.map ~f:Char.to_int 
            |> List.map ~f:Int.to_string 
            |> String.concat ~sep:".")
      done
    )

