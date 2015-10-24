open Core.Std
open Async.Std

module Hardcoded = struct
  let min_nodes = 64
  let max_nodes = 128
  let refresh_span = sec 15.
  let drop_last_seen_older_than = sec 1800.
  let drop_pending_older_than = sec 30.
  let send_ping_after = sec 600.

  let port = 8333
end

type t =
  { per_address : Node.t Address.Table.t
  ; interrupt : unit Ivar.t
  ; mutable process_headers : (node:Node.t -> headers:Header.t list -> unit)
  }

let connected_nodes t =
  Hashtbl.fold t.per_address ~init:[] ~f:(fun ~key:_ ~data acc ->
    match Node.status data with
    | Connected _ -> data :: acc
    | Pending -> acc)

let known_nodes t = Hashtbl.to_alist t.per_address |> List.map ~f:snd

let status_string = function
  | `Eof_with_unconsumed_data str ->
    sprintf "Received eof with unconsumed data (%d bytes)." (String.length str)
  | `Eof -> "Received eof."
  | `Stopped () -> assert false

let rec handle_connection t (node : Node.t) reader writer =
  let log_debug str ~now =
    Log.Global.debug ~time:now "%s %s"
      (Address.ipv4 (Node.address node) |> Option.value ~default:"")
      str
  in
  let log_error str ~now =
    Log.Global.debug ~time:now "%s %s"
      (Address.ipv4 (Node.address node) |> Option.value ~default:"")
      str
  in
  let send t =
    if Writer.is_open writer then
      let msg = Message.to_string t in
      Writer.write writer msg
  in
  let f msg_or_error =
    let now = Time.now () in
    match msg_or_error with
    | Error error -> log_error error ~now
    | Ok msg ->
      log_debug (Sexp.to_string (Message.sexp_of_t msg)) ~now;
      match (msg : Message.t) with
      | Version version ->
        let status =
          { Node.Status.services = Message.Version.addr_trans_services version
          ; send
          ; port = Message.Version.addr_trans_port version
          }
        in
        Node.set_status node (Node.Status.Connected status);
        send Message.Verack
      | Ping ping -> send (Message.Pong { Message.Pong.nonce = Message.Ping.nonce ping })
      | Addr addrs ->
        List.iter addrs ~f:(fun addr ->
          let address = Message.Addr.ip_address addr in
          if Address.is_ipv4 address then
            add_node t ~address ~port:(Message.Addr.port addr))
      | Getaddr ->
        let addrs =
          Hashtbl.fold t.per_address ~init:[] ~f:(fun ~key:_ ~data acc ->
            match Node.status data with
            | Pending -> acc
            | Connected connected ->
              let addr =
                Message.Addr.Fields_of_elem.create
                  ~time:(Node.last_seen data)
                  ~services:connected.services
                  ~ip_address:(Node.address data)
                  ~port:connected.port
              in
              addr :: acc
          )
        in
        send (Message.Addr addrs)
      | Verack -> ()
      | Inv _invs -> ()
      | Getheaders _ -> ()
      | Getblocks _ -> ()
      | Headers headers -> t.process_headers ~node ~headers
      | Notfound _ -> ()
      | Getdata _ -> ()
      | Tx _ -> ()
      | Block _ -> ()
      | Mempool -> ()
      | Merkleblock _ -> ()
      | Reject reject ->
        log_error (sprintf "reject %s" (Message.Reject.sexp_of_t reject |> Sexp.to_string)) ~now
      | Pong _ -> Node.set_last_seen node now
  in
  send (Message.version ());
  Reader.read_one_chunk_at_a_time reader
    ~handle_chunk:(fun chunk ~pos ~len ->
      let `Consumed consumed = Message.handle_chunk chunk ~f ~pos ~len in
      return (`Consumed (consumed, `Need_unknown)))
  >>| fun status ->
  log_debug (status_string status) ~now:(Time.now ());
  Ivar.fill_if_empty (Node.interrupt node) ();
  Hashtbl.remove t.per_address (Node.address node)
and connect t node ~port =
  let interrupt =
    Deferred.any
      [ Ivar.read t.interrupt
      ; Ivar.read (Node.interrupt node)
      ]
  in
  let handle_connection _ reader writer =
    don't_wait_for (interrupt >>= fun () -> Reader.close reader >>= fun () -> Writer.close writer);
    handle_connection t node reader writer
  in
  (* TODO: support ipv6. *)
  Option.value_map (Address.ipv4 (Node.address node))
    ~default:Deferred.unit
    ~f:(fun ipv4 ->
      Tcp.with_connection (Tcp.to_host_and_port ipv4 port) handle_connection ~interrupt)
and add_node t ~address ~port =
  if Hashtbl.mem t.per_address address || Hashtbl.length t.per_address >= Hardcoded.max_nodes then ()
  else begin
    let node = Node.create ~address in
    Hashtbl.add_exn t.per_address ~key:address ~data:node;
    don't_wait_for (Monitor.try_with (fun () -> connect t node ~port) >>| ignore)
  end

let close t =
  Ivar.fill_if_empty t.interrupt ()

let refresh t =
  let now = Time.now () in
  let to_remove =
    Hashtbl.fold t.per_address ~init:[] ~f:(fun ~key ~data acc ->
      let last_seen = Time.diff now (Node.last_seen data) in
      let drop =
        Time.Span.(<) Hardcoded.drop_last_seen_older_than last_seen ||
        match Node.status data with
        | Pending -> Time.Span.(<) Hardcoded.drop_pending_older_than last_seen
        | Connected _ -> false
      in
      if drop then begin
        Ivar.fill_if_empty (Node.interrupt data) ();
        key :: acc
      end else begin
        if Time.Span.(<) Hardcoded.send_ping_after last_seen then
          Node.send data (Message.Ping { Message.Ping.nonce = Int64.zero });
        acc
      end)
  in
  List.iter to_remove ~f:(Hashtbl.remove t.per_address);
  let connected_nodes =
    Hashtbl.fold t.per_address ~init:[] ~f:(fun ~key:_ ~data acc ->
      match Node.status data with
      | Pending -> acc
      | Connected _connected -> data :: acc
    )
  in
  let connected_node_count = List.length connected_nodes in
  if 0 < connected_node_count && connected_node_count < Hardcoded.min_nodes then begin
    List.nth_exn connected_nodes (Random.int connected_node_count)
    |> fun node -> Node.send node Message.Getaddr
  end;
  Log.Global.debug "%d/%d node(s)."
    connected_node_count
    (Hashtbl.length t.per_address)

let create () =
  let interrupt = Ivar.create () in
  let stop = Ivar.read interrupt in
  let process_headers ~node:_ ~headers:_ = () in
  let t =
    { per_address = Address.Table.create ()
    ; interrupt
    ; process_headers
    }
  in
  Clock.every ~stop Hardcoded.refresh_span (fun () -> refresh t);
  let error_handler addr exn =
    Log.Global.error "Error in server: %s %s"
      (Socket.Address.Inet.to_string addr)
      (Exn.to_string exn)
  in
  Tcp.Server.create
    ~on_handler_error:(`Call error_handler)
    (Tcp.on_port Hardcoded.port)
    (fun address reader writer ->
      let address = Socket.Address.Inet.to_string address in
      Log.Global.debug "Rcvd %s." address;
      let node = Node.create ~address:(Address.of_string address) in
      handle_connection t node reader writer
    )
  >>| fun (_ : (_, _) Tcp.Server.t) ->
  t

let set_callbacks t ~process_headers =
  t.process_headers <- process_headers

let add_node t ~ipv4_address ~port =
  Option.iter (Address.of_ipv4 ipv4_address) ~f:(fun address ->
    add_node t ~address ~port)
