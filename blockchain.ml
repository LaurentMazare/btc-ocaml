open Core.Std
open Async.Std

module Hardcoded = struct
  (* When answering an open Getheaders query, headers message should contain 2000 headers
     except for the last message that contains less headers. *)
  let max_headers = 2_000

  let get_more_headers_after = sec 600.

  (* If we haven't heard about the sync node while syncing in this span, try with another
     sync node. *)
  let sync_node_timeout = sec 60.
end

module Status = struct
  type t =
    | Not_connected
    | Syncing of Address.t
    | At_tip
end

module Header_node = struct
  type t =
    { header : Header.t option
    ; depth : int
    ; difficulty_sum : float
    ; hash : Hash.t
    } with fields

  let genesis_hash =
    Hash.of_hex "0x000000000019d6689c085ae165831e934ff763ae46a2a6c172b3f1b60a8ce26f"

  let genesis =
    { header = None
    ; depth = 0
    ; difficulty_sum = 0.
    ; hash = genesis_hash
    }
end

type t =
  { mutable status : Status.t
  ; headers : Header_node.t Hash.Table.t
  ; mutable current_tip : Header_node.t
  ; mutable has_changed_since_last_write : bool
  ; mutable last_batch_processed : Time.t
  ; blockchain_file : string
  ; stop : unit Ivar.t
  ; network : Network.t
  ; mutable checked_len : int
  } with fields

let process_header t (header : Header.t) ~mark_as_changed =
  let hash = Header.hash header in
  (* TODO: check [hash] vs [Header.hash header] *)
  match Hashtbl.find t.headers hash with
    (* XCR aalekseyev: If we return Ok here, this will let [process_headers] work even if we already know some
       of the blocks the peer is trying to send (e.g. when we are on an orphaned block).
       lmazare: indeed.*)
  | Some header_node -> Ok header_node
  | None ->
    match Hashtbl.find t.headers header.previous_block_header_hash with
    | Some previous_header ->
      let header_node =
        { Header_node.header = Some header
        ; depth = 1 + previous_header.depth
        ; difficulty_sum = Hash.difficulty hash +. previous_header.difficulty_sum
        ; hash
        }
      in
      Hashtbl.add_exn t.headers ~key:hash ~data:header_node;
      if mark_as_changed then
        t.has_changed_since_last_write <- true;
      if t.current_tip.difficulty_sum < header_node.difficulty_sum then
        t.current_tip <- header_node;
      Ok header_node
    | None ->
      Log.Global.error "Cannot find hash for previous block!\n  block: %s\n  prev:  %s\n  tip:   %s"
        (Hash.to_hex hash)
        (Hash.to_hex header.previous_block_header_hash)
        (Hash.to_hex t.current_tip.hash);
      Error "cannot find hash for previous block"

let write_blockchain_file t =
  let tmp_file = sprintf "%s.tmp" t.blockchain_file in
  (* Store the headers sorted by depth to make it easy to process them when reading the file. *)
  let headers =
    Hashtbl.to_alist t.headers
    |> List.sort ~cmp:(fun (_, hn1) (_, hn2) -> Int.compare hn1.Header_node.depth hn2.depth)
    |> List.map ~f:(fun (_, hn) -> hn.Header_node.header)
  in
  Writer.with_file tmp_file ~f:(fun writer ->
    Deferred.List.iter headers ~f:(function
      | None -> Deferred.unit
      | Some header ->
        Writer.write_bin_prot writer Header.bin_writer_t header;
        Writer.flushed writer)
  )
  >>= fun () ->
  (* The rename operation is atomic, this avoids corrupting the file if the process dies
     while writing it. *)
  Unix.rename ~src:tmp_file ~dst:t.blockchain_file

let process_headers t ~node ~headers =
  let address = Node.address node in
  match t.status with
  (* If we're currently syncing with [node], append the headers. *)
  | Syncing sync_address when Address.(=) sync_address address ->
    t.last_batch_processed <- Time.now ();
    let headers_len = List.length headers in
    let at_tip = headers_len < Hardcoded.max_headers in
    let headers_len_pre = Hashtbl.length t.headers in
    let last_header_node =
      List.fold headers ~init:None ~f:(fun acc (header : Header.t) ->
        match process_header t header ~mark_as_changed:true with
        | Ok header_node -> Some header_node
        | Error _ -> acc)
    in
    if at_tip then begin
      if Hashtbl.length t.headers = headers_len + headers_len_pre then
        t.status <- At_tip;
    end;
    Option.iter last_header_node ~f:(fun header_node ->
      Log.Global.debug "New blockchain length: %d, difficulty: %f."
        (Header_node.depth header_node)
        (Header_node.difficulty_sum header_node));
    if at_tip then ()
    else
      Option.iter last_header_node ~f:(fun header_node ->
        let from_hash = Header_node.hash header_node in
        Node.send node (Message.getheaders ~from_hash ~stop_hash:None)
      )
  | Syncing _ | Not_connected | At_tip ->
    (* Discard this message. *)
    ()

let need_syncing t ~now:now_ =
  match t.status with
  | Not_connected -> true
  | Syncing _ -> false
  | At_tip ->
    Time.(add t.last_batch_processed Hardcoded.get_more_headers_after <= now_)

let sync_timeout t ~now:now_ =
  let timeout =
    match t.status with
    | Not_connected | At_tip -> false
    | Syncing _ ->
      Time.(add t.last_batch_processed Hardcoded.sync_node_timeout <= now_)
  in
  if timeout then
    (* Maybe remove/blacklist the host ? *)
    t.status <- Not_connected

let start_syncing t node =
  t.status <- Syncing (Node.address node);
  (* CR aalekseyev: if the node doesn't know our tip (e.g. the node is not up to date, or
    we are on an orphaned block, this requests a download of the entire blockchain.
    We should instead send multiple recent hashes in ~from_hash (rename it to ~from_the_highest_of or something?) *)
  Node.send node (Message.getheaders ~from_hash:t.current_tip.hash ~stop_hash:None)

let close t =
  Ivar.fill_if_empty t.stop ()

let refresh t =
  let now = Time.now () in
  sync_timeout t ~now;
  let connected_nodes = Network.connected_nodes t.network in
  let connected_node_count = List.length connected_nodes in
  if 5 <= connected_node_count && need_syncing t ~now then begin
    List.nth_exn connected_nodes (Random.int connected_node_count)
    |> fun node -> start_syncing t node
  end

let create ~blockchain_file ~network =
  let stop = Ivar.create () in
  let headers = Hash.Table.of_alist_exn [ Header_node.genesis_hash, Header_node.genesis ] in
  let t =
    { status = Not_connected
    ; headers
    ; current_tip = Header_node.genesis
    ; blockchain_file
    ; has_changed_since_last_write = false
    ; last_batch_processed = Time.epoch
    ; stop
    ; network
    ; checked_len = 0
    }
  in
  Network.set_callbacks network ~process_headers:(process_headers t);
  Sys.file_exists blockchain_file
  >>= fun file_exists ->
  begin
    match file_exists with
    | `Yes ->
      Reader.with_file blockchain_file ~f:(fun reader ->
        let rec loop () =
          Reader.read_bin_prot reader Header.bin_reader_t
          >>= function
          | `Eof ->
            Deferred.unit
          | `Ok header ->
            process_header t header ~mark_as_changed:false
            |> ignore;
            loop ()
        in
        loop ()
      )
    | `No | `Unknown -> Deferred.unit
  end
  >>| fun () ->
  Log.Global.info "Read %d headers from %s."
    (Hashtbl.length t.headers) blockchain_file;
  let stop = Ivar.read stop in
  Clock.every' ~stop (sec 30.) (fun () ->
    Log.Global.debug "Current blockchain table size: %d, tip: %d %s"
      (Hashtbl.length t.headers)
      (Header_node.depth t.current_tip)
      (if t.has_changed_since_last_write then "writing blockchain file..." else "");
    if t.has_changed_since_last_write then begin
      write_blockchain_file t
      >>| fun () -> t.has_changed_since_last_write <- false
    end else Deferred.unit
  );
  Clock.every ~stop (sec 1.) (fun () -> refresh t);
  t

let blockchain_length t = Hashtbl.length t.headers
let tip_depth t = t.current_tip.depth
let tip_difficulty_sum t = t.current_tip.difficulty_sum
let tip_hash t = t.current_tip.hash
