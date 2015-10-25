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

  (* The number of nodes to be queried to check if a header is in the consensus
     blockchain. At least half of the nodes have to answer before the timeout so that we
     do not reject this header. *)
  let check_nodes = 10
  let check_timeout = sec 60.

  let on_check_failures_decrease_checked_len_by = 100
end

module Status = struct
  type t =
    | Not_connected
    | Syncing of Address.t
    | At_tip
end

module Header_check = struct
  type t =
    { addresses : Address.Hash_set.t
    ; hash_to_check : Hash.t
    ; hash_len : int
    ; start_time : Time.t
    }
end

type t =
  { mutable status : Status.t
  ; mutable headers : Header.t list
  ; mutable header_len : int
  ; mutable current_tip_hash : Hash.t
  ; mutable has_changed_since_last_write : bool
  ; mutable last_batch_processed : Time.t
  ; blockchain_file : string
  ; stop : unit Ivar.t
  ; network : Network.t
  ; mutable header_check : Header_check.t option
  ; mutable checked_len : int
  } with fields

let process_header t (header : Header.t) ~mark_as_changed =
  if Hash.(=) header.previous_block_header_hash t.current_tip_hash then begin
    t.headers <- header :: t.headers;
    t.header_len <- 1 + t.header_len;
    t.current_tip_hash <- Header.hash header;
    if mark_as_changed then
      t.has_changed_since_last_write <- true;
  end else
    Log.Global.error
      "Header current tip hash mismatch on block %d!\n  tip: %s\n  got: %s\n  nxt: %s."
      t.header_len
      (Hash.to_hex t.current_tip_hash)
      (Hash.to_hex header.previous_block_header_hash)
      (Hash.to_hex (Header.hash header))

let write_blockchain_file t =
  let tmp_file = sprintf "%s.tmp" t.blockchain_file in
  let headers = List.rev t.headers in
  Writer.with_file tmp_file ~f:(fun writer ->
    Deferred.List.iter headers ~f:(fun header ->
      Writer.write_bin_prot writer Header.bin_writer_t header;
      Writer.flushed writer)
  )
  >>= fun () ->
  (* The rename operation is atomic, this avoids corrupting the file if the process dies
     while writing it. *)
  Unix.rename ~src:tmp_file ~dst:t.blockchain_file

let genesis_hash =
  Hash.of_hex "0x000000000019d6689c085ae165831e934ff763ae46a2a6c172b3f1b60a8ce26f"

let process_headers t ~node ~headers =
  let address = Node.address node in
  match t.status with
  (* If we're currently syncing with [node], append the headers. *)
  | Syncing sync_address when Address.(=) sync_address address ->
    t.last_batch_processed <- Time.now ();
    let headers_len = List.length headers in
    let at_tip = headers_len < Hardcoded.max_headers in
    let headers_len_pre = t.header_len in
    List.iter headers ~f:(fun (header : Header.t) ->
      process_header t header ~mark_as_changed:true);
    if at_tip then begin
      if t.header_len = headers_len + headers_len_pre then
        t.status <- At_tip;
    end;
    Log.Global.debug "New blockchain length: %d." (List.length t.headers);
    if at_tip then ()
    else
      Node.send node (Message.getheaders ~from_hash:t.current_tip_hash ~stop_hash:None)
  | Syncing _ | Not_connected | At_tip ->
    match t.header_check, headers with
    | Some header_check, [ header ] when Hash_set.mem header_check.addresses address
          && Hash.(=) (Header.hash header) header_check.hash_to_check ->
      Hash_set.remove header_check.addresses address;
      Log.Global.debug "Received header confirmation.";
      (* If we had enough confirmations consider this hash as confirmed. *)
      if Hash_set.length header_check.addresses < Hardcoded.check_nodes / 2 then begin
        t.header_check <- None;
        t.checked_len <- header_check.hash_len
      end
    (* Discard this message. *)
    | _ -> ()

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
  Node.send node (Message.getheaders ~from_hash:t.current_tip_hash ~stop_hash:None)

let check_timeout t ~now:now_ =
  match t.header_check with
  | None -> ()
  | Some header_check ->
    if Time.(add header_check.start_time Hardcoded.check_timeout <= now_) then begin
      Log.Global.debug "Header check timout.";
      t.headers <- List.drop t.headers (t.header_len - t.checked_len);
      t.header_len <- t.checked_len;
      t.current_tip_hash <-
        Option.value_map (List.hd t.headers) ~default:genesis_hash ~f:Header.hash;
      t.has_changed_since_last_write <- true;
      t.header_check <- None;
      (* Maybe remove/blacklist the host ? *)
      t.status <- Not_connected;
      (* As the check failed, we are suspicious that even previously checked headers are
         correct so decrease the checked length. *)
      t.checked_len <-
        max 0 (t.checked_len - Hardcoded.on_check_failures_decrease_checked_len_by)
    end

let close t =
  Ivar.fill_if_empty t.stop ()

let refresh t =
  let now = Time.now () in
  sync_timeout t ~now;
  check_timeout t ~now;
  let connected_nodes = Network.connected_nodes t.network in
  let connected_node_count = List.length connected_nodes in
  if 5 <= connected_node_count && need_syncing t ~now then begin
    List.nth_exn connected_nodes (Random.int connected_node_count)
    |> fun node -> start_syncing t node
  end;
  if Hardcoded.check_nodes + 1 <= connected_node_count && t.checked_len < t.header_len
  && Option.is_none t.header_check then begin
    let addresses = Address.Hash_set.create () in
    let prev_header = (List.hd_exn t.headers).previous_block_header_hash in
    let getheaders =
      Message.getheaders
        ~from_hash:prev_header
        ~stop_hash:(Some t.current_tip_hash)
    in
    (* Use some Fisher-Yates sampling to get some distinct random nodes. *)
    let connected_nodes = Array.of_list connected_nodes in
    for i = 0 to Hardcoded.check_nodes do
      let rnd_i = Random.int (connected_node_count - i) in
      let rnd_node = connected_nodes.(rnd_i) in
      connected_nodes.(rnd_i) <- connected_nodes.(i);
      connected_nodes.(i) <- rnd_node;
      let address = Node.address rnd_node in
      match t.status with
      | Syncing sync_address when Address.(=) sync_address address -> ()
      | _ ->
        Hash_set.add addresses address;
        Node.send rnd_node getheaders
    done;
    Log.Global.debug "Checking %s %d." (Hash.to_hex t.current_tip_hash) t.header_len;
    let header_check =
      { Header_check.addresses
      ; hash_to_check = t.current_tip_hash
      ; hash_len = t.header_len
      ; start_time = now
      }
    in
    t.header_check <- Some header_check
  end

let create ~blockchain_file ~network =
  let stop = Ivar.create () in
  let t =
    { status = Not_connected
    ; headers = []
    ; header_len = 0
    ; current_tip_hash = genesis_hash
    ; blockchain_file
    ; has_changed_since_last_write = false
    ; last_batch_processed = Time.epoch
    ; stop
    ; network
    ; header_check = None
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
            process_header t header ~mark_as_changed:false;
            loop ()
        in
        loop ()
      )
    | `No | `Unknown -> Deferred.unit
  end
  >>| fun () ->
  Log.Global.info "Read %d headers from %s."
    t.header_len blockchain_file;
  let stop = Ivar.read stop in
  Clock.every' ~stop (sec 30.) (fun () ->
    Log.Global.debug "Current blockchain length: %d, verified %d. %s"
      t.header_len
      t.checked_len
      (if t.has_changed_since_last_write then "writing blockchain file..." else "");
    if t.has_changed_since_last_write then begin
      write_blockchain_file t
      >>| fun () -> t.has_changed_since_last_write <- false
    end else Deferred.unit
  );
  Clock.every ~stop (sec 1.) (fun () -> refresh t);
  t

let blockchain_length t = t.header_len
let verified_length t = t.checked_len
let tip_hash t = t.current_tip_hash
