open Core.Std
open Async.Std

module Hardcoded = struct
  (* When answering an open Getheaders query, headers message should contain 2000 headers
     except for the last message that contains less headers. *)
  let max_headers = 2_000

  let get_more_headers_after = sec 600.
end

module Status = struct
  type t =
    | Not_connected
    | Syncing of Address.t
    | At_tip
end

type t =
  { mutable status : Status.t
  ; mutable headers : Header.t list
  ; mutable current_tip_hash : Hash.t
  ; mutable has_changed_since_last_write : bool
  ; mutable last_batch_processed : Time.t
  ; blockchain_file : string
  } with fields

let process_header t (header : Header.t) ~mark_as_changed =
  if Hash.(=) header.previous_block_header_hash t.current_tip_hash then begin
    t.headers <- header :: t.headers;
    t.current_tip_hash <- Header.hash header;
    if mark_as_changed then
      t.has_changed_since_last_write <- true;
  end else
    Core.Std.printf "Header hash mismatch!\n%!"

let write_blockchain_file t =
  let tmp_file = sprintf "%s.tmp" t.blockchain_file in
  Writer.with_file tmp_file ~f:(fun writer ->
    Deferred.List.iter t.headers ~f:(fun header ->
      Writer.write_bin_prot writer Header.bin_writer_t header;
      Writer.flushed writer)
  )
  >>= fun () ->
  Unix.rename ~src:tmp_file ~dst:t.blockchain_file
  >>| fun () -> t.has_changed_since_last_write <- false

let genesis_hash =
  Hash.of_hex "0x000000000019d6689c085ae165831e934ff763ae46a2a6c172b3f1b60a8ce26f"

let create ~blockchain_file ~stop =
  let t =
    { status = Not_connected
    ; headers = []
    ; current_tip_hash = genesis_hash
    ; blockchain_file
    ; has_changed_since_last_write = false
    ; last_batch_processed = Time.epoch
    }
  in
  Sys.file_exists blockchain_file
  >>= fun file_exists ->
  begin
    match file_exists with
    | `Yes ->
      Reader.with_file blockchain_file ~f:(fun reader ->
        let rec loop acc =
          Reader.read_bin_prot reader Header.bin_reader_t
          >>= function
          | `Eof ->
            return acc
          | `Ok header ->
            loop (header :: acc)
        in
        loop []
      )
    | `No | `Unknown -> return []
  end
  >>| fun headers ->
  List.iter headers ~f:(fun header ->
    process_header t header ~mark_as_changed:false);
  Core.Std.printf "Read %d headers from %s.\n%!"
    (List.length t.headers) blockchain_file;
  Clock.every' ~stop (sec 30.) (fun () ->
    if t.has_changed_since_last_write then write_blockchain_file t
    else Deferred.unit
  );
  t

let process_headers t address headers =
  match t.status with
  | Syncing sync_address when Address.(=) sync_address address ->
    t.last_batch_processed <- Time.now ();
    let headers_len = List.length headers in
    let at_tip = List.length headers < Hardcoded.max_headers in
    let headers_len_pre = List.length t.headers in
    List.iter headers ~f:(fun (header : Header.t) ->
      process_header t header ~mark_as_changed:true);
    if at_tip then begin
      let headers_len_post = List.length t.headers in
      if headers_len_post = headers_len + headers_len_pre then
        t.status <- At_tip;
    end;
    Core.Std.printf "New blockchain length: %d\n%!" (List.length t.headers);
    if at_tip then `nothing else `get_headers_from t.current_tip_hash
  | Syncing _ | Not_connected | At_tip -> `nothing

let not_connected t =
  match t.status with
  | Not_connected -> true
  | Syncing _ | At_tip -> false

let maybe_start_syncing t node =
  let start_syncing =
    match t.status with
    | Syncing _ -> false
    | Not_connected -> true
    | At_tip ->
      Time.(add t.last_batch_processed Hardcoded.get_more_headers_after <= now ())
  in
  if start_syncing then begin
    t.status <- Syncing node;
    Some (`from_hash t.current_tip_hash)
  end else None
