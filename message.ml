open Core.Std

let protocol_version = 70002

let consume_compact_uint iobuf =
  let len_len = Iobuf.Consume.uint8 iobuf in
  if len_len <= 252 then len_len
  else if len_len = 253 then Iobuf.Consume.uint16_le iobuf
  else if len_len = 254 then Iobuf.Consume.uint32_le iobuf
  else if len_len = 255 then Iobuf.Consume.int64_le_trunc iobuf
  else failwithf "incorrect len len %d" len_len ()

let fill_compact_uint iobuf int =
  if int <= 252 then
    Iobuf.Fill.uint8 iobuf int
  else if int <= 0xFFFF then
    Iobuf.Fill.uint16_le iobuf int
  else if int <= 0xFFFFFFFF then
    Iobuf.Fill.uint32_le iobuf int
  else
    Iobuf.Fill.int64_le_trunc iobuf int

let consume_string iobuf =
  let len = consume_compact_uint iobuf in
  Iobuf.Consume.string iobuf ~len

let fill_string iobuf str =
  fill_compact_uint iobuf (String.length str);
  Iobuf.Fill.string iobuf str
 
let consume_list iobuf consume_elem =
  let len = consume_compact_uint iobuf in
  List.init len ~f:(fun _ -> consume_elem iobuf)

let fill_list fill_elem iobuf list =
  fill_compact_uint iobuf (List.length list);
  List.iter list ~f:(fill_elem iobuf)

let consume_hash iobuf = Iobuf.Consume.string iobuf ~len:32
let fill_hash iobuf str = Iobuf.Fill.padded_fixed_string iobuf str ~len:32 ~padding:'\000'

module Version = struct
  type t =
    { version : int
    ; services : Int64.t
    ; timestamp : Time.t
    ; addr_recv_services : Int64.t
    ; addr_recv_addr : Address.t
    ; addr_recv_port : int
    ; addr_trans_services : Int64.t
    ; addr_trans_addr : Address.t
    ; addr_trans_port : int
    ; nonce : Int64.t
    ; user_agent : string
    ; start_height : int
    ; relay : int
    } with sexp, fields

  let consume_iobuf iobuf =
    let version = Iobuf.Consume.int32_le iobuf in
    let services = Iobuf.Consume.int64_t_le iobuf in
    let timestamp = Iobuf.Consume.int64_t_le iobuf |> Int64.to_float |> Time.of_float in
    let addr_recv_services = Iobuf.Consume.int64_t_le iobuf in
    let addr_recv_addr = Iobuf.Consume.string iobuf ~len:16 |> Address.of_string in
    let addr_recv_port = Iobuf.Consume.int16_be iobuf in
    let addr_trans_services = Iobuf.Consume.int64_t_le iobuf in
    let addr_trans_addr = Iobuf.Consume.string iobuf ~len:16 |> Address.of_string in
    let addr_trans_port = Iobuf.Consume.int16_be iobuf in
    let nonce = Iobuf.Consume.int64_t_le iobuf in
    let user_agent = consume_string iobuf in
    let start_height = Iobuf.Consume.int32_le iobuf in
    let relay =
      if version <= 70001 then 1
      else Iobuf.Consume.uint8 iobuf
    in
    Fields.create
      ~version
      ~services
      ~timestamp
      ~addr_recv_services
      ~addr_recv_addr
      ~addr_recv_port
      ~addr_trans_services
      ~addr_trans_addr
      ~addr_trans_port
      ~nonce
      ~user_agent
      ~start_height
      ~relay

  let fill_iobuf iobuf t =
    let write f g = fun field -> f iobuf (Field.get field t |> g) in
    Fields.iter
      ~version:(write Iobuf.Fill.int32_le Fn.id)
      ~services:(write Iobuf.Fill.int64_t_le Fn.id)
      ~timestamp:(write Iobuf.Fill.int64_le (fun time -> Time.to_epoch time |> Float.to_int))
      ~addr_recv_services:(write Iobuf.Fill.int64_t_le Fn.id)
      ~addr_recv_addr:(write (Iobuf.Fill.padded_fixed_string ~padding:'\000' ~len:16) Address.to_string)
      ~addr_recv_port:(write Iobuf.Fill.int16_be Fn.id)
      ~addr_trans_services:(write Iobuf.Fill.int64_t_le Fn.id)
      ~addr_trans_addr:(write (Iobuf.Fill.padded_fixed_string ~padding:'\000' ~len:16) Address.to_string)
      ~addr_trans_port:(write Iobuf.Fill.int16_be Fn.id)
      ~nonce:(write Iobuf.Fill.int64_t_le Fn.id)
      ~user_agent:(write fill_string Fn.id)
      ~start_height:(write Iobuf.Fill.int32_le Fn.id)
      ~relay:(write Iobuf.Fill.uint8 Fn.id)
end

module Ping = struct
  type t =
    { nonce : Int64.t
    } with sexp, fields

  let consume_iobuf iobuf =
    Fields.map
      ~nonce:(fun _ -> Iobuf.Consume.int64_t_le iobuf)

  let fill_iobuf iobuf t =
    let write f = fun field -> f iobuf (Field.get field t) in
    Fields.iter
      ~nonce:(write Iobuf.Fill.int64_t_le)
end

module Pong = struct
  type t =
    { nonce : Int64.t
    } with sexp, fields

  let consume_iobuf iobuf =
    Fields.map
      ~nonce:(fun _ -> Iobuf.Consume.int64_t_le iobuf)

  let fill_iobuf iobuf t =
    let write f = fun field -> f iobuf (Field.get field t) in
    Fields.iter
      ~nonce:(write Iobuf.Fill.int64_t_le)
end

module Addr = struct
  type elem =
    { time : Time.t
    ; services : Int64.t
    ; ip_address : Address.t
    ; port : int
    } with sexp, fields

  let consume_elem iobuf =
    let time = Iobuf.Consume.uint32_le iobuf |> float |> Time.of_float in
    let services = Iobuf.Consume.int64_t_le iobuf in
    let ip_address = Iobuf.Consume.string iobuf ~len:16 |> Address.of_string in
    let port = Iobuf.Consume.int16_be iobuf in
    Fields_of_elem.create
      ~time
      ~services
      ~ip_address
      ~port

  let fill_elem iobuf elem =
    let write f g = fun field -> f iobuf (Field.get field elem |> g) in
    Fields_of_elem.iter
      ~time:(write Iobuf.Fill.uint32_le (fun time -> Time.to_epoch time |> Float.to_int))
      ~services:(write Iobuf.Fill.int64_t_le Fn.id)
      ~ip_address:(write (Iobuf.Fill.padded_fixed_string ~padding:'\000' ~len:16) Address.to_string)
      ~port:(write Iobuf.Fill.int16_be Fn.id)

  type t = elem list with sexp

  let consume_iobuf iobuf = consume_list iobuf consume_elem

  let fill_iobuf = fill_list fill_elem
end

module Inv = struct
  type type_identifier =
    | Msg_tx
    | Msg_block
    | Msg_filtered_block
    | Unknown of int
  with sexp

  let type_identifier_of_int = function
    | 1 -> Msg_tx
    | 2 -> Msg_block
    | 3 -> Msg_filtered_block
    | i -> Unknown i

  let int_of_type_identifier = function
    | Msg_tx -> 1
    | Msg_block -> 2
    | Msg_filtered_block -> 3
    | Unknown i -> i

  type elem =
    { type_identifier : type_identifier
    ; hash : string
    } with sexp, fields

  let consume_elem iobuf =
    let type_identifier = Iobuf.Consume.uint32_le iobuf |> type_identifier_of_int in
    let hash = consume_hash iobuf in
    Fields_of_elem.create
      ~type_identifier
      ~hash

  let fill_elem iobuf elem =
    let write f g = fun field -> f iobuf (Field.get field elem |> g) in
    Fields_of_elem.iter
      ~type_identifier:(write Iobuf.Fill.uint32_le int_of_type_identifier)
      ~hash:(write fill_hash Fn.id)

  type t = elem list with sexp

  let consume_iobuf iobuf = consume_list iobuf consume_elem

  let fill_iobuf = fill_list fill_elem
end

module Nbits = struct
  type t =
    { mantissa : int
    ; exponent : int
    } with sexp, bin_io

  let consume iobuf =
    let exponent = Iobuf.Consume.uint8 iobuf in
    let mantissa1 = Iobuf.Consume.uint8 iobuf in
    let mantissa2 = Iobuf.Consume.uint8 iobuf in
    let mantissa3 = Iobuf.Consume.uint8 iobuf in
    { mantissa = mantissa1 * 256 * 256 + mantissa2 * 256 + mantissa3
    ; exponent = exponent - 3
    }

  let fill_iobuf iobuf { mantissa; exponent } =
    Iobuf.Fill.uint8 iobuf (exponent + 3);
    Iobuf.Fill.uint8 iobuf (mantissa / (256 * 256));
    Iobuf.Fill.uint8 iobuf ((mantissa / 256) % 256);
    Iobuf.Fill.uint8 iobuf (mantissa % 256)
end

module Headers = struct
  type elem =
    { version : int
    ; previous_block_header_hash : string
    ; merkle_root_hash : string
    ; time : Time.t
    ; nbits : Nbits.t
    ; nonce : int
    } with sexp, fields, bin_io

  let consume_elem iobuf =
    let version = Iobuf.Consume.uint32_le iobuf in
    let previous_block_header_hash = consume_hash iobuf in
    let merkle_root_hash = consume_hash iobuf in
    let time = Iobuf.Consume.uint32_le iobuf |> float |> Time.of_float in
    let nbits = Nbits.consume iobuf in
    let nonce = Iobuf.Consume.uint32_le iobuf in
    let transaction_count = consume_compact_uint iobuf in
    assert (transaction_count = 0);
    Fields_of_elem.create
      ~version
      ~previous_block_header_hash
      ~merkle_root_hash
      ~time
      ~nbits
      ~nonce

  let fill_elem iobuf elem =
    let write f g = fun field -> f iobuf (Field.get field elem |> g) in
    Fields_of_elem.iter
      ~version:(write Iobuf.Fill.uint32_le Fn.id)
      ~previous_block_header_hash:(write fill_hash Fn.id)
      ~merkle_root_hash:(write fill_hash Fn.id)
      ~time:(write Iobuf.Fill.uint32_le (fun time -> Time.to_epoch time |> Float.to_int))
      ~nbits:(write Nbits.fill_iobuf Fn.id)
      ~nonce:(write Iobuf.Fill.uint32_le Fn.id);
    fill_compact_uint iobuf 0 (* transaction count *)

  type t = elem list with sexp

  let consume_iobuf iobuf = consume_list iobuf consume_elem

  let fill_iobuf = fill_list fill_elem

  let hash =
    let header_len = 80 in
    let iobuf = Iobuf.create ~len:(2+header_len) in
    fun elem ->
      Iobuf.reset iobuf;
      fill_elem iobuf elem;
      Iobuf.flip_lo iobuf;
      let hash1 = Cryptokit.Hash.sha256 () in
      for pos = 0 to header_len - 1 do
        hash1 # add_char (Iobuf.Peek.char iobuf ~pos)
      done;
      Cryptokit.hash_string (Cryptokit.Hash.sha256 ()) (hash1 # result)
end

module Reject = struct
  module Code = struct
    type t =
      | Invalid_message
      | Invalid_block
      | Invalid_transaction
      | Outdated_block_version
      | Outdated_version
      | Double_spend
      | Multiple_version
      | Non_standard_transaction
      | Below_dust_threshold
      | Not_enough_fee_or_priority
      | Wrong_block_chain
    with sexp

    let to_int = function
      | Invalid_message -> 1
      | Invalid_block -> 16
      | Invalid_transaction -> 16
      | Outdated_block_version -> 17
      | Outdated_version -> 17
      | Double_spend -> 18
      | Multiple_version -> 18
      | Non_standard_transaction -> 64
      | Below_dust_threshold -> 65
      | Not_enough_fee_or_priority -> 66
      | Wrong_block_chain -> 67

    let extra_data_len = function
      | Invalid_message -> 0
      | Invalid_block -> 32
      | Invalid_transaction -> 32
      | Outdated_block_version -> 32
      | Outdated_version -> 0
      | Double_spend -> 32
      | Multiple_version -> 0
      | Non_standard_transaction -> 32
      | Below_dust_threshold -> 32
      | Not_enough_fee_or_priority -> 32
      | Wrong_block_chain -> 32

    let of_int message_type int =
      match (int, message_type) with
      | (1, _) -> Invalid_message
      | (16, "block") -> Invalid_block
      | (16, "tx") -> Invalid_transaction
      | (17, "block") -> Outdated_block_version
      | (17, "version") -> Outdated_version
      | (18, "tx") -> Double_spend
      | (18, "version") -> Multiple_version
      | (64, "tx") -> Non_standard_transaction
      | (65, "tx") -> Below_dust_threshold
      | (66, "tx") -> Not_enough_fee_or_priority
      | (67, "block") -> Wrong_block_chain
      | _ -> failwithf "Unhandled reject code %d %s" int message_type ()
  end

  type t =
    { message : string
    ; code : Code.t
    ; reason : string
    ; extra_data : string
    } with sexp, fields

  let consume_iobuf iobuf =
    let message = consume_string iobuf in
    let code = Iobuf.Consume.uint8 iobuf |> Code.of_int message in
    let reason = consume_string iobuf in
    let len = Code.extra_data_len code in
    let extra_data = Iobuf.Consume.string iobuf ~len in
    Fields.create
      ~message
      ~code
      ~reason
      ~extra_data

  let fill_iobuf iobuf t =
    let write f g = fun field -> f iobuf (Field.get field t |> g) in
    Fields.iter
      ~message:(write fill_string Fn.id)
      ~code:(write Iobuf.Fill.uint8 Code.to_int)
      ~reason:(write fill_string Fn.id)
      ~extra_data:(write Iobuf.Fill.string Fn.id)
end

module Getblocks = struct
  type t =
    { version : int
    ; block_header_hashes : string list
    ; stop_hash : string option
    } with sexp, fields

  let zero_hash = String.of_char_list (List.init 32 ~f:(fun _ -> '\000'))

  let consume_iobuf iobuf =
    let version = Iobuf.Consume.uint32_le iobuf in
    let block_header_hashes = consume_list iobuf consume_hash in
    let stop_hash = consume_hash iobuf in
    let stop_hash = if String.(=) stop_hash zero_hash then None else Some stop_hash in
    Fields.create
      ~version
      ~block_header_hashes
      ~stop_hash

  let fill_iobuf iobuf t =
    let write f g = fun field -> f iobuf (Field.get field t |> g) in
    Fields.iter
      ~version:(write Iobuf.Fill.uint32_le Fn.id)
      ~block_header_hashes:(write (fill_list fill_hash) Fn.id)
      ~stop_hash:(write
        (fun iobuf stop_hash ->
          fill_hash iobuf (Option.value stop_hash ~default:zero_hash))
        Fn.id
      )
end

module Outpoint = struct
  type t =
    { hash : string
    ; index : int
    } with sexp, fields

  let consume_iobuf iobuf =
    let hash = consume_hash iobuf in
    let index = Iobuf.Consume.uint32_le iobuf in
    Fields.create
      ~hash
      ~index

  let fill_iobuf iobuf t =
    let write f g = fun field -> f iobuf (Field.get field t |> g) in
    Fields.iter
      ~hash:(write fill_hash Fn.id)
      ~index:(write Iobuf.Fill.uint32_le Fn.id)
end

module Transaction_input = struct
  type t =
    { previous_output : Outpoint.t
    ; signature_script : string
    ; sequence : int
    } with sexp, fields

  let consume_iobuf iobuf =
    let previous_output = Outpoint.consume_iobuf iobuf in
    let signature_script = consume_string iobuf in
    let sequence = Iobuf.Consume.uint32_le iobuf in
    Fields.create
      ~previous_output
      ~signature_script
      ~sequence

  let fill_iobuf iobuf t =
    let write f g = fun field -> f iobuf (Field.get field t |> g) in
    Fields.iter
      ~previous_output:(write Outpoint.fill_iobuf Fn.id)
      ~signature_script:(write fill_string Fn.id)
      ~sequence:(write Iobuf.Fill.uint32_le Fn.id)
end

module Transaction_output = struct
  type t =
    { value : Int64.t
    ; pk_script : string
    } with sexp, fields

  let consume_iobuf iobuf =
    let value = Iobuf.Consume.int64_t_le iobuf in
    let pk_script = consume_string iobuf in
    Fields.create
      ~value
      ~pk_script

  let fill_iobuf iobuf t =
    let write f g = fun field -> f iobuf (Field.get field t |> g) in
    Fields.iter
      ~value:(write Iobuf.Fill.int64_t_le Fn.id)
      ~pk_script:(write fill_string Fn.id)
end

module Lock_time = struct
  type t =
    | Time of Time.t
    | Block_height of int
  with sexp

  let consume_iobuf iobuf =
    let lock_time = Iobuf.Consume.uint32_le iobuf in
    if lock_time < 500_000_000 then Block_height lock_time
    else Time (Time.of_float (float lock_time))

  let fill_iobuf iobuf t =
    let lock_time =
      match t with
      | Time t -> Time.to_epoch t |> Float.to_int
      | Block_height bh -> bh
    in
    Iobuf.Fill.uint32_le iobuf lock_time
end

module Raw_transaction = struct
  type t =
    { tx_in : Transaction_input.t list
    ; tx_out : Transaction_output.t list
    ; lock_time : Lock_time.t
    } with sexp, fields

  let consume_iobuf iobuf =
    let version = Iobuf.Consume.uint32_le iobuf in
    if version <> 1 then
      failwithf "Raw transaction version %d is not supported" version ();
    let tx_in = consume_list iobuf Transaction_input.consume_iobuf in
    let tx_out = consume_list iobuf Transaction_output.consume_iobuf in
    let lock_time = Lock_time.consume_iobuf iobuf in
    Fields.create
      ~tx_in
      ~tx_out
      ~lock_time

  let fill_iobuf iobuf t =
    Iobuf.Fill.uint32_le iobuf 1;
    let write f = fun field -> f iobuf (Field.get field t) in
    Fields.iter
      ~tx_in:(write (fill_list Transaction_input.fill_iobuf))
      ~tx_out:(write (fill_list Transaction_output.fill_iobuf))
      ~lock_time:(write Lock_time.fill_iobuf)
end

module Notfound = Inv
module Getdata = Inv
module Getheaders = Getblocks

module Block = struct
  type t =
    { block_header : Headers.elem
    ; txns : Raw_transaction.t list
    } with sexp, fields

  let consume_iobuf iobuf =
    let block_header = Headers.consume_elem iobuf in
    let txns = consume_list iobuf Raw_transaction.consume_iobuf in
    Fields.create
      ~block_header
      ~txns

  let fill_iobuf iobuf t =
    let write f g = fun field -> f iobuf (Field.get field t |> g) in
    Fields.iter
      ~block_header:(write Headers.fill_elem Fn.id)
      ~txns:(write (fill_list Raw_transaction.fill_iobuf) Fn.id)
end

module Merkleblock = struct
  type t =
    { block_header : Headers.elem
    ; transaction_count : int
    ; hashes : string list
    ; flags : string
    } with sexp, fields

  let consume_iobuf iobuf =
    let block_header = Headers.consume_elem iobuf in
    let transaction_count = Iobuf.Consume.uint32_le iobuf in
    let hashes = consume_list iobuf consume_hash in
    let flags = consume_string iobuf in
    Fields.create
      ~block_header
      ~transaction_count
      ~hashes
      ~flags

  let fill_iobuf iobuf t =
    let write f g = fun field -> f iobuf (Field.get field t |> g) in
    Fields.iter
      ~block_header:(write Headers.fill_elem Fn.id)
      ~transaction_count:(write Iobuf.Fill.uint32_le Fn.id)
      ~hashes:(write (fill_list fill_hash) Fn.id)
      ~flags:(write fill_string Fn.id)
end

type t =
  | Version of Version.t
  | Verack
  | Addr of Addr.t
  | Getaddr
  | Ping of Ping.t
  | Pong of Pong.t
  | Inv of Inv.t
  | Notfound of Notfound.t
  | Getheaders of Getheaders.t
  | Getblocks of Getblocks.t
  | Headers of Headers.t
  | Reject of Reject.t
  | Getdata of Getdata.t
  | Tx of Raw_transaction.t
  | Block of Block.t
  | Mempool
  | Merkleblock of Merkleblock.t
  with sexp

let fill_payload iobuf = function
  | Version version -> Version.fill_iobuf iobuf version
  | Verack -> ()
  | Addr addr -> Addr.fill_iobuf iobuf addr
  | Getaddr -> ()
  | Ping ping -> Ping.fill_iobuf iobuf ping
  | Pong pong -> Pong.fill_iobuf iobuf pong
  | Inv inv -> Inv.fill_iobuf iobuf inv
  | Notfound notfound -> Notfound.fill_iobuf iobuf notfound
  | Headers headers -> Headers.fill_iobuf iobuf headers
  | Reject reject -> Reject.fill_iobuf iobuf reject
  | Getheaders getheaders -> Getheaders.fill_iobuf iobuf getheaders
  | Getblocks getblocks -> Getblocks.fill_iobuf iobuf getblocks
  | Getdata getdata -> Getdata.fill_iobuf iobuf getdata
  | Tx raw_transaction -> Raw_transaction.fill_iobuf iobuf raw_transaction
  | Block block -> Block.fill_iobuf iobuf block
  | Mempool -> ()
  | Merkleblock merkleblock -> Merkleblock.fill_iobuf iobuf merkleblock

let command_name = function
  | Version _ -> "version"
  | Verack -> "verack"
  | Addr _ -> "addr"
  | Getaddr -> "getaddr"
  | Ping _ -> "ping"
  | Pong _ -> "pong"
  | Inv _ -> "inv"
  | Notfound _ -> "notfound"
  | Headers _ -> "headers"
  | Reject _ -> "reject"
  | Getheaders _ -> "getheaders"
  | Getblocks _ -> "getblocks"
  | Getdata _ -> "getdata"
  | Tx _ -> "tx"
  | Block _ -> "block"
  | Mempool -> "mempool"
  | Merkleblock _ -> "merkleblock"

(* The following iobuf is shared between all the calls to [to_string].  As [to_string]
   ends up making a copy of the necessary part via [Iobuf.to_string] this should not be an
   issue. *)
let shared_iobuf = Iobuf.create ~len:(4*1024*1024)

let header_len = 24

let to_string t =
  Iobuf.reset shared_iobuf;
  (* Magic number. *)
  Iobuf.Fill.uint32_be shared_iobuf 0xf9beb4d9;
  (* Message type. *)
  Iobuf.Fill.padded_fixed_string shared_iobuf (command_name t) ~padding:'\000' ~len:12;
  (* Payload size, unknown for now. *)
  Iobuf.Fill.uint32_le shared_iobuf 0;
  (* Checksum, unknown for now. *)
  Iobuf.Fill.string shared_iobuf "    ";
  (* Payload. *)
  fill_payload shared_iobuf t;
  Iobuf.flip_lo shared_iobuf;
  (* Update the payload size. *)
  let payload_len = Iobuf.length shared_iobuf - header_len in
  Iobuf.Poke.uint32_le shared_iobuf payload_len ~pos:16;
  (* Update the checksum. *)
  let hash1 = Cryptokit.Hash.sha256 () in
  for idx = 0 to payload_len - 1 do
    hash1 # add_char (Iobuf.Peek.char shared_iobuf ~pos:(header_len + idx))
  done;
  let checksum = Cryptokit.hash_string (Cryptokit.Hash.sha256 ()) (hash1 # result) in
  Iobuf.Poke.string shared_iobuf checksum ~pos:20 ~len:4;
  Iobuf.to_string shared_iobuf

let version () =
  Version
    { Version.version = protocol_version
    ; services = Int64.of_int 1
    ; timestamp = Time.now ()
    ; addr_recv_services = Int64.of_int 1
    ; addr_recv_addr = Address.localhost
    ; addr_recv_port = 8333
    ; addr_trans_services = Int64.of_int 1
    ; addr_trans_addr = Address.localhost
    ; addr_trans_port = 8333
    ; nonce = Int64.zero
    ; user_agent = ""
    ; start_height = 0
    ; relay = 0
    }

let getheaders ~from_hash =
  Getheaders
    { Getblocks.version = protocol_version
    ; block_header_hashes = [ from_hash ]
    ; stop_hash = None
    }

let handle_msg bigstring ~pos ~payload_len ~f =
  let command_name =
    Bigstring.get_padded_fixed_string bigstring ~padding:'\000' ~len:12 ~pos:(pos + 4) ()
  in
  let msg =
    let payload = Iobuf.of_bigstring bigstring ~pos:(pos + header_len) ~len:payload_len in
    try
      match command_name with
      | "version" -> Ok (Version (Version.consume_iobuf payload))
      | "verack" -> Ok Verack
      | "addr" -> Ok (Addr (Addr.consume_iobuf payload))
      | "getaddr" -> Ok Getaddr
      | "ping" -> Ok (Ping (Ping.consume_iobuf payload))
      | "pong" -> Ok (Pong (Pong.consume_iobuf payload))
      | "inv" -> Ok (Inv (Inv.consume_iobuf payload))
      | "notfound" -> Ok (Notfound (Notfound.consume_iobuf payload))
      | "getdata" -> Ok (Getdata (Getdata.consume_iobuf payload))
      | "getheaders" -> Ok (Getheaders (Getheaders.consume_iobuf payload))
      | "getblocks" -> Ok (Getblocks (Getblocks.consume_iobuf payload))
      | "headers" -> Ok (Headers (Headers.consume_iobuf payload))
      | "reject" -> Ok (Reject (Reject.consume_iobuf payload))
      | "tx" -> Ok (Tx (Raw_transaction.consume_iobuf payload))
      | "block" -> Ok (Block (Block.consume_iobuf payload))
      | "mempool" -> Ok Mempool
      | "merkleblock" -> Ok (Merkleblock (Merkleblock.consume_iobuf payload))
      | otherwise -> Error (sprintf "Unknown command name: %s" otherwise)
    with
    | exn ->
      Error (sprintf "Exception while parsing message: %s" (Exn.to_string exn))
  in
  f msg

let handle_chunk bigstring ~pos ~len ~f =
  let total_len = pos + len in
  let rec loop ~pos =
    let total_len = total_len - pos in
    if total_len < header_len then pos
    else
      let payload_len = Bigstring.unsafe_get_uint32_le bigstring ~pos:(pos + 16) in
      let msg_len = header_len + payload_len in
      if total_len < msg_len then pos
      else begin
        handle_msg bigstring ~pos ~payload_len ~f;
        loop ~pos:(pos + msg_len)
      end
  in
  `Consumed (loop ~pos)
