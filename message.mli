open Core.Std

module Version : sig
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
end

module Ping : sig
  type t =
    { nonce : Int64.t
    } with sexp, fields
end

module Pong : sig
  type t =
    { nonce : Int64.t
    } with sexp, fields
end

module Addr : sig
  type elem =
    { time : Time.t
    ; services : Int64.t
    ; ip_address : Address.t
    ; port : int
    } with sexp, fields

  type t = elem list with sexp
end

module Inv : sig
  type type_identifier =
    | Msg_tx
    | Msg_block
    | Msg_filtered_block
    | Unknown of int
  with sexp

  type elem =
    { type_identifier : type_identifier
    ; hash : Hash.t
    } with sexp, fields

  type t = elem list with sexp
end

module Headers : sig
  type t = Header.t list with sexp
end

module Reject : sig
  module Code : sig
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
  end

  type t =
    { message : string
    ; code : Code.t
    ; reason : string
    ; extra_data : string
    } with sexp, fields
end

module Getblocks : sig
  type t =
    { version : int
    ; block_header_hashes : Hash.t list
    ; stop_hash : Hash.t option
    } with sexp
end

module Notfound : sig
  type t = Inv.t with sexp
end

module Getdata : sig
  type t = Inv.t with sexp
end

module Getheaders : sig
  type t = Getblocks.t with sexp
end

module Outpoint : sig
  type t =
    { hash : Hash.t
    ; index : int
    } with sexp
end

module Transaction_input : sig
  type t =
    { previous_output : Outpoint.t
    ; signature_script : string
    ; sequence : int
    } with sexp
end

module Transaction_output : sig
  type t =
    { value : Int64.t
    ; pk_script : string
    } with sexp
end

module Lock_time : sig
  type t =
    | Time of Time.t
    | Block_height of int
end

module Raw_transaction : sig
  type t =
    { tx_in : Transaction_input.t list
    ; tx_out : Transaction_output.t list
    ; lock_time : Lock_time.t
    } with sexp
end

module Block : sig
  type t =
    { block_header : Header.t
    ; txns : Raw_transaction.t list
    } with sexp
end

module Merkleblock : sig
  type t =
    { block_header : Header.t
    ; transaction_count : int
    ; hashes : Hash.t list
    ; flags : string
    } with sexp
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

val handle_chunk
  :  Bigstring.t
  -> pos : int
  -> len : int
  -> f:((t, string) Result.t -> unit)
  -> [ `Consumed of int ]

val version : unit -> t
val to_string : t -> string

(** [getheaders ~from_the_highest_of ~stop_hash] creates a request for new headers. This should be answered
    by a peer starting from its highest known point in [from_the_highest_of]. [from_the_highest_of] should
    be sorted in reverse order of block height.
*)
val getheaders
  :  from_the_highest_of:Hash.t list
  -> stop_hash:Hash.t option
  -> t
