open Core.Std

module Nbits : sig
  type t =
    { mantissa : int
    ; exponent : int
    } with sexp
end

type t =
  { version : int
  ; previous_block_header_hash : string
  ; merkle_root_hash : string
  ; time : Time.t
  ; nbits : Nbits.t
  ; nonce : int
  } with sexp, fields, bin_io

val hash : t -> string

val consume_iobuf : ('a, Iobuf.seek) Iobuf.t -> t

val fill_iobuf : (read_write, Iobuf.seek) Iobuf.t -> t -> unit
