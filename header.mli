open Core.Std

module Nbits : sig
  type t =
    { mantissa : int
    ; exponent : int
    } with sexp
end

type t =
  { version : int
  ; previous_block_header_hash : Hash.t
  ; merkle_root_hash : Hash.t
  ; time : Time.t
  ; nbits : Nbits.t
  ; nonce : int
  } with sexp, fields, bin_io

val hash : t -> Hash.t

val consume_iobuf : ([> read], Iobuf.seek) Iobuf.t -> t

val fill_iobuf : (read_write, Iobuf.seek) Iobuf.t -> t -> unit

val genesis : t
