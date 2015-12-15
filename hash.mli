open Core.Std

type t with sexp, bin_io

(* Double SHA256-hash of something, in the usual SHA256 byte order.
  Note that in bitcoin hashes are treated as 256-bit numbers
  (e.g. for difficulty computation).
  Byte string representation is little-endian encoding of such numbers.
*)
include Comparable.S with type t := t
include Stringable.S with type t := t
include Hashable.S with type t := t

(* on-the-wire representation (and to-be-hashed representation) is the same as
  [Stringable], so it's little-endian encoding of hash-as-a-number. *)
val consume : ([> read], Iobuf.seek) Iobuf.t -> t
val fill : (read_write, Iobuf.seek) Iobuf.t -> t -> unit

(* hex representation is big-endian so lower hash-numbers have leading zeroes
  to the left *)
val to_hex : t -> string
val of_hex : string -> t

val zero : t

val difficulty : t -> float
