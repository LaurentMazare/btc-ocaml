open Core.Std

type t with sexp, bin_io

(* string representation is little-endian *)
include Comparable.S with type t := t
include Stringable.S with type t := t
include Hashable.S with type t := t

(* on-the-wire representation (and to-be-hashed representation) is little-endian *)
val consume : ([> read], Iobuf.seek) Iobuf.t -> t
val fill : (read_write, Iobuf.seek) Iobuf.t -> t -> unit

(* hex representation is big-endian *)
val to_hex : t -> string
val of_hex : string -> t

val zero : t

val difficulty : t -> float
