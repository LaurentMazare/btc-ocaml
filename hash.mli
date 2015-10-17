open Core.Std

type t with sexp, bin_io

include Comparable.S with type t := t
include Stringable.S with type t := t

val consume : ([> read], Iobuf.seek) Iobuf.t -> t
val fill : (read_write, Iobuf.seek) Iobuf.t -> t -> unit

val to_hex : t -> string
val of_hex : string -> t

val zero : t
