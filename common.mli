open Core.Std

val consume_compact_uint : ('a, Iobuf.seek) Iobuf.t -> int
val fill_compact_uint : (read_write, Iobuf.seek) Iobuf.t -> int -> unit
