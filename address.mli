open Core.Std

type t with sexp, bin_io
val of_string : string -> t
val to_string : t -> string

val localhost : t
val ipv4 : t -> string option
val is_ipv4 : t -> bool
val of_ipv4 : string -> t option

include Comparable.S with type t := t
include Hashable.S with type t := t
