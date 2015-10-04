open Async.Std
type t

val create 
  :  blockchain_file : string
  -> stop : unit Deferred.t
  -> t Deferred.t

val process_headers
  :  t
  -> Address.t
  -> Message.Headers.elem list
  -> [ `get_headers_from of string | `nothing ]

val not_connected : t -> bool

val maybe_start_syncing : t -> Address.t -> [ `from_hash of string ] option
