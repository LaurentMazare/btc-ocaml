open Core.Std

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

type t =
  { version : int
  ; previous_block_header_hash : Hash.t
  ; merkle_root_hash : Hash.t
  ; time : Time.t
  ; nbits : Nbits.t
  ; nonce : int
  } with sexp, fields, bin_io

let consume_iobuf iobuf =
  let version = Iobuf.Consume.uint32_le iobuf in
  let previous_block_header_hash = Hash.consume iobuf in
  let merkle_root_hash = Hash.consume iobuf in
  let time = Iobuf.Consume.uint32_le iobuf |> float |> Time.of_float in
  let nbits = Nbits.consume iobuf in
  let nonce = Iobuf.Consume.uint32_le iobuf in
  let transaction_count = Common.consume_compact_uint iobuf in
  assert (transaction_count = 0);
  Fields.create
    ~version
    ~previous_block_header_hash
    ~merkle_root_hash
    ~time
    ~nbits
    ~nonce

let fill_iobuf iobuf elem =
  let write f g = fun field -> f iobuf (Field.get field elem |> g) in
  Fields.iter
    ~version:(write Iobuf.Fill.uint32_le Fn.id)
    ~previous_block_header_hash:(write Hash.fill Fn.id)
    ~merkle_root_hash:(write Hash.fill Fn.id)
    ~time:(write Iobuf.Fill.uint32_le (fun time -> Time.to_epoch time |> Float.to_int))
    ~nbits:(write Nbits.fill_iobuf Fn.id)
    ~nonce:(write Iobuf.Fill.uint32_le Fn.id);
  Common.fill_compact_uint iobuf 0 (* transaction count *)

let hash =
  let header_len = 80 in
  let iobuf = Iobuf.create ~len:(2+header_len) in
  fun elem ->
    Iobuf.reset iobuf;
    fill_iobuf iobuf elem;
    Iobuf.flip_lo iobuf;
    let hash1 = Cryptokit.Hash.sha256 () in
    for pos = 0 to header_len - 1 do
      hash1 # add_char (Iobuf.Peek.char iobuf ~pos)
    done;
    Cryptokit.hash_string (Cryptokit.Hash.sha256 ()) (hash1 # result)
    |> Hash.of_string

