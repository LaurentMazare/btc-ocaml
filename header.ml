open Core.Std

module Nbits = struct
  type t =
    { mantissa : int
    ; exponent : int
    } with sexp, bin_io

  let consume iobuf =
    let mantissa3 = Iobuf.Consume.uint8 iobuf in
    let mantissa2 = Iobuf.Consume.uint8 iobuf in
    let mantissa1 = Iobuf.Consume.uint8 iobuf in
    let exponent = Iobuf.Consume.uint8 iobuf in
    { mantissa = mantissa1 * 256 * 256 + mantissa2 * 256 + mantissa3
    ; exponent = exponent - 3
    }

  let fill_iobuf iobuf { mantissa; exponent } =
    Iobuf.Fill.uint8 iobuf (mantissa % 256);
    Iobuf.Fill.uint8 iobuf ((mantissa / 256) % 256);
    Iobuf.Fill.uint8 iobuf (mantissa / (256 * 256));
    Iobuf.Fill.uint8 iobuf (exponent + 3)

end

type t =
  { version : int
  ; previous_block_header_hash : Hash.t
  ; merkle_root_hash : Hash.t
  ; time : Time.t
  ; nbits : Nbits.t
  ; nonce : int
  } with sexp, fields, bin_io

let genesis = {
  version = 1;
  previous_block_header_hash = Hash.zero;
  merkle_root_hash = Hash.of_hex "4a5e1e4baab89f3a32518a88c31bc87f618f76673e2cc77ab2127b7afdeda33b";
  time = Time.of_string "2009-01-03 18:15:05 UTC";
  nbits = { mantissa = 0x00ffff; exponent = 0x1d - 3; };
  nonce = 0x7c2bac1d;
  }

let consume_from_network iobuf =
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

let fill_for_hashing iobuf elem =
  let write f g = fun field -> f iobuf (Field.get field elem |> g) in
  Fields.iter
    ~version:(write Iobuf.Fill.uint32_le Fn.id)
    ~previous_block_header_hash:(write Hash.fill Fn.id)
    ~merkle_root_hash:(write Hash.fill Fn.id)
    ~time:(write Iobuf.Fill.uint32_le (fun time -> Time.to_epoch time |> Float.to_int))
    ~nbits:(write Nbits.fill_iobuf Fn.id)
    ~nonce:(write Iobuf.Fill.uint32_le Fn.id)

let fill_for_network iobuf elem =
  fill_for_hashing iobuf elem;
  Common.fill_compact_uint iobuf 0 (* transaction count *)

let to_string elem =
  let header_len = 80 in
  let iobuf = Iobuf.create ~len:(2+header_len) in
  Iobuf.reset iobuf;
  fill_for_hashing iobuf elem;
  Iobuf.flip_lo iobuf;
  let buffer = Buffer.create 0 in
  for pos = 0 to header_len - 1 do
    Buffer.add_char buffer (Iobuf.Peek.char iobuf ~pos)
  done;
  Buffer.to_bytes buffer

let char_of_hex i =
  if 0 <= i && i < 10 then Char.of_int_exn (Char.to_int '0' + i)
  else if 10 <= i && i < 16 then Char.of_int_exn (Char.to_int 'a' + i - 10)
  else failwithf "not in hex range %d" i ()

let to_hex str =
  let len = String.length str in
  String.init (2*len) ~f:(fun i ->
    let c = str.[i / 2] |> Char.to_int in
    if i % 2 = 0 then char_of_hex (c / 16) else char_of_hex (c % 16)
  )

let () =
  Core.Std.Printf.printf "%S\n%!" (to_hex (to_string genesis))

let hash =
  let header_len = 80 in
  let iobuf = Iobuf.create ~len:(2+header_len) in
  fun elem ->
    Iobuf.reset iobuf;
    fill_for_hashing iobuf elem;
    Iobuf.flip_lo iobuf;
    let hash1 = Cryptokit.Hash.sha256 () in
    for pos = 0 to header_len - 1 do
      hash1 # add_char (Iobuf.Peek.char iobuf ~pos)
    done;
    Cryptokit.hash_string (Cryptokit.Hash.sha256 ()) (hash1 # result)
    |> Hash.of_string

