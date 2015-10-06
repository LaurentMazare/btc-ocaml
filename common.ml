open Core.Std

let consume_hash iobuf = Iobuf.Consume.string iobuf ~len:32

let fill_hash iobuf str = Iobuf.Fill.padded_fixed_string iobuf str ~len:32 ~padding:'\000'

let consume_compact_uint iobuf =
  let len_len = Iobuf.Consume.uint8 iobuf in
  if len_len <= 252 then len_len
  else if len_len = 253 then Iobuf.Consume.uint16_le iobuf
  else if len_len = 254 then Iobuf.Consume.uint32_le iobuf
  else if len_len = 255 then Iobuf.Consume.int64_le_trunc iobuf
  else failwithf "incorrect len len %d" len_len ()

let fill_compact_uint iobuf int =
  if int <= 252 then
    Iobuf.Fill.uint8 iobuf int
  else if int <= 0xFFFF then
    Iobuf.Fill.uint16_le iobuf int
  else if int <= 0xFFFFFFFF then
    Iobuf.Fill.uint32_le iobuf int
  else
    Iobuf.Fill.int64_le_trunc iobuf int


