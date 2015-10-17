open Core.Std

let hex_of_char c =
  if Char.('0' <= c && c <= '9') then Char.to_int c - Char.to_int '0'
  else if Char.('a' <= c && c <= 'f') then 10 + Char.to_int c - Char.to_int 'a'
  else failwithf "char %c is not hex" c ()

let char_of_hex i =
  if 0 <= i && i < 10 then Char.of_int_exn (Char.to_int '0' + i)
  else if 10 <= i && i < 16 then Char.of_int_exn (Char.to_int 'a' + i - 10)
  else failwithf "not in hex range %d" i ()

let of_hex str =
  if String.is_prefix str ~prefix:"0x" then
    let len = String.length str in
    if len % 2 = 0 then
      String.init (len / 2 - 1) ~f:(fun i ->
        Char.of_int_exn
          (16 * hex_of_char str.[len - 2*i - 2] + hex_of_char str.[len - 2*i - 1]))
    else failwith "Input string size is odd"
  else
    failwith "Incorrect prefix"

let to_hex str =
  let len = String.length str in
  let hex =
    String.init (2*len) ~f:(fun i ->
      let c = str.[len-1 - i / 2] |> Char.to_int in
      if i % 2 = 0 then char_of_hex (c / 16) else char_of_hex (c % 16)
    )
  in
  "0x" ^ hex

include String

let consume iobuf = Iobuf.Consume.string iobuf ~len:32

let fill iobuf str = Iobuf.Fill.padded_fixed_string iobuf str ~len:32 ~padding:'\000'

let zero = String.of_char_list (List.init 32 ~f:(fun _ -> '\000'))

