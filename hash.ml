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
  let len = String.length str in
  if len = (256 / 4) then
    String.rev (String.init (len / 2) ~f:(fun i ->
      Char.of_int_exn
        (16 * hex_of_char str.[2*i] + hex_of_char str.[2*i + 1])))
  else failwith "Input string size is odd"

let to_hex str =
  let str = String.rev str in
  assert (String.length str = (256 / 8));
  let len = String.length str in
  String.init (2*len) ~f:(fun i ->
    let c = str.[i / 2] |> Char.to_int in
    if i % 2 = 0 then char_of_hex (c / 16) else char_of_hex (c % 16)
  )

let difficulty t =
  let rec loop acc index =
    if index < 0 then acc
    else
      let c = String.get t index |> Char.to_int in
      loop (256. *. acc +. float c) (index - 1)
  in
  (* 1.1579e77 ~ 2^256 *)
  1.1579e77 /. loop 0. (String.length t - 1)

include String

let consume iobuf = (Iobuf.Consume.string iobuf ~len:32)

(* CR aalekseyev: why pad?? *)
let fill iobuf str = Iobuf.Fill.tail_padded_fixed_string iobuf str ~len:32 ~padding:'\000'

let zero = String.of_char_list (List.init 32 ~f:(fun _ -> '\000'))

let to_hex s =
  let res = to_hex s in
  assert (of_hex res = s);
  res

let of_hex s =
  let res = of_hex s in
  assert (to_hex res = s);
  res
