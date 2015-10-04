open Core.Std

include String
let of_string = Fn.id
let to_string = Fn.id

let localhost = "::ffff:127.0.0.1"

let ipv4_prefix = "\000\000\000\000\000\000\000\000\000\000\255\255"

let is_ipv4 t =
  if Int.(<>) (String.length t) 16 then false
  else String.is_prefix t ~prefix:ipv4_prefix

let ipv4 t =
  if is_ipv4 t then
    Some (
      sprintf "%d.%d.%d.%d"
        (Char.to_int t.[12])
        (Char.to_int t.[13])
        (Char.to_int t.[14])
        (Char.to_int t.[15])
    )
  else None

let of_ipv4 str =
  try
    let f s = Int.of_string s |> Char.of_int_exn in
    match String.split str ~on:'.' |> List.map ~f with
    | [ a; b; c; d ] ->
      Some (sprintf "%s%c%c%c%c" ipv4_prefix a b c d)
    | _ -> None
  with _ -> None
