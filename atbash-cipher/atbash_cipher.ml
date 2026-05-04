open Base

let encodec c =
  match c with
  | _ when Char.is_alpha c ->
      Char.of_int_exn (Char.to_int 'z' - (Char.to_int c - Char.to_int 'a'))
  | _ -> c

let chunk size str =
  let len = String.length str in
  let buf = Buffer.create (len + (len / 5) + 1) in
  String.foldi str ~init:buf ~f:(fun idx acc c ->
      let _ =
        if idx <> 0 && Int.rem idx size = 0 then
          Buffer.add_string acc (" " ^ Char.to_string c)
        else Buffer.add_char acc c
      in
      acc)
  |> Buffer.contents

let encodes str =
  let len = String.length str in
  let buf = Buffer.create len in
  String.fold str ~init:buf ~f:(fun acc c ->
      if Char.is_alphanum c then (
        Char.lowercase c |> encodec |> Buffer.add_char acc;
        acc)
      else acc)
  |> Buffer.contents

let encode ?block_size s =
  let size = Option.value ~default:5 block_size in
  encodes s |> chunk size

let decode s = encodes s
