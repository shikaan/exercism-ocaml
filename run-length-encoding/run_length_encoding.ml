open Base

let encodepair cnt char =
  if cnt > 1 then Int.to_string cnt ^ Char.to_string char
  else Char.to_string char

let encode s =
  if String.length s = 0 then s
  else
    let (lastc, cnt), acc =
      String.fold s
        ~init:((String.get s 0, 0), "")
        ~f:(fun ((lastc, cnt), acc) char ->
          match char with
          | _ when Char.equal lastc char -> ((lastc, cnt + 1), acc)
          | _ -> ((char, 1), acc ^ encodepair cnt lastc))
    in
    acc ^ encodepair cnt lastc

let decodepair strcnt char =
  let cnt = if String.equal strcnt "" then 1 else Int.of_string strcnt in
  String.make cnt char

let decode s =
  if String.length s = 0 then s
  else
    let _, acc =
      String.fold s ~init:("", "") ~f:(fun (strcnt, acc) char ->
          match char with
          | _ when Char.is_digit char -> (strcnt ^ Char.to_string char, acc)
          | _ -> ("", acc ^ decodepair strcnt char))
    in
    acc
