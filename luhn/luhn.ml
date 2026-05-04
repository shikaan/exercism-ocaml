open Base;;

let even x = Int.rem x 2 = 0

let hash idx n =
  if even idx then n
  else
    let d = n * 2 in
    match d with d when d > 9 -> d - 9 | d -> d

let digit c = match Char.get_digit c with Some n -> n | None -> 0

let validstring s = String.length s > 1 && String.for_all s ~f:Char.is_digit 

let valid s =
  List.(
    let cl = String.filter s ~f:(fun c -> not (Char.is_whitespace c)) in
    if validstring cl then
      let dits = map (String.to_list cl |> rev) ~f:digit in
      let hashed = foldi dits ~f:(fun i acc e -> acc + hash i e) ~init:0 in
      Int.rem hashed 10 = 0
    else false)
