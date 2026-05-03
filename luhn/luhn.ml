let digit n =
  let d = n * 2 in
  Result.(
    match d with
    | d when d < 0 -> Error "must be positive"
    | d when d > 9 -> Ok (d - 9)
    | d -> Ok d)

let parsedigit c = Char.code c - Char.code '0'

let valid s =
  let digits = List.(String.to_seq s |> of_seq |> rev |> map parsedigit) in
  let hash = List.(digits
    |> mapi (fun i x -> Result.get_ok (digit(if i mod 2 = 0 then x else x * 2)))
    |> fold_left (+) 0) in
  hash mod 10 = 0

