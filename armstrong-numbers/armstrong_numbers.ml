  let rec pow base exp =
    if exp = 0 then 1
    else base * pow base (exp - 1)

let rec armstrong n e sum =
  match n with
    | 0 -> sum
    | _ -> 
        let sum = sum + pow (Int.rem n 10) e in 
        armstrong (n/10) e sum;;

let validate n =
  let exp = String.length (Int.to_string n) in
  n = armstrong n exp 0 
