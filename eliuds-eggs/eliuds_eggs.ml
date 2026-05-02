let rec countbits n sum =
  match n with
    | 0 -> sum
    | _ -> 
        let sum = sum + (Int.logand n 1) in
        countbits (Int.shift_right n 1) sum

let egg_count number = countbits number 0
