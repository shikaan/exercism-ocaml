type nucleotide = A | C | G | T

let hamming_distance l s =
  if List.(length l <> length s) then 
    Result.Error "strands must be of equal length"
  else
    Result.Ok(List.fold_left2 (fun acc x y -> acc + Bool.to_int(x <> y)) 0 l s)
