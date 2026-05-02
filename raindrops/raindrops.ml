let divisibleby n m = Int.rem n m = 0

(* let raindrop n = *)
(*   let buffer = Buffer.create 5 in *)
(*   if divisibleby n 3 then Buffer.add_string buffer "Pling"; *)
(*   if divisibleby n 5 then Buffer.add_string buffer "Plang"; *) 
(*   if divisibleby n 7 then Buffer.add_string buffer "Plong"; *) 
(*   if (Buffer.length buffer) = 0 then Buffer.add_string buffer (Int.to_string n); *) 
(*   String.of_bytes (Buffer.to_bytes buffer) *)

let raindrop n =
  let combos = [ (3, "Pling"); (5, "Plang"); (7, "Plong") ] in
  let result =
    combos
    |> List.filter (fun (m, _) -> divisibleby n m)
    |> List.map snd |> String.concat ""
  in
  if result = "" then Int.to_string n else result
