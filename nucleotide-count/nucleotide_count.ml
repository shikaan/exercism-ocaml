open Base

let empty = Map.empty (module Char)

let isvalid c = match c with 'A' | 'C' | 'G' | 'T' -> true | _ -> false
let isinvalid c = not (isvalid c)

let count_nucleotide s c =
  match String.find s ~f:isinvalid with
  | Some invalid -> Result.Error invalid
  | None ->
      Result.(
        if isvalid c then Ok (String.count ~f:(fun e -> Char.equal e c) s)
        else Error c)

(* let count_nucleotides s = *)
(*   match String.find s ~f:isinvalid with *)
(*   | Some invalid -> Result.Error invalid *)
(*   | None -> *)
(*       Result.Ok( *)
(*         String.fold s ~init:empty ~f:(fun acc c -> *)
(*             Map.update acc c ~f:(function None -> 1 | Some n -> n + 1))) *)

let count_nucleotides s =
  Result.(
    String.fold s ~init:(Ok empty) ~f:(fun acc c -> match acc with
    | Error _ -> acc
    | Ok acc ->
        if isinvalid c then Error c
        else Ok (Map.update acc c ~f:(function None -> 1 | Some n -> n + 1)))
  )
