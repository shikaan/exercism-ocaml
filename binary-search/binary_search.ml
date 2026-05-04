open Base;;

let find arr item =
  Array.sort arr ~compare;
  let rec aux arr item offset =
    let len = Array.length arr in
    let hlen = len / 2 in
    match len with
    | 0 -> Error "value not in array"
    | 1 ->
        if Array.get arr 0 = item then Ok offset else Error "value not in array"
    | _ -> (
        let mid = Array.get arr hlen in
        match mid with
        | mid when mid = item -> Ok (offset + hlen)
        | mid when mid < item ->
            aux (Array.sub arr ~pos:hlen ~len:(len - hlen)) item (offset + hlen)
        | _ -> aux (Array.sub arr ~pos:0 ~len:hlen) item offset)
  in
  aux arr item 0
