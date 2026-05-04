open Base;;

let get_count rows lr lc row col =
  let c =
    List.cartesian_product [ row - 1; row; row + 1 ] [ col - 1; col; col + 1 ]
    |> List.filter ~f:(fun (r, c) -> r >= 0 && c >= 0 && r < lr && c < lc)
    |> List.count ~f:(fun (r, c) ->
        List.nth rows r
        |> Option.value_map ~default:false ~f:(fun s ->
            Char.equal (String.get s c) '*'))
  in
  if c > 0 then Char.of_int_exn (c + Char.to_int '0') else ' '

let annotate rows =
  if List.length rows = 0 then rows
  else
    let lr, lc = (List.length rows, String.length (List.hd_exn rows)) in
    List.mapi rows ~f:(fun ri r ->
        String.mapi r ~f:(fun ci c ->
            match c with
            | ' ' -> get_count rows lr lc ri ci
            | _ -> c))
