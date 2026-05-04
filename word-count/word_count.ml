open Base

let seps = [ '\n'; '\t'; ' '; ','; ';'; '.'; ':'; '!'; '&'; '@'; '$'; '%'; '^'; ]

let unquote w = String.chop_prefix_if_exists ~prefix:"'" w 
  |> String.chop_suffix_if_exists ~suffix:"'" 
  |> String.chop_prefix_if_exists ~prefix:"\"" 
  |> String.chop_suffix_if_exists ~suffix:"\"" 

let word_count s =
  String.split_on_chars s ~on:seps
  |> List.fold
       ~init:(Map.empty (module String))
       ~f:(fun acc w ->
         let key = unquote w |> String.lowercase in
         if String.length key > 0 then
           match Map.find acc key with
           | Some c -> Map.set acc ~key ~data:(c + 1)
           | None -> Map.set acc ~key ~data:1
         else acc)
