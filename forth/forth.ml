open Base;;

type word =
  | Plus
  | Minus
  | Star
  | Slash
  | Dup
  | Drop
  | Swap
  | Over
  | Custom of string

type token = Number of int | Word of word

let parse_token = function
  | "+" -> Word Plus
  | "-" -> Word Minus
  | "*" -> Word Star
  | "/" -> Word Slash
  | "DUP" -> Word Dup
  | "DROP" -> Word Drop
  | "OVER" -> Word Over
  | "SWAP" -> Word Swap
  | s -> (
      match Int.of_string_opt s with
      | Some n -> Number n
      | None -> Word (Custom s))

let parse s =
  String.split_on_chars ~on:[ ' ' ] s
  |> List.map ~f:String.uppercase
  |> List.map ~f:parse_token

let bin op stack = match stack with
    | a :: b :: rest -> (op b  a) :: rest
    | _ -> raise (Invalid_argument "bin: needs two args")

let dup stack = match stack with
  | a :: rest -> a :: a :: rest
  | _ -> raise (Invalid_argument "dup: needs one arg")

let drop stack = match stack with
  | _ :: rest -> rest
  | _ -> raise (Invalid_argument "drop: needs one arg")

let swap stack = match stack with
  | a :: b :: rest -> b :: a :: rest
  | _ -> raise (Invalid_argument "swap: needs two args")

let over stack = match stack with
  | a :: b :: rest -> b :: a :: b :: rest
  | _ -> raise (Invalid_argument "over: needs two args")

let rec evaluate_tokens stack =
  List.fold ~init:stack ~f:(fun stack token ->
      match token with
      | Number n -> n :: stack
      | Word word -> (
          match word with
          | Plus -> bin ( + ) stack
          | Minus -> bin ( - ) stack
          | Star -> bin ( * ) stack
          | Slash -> bin ( / ) stack
          | Dup -> dup stack
          | Drop -> drop stack
          | Swap -> swap stack
          | Over -> over stack
          | _ -> raise (Invalid_argument "evaluate: unexpected")))

let evaluate_line stack line =
  let tokens = parse line in
  if List.is_empty tokens then stack
  else evaluate_tokens stack tokens

let evaluate lines =
  let stack = [] in
  try Some (List.fold lines ~init:stack ~f:evaluate_line |> List.rev)
  with _ -> None

