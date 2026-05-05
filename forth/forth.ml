open Base

type token = Number of int | Word of string | Colon | Semicolon

let parse_token = function
  | ":" -> Colon
  | ";" -> Semicolon
  | s -> ( match Int.of_string_opt s with Some n -> Number n | None -> Word s)

let parse_line s =
  String.split_on_chars ~on:[ ' ' ] s
  |> List.map ~f:String.uppercase
  |> List.map ~f:parse_token

let bin op stack =
  match stack with
  | a :: b :: rest -> op b a :: rest
  | _ -> raise (Invalid_argument "bin: needs two args")

let dup stack =
  match stack with
  | a :: rest -> a :: a :: rest
  | _ -> raise (Invalid_argument "dup: needs one arg")

let drop stack =
  match stack with
  | _ :: rest -> rest
  | _ -> raise (Invalid_argument "drop: needs one arg")

let swap stack =
  match stack with
  | a :: b :: rest -> b :: a :: rest
  | _ -> raise (Invalid_argument "swap: needs two args")

let over stack =
  match stack with
  | a :: b :: rest -> b :: a :: b :: rest
  | _ -> raise (Invalid_argument "over: needs two args")

let replace env =
  List.concat_map ~f:(fun x ->
      match x with
      | Word name -> Map.find env name |> Option.value ~default:[ x ]
      | _ -> [ x ])

let evaluate_tokens env stack tokens =
  replace env tokens
  |> List.fold ~init:stack ~f:(fun stack token ->
      match token with
      | Number n -> n :: stack
      | Word "+" -> bin ( + ) stack
      | Word "-" -> bin ( - ) stack
      | Word "*" -> bin ( * ) stack
      | Word "/" -> bin ( / ) stack
      | Word "DROP" -> drop stack
      | Word "DUP" -> dup stack
      | Word "OVER" -> over stack
      | Word "SWAP" -> swap stack
      | _ -> raise (Invalid_argument "evaluate: unexpected"))

let define env tokens =
  match tokens with
  | Colon :: Word name :: rest -> (
      match List.last_exn rest with
      | Semicolon ->
          Map.set env ~key:name ~data:(List.drop_last_exn rest |> replace env)
      | _ -> raise (Invalid_argument "define: unterminated statement"))
  | _ -> raise (Invalid_argument "define: malformed definition")

let evaluate_line (stack, env) line =
  let tokens = parse_line line in
  match tokens with
  | [] -> (stack, env)
  | Colon :: _ -> (stack, define env tokens)
  | _ -> (evaluate_tokens env stack tokens, env)

let evaluate lines =
  let stack, env = ([], Map.empty (module String)) in
  try
    let s, _ = List.fold lines ~init:(stack, env) ~f:evaluate_line in
    Some (List.rev s)
  with _ -> None
