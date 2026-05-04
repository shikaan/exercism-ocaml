open Base

let are_balanced =
  String.fold_until ~init:[]
    ~f:(fun stk c ->
      match c with
      | '(' | '[' | '{' -> Continue (c :: stk)
      | ')' -> ( match stk with '(' :: rst -> Continue rst | _ -> Stop false)
      | ']' -> ( match stk with '[' :: rst -> Continue rst | _ -> Stop false)
      | '}' -> ( match stk with '{' :: rst -> Continue rst | _ -> Stop false)
      | _ -> Continue stk)
    ~finish:List.is_empty
