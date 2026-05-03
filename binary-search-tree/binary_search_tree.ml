open Base

type bst =
  | Empty
  | Node of { value: int; left: bst; right: bst }

let empty = Empty

let value = function
  | Empty -> Result.Error "empty"
  | Node n -> Result.Ok n.value

let left = function
  | Empty -> Result.Error "empty"
  | Node n -> Result.Ok n.left

let right = function
  | Empty -> Result.Error "empty"
  | Node n -> Result.Ok n.right

let rec insert v = function
  | Empty -> Node { value = v; left = Empty; right = Empty }
  | Node n ->
      if v > n.value then Node { n with right = insert v n.right }
      else Node { n with left = insert v n.left }

let to_list tree =
  let rec aux list tree =
    match tree with
    | Empty -> list
    | Node n ->
        let llist = aux list n.left in
        let mlist = List.append llist [ n.value ] in
        aux mlist n.right
  in
  aux [] tree

