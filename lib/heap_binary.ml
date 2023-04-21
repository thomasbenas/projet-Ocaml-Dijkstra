type 'a heap = Empty | Node of int * 'a * 'a heap * 'a heap

let empty_heap = Empty

let is_empty_heap = function
  | Empty -> true
  | _ -> false

let merge_heap h1 h2 =
  let rank = function
    | Empty -> 0
    | Node (r, _, _, _) -> r
  in
  let make_node x a b =
    if rank a >= rank b then Node (rank b + 1, x, a, b)
    else Node (rank a + 1, x, b, a)
  in
  let rec merge_aux h1 h2 = match h1, h2 with
    | Empty, h | h, Empty -> h
    | Node (_, x, a1, b1), Node (_, y, a2, b2) ->
        if x < y then make_node x a1 (merge_aux b1 h2)
        else make_node y a2 (merge_aux h1 b2)
  in merge_aux h1 h2

let add_heap x h = merge_heap (Node (1, x, Empty, Empty)) h

let find_min_heap = function
  | Empty -> raise Not_found
  | Node (_, x, _, _) -> x

let delete_min_heap = function
  | Empty -> raise Not_found
  | Node (_, _, a, b) -> merge_heap a b

