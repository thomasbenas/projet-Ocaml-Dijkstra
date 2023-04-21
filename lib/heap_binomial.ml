type 'a binomial_t = Node of 'a * 'a binomial_t list * int  
type 'a binomial_heap_t = 'a binomial_t list

let singleton_tree k = Node (k, [], 0)  
let key (Node (k, _, _)) = k  
let child_list (Node (_, c, _)) = c

let empty_heap = []


let is_empty_heap (h : 'a binomial_heap_t) : bool =
  match h with
  | [] -> true
  | _ -> false

      
let link ((Node (k1, c1, r1)) as t1) ((Node (k2, c2, r2)) as t2) =  
  if r1 <> r2 then failwith "Cannot link two binomial trees with different ranks"
  else if k1 < k2 then Node (k1, t2::c1, r1+1)
  else Node (k2, t1::c2, r2+1)

let link_pair l =  
  let rec aux acc = function
    | [] -> acc
    | _::[] -> failwith "the number of elements must be 2^r"
    | t1::t2::tl -> aux (link t1 t2 :: acc) tl
  in
  aux [] l

let to_binomial_tree l =  
  let singletons = List.map singleton_tree l in
  let rec aux = function
    | [] -> failwith "Empty list"
    | t::[] -> t
    | l -> aux (link_pair l)
  in
  aux singletons
  
let add_heap k h =  
  let rec aux acc (Node (_, _, r1) as bt1) = function
    | [] -> List.rev (bt1::acc)
    | (Node (_, _, r2) as bt2)::tl ->
      if r1 = r2 then aux acc (link bt1 bt2) tl
      else if r1 < r2 then List.rev_append acc (bt1::bt2::tl)
      else aux (bt2::acc) bt1 tl
  in
  aux [] (singleton_tree k) h

let rec merge h1 h2 =  
  match h1, h2 with
  | h, [] | [], h -> h
  | (Node (_, _, r1) as bt1)::tl1, (Node (_, _, r2) as bt2)::tl2 ->
    if r1 < r2 then bt1::merge tl1 h2
    else if r1 > r2 then bt2::merge h1 tl2
    else merge (link bt1 bt2::tl1) tl2

let find_min_heap = function  
  | [] -> failwith "Empty heap"
  | Node (k1, _, _)::tl ->
    List.fold_left (fun acc (Node (k, _, _)) -> min acc k) k1 tl
  
let split_by_min h =  
  let rec aux pre (a, m, b) = function
    | [] -> List.rev a, m, b
    | x::tl ->
      if key x < key m then aux (x::pre) (pre, x, tl) tl
      else aux (x::pre) (a, m, b) tl
  in
  match h with 
    | [] -> failwith "Empty heap"
    | bt::tl -> aux [bt] ([], bt, []) tl

let delete_min_heap h =  
  let a, m, b = split_by_min h in
  merge (merge a b) (child_list m |> List.rev)