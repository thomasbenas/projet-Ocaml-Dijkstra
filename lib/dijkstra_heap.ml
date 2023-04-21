open Heap_binomial


let dijkstra graph start =
  let infinity = max_int in
  let dist = Hashtbl.create 10 in
  let heap = ref empty_heap in
  let pred = Hashtbl.create 10 in
  let add_node n d =
    heap := add_heap (n, d) !heap;
    Hashtbl.replace dist n d
  in
  let rec process_next_node () =
    if is_empty_heap !heap then ()
    else
      let (u, d) = find_min_heap !heap in
      heap := delete_min_heap !heap;
      let neighbors = try List.assoc u graph with Not_found -> [] in
      List.iter (fun (v, w) ->
        let alt = d + w in
        let dv = try Hashtbl.find dist v with Not_found -> infinity in
        if alt < dv then begin
          add_node v alt;
          Hashtbl.replace pred v u
        end
      ) neighbors;
      process_next_node ()
  in
  Hashtbl.add dist start 0;
  add_node start 0;
  process_next_node ();
  pred, dist

  let print_result pred dist start =
    let print_path dest =
      let rec build_path dest path =
        if dest = start then start :: path
        else build_path (Hashtbl.find pred dest) (dest :: path)
      in
      let path = build_path dest [] in
      print_string (String.concat " -> " (List.map string_of_int path))
    in
    Hashtbl.iter (fun dest dist ->
      Printf.printf "Shortest distance from %d to %d: %d\n"
        start dest dist;
      Printf.printf "Shortest path: ";
      print_path dest;
      print_newline ()
    ) dist