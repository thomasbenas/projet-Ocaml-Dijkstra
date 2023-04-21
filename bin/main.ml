open Projet.Dijkstra_heap


let graph = [  (0, [(1, 7); (2, 9); (5, 14)]);
  (1, [(0, 7); (2, 10); (3, 15)]);
  (2, [(0, 9); (1, 10); (3, 11); (5, 2)]);
  (3, [(1, 15); (2, 11); (4, 6)]);
  (4, [(3, 6); (5, 9)]);
  (5, [(0, 14); (2, 2); (4, 9)])
];;

let start_vertex = 0;;
let pred, dist = dijkstra graph start_vertex;;
print_result pred dist start_vertex;;