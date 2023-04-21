open Projet.Dijkstra_heap
open Projet.Utils
let start_vertex = 0;;
let pred, dist = dijkstra (generate_graph 100) start_vertex;;
print_result pred dist start_vertex;;

