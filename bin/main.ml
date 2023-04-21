open Projet.Dijkstra_heap
open Projet.Utils

let start_vertex = 0;;
let pred, dist, execution_time = dijkstra (generate_graph 2000) start_vertex;;
print_result pred dist start_vertex execution_time;;