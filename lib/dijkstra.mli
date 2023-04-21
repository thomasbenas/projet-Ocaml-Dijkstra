type node = int
type edge = { dest : node; weight : node; }
type graph = edge list array
val dijkstra : graph -> node -> int array
