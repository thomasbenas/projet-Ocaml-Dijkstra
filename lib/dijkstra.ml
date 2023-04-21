type node = int
type edge = {dest : node; weight : int}
type graph = edge list array

let dijkstra (graph : graph) (source : node) : int array =
  let n = Array.length graph in
  let dist = Array.make n max_int in
  let visited = Array.make n false in
  dist.(source) <- 0;
  let rec loop () =
    let u = ref (-1) in
    for i = 0 to n - 1 do
      if not visited.(i) && (!u = -1 || dist.(i) < dist.(!u)) then
        u := i
    done;
    if !u = -1 then
      dist
    else begin
      visited.(!u) <- true;
      List.iter (fun {dest; weight} ->
        dist.(dest) <- min dist.(dest) (dist.(!u) + weight)
      ) graph.(!u);
      loop ()
    end
  in
  loop ()
