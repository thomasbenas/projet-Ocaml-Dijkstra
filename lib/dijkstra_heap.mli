val dijkstra :
  ('a * ('a * int) list) list ->
  'a -> ('a, 'a) Hashtbl.t * ('a, int) Hashtbl.t
