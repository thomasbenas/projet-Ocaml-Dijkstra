open Heap_binary

let dijkstra graph start =
  let start_time = Sys.time () in   (* Ajout du timestamp au début *)
  let infinity = max_int in
  let dist = Hashtbl.create 10 in   (* Table de hachage pour stocker les distances *)
  let heap = ref empty_heap in   (* Tas binaire pour stocker les sommets à traiter *)
  let pred = Hashtbl.create 10 in   (* Table de hachage pour stocker les prédécesseurs *)
  let add_node n d =   (* Fonction pour ajouter un sommet et sa distance dans le tas *)
    heap := add_heap (n, d) !heap;
    Hashtbl.replace dist n d
  in
  let rec process_next_node () =   (* Fonction récursive pour traiter les sommets suivants *)
    if is_empty_heap !heap then ()   (* Si le tas est vide, on a fini *)
    else
      let (u, d) = find_min_heap !heap in   (* On prend le sommet de distance minimale *)
      heap := delete_min_heap !heap;   (* On le retire du tas *)
      let neighbors = try List.assoc u graph with Not_found -> [] in   (* On récupère ses voisins *)
      List.iter (fun (v, w) ->   (* On itère sur les voisins *)
        let alt = d + w in   (* On calcule la distance alternative via ce sommet *)
        let dv = try Hashtbl.find dist v with Not_found -> infinity in   (* On récupère la distance connue de v *)
        if alt < dv then begin   (* Si l'alternative est meilleure *)
          add_node v alt;   (* On ajoute v au tas avec sa nouvelle distance *)
          Hashtbl.replace pred v u   (* On met à jour le prédécesseur de v *)
        end
      ) neighbors;
      process_next_node ()   (* On traite le sommet suivant *)
  in
  Hashtbl.add dist start 0;   (* Initialisation de la distance de départ *)
  add_node start 0;   (* Ajout du sommet de départ dans le tas *)
  process_next_node ();   (* Traitement des sommets suivants *)
  let end_time = Sys.time () in   (* Ajout du timestamp à la fin *)
  let execution_time = end_time -. start_time in   (* Calcul de la différence pour obtenir le temps d'exécution *)
  (pred, dist, execution_time)   (* Retourne les tables de hachage des prédécesseurs, des distances et le temps d'exécution *)
