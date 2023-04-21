let print_result pred dist start execution_time =
  let _print_path dest =
    let rec build_path dest path =
      if dest = start then start :: path
      else build_path (Hashtbl.find pred dest) (dest :: path)
    in
    let path = build_path dest [] in
    print_string (String.concat " -> " (List.map string_of_int path))
  in
  Hashtbl.iter (fun dest dist ->
    Printf.printf "Plus courte distance depuis %d vers %d: %d\n"
      start dest dist;
    print_newline ()
  ) dist;
  Printf.printf "Temps d'exécution: %f secondes\n" execution_time;; (* Add print for execution time *)



let generate_graph n =
  let rec generate_helper i acc = (* Fonction récursive qui génère les éléments du tableau *)
    if i = n then acc (* Si on a atteint la fin du tableau, on renvoie le tableau complet *)
    else
      let new_elem = (i, generate_edges ()) in (* Sinon, on crée un nouvel élément qui est une paire composée de l'indice et de la liste d'arêtes générée *)
      generate_helper (i+1) (new_elem::acc) (* On continue la récursion pour le prochain élément en ajoutant le nouvel élément à la liste *)
  and generate_edges () = (* Fonction auxiliaire qui génère la liste d'arêtes pour un élément donné *)
    let num_edges = Random.int n in (* On choisit un nombre aléatoire d'arêtes à générer *)
    let rec generate_edges_helper i acc = (* Fonction récursive qui génère chaque arête *)
      if i = num_edges then acc (* Si on a généré le nombre d'arêtes voulu, on renvoie la liste d'arêtes *)
      else
        let dest = Random.int n in (* On choisit une destination aléatoire pour l'arête (qui n'est pas égale à l'origine) *)
        let weight = Random.int 15 + 1 in (* On choisit une pondération aléatoire entre 1 et 15 *)
        if List.mem_assoc dest acc then (* Si la destination a déjà été utilisée pour une autre arête, on en choisit une nouvelle *)
          generate_edges_helper i acc
        else
          generate_edges_helper (i+1) ((dest, weight)::acc) (* On continue la récursion pour générer la prochaine arête *)
    in generate_edges_helper 0 [] (* On lance la génération d'arêtes avec une liste vide en tant qu'accumulateur *)
  in generate_helper 0 [] (* On lance la génération du tableau avec une liste vide en tant qu'accumulateur *)


