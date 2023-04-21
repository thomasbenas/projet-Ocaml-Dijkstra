open Heap_cours

type 'a graphe = { 
    sommets: 'a array; 
    aretes: ((int * float) list) array
} ;;


let dijkstra graphe sommet_depart =
    let nb_sommets = Array.length graphe.sommets in
    let distances = Array.make nb_sommets infinity in 
    let predecesseurs = Array.make nb_sommets (-1) in 

    distances.(sommet_depart) <- 0. ; predecesseurs.(sommet_depart) <- sommet_depart ;

    let rec dijkstra_aux tas =
        match tas with
        | Vide -> distances, predecesseurs
        | Noeud(_, (sommet_dist_min, dist_depuis_depart), _) -> 
            let nouveau_tas = supprimer_premier_noeud tas in
            if dist_depuis_depart > distances.(sommet_dist_min)
            then dijkstra_aux nouveau_tas
            else dijkstra_aux (List.fold_left (fun noeud (sommet, dist_actuelle) -> 
                let nouvelle_dist = distances.(sommet_dist_min) +. dist_actuelle in
                if nouvelle_dist < distances.(sommet)
                then (distances.(sommet) <- nouvelle_dist ; predecesseurs.(sommet) <- sommet_dist_min ; ajouter (sommet, distances.(sommet)) noeud) 
                else noeud) nouveau_tas graphe.aretes.(sommet_dist_min)
            )
	in 
    
    dijkstra_aux (creer_tas [(sommet_depart, 0.)]) ;;

let print_resultat graphe source =
    print_string "--- Tas classique ---\n";
    let dist, pred = dijkstra graphe source in
    let n = Array.length dist in
    print_string "Distances : ";
    for i = 0 to n - 1 do
        print_float dist.(i);
        print_string " ";
    done;
    print_newline ();
    print_string "Predecesseurs : ";
    for i = 0 to n - 1 do
        print_int pred.(i);
        print_string " ";
    done;
    print_newline ();
      
