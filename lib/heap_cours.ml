type 't tas = Vide | Noeud of 't tas * 't * 't tas ;;

let rec ajouter element tas = 
	match tas with
	| Vide -> Noeud (Vide, element, Vide)
	| Noeud (fils_gauche, valeur, fils_droit) -> 
		Noeud (fils_droit, min valeur element, ajouter (max valeur element) fils_gauche) ;;

let rec ajouter_plusieurs liste tas = 
  match liste with
  | [] -> tas
  | elt1::reste -> ajouter elt1 (ajouter_plusieurs reste tas) ;;

let rec supprimer_premier_noeud tas = 
  match tas with
  | Vide -> Vide
  | Noeud (Vide, _, fils_droit) -> fils_droit
  | Noeud (fils_gauche, _, Vide) -> fils_gauche
  | Noeud ((Noeud (_fg_fg, _fg_val, _fg_fd) as fg), _valeur, (Noeud (_fd_fg, _fd_val, _fd_fd) as fd)) -> 
    if _fg_val < _fd_val
    then Noeud (supprimer_premier_noeud fg, _fg_val, fd)
    else Noeud (fg, _fd_val, supprimer_premier_noeud fd) ;;

let vider tas = 
  let rec vider_rt tas acc =
      match tas with
      | Vide -> acc
      | Noeud (_, valeur, _) as noeud -> vider_rt (supprimer_premier_noeud noeud) (acc @ [valeur])
  in vider_rt tas [] ;; 

let creer_tas liste_elements = ajouter_plusieurs liste_elements Vide ;;