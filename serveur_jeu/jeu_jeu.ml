let creer_compteur () =
  let i = ref 0 in
  let compter () =
    let j = !i in
    let () = incr i in
    j
  in
  compter

type evenement =
  | Changement_nombre_joueurs of int
  | Changement_numero of int
  | Changement_taille_plateau of (int * int) (* x - y *)
  | Changement_taille_gagnante of int
  | Coup of (int * int * int) (* x - y - joueur *)
  | Changement_tour of int
  | Fin_partie of int (* gagnant *)

type jeu = {
  id_partie: int;
  taille_gagnante: int;
  nombre_joueurs: int;
  carte: int array array;
  evenements: evenement Lwt_stream.t array;
  pousser: int -> evenement -> unit;
  fermer: unit -> unit;
  mutable tour: int;
  mutable gagnant: int option;
}

exception Pas_une_partie of int
exception Pas_un_joueur of int
exception Trop_peu_de_joueurs of int (* Minimum 2 *)
exception Trop_de_joueurs of int (* Max 8 *)
exception Taille_plateau_trop_petite of int (* Minimum 2 *)
exception Taille_plateau_trop_grande of int (* max 16x16 *)
exception Taille_gagnante_trop_petite of int (* Minimum 3 *)
exception Taille_gagnante_trop_grande of int (* Maximum la taille du plateau *)
exception Coup_hors_zone of (int * int)
exception Coup_deja_pris of (int * int) (* x - y *)
exception Pas_ton_tour of (int * int) (* Mon numéro - celui dont c'est réellement le tour *)
exception Pas_de_message (* quand je cherche à faire un pop *)
exception Erreur_interne of bytes

let creer_jeu compteur nombre_joueurs taille_plateau_w taille_plateau_h taille_gagnante =
  let () = if nombre_joueurs < 2
    then raise (Trop_peu_de_joueurs nombre_joueurs) in
  let () = if nombre_joueurs > 8
    then raise (Trop_de_joueurs nombre_joueurs) in
  let () = if taille_plateau_w < 2
    then raise (Taille_plateau_trop_petite taille_plateau_w) in
  let () = if taille_plateau_w > 16
    then raise (Taille_plateau_trop_grande taille_plateau_w) in
  let () = if taille_plateau_h < 2
    then raise (Taille_plateau_trop_petite taille_plateau_h) in
  let () = if taille_plateau_h > 16
    then raise (Taille_plateau_trop_grande taille_plateau_h) in
  let () = if taille_gagnante < 3
    then raise (Taille_gagnante_trop_petite taille_gagnante) in
  let () = if taille_gagnante > min taille_plateau_w taille_plateau_h
    then raise (Taille_gagnante_trop_grande taille_gagnante) in
  let flux = Array.init 5 (fun _ -> Lwt_stream.create ()) in
  {
    id_partie = compteur ();
    taille_gagnante = taille_gagnante;
    nombre_joueurs = nombre_joueurs;
    carte = Array.make_matrix taille_plateau_h taille_plateau_w (-1);
    evenements = Array.map (fst) flux;
    pousser = (fun i msg ->
        if i >= 0 && i < Array.length flux
        then (snd flux.(i)) (Some msg)
        else (raise (Pas_un_joueur i)));
    fermer = (fun () -> Array.iter (fun (_, f) -> f None) flux);
    tour = 0;
    gagnant = None;
  }

let alignement carte i j =
  let compter (di, dj) =
    let rec aux (di, dj) k =
      let i_ = i + k * di in
      let j_ = j + k * dj in
      if i_ >= 0 && i_ < Array.length carte
         && j_ >= 0 && j_ < Array.length carte.(i_)
         && carte.(i_).(j_) = carte.(i).(j)
      then aux (di, dj) (k + 1)
      else k
    in
    let k_max = aux (di, dj) 0 in
    let k_min = aux (-di, -dj) 0 in
    k_max + k_min - 1
  in
  let directions = [(0, 1) ; (1, 1) ; (1, 0) ; (1, -1)] in
  (* La moitié suffit, on compte dans les 2 directions *)
  let resultats = List.map (compter) directions in
  List.fold_left (max) 1 resultats
    
let coup jeu numero_joueur x y =
  if numero_joueur >= 0 && numero_joueur < jeu.nombre_joueurs then
    if y >= 0 && y < Array.length jeu.carte
       && x >= 0 && x < Array.length jeu.carte.(y) then
      if jeu.carte.(y).(x) = -1 then
        if jeu.tour = numero_joueur then
          (* On attribue ce coup *)
          let () = jeu.carte.(y).(x) <- numero_joueur in
          (* On pousse un événement *)
          let () = for i = 0 to -1 + jeu.nombre_joueurs do
              jeu.pousser i (Coup (x, y, numero_joueur)) done in
          (* On vérifie si on a gagné *)
          if alignement jeu.carte y x >= jeu.taille_gagnante then
            let () = jeu.gagnant <- Some numero_joueur in
            for i = 0 to -1 + jeu.nombre_joueurs do
              jeu.pousser i (Fin_partie numero_joueur) done
          else (* On passe au joueur suivant *)
            let () = jeu.tour <- (1 + jeu.tour) mod jeu.nombre_joueurs in
            for i = 0 to -1 + jeu.nombre_joueurs do
              jeu.pousser i (Changement_tour jeu.tour) done
          else raise (Pas_ton_tour (numero_joueur, jeu.tour))
        else raise (Coup_deja_pris (x, y))
      else raise (Coup_hors_zone (x, y))
    else raise (Pas_un_joueur numero_joueur)

let recapituler jeu i =
  let non_lus = Lwt_stream.get_available jeu.evenements.(i) in
  let _ = non_lus in (* On les oublie *)
  let p = jeu.pousser i in
  p (Changement_nombre_joueurs (jeu.nombre_joueurs));
  p (Changement_numero i);
  p (Changement_taille_plateau
       (Array.length jeu.carte,
        let largeurs = Array.map (Array.length) jeu.carte in
        Array.fold_left (min) max_int largeurs));
  p (Changement_taille_gagnante jeu.taille_gagnante);
  Array.iteri
    (fun y -> Array.iteri
        (fun x joueur ->
           if joueur >= 0 then
             p (Coup (x, y, joueur)))) jeu.carte;
  (match jeu.gagnant with
   | None -> p (Changement_tour (jeu.tour))
   | Some g -> p (Fin_partie g))
