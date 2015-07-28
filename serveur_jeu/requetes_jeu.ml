include Jeu_jeu

let compteur = creer_compteur ()

let table_parties = Hashtbl.create 1

let rec code_exception = function
  | Pas_un_joueur x -> [1 ; x]
  | Trop_peu_de_joueurs n -> [2 ; n]
  | Trop_de_joueurs n -> [3 ; n]
  | Taille_plateau_trop_petite n -> [4 ; n]
  | Taille_plateau_trop_grande n -> [5 ; n]
  | Taille_gagnante_trop_petite n -> [6 ; n]
  | Taille_gagnante_trop_grande p -> [7 ; p]
  | Coup_hors_zone (x, y) -> [8 ; x ; y]
  | Coup_deja_pris (x, y) -> [9 ; x ; y]
  | Pas_ton_tour (moi, lui) -> [10 ; moi ; lui]
  | Pas_de_message -> [11]
  | Erreur_interne chaine ->
    let () = Printf.eprintf "Erreur interne : %s.\n%!"
        chaine in
    [12]
  | exc ->
    code_exception (Erreur_interne (Printexc.to_string exc))

let trouver_partie id =
  try Hashtbl.find table_parties id
  with Not_found -> raise (Pas_une_partie id)

let creer_partie nombre_joueurs taille_plateau_w taille_plateau_h taille_gagnante =
  try
    let nouveau_jeu = creer_jeu compteur nombre_joueurs taille_plateau_w taille_plateau_h taille_gagnante in
    let () = Hashtbl.add table_parties nouveau_jeu.id_partie nouveau_jeu in
    Lwt.return ([0], Some nouveau_jeu.id_partie)
  with exc ->
    let code = code_exception exc in
    Lwt.return (code, None)

let detruire_partie id_partie =
  try
    let () =
      let partie = trouver_partie id_partie in
      partie.fermer () in
    let () =
      try Hashtbl.remove table_parties id_partie
      with _ -> raise (Pas_une_partie id_partie) in
    Lwt.return [0]
  with exc ->
    let code = code_exception exc in
    Lwt.return code

let peek_msg id_partie joueur =
  try
    let partie = trouver_partie id_partie in
    let flux =
      if joueur >= 0 && joueur < Array.length partie.evenements
      then partie.evenements.(joueur)
      else raise (Pas_un_joueur joueur) in
    let ev = Lwt_stream.peek flux in
    let fin =
      Lwt.try_bind
        (fun () -> ev)
        (function
          | None -> Lwt.fail Pas_de_message
          | some_ev -> Lwt.return ([0], some_ev))
        (fun exc ->
           let code = code_exception exc in
           Lwt.return (code, None)) in
    fin
  with exc ->
    let code = code_exception exc in
    Lwt.return (code, None)
        
let pop_msg id_partie joueur =
  try
    let partie = trouver_partie id_partie in
    let flux =
      if joueur >= 0 && joueur < Array.length partie.evenements
      then partie.evenements.(joueur)
      else raise (Pas_un_joueur joueur) in
    match Lwt_stream.get_available_up_to 1 flux with
    | [_] -> Lwt.return [0]
    | _ -> raise Pas_de_message
  with exc ->
    let code = code_exception exc in
    Lwt.return code

let jouer id_partie joueur x y =
  try
    let partie = trouver_partie id_partie in
    let () = coup partie joueur x y in
    Lwt.return [0]
  with exc ->
    let code = code_exception exc in
    Lwt.return code

let recapituler id_partie joueur =
  try
    let partie = trouver_partie id_partie in
    let () = recapituler partie joueur in
    Lwt.return [0]
  with exc ->
    let code = code_exception exc in
    Lwt.return code
