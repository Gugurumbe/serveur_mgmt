open Requetes_jeu

(* Très important : on met les id de joueurs entre dollar $ pour pouvoir les remplacer par leur nom dans le sgsj. *)

let ecrire_evenement = function
  | Changement_nombre_joueurs n ->
    Printf.sprintf "nombre_joueurs: %d" n
  | Changement_numero n ->
    Printf.sprintf "moi: $%d$" n
  | Changement_taille_plateau (w, h) ->
    Printf.sprintf "taille_plateau: %d,%d" w h
  | Changement_taille_gagnante n ->
    Printf.sprintf "taille_gagnante: %d" n
  | Coup (x, y, joueur) ->
    Printf.sprintf "coup: %d,%d,$%d$" x y joueur
  | Changement_tour tour ->
    Printf.sprintf "tour: $%d$" tour
  | Fin_partie gagnant ->
    Printf.sprintf "gagnant: $%d$" gagnant

let ecrire_code liste =
  let chaines = List.map (string_of_int) liste in
  Bytes.concat "." chaines

let lire_id_jeu chaine =
  let rec chercher_point i =
    if i < Bytes.length chaine then
      let c = Bytes.get chaine i in
      if c = '.' then i
      else chercher_point (i + 1)
    else i in
  let i_point = chercher_point 0 in
  let id_jeu = Bytes.sub chaine 0 i_point in
  let id_joueur =
    if i_point + 1 < Bytes.length chaine
    then
      let deb = i_point + 1 in
      let longueur_restante = - deb + Bytes.length chaine in
      Bytes.sub chaine deb longueur_restante
    else "" in
  let id_jeu =
    try int_of_string id_jeu
    with _ -> -1 in
  let id_joueur =
    try int_of_string id_joueur
    with _ -> -1 in
  (id_jeu, id_joueur)
(* Ne lève jamais d'exception, mais met à -1 par défaut pour solliciter un code d'erreur Pas_un_joueur ou Pas_une_partie. *)

let lire_parametre chaine =
  (* Idem, par défaut il y a 3 lignes, 3 colonnes et on gagne en en mettant 3 d'un coup. *)
  try
    (* Pas besoin de se casser la tête à échapper les virgules *)
    let string_of_revchars liste =
      let liste = List.rev liste in
      let liste = List.map (Bytes.make 1) liste in
      Bytes.concat "" liste
    in
    let rec separer liste mot_en_cours i =
      if i < Bytes.length chaine then
        let c = Bytes.get chaine i in
        if c = ',' then
          separer
            ((string_of_revchars mot_en_cours) :: liste)
            [] (i + 1)
        else separer liste (c :: mot_en_cours) (i + 1)
      else
        List.rev ((string_of_revchars mot_en_cours) :: liste)
    in
    match List.map (int_of_string) (separer [] [] 0) with
    | [p ; n ; n_gagnant] ->
      (p, n, n_gagnant)
    | _ -> failwith "Ça va pas du tout."
  with _ -> (3, 3, 3)

(* Schéma : type de requête / identifiant / ... *)

let traiter_requete mdp_admin lignes_requete =
  let (reponse, add) = Lwt_stream.create () in
  let repondre ligne = add (Some ligne) in
  let repondre_code code =
    let chaines = List.map (string_of_int) code in
    let chaine = Bytes.concat "." chaines in
    repondre chaine in
  let terminer () = Lwt.return (add None) in
  let travailler () =
    let open Lwt in
    (Lwt_stream.get lignes_requete)
    >>= (function
        | None -> terminer ()
        | Some "creer_partie" ->
          (Lwt_stream.get lignes_requete)
          >>= (function
              | None -> terminer ()
              | Some id_admin when id_admin = mdp_admin ->
                (Lwt_stream.get lignes_requete)
                >>= (function
                    | None -> terminer ()
                    | Some nombre_joueurs ->
                      try
                        let n = int_of_string nombre_joueurs in
                        (Lwt_stream.get lignes_requete)
                        >>= (function
                            | None -> terminer()
                            | Some parametre ->
                              let (w, h, g) = lire_parametre parametre in
                              (creer_partie n w h g)
                              >>= (function
                                  | (code, None) ->
                                    let () = repondre (ecrire_code code) in
                                    let () = repondre "" in
                                    terminer ()
                                  | (code, Some id) ->
                                    let () = repondre (ecrire_code code) in
                                    let () = repondre (string_of_int id) in
                                    terminer ()))
                      with exc ->
                        let () = Printf.eprintf "Erreur : %s.\n%!" (Printexc.to_string exc) in (* Utile au début *)
                        terminer ())
              | Some faux_id_admin ->
                let () = Printf.eprintf "ERREUR : mauvais id admin (créer une partie).\n%!" in
                terminer ())
        | Some "detruire_partie" ->
          (Lwt_stream.get lignes_requete)
          >>= (function
              | None -> terminer ()
              | Some id_admin when id_admin = mdp_admin ->
                (Lwt_stream.get lignes_requete)
                >>= (function
                    | None -> terminer ()
                    | Some n ->
                      try
                        let id_partie = int_of_string n in
                        (detruire_partie id_partie)
                        >>= (fun code ->
                            let () = repondre_code code in
                            terminer ())
                      with _ -> terminer ())
              | Some faux_id_admin ->
                let () = Printf.eprintf "ERREUR : mauvais id admin (supprimer une partie).\n%!" in
                terminer ())
        | Some "jeu" ->
          (Lwt_stream.get lignes_requete)
          >>= (fun type_requete ->
              (Lwt_stream.get lignes_requete)
              >>= (function
                  | None -> terminer ()
                  | Some identifiant ->
                    let (id_partie, id_joueur) = lire_id_jeu identifiant in
                    match type_requete with
                    | None -> terminer ()
                    (* Impossible en principe, on vient de lire un identifiant *)
                    | Some "peek" ->
                      (peek_msg id_partie id_joueur)
                      >>= (fun (code, ev) ->
                          let () = repondre_code code in
                          let () = repondre
                              (match ev with
                               | None -> ""
                               | Some ev -> ecrire_evenement ev) in
                          terminer ())
                    | Some "pop" ->
                      (pop_msg id_partie id_joueur)
                      >>= (fun code ->
                          let () = repondre_code code in
                          let () = repondre "" in
                          terminer ())
                    | Some "coup" ->
                      (Lwt_stream.nget 2 lignes_requete)
                      >>= (function
                          | [x ; y] ->
                            begin try
                                (jouer id_partie id_joueur (int_of_string x) (int_of_string y))
                                >>= (fun code ->
                                    let () = repondre_code code in
                                    let () = repondre "" in
                                    terminer ())
                              with _ -> terminer () end
                          | _ -> terminer ())
                    | Some rq ->
                      let () = Printf.eprintf "Requête de jeu inconnue : %s.\n%!" rq in
                      terminer ()))
        | Some rq ->
          let () = Printf.eprintf "Requête inconnue : %s.\n%!" rq in
          terminer ())
  in
  let () = Async_jeu.async (travailler) in
  reponse
