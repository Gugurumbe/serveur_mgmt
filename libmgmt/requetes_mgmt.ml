include Joueur_mgmt

let rec code_exception = function
  | Joueur_inconnu -> [1]
  | Nom_pris -> [2]
  | Nom_trop_court -> [3]
  | Nom_trop_long -> [4]
  | Non_invite -> [5]
  | Doublon_invite -> [6]
  | Non_invitable liste -> 7 :: liste
  | Pas_de_message -> [8]
  | Erreur_jeu liste -> 9 :: liste
  | Erreur_interne exc ->
    let () = Printf.eprintf
        "Erreur interne : %s.\n%!"
        exc in
    [10]
  | exc ->
    code_exception (Erreur_interne (Printexc.to_string exc))

let trouver_en_jeu id : tjoueur_en_jeu =
  try Hashtbl.find joueurs_en_jeu id
  with Not_found -> raise Joueur_inconnu

let trouver_hors_jeu id : tjoueur_hors_jeu =
  try Hashtbl.find joueurs_hors_jeu id
  with Not_found -> raise Joueur_inconnu

let nouveau_joueur creer_partie detruire_partie nom =
  try
    let j = new joueur_hors_jeu creer_partie detruire_partie nom in
    let () = j#deconnecter_plus_tard in
    Lwt.return ([0], j#id_prive)
  with exc ->
    let code = code_exception exc in
    Lwt.return (code, "")

let peek_msg id_prive =
  try
    let j = trouver_hors_jeu id_prive in
    Lwt.try_bind
      (fun () -> j#peek)
      (fun ev -> Lwt.return ([0], Some ev))
      (fun exc ->
         let code = code_exception exc in
         Lwt.return (code, None))
  with exc ->
    let code = code_exception exc in
    Lwt.return (code, None)

let pop_msg id_prive =
  try
    let j = trouver_hors_jeu id_prive in
    Lwt.try_bind
      (fun () -> j#pop)
      (fun () -> Lwt.return [0])
      (fun exc ->
         let code = code_exception exc in
         Lwt.return code)
  with exc ->
    let code = code_exception exc in
    Lwt.return code

let inviter id_prive joueurs parametre =
  try
    let j = trouver_hors_jeu id_prive in
    Lwt.try_bind
      (fun () -> j#inviter (joueurs, parametre))
      (fun () -> Lwt.return [0])
      (fun exc ->
         let code = code_exception exc in
         Lwt.return code)
  with exc ->
    let code = code_exception exc in
    Lwt.return code

let annuler_invitation id_prive =
  try
    let j = trouver_hors_jeu id_prive in
    j#annuler_invitation
  with exc ->
    ()

let deconnecter id_prive =
  let deconnecter_en_jeu () =
    try
      let j = Hashtbl.find joueurs_en_jeu id_prive in
      j#deconnecter
    with
    | Not_found -> Lwt.return ()
    (* Les autres sont transmises *)
  in
  let deconnecter_hors_jeu () = 
    try
      let j = Hashtbl.find joueurs_hors_jeu id_prive in
      j#deconnecter
    with
    | Not_found -> Lwt.return ()
  in
  try
    let deco () =
      Lwt.bind
      (deconnecter_en_jeu ())
      (deconnecter_hors_jeu) in
    Lwt.try_bind
      (deco)
      (fun () ->
         Lwt.return [0])
      (fun exc -> Lwt.return (code_exception exc))
  with exc ->
    Lwt.return (code_exception exc)

let recapituler id_prive =
  try
    let j = trouver_hors_jeu id_prive in
    let flux = j#recapituler in
    Lwt.return ([0], flux)
  with exc ->
    let code = code_exception exc in
    let (flux, add) = Lwt_stream.create () in
    let () = add None in
    (* Flux vide *)
    Lwt.return (code, flux)

type resultat_traitement =
  | Erreur of int list
  | Succes of Bytes.t Lwt_stream.t

let traiter_requete_jeu id_prive req =
  try
    let j = trouver_en_jeu id_prive in
    let flux = j#traiter_requete_jeu req in
    Succes flux
  with exc ->
    Erreur (code_exception exc)

let traiter_reponse_jeu id_prive rep =
  try
    let j = trouver_en_jeu id_prive in
    let flux = j#traiter_reponse rep in
    Succes flux
  with exc ->
    Erreur (code_exception exc)
