open Requetes_mgmt

let ecrire_evenement = function
  | Changement_nom moi ->
    Printf.sprintf "mon_nom: %s" moi (* Déjà échappé *)
  | Nouveau_joueur gus ->
    Printf.sprintf "nouveau_joueur: %s" gus 
  | Depart_joueur gus ->
    Printf.sprintf "depart_joueur: %s" gus
  | Nouvelle_invitation (joueurs, parametre) ->
    Printf.sprintf "invitation: %s,%s"
      (Bytes.concat "," joueurs) parametre
  | Invitation_annulee (joueurs, parametre, annulant) ->
    Printf.sprintf "invitation_annulee: %s,%s,%s"
      (Bytes.concat "," joueurs) parametre annulant

let ecrire_code liste =
  let chaines = List.map (string_of_int) liste in
  Bytes.concat "." chaines

let lire_code chaine =
  let chars_of_string chaine =
    let rec aux accurev i =
      if i < Bytes.length chaine
      then let c = Bytes.get chaine i in
        aux (c :: accurev) (i + 1)
      else List.rev accurev
    in
    aux [] 0 
  in
  let string_of_chars chars =
    let liste = List.map (Bytes.make 1) chars in
    Bytes.concat "" liste
  in
  let rec separer elements element_en_cours = function
    | [] ->
      let nouvel_element = string_of_chars
          (List.rev element_en_cours) in
      List.rev (nouvel_element :: elements)
    | '.' :: reste ->
      let nouvel_element = string_of_chars
          (List.rev element_en_cours) in
      separer (nouvel_element :: elements) [] reste
    | c :: reste ->
      separer elements (c :: element_en_cours) reste
  in
  List.map (int_of_string)
    (separer [] [] (chars_of_string chaine))

let traiter_requete mdp_admin sockaddr_backend lignes_requete =
  let (reponse, add) = Lwt_stream.create () in
  let repondre ligne = add (Some ligne) in
  let repondre_code code = repondre (ecrire_code code) in
  let terminer () =
    Lwt.return (add None) in
  let creer_partie n param =
    let open Lwt in
    let chan = Lwt_io.open_connection sockaddr_backend in
    let fin_normale () =
      chan
      >>= (fun (chan_in, chan_out) ->
          (Lwt_io.write chan_out
             (Printf.sprintf "creer_partie\n%s\n%d\n%s\n"
                mdp_admin n param))
          >>= (fun () ->
              (Lwt_io.read_line chan_in)
              >>= (fun code ->
                  (Lwt_io.read_line chan_in)
                  >>= (fun id_partie ->
                      Lwt.return (lire_code code, id_partie))))) in
    let fin_sans_exception =
      try_bind
        (fin_normale)
        (fun x -> return x)
        (fun exc ->
           let () =
             Printf.eprintf
               "Erreur : le serveur de jeu n'a pas réagi de façon normale (%s).\n%!"
               (Printexc.to_string exc) in
           Lwt.return ([-1], "")) in
    let fermer () =
      (chan)
      >>= (fun (i, o) ->
          (try_bind
             (fun () -> Lwt_io.close i)
             (fun () -> Lwt.return ())
             (fun _ -> Lwt.return ()))
          >>= (fun () ->
              try_bind
                (fun () -> Lwt_io.close o)
                (fun () -> Lwt.return ())
                (fun _ -> Lwt.return ()))) in
    fin_sans_exception
    >>= (fun resultat ->
        (fermer ())
        >>= (fun () -> return resultat))
  in
  let detruire_partie partie =
    let open Lwt in
    let chan = Lwt_io.open_connection sockaddr_backend in
    let fin_normale () =
      (chan)
      >>= (fun (chan_in, chan_out) ->
          (Lwt_io.write chan_out
             (Printf.sprintf "detruire_partie\n%s\n%s\n"
                mdp_admin partie))
          >>= (fun () ->
              (Lwt_io.read_line chan_in)
              >>= (fun code ->
                  Lwt.return (lire_code code)))) in
    let fin_sans_exception =
      try_bind
        (fin_normale)
        (fun x -> return x)
        (fun exc ->
           let () =
             Printf.eprintf
               "Erreur : le serveur de jeu n'a pas réagi de façon normale (%s).\n%!"
               (Printexc.to_string exc) in
           Lwt.return [-1]) in
    let fermer () =
      (chan)
      >>= (fun (i, o) ->
          (try_bind
             (fun () -> Lwt_io.close i)
             (fun () -> return ())
             (fun _ -> Lwt.return ()))
          >>= (fun () ->
              try_bind
                (fun () -> Lwt_io.close o)
                (fun () -> return ())
                (fun _ -> Lwt.return ()))) in
    fin_sans_exception
    >>= (fun res ->
        (fermer ())
        >>= (fun () -> return res))
  in
  let requete_jeu id_prive req =
    let requete_traitee = traiter_requete_jeu id_prive req in
    let (reponse_brute, add) = Lwt_stream.create () in
    let repondre chaine = add (Some chaine) in
    let terminer () = Lwt.return (add None) in
    let travailler () =
      let open Lwt in
      match requete_traitee with
      | Erreur code ->
        repondre (ecrire_code code);
        repondre "";
        terminer ()
      | Succes requete_traitee ->
        let chan = Lwt_io.open_connection sockaddr_backend in
        let fin_normale () =
          (chan)
          >>= (fun (chan_in, chan_out) ->
              (Lwt_io.write_lines chan_out
                 requete_traitee)
              >>= (fun () ->
                  Lwt_stream.iter
                    (repondre)
                    (Lwt_io.read_lines chan_in))) in
        let fin_sans_exception =
          try_bind
            (fin_normale)
            (fun x -> return x)
            (fun exc ->
               let () =
                 Printf.eprintf
                   "Erreur : le serveur de jeu n'a pas réagi de façon normale (%s).\n%!"
                   (Printexc.to_string exc) in
               return ()) in
        let fermer () =
          (chan)
          >>= (fun (i, o) ->
              (try_bind
                 (fun () -> Lwt_io.close i)
                 (fun () -> return ())
                 (fun _ -> Lwt.return ()))
              >>= (fun () ->
                  try_bind
                    (fun () -> Lwt_io.close o)
                    (fun () -> return ())
                    (fun _ -> Lwt.return ()))) in
        fin_sans_exception >>= fermer
    in
    let () = Async_mgmt.async (travailler) in
    match traiter_reponse_jeu id_prive reponse_brute with
    | Erreur code ->
      let () = Printf.eprintf "Ne devrait pas arriver...\n" in
      exit (-1)
    | Succes reponse ->
      reponse
  in
  let get_line () =
    Lwt.bind
    (Lwt_stream.get lignes_requete)
    (function
      | None ->
        Lwt.return None
      | Some ligne ->
        Lwt.return (Some (Bytes.trim ligne)))
  in
  let travailler () =
    let open Lwt in
    (get_line ())
    >>= (function
        | None -> terminer ()
        | Some "nouveau_joueur" ->
          (* let () = Printf.printf "Nouveau joueur !\n%!" in *)
          (get_line ())
          >>= (function
              | None ->
                (* let () = Printf.printf "Euh... c'est fini.\n%!" in *)
                terminer ()
              | Some nom ->
                (* let () = Printf.printf "Il veut s'appeler %s.\n%!" nom in *)
                (nouveau_joueur
                   creer_partie detruire_partie
                   nom)
                >>= (fun (code, id) ->
                    let () = repondre_code code in
                    let () = repondre id in
                    terminer ()))
        | Some "peek" ->
          (get_line ())
          >>= (function
              | None -> terminer ()
              | Some id_prive ->
                (peek_msg id_prive)
                >>= (function
                    | code, None ->
                      let () = repondre_code code in
                      let () = repondre "" in
                      terminer ()
                    | code, Some ev ->
                      let () = repondre_code code in
                      let () = repondre (ecrire_evenement ev) in
                      terminer ()))
        | Some "pop" ->
          (get_line ())
          >>= (function
              | None -> terminer ()
              | Some id_prive ->
                (pop_msg id_prive)
                >>= (fun code ->
                    let () = repondre_code code in
                    terminer ()))
        | Some "deconnecter" ->
          (get_line ())
          >>= (function
              | None -> terminer ()
              | Some id_prive ->
                (deconnecter id_prive)
                >>= (fun code ->
                    let () = repondre_code code in
                    terminer ()))
        | Some "inviter" ->
          (Lwt_stream.nget 2  lignes_requete)
          >>= (function
              | [id_prive ; nombre_invites] ->
                begin try
                    let nombre_invites =
                      int_of_string nombre_invites in
                    (Lwt_stream.nget
                       (nombre_invites + 1)
                       lignes_requete)
                    >>= (fun liste ->
                        if List.length liste <> nombre_invites + 1
                        then terminer ()
                        else
                          let tab = Array.of_list liste in
                          let joueurs =
                            Array.sub tab 0 nombre_invites in
                          let parametre = tab.(nombre_invites) in
                          let joueurs = Array.to_list joueurs in
                          let fin_requete =
                            Lwt_stream.iter (ignore)
                              lignes_requete in
                          let resultat =
                            inviter id_prive joueurs parametre in
                          let () = Async_mgmt.async
                              (fun () -> fin_requete
                                >>= (fun () ->
                                    let () =
                                      annuler_invitation
                                        id_prive in
                                    Lwt.return ())) in
                          (* Si elle est acceptée à temps, ça ne l'annulera pas. *)
                          (resultat)
                          >>= (fun code -> repondre_code code;
                                terminer ()))
                  with _ ->
                    (* nombre_invites n'est pas un entier *)
                    terminer () end
              | _ -> terminer ())
(* On commence par envoyer le nombre de gens sur la première ligne, puis les gens, puis le paramètre. Ensuite, si le flux s'interromp, on appelle annuler_invitation. *)
        | Some "recapituler" ->
          (get_line ())
          >>= (function
              | None -> terminer ()
              | Some id_prive ->
                (recapituler id_prive)
                >>= (fun (code, flux) ->
                    let () = repondre_code code in
                    let fin_flux =
                      Lwt_stream.iter
                        (fun ev ->
                           repondre (ecrire_evenement ev))
                        flux in
                    fin_flux >>= terminer))
        | Some "jeu" ->
          let requete_brute, pousser = Lwt_stream.create () in
          (Lwt_stream.nget 2 lignes_requete)
          >>= (function
              | [sous_cmd ; id] ->
                let () = List.iter
                    (fun x -> pousser (Some x))
                    ["jeu" ; sous_cmd ; id] in
                let reponse = requete_jeu id requete_brute in
                let () = Async_mgmt.async
                    (fun () ->
                       (Lwt_stream.iter
                          (fun ligne ->
                             pousser (Some ligne))
                          lignes_requete)
                       >>= (fun () ->
                           return (pousser None))) in
                (Lwt_stream.iter
                   (repondre) reponse)
                >>= terminer
              | _ -> terminer ())
        | Some cmd ->
          let () = Printf.eprintf "Commande invalide : %s.\n%!" cmd in
          terminer ())
  in
  let () = Async_mgmt.async (travailler) in
  reponse
