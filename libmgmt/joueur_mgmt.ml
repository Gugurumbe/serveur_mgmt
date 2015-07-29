let remplacer remplacement chaine =
  let rec prochain_dollar i =
    if i < Bytes.length chaine then
      let c = Bytes.get chaine i in
      if c = '\\' then prochain_dollar (i + 2)
      else if c = '$' then i
      else prochain_dollar (i + 1)
    else i in
  let dechapper chaine =
    (* Il faut supprimer les "\\" devant les dollars de texte *)
    let string_of_chars_rev liste =
      let liste = List.rev liste in
      let liste = List.map (Bytes.make 1) liste in
      Bytes.concat "" liste in
    let chars =
      let rec aux i liste =
        if i < Bytes.length chaine then
          let c = Bytes.get chaine i in
          aux (i + 1) (c :: liste)
        else List.rev liste in
      aux 0 [] in
    let rec aux accurev = function
      | [] -> string_of_chars_rev accurev
      | '\\' :: '\\' :: reste -> aux ('\\' :: accurev) reste
      | '\\' :: '$' :: reste -> aux ('$' :: accurev) reste
      | c :: reste -> aux (c :: accurev) reste
    in
    aux [] chars
  in
  let rec traiter_morceau accu_morceaux i_debut a_remplacer =
    if i_debut < Bytes.length chaine then
      let i_fin = prochain_dollar i_debut in
      let brut = Bytes.sub chaine i_debut (i_fin - i_debut) in
      let dechappe = dechapper brut in
      let operationnel =
        if a_remplacer then remplacement dechappe
        else dechappe in
      let accu_morceaux = operationnel :: accu_morceaux in
      let i_debut = i_fin + 1 in (* pour pas redétecter le même dollar *)
      let a_remplacer = not a_remplacer in
      traiter_morceau accu_morceaux i_debut a_remplacer
    else Bytes.concat "" (List.rev accu_morceaux)
  in
  traiter_morceau [] 0 false
    
type evenement =
  | Changement_nom of Bytes.t (* La version échappée du nom demandé *)
  | Nouveau_joueur of Bytes.t
  | Depart_joueur of Bytes.t
  | Nouvelle_invitation of (Bytes.t list * Bytes.t)
  (* Les noms des gens (dont moi, tous uniques) x les arguments supplémentaires *)
  | Invitation_annulee of (Bytes.t list * Bytes.t * Bytes.t)
  (* L'invitation en question x arguments x le nom du type qui a refusé *)

class type tjoueur_hors_jeu =
  object
    method deconnecter: unit Lwt.t
    (* Se déconnecte maintenant *)
    method deconnecter_plus_tard: unit
    (* Repousse la mort *)
    (* Le constructeur de la classe dit bonjour à tout le monde. *)
    method ajouter_evenement: evenement -> unit
    method recapituler: evenement Lwt_stream.t
    method invitation_invalide: int list -> unit
    (* L'invitation a été jugée invalide par le serveur de jeu *)
    method invitation_refusee_par: int list -> unit
    method invitation_acceptee: Bytes.t -> int -> Bytes.t array -> unit
    (* Je fais maintenant partie du jeu x, en position y. Je retourne le thread initié par inviter, je me supprime de la table des joueurs hors jeu, je crée un joueur en jeu. Le tableau est celui des ids privés des adversaires. *)
    method private flux_evenements: evenement Lwt_stream.t
    method id_prive: Bytes.t
    method nom_reel: Bytes.t
    method invitation_demandee: (Bytes.t list * Bytes.t) option
    method invitation_compatible: (Bytes.t list * Bytes.t) -> bool
    method inviter: Bytes.t list * Bytes.t -> unit Lwt.t
    (* Peut lever tout un tas d'exceptions dans le thread retourné ! Si l'invitation est réussie, une requête est envoyée au serveur de jeu. Si le serveur répond "0", alors les adversaires sortent de la table des joueurs hors jeu, annulent les invitations restantes dans la table hors jeu, disent au revoir et s'inscrivent dans la table des joueurs en jeu. Au préalable, j'annule mon invitation. Une fois qu'elle est modifiée, je regarde si elle est compatible avec celles des autres. Si oui, je notifie les autres et je regarde si c'est prêt. Si non, je la refuse tout de suite. *)
    method annuler_invitation: unit
    (* Si j'ai une invitation, tous ceux qui la partagent recevront Non_invitable *)
    (* Pour refuser une invitation, soit j'attend que le type se décourage, soit j'invite qqn d'autre. *)
    method peek: evenement Lwt.t (* Lève l'exception Pas_de_message si le flux de messages est arrêté... ce qui ne se rencontrera pas, mais bon. *)
    method pop: unit Lwt.t (* Lève l'exception Pas_de_message plutôt que d'attendre le prochain meessage *)

    (* NB : si le joueur reste inactif trop longtemps, il se détruit tout seul comme un grand, envoie un événement chez tous les joueurs hors jeu pour dire qu'il se déconnecte, annule toutes les invitations des gens qui l'invitent. *)
  end

class type tjoueur_en_jeu =
  object
    method deconnecter: unit Lwt.t
    method deconnecter_plus_tard: unit
    (* Repousse la mort *)
    method id_prive: Bytes.t
    method nom_reel: Bytes.t
    method id_partie: Bytes.t
    method adversaires: tjoueur_en_jeu array (* Pour les remplacements dans la réponse *)
    method mon_numero: int
    method traiter_requete_jeu: Bytes.t Lwt_stream.t -> Bytes.t Lwt_stream.t
    (* Si le premier mot n'est pas "jeu", le flux est vide. Sinon, transmet les 2 premières lignes ("jeu" et le nom de la commande), change la 3ème ligne (quelle qu'elle soit, elle devient id_partie^"."^mon_numero). On ne vérifie pas ici si l'ID privé correspond bien à celui de ce joueur. *)
    method traiter_reponse: Bytes.t Lwt_stream.t -> Bytes.t Lwt_stream.t
    (* Remplace tous les nombres entre dollars $3$ par la chaine adversaires.(...) correspondante *)
  end

exception Joueur_inconnu
exception Nom_pris
exception Nom_trop_court
exception Nom_trop_long (* Ces quatre exceptions sont levées dans le constructeur de joueur_hors_jeu *)
exception Non_invite (* Je ne me suis pas invité *)
exception Doublon_invite (* J'ai invité plusieurs fois le même type *)
exception Non_invitable of int list (* La place des gens non invitables *)
exception Pas_de_message
exception Erreur_jeu of int list
exception Erreur_interne of Bytes.t

let joueurs_en_jeu = Hashtbl.create 1

let joueurs_hors_jeu = Hashtbl.create 1

let max_delai = 600. (* Au bout de 10 minutes d'inactivité, on considère que le type est mort. *)

class joueur_en_jeu detruire_partie id_prive nom_reel id_partie mon_numero ids_prives_adversaires : tjoueur_en_jeu =
  object(self)
    initializer Hashtbl.add joueurs_en_jeu id_prive (self :> tjoueur_en_jeu)
    initializer self#deconnecter_plus_tard
    val mutable fusible = None (* À réveiller si on veut annuler un timeout *)
    method deconnecter =
      (* On détruit la partie, on vire les adversaires de la liste des joueurs en jeu *)
      Lwt.bind
      (detruire_partie id_partie)
      (fun code ->
         if code <> [0] then
           Printf.eprintf "Attention : la partie %s n'a peut-être pas été supprimée. Code : %s.\n%!"
             id_partie
             (Bytes.concat "."
                (List.map (string_of_int) code));
          Lwt.return
            (Array.iter
               (fun id ->
                  try Hashtbl.remove joueurs_en_jeu id
                  with _ -> ())
               ids_prives_adversaires))
    method deconnecter_plus_tard =
      let () =
        match fusible with
        | None -> ()
        | Some fusible ->
          Lwt.wakeup fusible ()
      in
      let (annuler, f) = Lwt.wait () in
      let () = fusible <- Some f in
      let oui = 
        Lwt.bind
          (Lwt_unix.sleep max_delai)
          (fun () -> Lwt.return true) in
      let non = Lwt.bind annuler (fun () -> Lwt.return false) in
      let travailler () =
        Lwt.bind
          (Lwt.choose [oui ; non])
          (function
            | true -> self#deconnecter
            | false -> Lwt.return ())
      in
      Lwt.async (travailler)
    method id_prive = Bytes.copy id_prive
    method nom_reel = Bytes.copy nom_reel
    method id_partie = Bytes.copy id_partie
    method adversaires =
      try
        Array.map (Hashtbl.find joueurs_en_jeu) ids_prives_adversaires
      with Not_found ->
        raise (Erreur_interne "Impossible de trouver un joueur en jeu dans la table.")
    method mon_numero = mon_numero
    method traiter_requete_jeu req =
      let open Lwt in
      let (traite, add) = Lwt_stream.create () in
      let traiter chaine = add (Some chaine) in
      let terminer () =
        let () = try add None
          with _ -> () in
        return () in
      let travailler () =
        (Lwt_stream.get req)
        >>= (function
            | Some "jeu" ->
              let () = traiter "jeu" in
              (Lwt_stream.get req)
              >>= (function
                  | Some cmd ->
                    let () = traiter cmd in
                    (Lwt_stream.get req)
                    >>= (function
                        | Some _ ->
                          let id_jeu =
                            id_partie^"."^(string_of_int mon_numero) in
                          let () = traiter id_jeu in
                          let fin =
                            Lwt_stream.iter (traiter) req in
                          fin >>= terminer
                        | None -> terminer ())
                  | None -> terminer ())
            | _ -> terminer ())
      in
      let travailler_sans_exception () =
        try_bind
          (travailler)
          (terminer)
          (fun exc ->
             Printf.eprintf
               "Erreur lors de la transmission d'une requête : %s.\n%!"
               (Printexc.to_string exc);
             terminer ())
      in
      let () = async (travailler_sans_exception) in
      traite
    method traiter_reponse reponse =
      let remplacement str =
        try
          let i =
            try int_of_string str
            with exc ->
              let () = Printf.eprintf "Erreur : le serveur a renvoyé un entier qui n'est pas un numéro de joueur.\n%!" in
              raise exc
          in
          let id_prive = ids_prives_adversaires.(i) in
          let adv = Hashtbl.find joueurs_en_jeu id_prive in
          adv#nom_reel
        with _ ->
          "???"
      in
      Lwt_stream.map
        (remplacer remplacement)
        reponse
  end

let creer_id_prive () =
  let alphabet = "1234567890AZERTYUIOPQSDFGHJKLMWXCVBNazertyuiopqsdfghjklmwxcvbn" in
  let n = Bytes.length alphabet in
  let lettre () = Bytes.get alphabet (Random.int n) in
  let rec trouver_id_prive () =
    let chaine = Bytes.init 32 (fun _ -> lettre ()) in
    if Hashtbl.mem joueurs_en_jeu chaine
       || Hashtbl.mem joueurs_hors_jeu chaine
    then trouver_id_prive ()
    else chaine
  in
  trouver_id_prive ()
  
class joueur_hors_jeu
    (creer_partie: int -> Bytes.t -> (int list * Bytes.t) Lwt.t)
    (* nombre de joueurs -> paramètre -> code, nom de la partie *)
    detruire_partie
    nom_reel : tjoueur_hors_jeu =
  object(self)
    val flux = Lwt_stream.create ()
    val id_prive = creer_id_prive ()
    val nom_reel = "\""^(Bytes.escaped nom_reel)^"\""
    val mutable attente_resultat_invitation = None
    val mutable fusible = None
    (* À réveiller si on veut annuler un timeout *)
    val mutable invitation_demandee = None
    initializer
      let () = 
        self#ajouter_evenement
          (Changement_nom nom_reel) in
      let verifier_hors_jeu _ j =
        if j#nom_reel = nom_reel then
          raise Nom_pris
      in
      let verifier_en_jeu _ j =
        if j#nom_reel = nom_reel then
          raise Nom_pris
      in
      let () = if Bytes.length nom_reel > 32 then
          raise Nom_trop_long in
      (* 32, en comptant les 2 guillemets et 8 caractères pour chaque caractère spécial... *)
      let () = if Bytes.length nom_reel < 3 then
          raise Nom_trop_court in
      let () = Hashtbl.iter (verifier_hors_jeu) joueurs_hors_jeu in
      let () = Hashtbl.iter (verifier_en_jeu) joueurs_en_jeu in
      (* On dit bonjour à tout le monde *)
      let () = Hashtbl.iter
          (fun _ autre_joueur ->
             let () =
               autre_joueur#ajouter_evenement
                 (Nouveau_joueur nom_reel) in
             self#ajouter_evenement
               (Nouveau_joueur (autre_joueur#nom_reel)))
          joueurs_hors_jeu in
      let () = Hashtbl.add joueurs_hors_jeu id_prive (self :> tjoueur_hors_jeu) in
      self#deconnecter_plus_tard
    method ajouter_evenement ev =
      (snd flux) (Some ev)
    method deconnecter =
      (* On annule les invitations *) 
      let i_invit liste =
        (* Cette fonction retourne soit None, soit ma position dans l'invitation liste *)
        let rec aux i = function
          | [] -> None
          | hd :: _ when hd = nom_reel -> Some i
          | _ :: tl -> aux (i + 1) tl
        in
        aux 0 liste
      in
      let () = Hashtbl.iter
          (fun _ j ->
             match j#invitation_demandee with
             | None -> ()
             | Some (liste, _) ->
               match i_invit liste with
               | None -> ()
               | Some i ->
                 j#invitation_refusee_par [i])
          joueurs_hors_jeu
      in
      (* J'annule ma propre invitation *)
      let () = self#annuler_invitation in
      (* On dit au revoir *)
      let () = 
        Hashtbl.iter
          (fun _ autre_joueur ->
             autre_joueur#ajouter_evenement
               (Depart_joueur nom_reel))
          joueurs_hors_jeu in
      let () = Hashtbl.remove joueurs_hors_jeu id_prive in
      Lwt.return ()
    method deconnecter_plus_tard =
      let () =
        match fusible with
        | None -> ()
        | Some fusible ->
          Lwt.wakeup fusible ()
      in
      let (annuler, f) = Lwt.wait () in
      let () = fusible <- Some f in
      let oui = 
        Lwt.bind
          (Lwt_unix.sleep max_delai)
          (fun () -> Lwt.return true) in
      let non = Lwt.bind annuler (fun () -> Lwt.return false) in
      let travailler () =
        Lwt.bind
          (Lwt.choose [oui ; non])
          (function
            | true -> self#deconnecter
            | false -> Lwt.return ())
      in
      Lwt.async (travailler)
    method private flux_evenements =
      fst flux
    method invitation_acceptee id_jeu numero ids_adversaires =
      (* je valide la procédure d'invitation : attente_resultat_invitation *)
      let () =
        match attente_resultat_invitation with
        | None -> failwith "Erreur : j'effectue une invitation, mais je n'attends pas le résultat"
        | Some x -> Lwt.wakeup x ()
      in
      (* je dis au revoir *)
      let () = 
        Hashtbl.iter
          (fun _ autre_joueur ->
             autre_joueur#ajouter_evenement
               (Depart_joueur nom_reel))
          joueurs_hors_jeu in
      (* je ne suis plus un joueur hors jeu *) 
      let () = Hashtbl.remove joueurs_hors_jeu id_prive in
      (* je crée un joueur en jeu *)
      let nouveau_joueur =
        new joueur_en_jeu detruire_partie id_prive nom_reel id_jeu numero ids_adversaires in
      ignore nouveau_joueur
    method invitation_compatible (autre_invitation: Bytes.t list * Bytes.t) =
      invitation_demandee = None
      || invitation_demandee = Some autre_invitation
    method invitation_invalide code =
      let () =
        match attente_resultat_invitation with
        | None -> ()
        | Some x ->
          Lwt.wakeup_exn x (Erreur_jeu code)
      in
      let () = attente_resultat_invitation <- None in
      invitation_demandee <- None
    method invitation_refusee_par i =
      let () =
        match attente_resultat_invitation with
        | None -> ()
        | Some x ->
          Lwt.wakeup_exn x (Non_invitable i) in
      let () = attente_resultat_invitation <- None in
      invitation_demandee <- None
    method annuler_invitation =
      let () = 
        match invitation_demandee with
        | None -> ()
        | Some (liste_joueurs, param) ->
          (* je suis présent dans cette invitation *)
          let rec trouver i = function
            | [] -> failwith "Ne peut arriver"
            | a :: _ when a = nom_reel -> i
            | _ :: b -> trouver (i + 1) b
          in
          let i = trouver 0 liste_joueurs in
          Hashtbl.iter
            (fun _ j ->
               if j#invitation_demandee = invitation_demandee
               (* Ce type partage mon invitation *)
               then j#invitation_refusee_par [i]
               else if List.exists
                   ((=) j#nom_reel) liste_joueurs
                   (* Il n'avait pas répondu à mon invitation *)
               then j#ajouter_evenement
                   (Invitation_annulee (liste_joueurs,
                                        param,
                                        nom_reel)))
            joueurs_hors_jeu in
      invitation_demandee <- None
    method id_prive = Bytes.copy id_prive
    method inviter (noms_joueurs, param) =
      (* On ne peut pas faire 2 invitations en même temps. *)
      let () = self#annuler_invitation in
      (* D'abord, je regarde si c'est valide : les joueurs existent (sinon : Non_invitable), je m'invite (sinon : Non_invite), ils sont tous différents (sinon : Doublon_invite). *)
      let joueurs =
        try
          Lwt.return
            (Array.mapi
              (fun i nom ->
                 let j = ref None in
                 let () = Hashtbl.iter
                     (fun _ joueur ->
                        if joueur#nom_reel = nom
                        then j := Some joueur)
                     joueurs_hors_jeu in
                 match !j with
                 | None -> raise (Non_invitable [i])
                 | Some j -> j)
              (Array.of_list noms_joueurs))
        with exc ->
          Lwt.fail exc
      in
      let joueurs =
        Lwt.bind joueurs
          (fun joueurs ->
             if List.exists (fun j -> j#id_prive = id_prive)
                 (Array.to_list joueurs)
             then Lwt.return joueurs
             else Lwt.fail Non_invite) in
      let joueurs =
        let rec doublon = function
          | [] -> false
          | a :: b when List.exists
                (fun b -> a#id_prive = b#id_prive)
                b -> false
          | _ :: b -> doublon b
        in
        Lwt.bind joueurs
          (fun joueurs ->
             let j = Array.to_list joueurs in
             if doublon j then
               Lwt.fail Doublon_invite
             else Lwt.return joueurs)
      in
      (* Ensuite, je regarde si l'invitation est compatible (invitation_compatible). *)
      let joueurs =
        Lwt.bind joueurs
          (fun joueurs ->
             let j = Array.mapi (fun a b -> (a, b)) joueurs in
             let j = Array.to_list j in
             let non_invitables =
               List.filter (fun (_, j) -> 
                   not
                     (j#invitation_compatible
                        (noms_joueurs,
                         param)))
                 j in
             let non_invitables = List.map (fst)
                 non_invitables in
             match non_invitables with
             | [] -> Lwt.return joueurs
             | liste -> Lwt.fail (Non_invitable liste))
      in
      (* Si non, je lève l'exception Non_invitable avec la liste des gens dont l'invitation est incompatible. *)
      (* Si oui, j'envoie une Nouvelle_invitation à tous les gens de mon invitation. Si tous partagent mon invitation, alors je crée une nouvelle partie (creer_partie). *)
      Lwt.bind joueurs
        (fun joueurs ->
           let () = invitation_demandee <-
               Some (noms_joueurs, param) in
           let () = Array.iter
               (fun j -> j#ajouter_evenement
                   (Nouvelle_invitation (noms_joueurs, param)))
               joueurs in
           let non_prets =
             List.filter
               (fun j -> j#invitation_demandee = None)
               (Array.to_list joueurs) in
           let (fin, res) = Lwt.wait () in
           let () = attente_resultat_invitation
             <- Some res in
           let () =
             if non_prets = [] then
               Lwt.async
                 (fun () ->
                    Lwt.bind
                      (creer_partie (Array.length joueurs) param)
                      (function
                        | (code, "") ->
                          let () = Array.iter
                              (fun j -> j#invitation_invalide code)
                              joueurs in
                          Lwt.return ()
                        | (_, id_partie) ->
                          let ids_adversaires = Array.map
                              (fun j -> j#id_prive)
                              joueurs in
                          let () = Array.iteri
                              (fun i j ->
                                 j#invitation_acceptee
                                   id_partie i
                                   ids_adversaires)
                              joueurs in
                          Lwt.return ()))
           in
           fin)                     
      (* Si ça marche, chaque joueur reçoit (invitation_acceptee). *)
      (* Si ça ne marche pas, on termine les invitations avec Erreur_jeu. *) 
    method invitation_demandee = invitation_demandee
    method nom_reel = Bytes.copy nom_reel
    method peek =
      Lwt.bind
        (Lwt_stream.peek (self#flux_evenements))
        (function | None -> Lwt.fail Pas_de_message
                  | Some msg -> Lwt.return msg)
    method pop =
      match Lwt_stream.get_available_up_to 1
              (self#flux_evenements) with
      | [] -> Lwt.fail Pas_de_message
      | [_] -> Lwt.return ()
      | _ -> failwith "Ne peut arriver (up to)"
    method recapituler =
      (* On détruit ce qui n'est pas encore lu *)
      let _ = Lwt_stream.get_available (self#flux_evenements) in
      let (flux, pousser) = Lwt_stream.create () in
      let ev x = pousser (Some x) in
      let terminer () = pousser None in
      ev (Changement_nom nom_reel);
      Hashtbl.iter
        (fun _ j ->
           ev (Nouveau_joueur (j#nom_reel)))
        joueurs_hors_jeu;
      Hashtbl.iter
        (fun _ j ->
           match j#invitation_demandee with
           | Some (liste, param)
             when List.exists
                 ((=) nom_reel) liste ->
             ev (Nouvelle_invitation (liste, param));
           | _ -> ())
        joueurs_hors_jeu;
      terminer ();
      flux
  end
