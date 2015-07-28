type evenement =
  | Changement_nom of Bytes.t (* La version échappée du nom demandé *)
  | Nouveau_joueur of Bytes.t
  | Depart_joueur of Bytes.t
  | Nouvelle_invitation of (Bytes.t list * Bytes.t)
  (* Les noms des gens (dont moi, tous uniques) x les arguments supplémentaires *)
  | Invitation_annulee of (Bytes.t list * Bytes.t * Bytes.t)
  (* L'invitation en question x arguments x le nom du type qui a refusé *)

val nouveau_joueur:
  (int -> Bytes.t -> (int list * Bytes.t) Lwt.t) ->
  (* fonction pour créer une partie *)
  (Bytes.t -> int list Lwt.t) ->
  (* fonction pour détruire une partie *)
  Bytes.t ->
  (* le nom du type *)
  (int list * Bytes.t) Lwt.t
(* Code d'erreur, id privé *)

val peek_msg: Bytes.t -> (* L'id privé *)
  (int list * evenement option) Lwt.t
(* code, evenement *)

val pop_msg: Bytes.t -> int list Lwt.t

val inviter: Bytes.t -> Bytes.t list (* les types *) -> Bytes.t ->
  int list Lwt.t
(* Retourne probablement dans longtemps. Conseil : si la socket se déconnecte, ou timeout, appeler annuler_invitation. Ce n'est pas une requête. *)

val annuler_invitation: Bytes.t -> unit
(* Ne lève pas d'exception, même si le joueur est déjà en jeu. Ne fait rien par défaut. Si une invitation est en cours, cette fonction la fait échouer. *)

val deconnecter: Bytes.t -> int list Lwt.t
(* Au cas où le client veuille se déconnecter de lui-même, avant le timeout de 10 minutes *)

val recapituler: Bytes.t -> (int list * evenement Lwt_stream.t) Lwt.t
(* Supprime tous les messages en attente et fait comme si le client venait d'arriver. Il faut d'abord regarder les messages du flux puis revenir à peek et pop. *)

type resultat_traitement =
  | Erreur of int list
  | Succes of Bytes.t Lwt_stream.t

val traiter_requete_jeu: Bytes.t -> Bytes.t Lwt_stream.t -> resultat_traitement 
(* Pour appeler cette fonction, il faut avoir déjà jeté un œil à la requête, pour savoir que c'est une requête de jeu (ligne 1) et pour connaître l'ID privé du joueur (ligne 3). *)

val traiter_reponse_jeu: Bytes.t -> Bytes.t Lwt_stream.t -> resultat_traitement
(* Il faut se souvenir de l'ID privé du joueur qui a fait la requête, ici. *)
