type evenement =
  | Changement_nom of bytes (* La version échappée du nom demandé *)
  | Nouveau_joueur of bytes
  | Depart_joueur of bytes
  | Nouvelle_invitation of (bytes list * bytes)
  (* Les noms des gens (dont moi, tous uniques) x les arguments supplémentaires *)
  | Invitation_annulee of (bytes list * bytes * bytes)
  (* L'invitation en question x arguments x le nom du type qui a refusé *)

val nouveau_joueur:
  (int -> bytes -> (int list * bytes) Lwt.t) ->
  (* fonction pour créer une partie *)
  (bytes -> int list Lwt.t) ->
  (* fonction pour détruire une partie *)
  bytes ->
  (* le nom du type *)
  (int list * bytes) Lwt.t
(* Code d'erreur, id privé *)

val peek_msg: bytes -> (* L'id privé *)
  (int list * evenement option) Lwt.t
(* code, evenement *)

val pop_msg: bytes -> int list Lwt.t

val inviter: bytes -> bytes list (* les types *) -> bytes ->
  int list Lwt.t
(* Retourne probablement dans longtemps. Conseil : si la socket se déconnecte, ou timeout, appeler annuler_invitation. Ce n'est pas une requête. *)

val annuler_invitation: bytes -> unit
(* Ne lève pas d'exception, même si le joueur est déjà en jeu. Ne fait rien par défaut. Si une invitation est en cours, cette fonction la fait échouer. *)

val deconnecter: bytes -> int list Lwt.t
(* Au cas où le client veuille se déconnecter de lui-même, avant le timeout de 10 minutes *)

val recapituler: bytes -> (int list * evenement Lwt_stream.t) Lwt.t
(* Supprime tous les messages en attente et fait comme si le client venait d'arriver. Il faut d'abord regarder les messages du flux puis revenir à peek et pop. *)

type resultat_traitement =
  | Erreur of int list
  | Succes of bytes Lwt_stream.t

val traiter_requete_jeu: bytes -> bytes Lwt_stream.t -> resultat_traitement 
(* Pour appeler cette fonction, il faut avoir déjà jeté un œil à la requête, pour savoir que c'est une requête de jeu (ligne 1) et pour connaître l'ID privé du joueur (ligne 3). *)

val traiter_reponse_jeu: bytes -> bytes Lwt_stream.t -> resultat_traitement
(* Il faut se souvenir de l'ID privé du joueur qui a fait la requête, ici. *)
