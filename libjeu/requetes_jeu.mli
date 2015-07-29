(* Aucune de ces fonctions ne lève d'exception. Elles retournent plutôt un code d'erreur. Si une exception non attendue se produit, on génère une erreur interne. On l'affiche dans l'erreur standard *)

(* [0] : OK *)
(* [1 ; x] : l'entier x ne fait pas référence à un joueur *)
(* [2 ; n] : on ne peut pas mettre n joueurs, c'est trop peu. Minimum : 2 *)
(* [3 ; n] : on ne peut pas mettre n joueurs, c'est trop. Maximum : 8 *)
(* [4 ; n] : on ne peut pas faire un plateau à n lignes ou n colonnes, c'est trop peu. Minimum : 2. *)
(* [5 ; n] : on ne peut pas faire un plateau à n lignes ou n colonnes, c'est trop. Maximum : 16. *)
(* [6 ; n] : on ne peut pas gagner en alignant n symboles, c'est trop peu. Minimum : 3. *)
(* [7 ; n] : on ne peut pas gagner en alignant n symboles, c'est trop. Maximum : la dimension minimale du plateau. *)
(* [8 ; x ; y] : on ne peut pas faire un coup en (x, y), c'est hors zone. *)
(* [9 ; x ; y] : on ne peut pas faire un coup en (x, y), car c'est déjà pris par j. *)
(* [10 ; moi ; lui] : ce n'est pas à "moi" de jouer, c'est à "lui". *)
(* [11] : il n'y a pas de message à récupérer (je suis déconnecté) ou à supprimer. *)
(* [12] : erreur interne du serveur, regarder la sortie standard. *)

type evenement =
  | Changement_nombre_joueurs of int
  | Changement_numero of int
  | Changement_taille_plateau of (int * int) (* x - y *)
  | Changement_taille_gagnante of int
  | Coup of (int * int * int) (* x - y - joueur *)
  | Changement_tour of int
  | Fin_partie of int (* gagnant *)

val recapituler:
  int -> (* ID de la partie *)
  int -> (* numéro de joueur *)
  int list Lwt.t (* Supprime tous les messages en attente, et répète tous ceux survenus depuis le début, sans répéter à chaque fois le changement de tour *)

val creer_partie:
  int -> (* nombre de joueurs *)
  int -> (* largeur du plateau *)
  int -> (* hauteur du plateau *)
  int -> (* nombre d'alignements nécessaires pour gagner *)
  (int list * int option) Lwt.t
(* code d'erreur, id de partie créé *)

val detruire_partie:
  int -> (* Le numéro de la partie *)
  int list Lwt.t (* code d'erreur *)

val peek_msg:
  int -> (* le numéro de la partie *)
  int -> (* le numéro du joueur *)
  (int list * evenement option) Lwt.t
(* le code d'erreur, le message suivant *)

val pop_msg:
  int -> (* numéro de partie *)
  int -> (* numéro de joueur *)
  int list Lwt.t (* code d'erreur *)
(* retourne immédiatement, et peek_msg regardera le prochain message. on peut toujours retrouver l'état du jeu en faisant recapituler. *)

val jouer:
  int ->
  int ->
  int -> (* x *)
  int -> (* y *)
  int list Lwt.t
