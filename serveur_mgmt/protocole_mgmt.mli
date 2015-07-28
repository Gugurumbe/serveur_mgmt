val traiter_requete:
  bytes -> (* le mdp admin *)
  Unix.sockaddr -> (* le backend *)
  bytes Lwt_stream.t -> (* la requête *)
  bytes Lwt_stream.t (* la réponse *)
