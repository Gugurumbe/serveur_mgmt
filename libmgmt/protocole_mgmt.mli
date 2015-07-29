val traiter_requete:
  Bytes.t -> (* le mdp admin *)
  Unix.sockaddr -> (* le backend *)
  Bytes.t Lwt_stream.t -> (* la requête *)
  Bytes.t Lwt_stream.t (* la réponse *)
