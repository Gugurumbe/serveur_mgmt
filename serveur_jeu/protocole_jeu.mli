val traiter_requete:
  bytes -> (* identifiant administrateur *)
  bytes Lwt_stream.t -> (* Lignes de la requête *)
  bytes Lwt_stream.t (* Lignes de la réponse *)
