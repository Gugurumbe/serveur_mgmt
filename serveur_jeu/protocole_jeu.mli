val traiter_requete:
  Bytes.t -> (* identifiant administrateur *)
  Bytes.t Lwt_stream.t -> (* Lignes de la requête *)
  Bytes.t Lwt_stream.t (* Lignes de la réponse *)
