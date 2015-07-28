(* 20 s de timeout : au bout de 20s, on ferme artificiellement la socket. *)

let travailler (chan_in, chan_out) =
  let requete = Lwt_io.read_lines chan_in in
  let reponse = Protocole_jeu.traiter_requete (Config_jeu.id_admin ()) requete in
  let fin = Lwt_io.write_lines chan_out reponse in
  let timeout = Lwt_unix.sleep 20. in
  let fermer_socket () =
    let open Lwt in
    (Lwt_unix.yield ())
    >>= (fun () -> try_bind
            (fun () -> Lwt_io.close chan_out)
            (fun () -> return ())
            (fun _ -> return ()))
    >>= (fun () -> try_bind
            (fun () -> Lwt_io.close chan_in)
            (fun () -> return ())
            (fun _ -> return ()))
  in
  let fin = Lwt.bind fin fermer_socket in
  let timeout = Lwt.bind timeout fermer_socket in
  let fin () = Lwt.choose [fin; timeout] in
  Async_jeu.async (fin)

let serveur = Lwt_io.establish_server (Config_jeu.sockaddr ()) (travailler)

let () = Lwt_main.run (fst (Lwt.wait ()))
