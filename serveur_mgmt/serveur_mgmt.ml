(* 20 s de timeout : au bout de 20s, on ferme artificiellement la socket. *)

let travailler (chan_in, chan_out) =
  let requete = Lwt_io.read_lines chan_in in
  let reponse = Protocole_mgmt.traiter_requete (Config_mgmt.id_admin ()) (Config_mgmt.backend ()) requete in
  let () = Async_mgmt.async (fun () -> Lwt_io.write_lines chan_out reponse) in
  let (fin, rev_fin) = Lwt.wait () in
  let () = Lwt_stream.on_terminate reponse
      (fun () ->
         (* let () = Printf.printf "Fin de la réponse.\n%!" in *)
         Lwt.wakeup rev_fin ()) in
  let timeout = Lwt_unix.sleep 20. in
  let close_in () =
    (* let () = Printf.printf "Je ferme l'entrée...\n%!" in *)
    Lwt.catch
      (fun () ->
         Lwt_io.close chan_in)
      (fun exc ->
         Lwt.return ())
  in
  let close_out () =
    (* let () = Printf.printf "Je ferme la sortie...\n%!" in *)
    Lwt.catch
      (fun () ->
         Lwt_io.close chan_out)
      (fun exc -> 
         Lwt.return ())
  in
  let fermer_socket () =
    let open Lwt in
    (Lwt_unix.yield ())
    >>= (close_out)
    >>= (close_in)
  in
  let fin () = Lwt.bind fin fermer_socket in
  let timeout () = Lwt.bind timeout fermer_socket in
  let fin () = Lwt.choose [fin (); timeout ()] in
  Async_mgmt.async (fin)

let serveur = Lwt_io.establish_server (Config_mgmt.frontend ()) (travailler)

let () = Lwt_main.run (fst (Lwt.wait ()))
