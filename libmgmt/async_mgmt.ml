let pool = ref []

let nettoyer () =
  pool :=
    List.filter
      (fun t ->
         Lwt.state t = Lwt.Sleep)
      !pool

let async f =
  let fin = Lwt.try_bind
      f
      (fun _ -> Lwt_unix.yield ())
      (fun _ -> Lwt_unix.yield ()) in
  let () = pool := (fin) :: (!pool) in
  let () = nettoyer () in
  ()
  
