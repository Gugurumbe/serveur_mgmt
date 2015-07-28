type connexion =
  | Non_specifiee
  | Unix of Bytes.t
  | Inet of Unix.inet_addr
  | Inet4_any
  | Inet6_any
  | Inet4_loopback
  | Inet6_loopback

exception Invalid_command_line

let connexion_frontend = ref Non_specifiee

let port_frontend = ref None
    
let connexion_backend = ref Non_specifiee

let port_backend = ref None

let id_admin = ref None

let set_frontend sockaddr =
  match !connexion_frontend with
  | Non_specifiee ->
    connexion_frontend := sockaddr
  | _ ->
    raise Invalid_command_line

let set_port_frontend nouveau_port =
  match !port_frontend with
  | None -> port_frontend := Some nouveau_port
  | Some _ ->
    raise Invalid_command_line

let set_backend sockaddr =
  match !connexion_backend with
  | Non_specifiee ->
    connexion_backend := sockaddr
  | _ ->
    raise Invalid_command_line

let set_port_backend nouveau_port =
  match !port_backend with
  | None -> port_backend := Some nouveau_port
  | Some _ ->
    raise Invalid_command_line

let set_id_admin id =
  match !id_admin with
  | None -> id_admin := Some id
  | Some _ ->
    raise Invalid_command_line 

let speclist =
  [("-funix",
    Arg.String (fun s -> set_frontend (Unix s)),
    "Sets the frontend sockaddr to type unix, for a given filename.");
   ("-finet",
    Arg.String (fun s -> set_frontend
               (Inet
                  (try Unix.inet_addr_of_string s
                   with _ -> raise Invalid_command_line))),
    "Sets the frontend sockaddr to type inet. The inet address can either be an IPV4 address or an IPV6 address.");
   ("-finet4-any",
    Arg.Unit (fun () -> set_frontend (Inet4_any)),
    "Sets the frontend sockaddr to type inet. The server will listen to any IPV4 address.");
   ("-finet6-any",
    Arg.Unit (fun () -> set_frontend (Inet6_any)),
    "Sets the frontend sockaddr to type inet. The server will listen to any IPV6 address.");
   ("-finet4-loopback",
    Arg.Unit (fun () -> set_frontend (Inet4_loopback)),
    "Sets the frontend sockaddr to type inet. The server will listen to the loopback IPV4 address.");
   ("-finet6-loopback",
    Arg.Unit (fun () -> set_frontend (Inet6_loopback)),
    "Sets the frontend sockaddr to type inet. The server will listen to the loopback IPV6 address.");
   ("-fport",
    Arg.Int (set_port_frontend),
    "Sets the listening port. Only works with inet addresses.");
   ("-bunix",
    Arg.String (fun s -> set_backend (Unix s)),
    "Sets the backend sockaddr to type unix, for a given filename. The server will connect to this address for game requests.");
   ("-binet",
    Arg.String (fun s -> set_backend
               (Inet
                  (try Unix.inet_addr_of_string s
                   with _ -> raise Invalid_command_line))),
    "Sets the backend sockaddr to type inet. The inet address can either be an IPV4 address or an IPV6 address. The server will connect to this address for game requests. The server doesn't lookup names.");
   ("-bport",
    Arg.Int (set_port_backend),
    "Sets the connection port. Only works with inet addresses.");
   ("-admin",
    Arg.String (fun s -> set_id_admin s),
    "Sets the admin ID. Defaults to the content of $MDP_ADMIN. The admin ID is compulsory.")]

let verifier () =
  match !connexion_frontend, !port_frontend,
        !connexion_backend, !port_backend,
        !id_admin with
  | (Non_specifiee, _, _, _, _)
  | (_, _, Non_specifiee, _, _) ->
    let () = Printf.eprintf "The sockaddr is not set.\n%!" in
    raise Invalid_command_line
  | (Unix _, Some _, _, _, _)
  | (_, _, Unix _, Some _, _) ->
    let () = Printf.eprintf "The port number doesn't make sense for a unix socket.\n%!" in
    raise Invalid_command_line
  | (Inet _, None, _, _, _)
  | (Inet4_any, None, _, _, _)
  | (Inet6_any, None, _, _, _)
  | (Inet4_loopback, None, _, _, _) 
  | (Inet6_loopback, None, _, _, _)
  | (_, _, Inet _, None, _)
  | (_, _, Inet4_any, None, _)
  | (_, _, Inet6_any, None, _)
  | (_, _, Inet4_loopback, None, _) 
  | (_, _, Inet6_loopback, None, _) ->
    let () = Printf.eprintf "The port number is not set.\n%!" in
    raise Invalid_command_line
  | (_, _, _, _, None) ->
    begin
      try id_admin := Some (Sys.getenv "MDP_ADMIN")
      with Not_found ->
        let () = Printf.eprintf "The admin ID is not set.\n%!" in
        raise Invalid_command_line
    end
  | _ -> ()

let () =
  try
    Arg.parse
      speclist
      (fun str -> Printf.eprintf "Warning: unused argument %s.\n%!" str)
      "A multipurpose networking front-end to game servers. It must be given two addresses : a listening front-end address and a back-end address for game requests, and an admin id to work. Available options: ";
    verifier ()
  with Invalid_command_line ->
    let () =
      Printf.eprintf "The options you gave don't make sense. Please try --help.\n%!" in
    exit 1

let id_admin =
  match !id_admin with
  | None -> failwith "Impossible"
  | Some i -> i

let id_admin () = Bytes.copy id_admin

let sockaddr_frontend =
  match (!connexion_frontend, !port_frontend) with
  | (Unix fname, _) ->
    Unix.ADDR_UNIX fname
  | (Inet addr, Some port) ->
    Unix.ADDR_INET (addr, port)
  | (Inet4_any, Some port) ->
    Unix.ADDR_INET (Unix.inet_addr_any, port)
  | (Inet6_any, Some port) ->
    Unix.ADDR_INET (Unix.inet6_addr_any, port)
  | (Inet4_loopback, Some port) ->
    Unix.ADDR_INET (Unix.inet_addr_loopback, port)
  | (Inet6_loopback, Some port) ->
    Unix.ADDR_INET (Unix.inet6_addr_loopback, port)
  | _ -> failwith "Ne peut arriver, j'ai vérifié ()"

let sockaddr_backend =
  match (!connexion_backend, !port_backend) with
  | (Unix fname, _) ->
    Unix.ADDR_UNIX fname
  | (Inet addr, Some port) ->
    Unix.ADDR_INET (addr, port)
  | (Inet4_any, Some port) ->
    Unix.ADDR_INET (Unix.inet_addr_any, port)
  | (Inet6_any, Some port) ->
    Unix.ADDR_INET (Unix.inet6_addr_any, port)
  | (Inet4_loopback, Some port) ->
    Unix.ADDR_INET (Unix.inet_addr_loopback, port)
  | (Inet6_loopback, Some port) ->
    Unix.ADDR_INET (Unix.inet6_addr_loopback, port)
  | _ -> failwith "Ne peut arriver, j'ai vérifié ()"

let frontend () =
  match sockaddr_frontend with
  | Unix.ADDR_UNIX fname ->
    Unix.ADDR_UNIX (Bytes.copy fname)
  | inet -> inet

let backend () =
  match sockaddr_backend with
  | Unix.ADDR_UNIX fname ->
    Unix.ADDR_UNIX (Bytes.copy fname)
  | inet -> inet
