type connexion =
  | Non_specifiee
  | Unix of bytes
  | Inet of Unix.inet_addr
  | Inet4_any
  | Inet6_any
  | Inet4_loopback
  | Inet6_loopback

exception Invalid_command_line

let connexion = ref Non_specifiee

let port = ref None

let id_admin = ref None

let set_sockaddr sockaddr =
  match !connexion with
  | Non_specifiee ->
    connexion := sockaddr
  | _ ->
    raise Invalid_command_line

let set_port nouveau_port =
  match !port with
  | None -> port := Some nouveau_port
  | Some _ ->
    raise Invalid_command_line

let set_id_admin id =
  match !id_admin with
  | None -> id_admin := Some id
  | Some _ ->
    raise Invalid_command_line 

let speclist =
  [("-unix",
    Arg.String (fun s -> set_sockaddr (Unix s)),
    "Sets the sockaddr to type unix, for a given filename.");
   ("-inet",
    Arg.String (fun s -> set_sockaddr
               (Inet
                  (try Unix.inet_addr_of_string s
                   with _ -> raise Invalid_command_line))),
    "Sets the sockaddr to type inet. The inet address can either be an IPV4 address or an IPV6 address.");
   ("-inet4-any",
    Arg.Unit (fun () -> set_sockaddr (Inet4_any)),
    "Sets the sockaddr to type inet. The server will listen to any IPV4 address.");
   ("-inet6-any",
    Arg.Unit (fun () -> set_sockaddr (Inet6_any)),
    "Sets the sockaddr to type inet. The server will listen to any IPV6 address.");
   ("-inet4-loopback",
    Arg.Unit (fun () -> set_sockaddr (Inet4_loopback)),
    "Sets the sockaddr to type inet. The server will listen to the loopback IPV4 address.");
   ("-inet6-loopback",
    Arg.Unit (fun () -> set_sockaddr (Inet6_loopback)),
    "Sets the sockaddr to type inet. The server will listen to the loopback IPV6 address.");
   ("-port",
    Arg.Int (set_port),
    "Sets the listening port. Only works with inet addresses.");
   ("-admin",
    Arg.String (fun s -> set_id_admin s),
    "Sets the admin ID. Defaults to the content of $MDP_ADMIN. The admin ID is compulsory.")]

let verifier () =
  match !connexion, !port, !id_admin with
  | (Non_specifiee, _, _) ->
    let () = Printf.eprintf "The sockaddr is not set.\n%!" in
    raise Invalid_command_line
  | (Unix _, Some _, _) ->
    let () = Printf.eprintf "The port number doesn't make sense for a unix socket.\n%!" in
    raise Invalid_command_line
  | (Inet _, None, _)
  | (Inet4_any, None, _)
  | (Inet6_any, None, _)
  | (Inet4_loopback, None, _) 
  | (Inet6_loopback, None, _) ->
    let () = Printf.eprintf "The port number is not set.\n%!" in
    raise Invalid_command_line
  | (_, _, None) ->
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
      "A multipurpose networking game server. It must be given an address and an admin id to work. Available options: ";
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

let sockaddr =
  match (!connexion, !port) with
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

let sockaddr () =
  match sockaddr with
  | Unix.ADDR_UNIX fname ->
    Unix.ADDR_UNIX (Bytes.copy fname)
  | inet -> inet
