open Printf
open Debug

let _ =
   if !debug_load then
      eprintf "Loading SocketIo%t" eflush


let open_client port host = 
  let {Unix.h_name = name; Unix.h_aliases = a; Unix.h_addrtype = atype; Unix.h_addr_list = l} =
    Unix.gethostbyname host
  in
  Unix.open_connection (Unix.ADDR_INET (l.(0), port)) 

(*
let open_client port host = Unix.open_connection (Unix.ADDR_INET(Unix.inet_addr_of_string "128.84.254.214", port))
*)
let close_client (in_channel, out_channel) = 
	 (close_in  in_channel ;
	 close_out out_channel )

let make_socket port = 
  let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 and
      {Unix.h_name = name; Unix.h_aliases = a; Unix.h_addrtype = atype; Unix.h_addr_list = l} =
    Unix.gethostbyname (Unix.gethostname ())
  in
  let address = Unix.ADDR_INET (l.(0), port)
  in
  Unix.bind fd address ;
  Unix.listen fd 1;
  fd

(*
let make_socket port host = 
	let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0
	in  let address = Unix.ADDR_INET (Unix.inet_addr_of_string "128.84.254.214",  port)
	in Unix.bind fd address ;
	Unix.listen fd 1;
	fd;;
*)

let destroy_socket fd = Unix.shutdown fd Unix.SHUTDOWN_ALL ;;

let accept_client fd = 
  let (fd2, addr) = Unix.accept fd
  in
  fd2 
