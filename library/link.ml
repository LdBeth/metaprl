(*
 * Link module: provides nuprl/nuprl-light link functionality
 *)
open Lm_debug

open Refiner.Refiner.Term
open Refiner.Refiner.TermType
open MathBus
open SocketIo
open Mbterm
open Nuprl5

let _ =
   show_loading "Loading LinkMini%t"

type sockopt = Fd of Unix.file_descr | Null of unit

type link = (in_channel * out_channel) * sockopt

let dest_link ((in_channel, out_channel), socket) =
  match socket with Fd fd -> ((in_channel, out_channel), (Fd fd))
  | Null _ -> ((in_channel, out_channel), (Null ()))

let iconnect_term port host =
  mk_term (mk_op nuprl5_opname
	     [(make_param (Token "!connect")); (make_param (Number (Lm_num.num_of_int port)));
	       (make_param (String host))])
    []

let idisconnect_term error_p =
  mk_term (mk_op nuprl5_opname
	     [(make_param (Token "!disconnect"));
	       (make_param (ParamList [(make_param (Token "bool"));
					(make_param (Number (Lm_num.num_of_int (if error_p then 1 else 0))))]))]) []

let cautious_in = ref Unix.stderr
let cautious_out = ref Unix.stderr
let cautious_socket = ref Unix.stderr

let disconnect link =
 (*send link disconnect_term at orb level*)
  let ((in_channel, out_channel), socket) = dest_link link in
  close_client (in_channel, out_channel);
  match socket with Fd fd -> destroy_socket fd
  | Null _ -> ()

(*
let rec recv ((in_channel, out_channel), socket) =
	try (term_of_mbterm (read_node in_channel)) with
	Unix.Unix_error (e, s1, s2) when ((e = Unix.EWOULDBLOCK) or (e = Unix.EAGAIN)) ->
	recv ((in_channel, out_channel), socket)
*)

let recv ((in_channel, out_channel), socket) =
  Unix.clear_nonblock (Unix.descr_of_in_channel in_channel);
  print_string "in recv";
  let term = term_of_mbterm (read_node in_channel)
  in
  Unix.set_nonblock (Unix.descr_of_in_channel in_channel);

  (* for debugging *)
  print_newline();
  print_string "recv";
  print_newline();
  Mbterm.print_term term;

  term

(* LAL set nonblocking before/after nohang instead*)

(* opens local_socket, connects to remote, returns Unidirectional link*)
let connect_with_callback host port =
  let (in_channel, out_channel) = open_client port host
  in
  (* cautious_in := Unix.descr_of_in_channel in_channel;
  cautious_out := Unix.descr_of_out_channel out_channel;*)
  (*Unix.set_nonblock (Unix.descr_of_in_channel in_channel);*)
  ((in_channel, out_channel), (Null ()))

(* called after port-term sent and returned successful*)
(* let connect_callback link =
  let ((in_channel, out_channel), socket) = dest_link link
  in
  let descr = accept_client socket
  in
  cautious_in := descr;
  Unix.set_nonblock descr;
  (((Unix.in_channel_of_descr descr), out_channel), socket)
*)

let send ((in_channel, out_channel), socket) term =
  Basic.debug_term := term;

  (* for debugging *)
  print_newline();
  print_string "send";
  print_newline();
  Mbterm.print_term term;

  write_node (mbterm_of_term term) out_channel;
  let rec tf () =
   try flush out_channel with
   Sys_error e -> (Unix.sleep 1; tf ()) in
   tf ()

(*
let recv_nohang ((in_channel, out_channel), socket) =
	 try (Some (term_of_mbterm (read_node in_channel))) with
	 Unix.Unix_error (e, s1, s2) when ((e = Unix.EWOULDBLOCK) or (e = Unix.EAGAIN)) -> None*)

let recv_nohang ((in_channel, out_channel), socket) =
  let {Unix.st_dev = x; Unix.st_ino = y; Unix.st_kind = z;
	Unix.st_perm = o; Unix.st_nlink = p; Unix.st_uid = q; Unix.st_gid = r;
	Unix.st_rdev = s; Unix.st_size = size; Unix.st_atime = m;
	Unix.st_mtime = n; Unix.st_ctime = f} =
    Unix.fstat (Unix.descr_of_in_channel in_channel)
  in
  if size = 0 then None else (Some (term_of_mbterm (read_node in_channel)))
