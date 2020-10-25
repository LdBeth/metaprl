(*
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/htmlman/default.html or visit http://metaprl.org/
 * for more information.
 *
 * Copyright (C) 1998 Lori Lorigo, Richard Eaton, Cornell University
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * Authors: Lori Lorigo, Richard Eaton
 *)

open Lm_debug

open Unix
open List
open Lm_num
open Term_sig
open Refiner.Refiner.Term
open Refiner.Refiner.TermType
open Nuprl5
open Opname

let _ =
   show_loading "Loading Basic%t"

let inteq i j = (i = j)
let nullp l = l = []
let chareq a b = (a = b)
let stringeq a b = (a = b)

let listeq p a b =
   (List.length a = List.length b)
   && try for_all2 p a b with _ -> false

let rec parmeq p q =

 match dest_param p, dest_param q with

    Number pn, Number qn	-> eq_num pn qn
  | ParamList pl, ParamList ql	-> listeq parmeq pl ql
  | ObId poid, ObId qoid	-> listeq parmeq poid qoid

  |_ -> p = q

let oideq = listeq parmeq

let opeq a b =
 match dest_op a with
  	{ op_name = aopname;
	  op_params = aparms }
  -> (match dest_op b with
  	{ op_name = bopname;
	  op_params = bparms }
	-> (aopname = bopname && listeq parmeq aparms bparms))

open Hashtbl

(*	TODO PERF
	these maps are unfortunate as we will be consing at every lookup
 *)
let rec parmhash p =

 match dest_param p with

    Number pn	-> hash (string_of_num pn)
  | ParamList pl	-> hash (map parmhash pl)
  | ObId poid	-> hash (map parmhash poid)

  | _ -> hash p

(*
 * common terms
 *)
let fdl_opname = mk_opname "FDL" nil_opname
let token s = Token (mk_opname s fdl_opname)

let imessage_parameter = make_param (token "!message")
let imessage_op parms = mk_nuprl5_op (imessage_parameter :: parms)

(* !natural{n} *)
let inatural_parameter = make_param (token "!natural")
let inatural_op p = mk_nuprl5_op [inatural_parameter; p]
let inatural_term i = mk_term (inatural_op (make_param (Number (Lm_num.num_of_int i)))) []

(* !token{t} *)
let itoken_parameter = make_param (token "!token")
let itoken_op p =  mk_nuprl5_op [itoken_parameter; p]
let itoken_term s = mk_term (itoken_op (make_param (token s))) []

(* !string{s} *)
let istring_parameter = make_param (token "!string")
let istring_op p =  mk_nuprl5_op [istring_parameter; p]
let istring_term s = mk_term (istring_op (make_param (String s))) []

(* !text{s} *)
let itext_parameter = make_param (token "!text")
let itext_op p = mk_nuprl5_op [itext_parameter; p]
let itext_term s = mk_term (itext_op (make_param (String s))) []

let ioid_parameter = make_param (token "!oid")
let ioid_op p = mk_nuprl5_op [make_param (token "!oid"); p]
let ioid_term o = mk_term (ioid_op (make_param (ObId o))) []

let inil_parameter =
	make_param (ParamList [(make_param (token "bool"));
			       (make_param (Number (Lm_num.num_of_int 1)))])

let iterm_op = mk_nuprl5_op [make_param (token "!term")]

let imessage_term sl ol tl =
  mk_term
    (imessage_op
      ( (map (function s -> make_param (token s)) sl)
      @ (map (function o -> make_param (ObId o)) ol)
      ))
    (map (function t -> mk_bterm [] t) tl)

let iterm_term t = mk_term iterm_op [mk_bterm [] t]
let iterm_bterms bterms = mk_term iterm_op bterms
let ivoid_op = (mk_nuprl5_op [make_param (token "!void")])
let ivoid_term = mk_term ivoid_op []

let ivoid_term_p t =
  match Lib_term.dest_term t with
  { term_op = op; term_terms = []} when opeq op ivoid_op
     -> true
  |_ -> false

(*
 * failure
 *)

exception Nuprl5_Exception of (string * term)

let error sl oids tl =
  print_string (String.concat " " sl);
  print_newline();
  List.iter Mbterm.print_term tl;

  raise (Nuprl5_Exception ((String.concat " " sl),(imessage_term ("MetaprlLibrary" :: sl) oids tl)))

let special_error_handler body handler =
  try body ()
  with
    Nuprl5_Exception (s,t) -> handler s t

let error_handler body handler =
  try body ()
  with
    Nuprl5_Exception (s,t) -> handler t

let unconditional_error_handler body handler =
  try body ()
  with
    Nuprl5_Exception (s,t) -> handler t
  | _ ->
      ( (); (* TODO dump error to stdout *)
       handler (itext_term "Unexpected Metaprl failure"))

let unwind_error body unwind =
  try body ()
  with
    e -> (unwind ()); raise e

let parameters_of_term t =
  match Lib_term.dest_term t with
    { term_op = op; term_terms = _}
    -> match dest_op op with
      { op_name = _; op_params = parms } -> parms

let operator_of_term t =
  match Lib_term.dest_term t with
    { term_op = op; term_terms = _} -> op

let bound_terms_of_term t =
  match Lib_term.dest_term t with
    { term_op = _; term_terms = bterms} -> bterms

let term_of_unbound_term bterm =
  match dest_bterm bterm with
    { bvars = []; bterm = t } -> t
  | _ -> error ["unbound"; "bound"] [] [(mk_term iterm_op [bterm])]

let unbound_bterm_p bterm =
  match dest_bterm bterm with
    { bvars = []; bterm = _ } -> true
  | _ -> false

let parameter_of_carrier p t =
  match Lib_term.dest_term t with
    { term_op = o; term_terms = []}
    -> (match dest_op o with
      { op_name = opname; op_params = [p'; c] } when (parmeq p p' && nuprl5_opname_p opname)
      -> c
    |_ -> error ["term"; "carrier"; "op"] [] [t; mk_term (mk_nuprl5_op [p]) []])
  |_ -> error ["term"; "carrier"; "subterms"] [] [t]

let parameters_of_carrier p t =
  match Lib_term.dest_term t with
    { term_op = o; term_terms = []}
    -> (match dest_op o with
      { op_name = opname; op_params = p':: r } when (parmeq p p' && nuprl5_opname_p opname)
      -> r
    | _ -> error ["term"; "carrier"; "op"] [] [t; mk_term (mk_nuprl5_op [p]) []])
  | _ -> error ["term"; "carrier"; "subterms"] [] [t]

let token_parameter_to_string p =
  match dest_param p with
    Token s -> fst (dst_opname s)
  | _ -> error ["parameter"; "token"; "not"] [] []

let ipui_addr_parameter = make_param (token "!pui_addr")
let number_of_ipui_addr_term t =
  match dest_param (parameter_of_carrier ipui_addr_parameter t) with
    Number n when Lm_num.is_integer_num n -> Lm_num.int_of_num n
  | _ -> error ["term"; "!pui_addr"; "parameter type"] [] [t]

let number_of_inatural_term t =
  match dest_param (parameter_of_carrier inatural_parameter t) with
    Number n when Lm_num.is_integer_num n -> Lm_num.int_of_num n
  |_ -> error ["term"; "!natural"; "parameter type"] [] [t]

let num_of_inatural_term t =
  match dest_param (parameter_of_carrier inatural_parameter t) with
    Number n -> n
  |_ -> error ["term"; "!natural"; "parameter type"] [] [t]

let string_of_itext_term t =
  match dest_param (parameter_of_carrier itext_parameter t) with
    String s -> s
  |_ -> error ["term"; "!text"; "parameter type"] [] [t]

let string_of_istring_term t =
  match dest_param (parameter_of_carrier istring_parameter t) with
    String s -> s
  |_ -> error ["term"; "!string"; "parameter type"] [] [t]

let string_of_itoken_term t =
  match dest_param (parameter_of_carrier itoken_parameter t) with
    Token s -> fst (dst_opname s)
  |_ -> (
  print_endline "string_of_itoken_term";
  Mbterm.print_term t;

	 error ["term"; "!string"; "parameter type"] [] [t]
	 )

let oid_of_ioid_term t =
  match dest_param (parameter_of_carrier ioid_parameter t) with
    ObId o -> o
  |_ -> (* print_string "failing here"; Mbterm.print_term t;
	   print_newline();
	   *)
     error ["term"; "!oid"; "parameter type"] [] [t]

let dest_obid_param p =
  match dest_param p with
    ObId o -> o
  |_ -> error ["parameter"; "obid"] [] []

let oids_of_ioid_term t = map dest_obid_param (parameters_of_carrier ioid_parameter t)

let dest_token_param p =
  match dest_param p with
    Token s -> fst (dst_opname s)
  |_ -> error ["parameter"; "token"] [] []

let dest_int_param p =
  match dest_param p with
    Number n when Lm_num.is_integer_num n -> Lm_num.int_of_num n
  |_ -> error ["parameter"; "int"] [] []

let dest_num_param p =
  match dest_param p with
    Number n -> n
  |_ -> error ["parameter"; "num"] [] []

(*
 * stamps
 *)

(* todo change time from int to time parameter type. *)

type stamp = {term: term;
	      process_id: string;
	      transaction_seq: int;
	      seq: int;
	      time: num
	      }

let print_stamp s =
  print_string "STAMP{";
  print_string s.process_id;
  print_string ",";
  print_int s.transaction_seq;
  print_string ",";
  print_int s.seq;
  print_string "}"

let dest_stamp stamp = stamp

let istamp_parameter = make_param (token "!stamp" )
let istamp_op parms = mk_nuprl5_op (istamp_parameter :: parms)

(* unused
exception InvalidStampTerm of term
*)

let term_to_stamp t =
  match Lib_term.dest_term t with
   { term_op = op;
     term_terms = [] }
    -> (match dest_op op with
	{ op_name = opname;
	  op_params = [istamp; pseq; ptime; ptseq; ppid] }
	    when (nuprl5_opname_p opname && parmeq istamp istamp_parameter)
         ->
	(match dest_param ppid with Token pid ->
	(match dest_param ptseq with Number tseq when Lm_num.is_integer_num tseq ->
	(match dest_param pseq with Number seq when Lm_num.is_integer_num seq ->
	       (* print_string "tts "; *)
           {term = t;
            process_id = fst (dst_opname pid);
            transaction_seq = Lm_num.int_of_num tseq;
            seq = Lm_num.int_of_num seq;
            time = (try (destruct_time_parameter ptime)
		    with Invalid_argument s when s = "destruct_time_parameter_b"
				-> error ["stamp"; "term"; "invalid"; "timeb"] [] [t]
			| Invalid_argument s when s = "destruct_time_parameter_c"
				-> error ["stamp"; "term"; "invalid"; "timec"] [] [t]
			| Invalid_argument s when s = "destruct_time_parameter_d"
				-> error ["stamp"; "term"; "invalid"; "timed"] [] [t]
			| Invalid_argument s when s = "destruct_time_parameter_e"
				-> error ["stamp"; "term"; "invalid"; "timee"] [] [t]
			|_ -> error ["stamp"; "term"; "invalid"; "time"] [] [t]
			)
			}
	| _ -> error ["stamp"; "term"; "invalid"; "sequence"] [] [t])
	| _ -> error ["stamp"; "term"; "invalid"; "transaction"] [] [t])
	| _ -> error ["stamp"; "term"; "invalid"; "pid"] [] [t])
       | _ -> error ["stamp"; "term"; "invalid"; "op"] [] [t])
   | _ -> error ["stamp"; "term"; "invalid"; "term"] [] [t]

let stamp_to_term stamp = stamp.term

let stamp_to_object_id stamp = make_object_id (List.tl (parameters_of_term stamp.term))

let in_transaction_p = fun
  { process_id = pid1; transaction_seq = tseq1; _ }
  { process_id = pid2; transaction_seq = tseq2; _ } ->
    pid1 = pid2 && tseq1 = tseq2

let transaction_less = fun
  { term = term1; process_id = pid1; seq = seq1; time = time1; _ }
  { term = term2; process_id = pid2; seq = seq2; time = time2; _ } ->

    if not (stringeq pid1 pid2) then error ["stamp"; "less"; "incomparable"] [] [term1; term2]
    else if (eq_num time1 time2) then seq1 < seq2
         else (lt_num time1 time2)

(* TODO pid should include inet addr and time as well as process id to insure uniqueness *)
let get_pid =
   let pidref = ref None in fun () ->
      match !pidref with
         Some pid -> pid
       | None -> begin
            let inet_addr =
               try (gethostbyname (gethostname ())).h_addr_list.(0)
               with Not_found -> raise (Invalid_argument "Basic FDL: can not resolve local hostname!")
            in
            let pid =
               String.concat "_"
			      [ string_of_inet_addr inet_addr
			      ; string_of_int (getpid())
			      ; string_of_int (Stdlib.truncate (time()))
			      ]
            in
               pidref := Some pid;
               pid
         end

let stamp_count = ref 0

let make_stamp pid tseq seq time =
	{ term = (mk_term (istamp_op
				[ make_param (Number (Lm_num.num_of_int seq))
   				; make_time_parameter time
				; make_param (Number (Lm_num.num_of_int tseq))
   				; make_param (token pid)
   				])
			[])
	; process_id = pid
	; transaction_seq = tseq
	; seq = seq
	; time = time
	}

let equal_stamps_p a b =
 a.process_id = b.process_id
 && a.transaction_seq = b.transaction_seq
 && a.seq = b.seq
 && (eq_num a.time b.time)

let get_stamp () =
   make_stamp (get_pid ()) !stamp_count !stamp_count (num_of_int (Stdlib.truncate (time())))

let new_stamp () =
   incr stamp_count; get_stamp ()

let sequence () =
   incr stamp_count; !stamp_count

let itransaction_id_parameter = make_param (token "!transaction_id")
let itransaction_id_op pl = mk_nuprl5_op (itransaction_id_parameter :: pl)

let tid () =
    (mk_term
      (itransaction_id_op
		[ make_param (Number (Lm_num.num_of_int (sequence())))
		; make_param (token  (get_pid ()))
		])
     [])

let tideq s t =
 opeq (operator_of_term s) (operator_of_term t)

(* expect Fatal error: uncaught exception Incomparable_Stamps
   should try other tests and make outcome more apparent ie print test ok
 *)
(* unused
let test () =
 let s1 = (make_stamp "goo" 2 1 (num_of_int 2))
 and s2 = (make_stamp "moo" 1 2 (num_of_int 2))
 and s3 = (make_stamp "goo" 2 2 (num_of_int 3))
   in (in_transaction_p s3 s1) &
      (transaction_less s1 s3) &
      (transaction_less s3 s2)
;;
*)

let icons_op = (mk_nuprl5_op [make_param (token "!cons")])
let icons_term op h t = mk_term op [mk_bterm [] h; mk_bterm [] t]

let hd_of_icons_term iop t =
  match Lib_term.dest_term t with
    { term_op = op; term_terms = [l; r] } when opeq op iop
       ->  term_of_unbound_term l
    |_ -> error ["icons"; "not"] [] [t]

let tl_of_icons_term iop t =
  match Lib_term.dest_term t with
    { term_op = op; term_terms = [l; r] } when opeq op iop
       ->  term_of_unbound_term r
    |_ -> error ["icons"; "not"] [] [t]

let inil_term = mk_term icons_op []

let list_to_ilist_by_op_map op f l =
 let rec aux ll =
   if nullp ll
      then mk_term op []
      else mk_term op [mk_bterm [] (f (hd ll)); mk_bterm [] (aux (tl ll))] in
 aux l

let list_to_ilist_by_op op l =
 list_to_ilist_by_op_map op (function x -> x) l

let list_to_ilist l = list_to_ilist_by_op icons_op l

let list_to_ilist_map f l = list_to_ilist_by_op_map icons_op f l

let map_isexpr_to_list_by_op iop f t =
 let rec aux t acc =
  match Lib_term.dest_term t with
    { term_op = op; term_terms = [] } when opeq op iop
       -> acc
    | { term_op = op; term_terms = [] } when not (opeq op iop)
       -> (f t) :: acc
    | { term_op = op; term_terms = [l; r] } when ((opeq op iop) && (unbound_bterm_p l) && (unbound_bterm_p r))
       -> aux (term_of_unbound_term l) (aux (term_of_unbound_term r) acc)
    |_ -> (f t) :: acc
  in
 aux t []

let map_isexpr_by_op iop f t =
 let rec aux t =
  match Lib_term.dest_term t with
    { term_op = op; term_terms = [] } when opeq op iop
       -> ()
    | { term_op = op; term_terms = [] } when not (opeq op iop)
       -> (f t); ()
    | { term_op = op; term_terms = [l; r] } when ((opeq op iop) && (unbound_bterm_p l) && (unbound_bterm_p r))
       -> aux (term_of_unbound_term l); aux (term_of_unbound_term r); ()
    |_ -> (f t); ()
  in
 aux t

let map_isexpr_to_list f t = map_isexpr_to_list_by_op icons_op f t

let isome_op = (mk_nuprl5_op [make_param (token "!some")])
let isome_term t = mk_term isome_op [mk_bterm [] t]

let ioption_term tt =
 match tt with
   None -> ivoid_term
 | Some t -> isome_term t

let option_of_ioption_term t =
  if ivoid_term_p t
     then None
     else  match Lib_term.dest_term t with
              { term_op = op; term_terms = [s] } when opeq op isome_op
               ->  Some (term_of_unbound_term s)
	      |_ -> error ["isome"; "not"] [] [t]

let iproperty_parameter = make_param (token "!property")
let iproperty_term name_prop =
  mk_term (mk_nuprl5_op [iproperty_parameter; make_param (token (fst name_prop))])
	  [mk_bterm [] (snd name_prop)]

let string_of_token_parameter p =
  match dest_param p with
    Token s -> fst (dst_opname s)
  |_ -> error ["parameter"; "token"; "not"; ""] [] []

let property_of_iproperty_term pt =
  match Lib_term.dest_term pt with
    { term_op = pto; term_terms = [prop] }
    -> (match dest_op pto with
	{ op_name = po; op_params = [iprop; name] } when (nuprl5_opname_p po
							  && parmeq iprop iproperty_parameter)
	  -> (string_of_token_parameter name, term_of_unbound_term prop)
	|_ -> error ["iproperty"; "op"; "not"; ""] [] [pt])
    |_ -> error ["iproperty"; "term"; "not"; ""] [] [pt]

let debug_term = ref ivoid_term
