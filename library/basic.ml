open Printf
open Debug

open Unix
open List
open Utils
open Opname
open Num
open Refiner.Refiner.Term
open Nuprl5

let _ =
   if !debug_load then
      eprintf "Loading Basic%t" eflush

let inteq i j = (i = j)
let nullp l = l = []
let chareq a b = (a = b)
let stringeq a b = (a = b)

let listeq p a b =
   (List.length a = List.length b)
   & try for_all2 p a b with _ -> false

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
	-> (aopname = bopname & listeq parmeq aparms bparms))


let nuprl5_opname_p opn = stringeq opn nuprl5_opname


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

let mk_nuprl5_op pl = mk_op nuprl5_opname pl

let imessage_parameter = make_param (Token "!message")
let imessage_op parms = mk_nuprl5_op (imessage_parameter :: parms)

(* !natural{n} *)
let inatural_parameter = make_param (Token "!natural")
let inatural_op p = mk_nuprl5_op [inatural_parameter; p]
let inatural_term i = mk_term (inatural_op (make_param (Number (Num.Int i)))) []

(* !token{t} *)
let itoken_parameter = make_param (Token "!token")
let itoken_op p =  mk_nuprl5_op [itoken_parameter; p]
let itoken_term s = mk_term (itoken_op (make_param (Token s))) []

(* !string{s} *)
let istring_parameter = make_param (Token "!string")
let istring_op p =  mk_nuprl5_op [istring_parameter; p]
let istring_term s = mk_term (istring_op (make_param (String s))) []

(* !text{s} *)
let itext_parameter = make_param (Token "!text")
let itext_op p = mk_nuprl5_op [itext_parameter; p]
let itext_term s = mk_term (itext_op (make_param (String s))) []

let ioid_parameter = make_param (Token "!oid")
let ioid_op p = mk_nuprl5_op [make_param (Token "!oid"); p]
let ioid_term o = mk_term (ioid_op (make_param (ObId o))) []

let iterm_op = mk_nuprl5_op [make_param (Token "!term")]

let imessage_term sl ol tl =
  mk_term
    (imessage_op
      ( (map (function s -> make_param (Token s)) sl) 
      @ (map (function o -> make_param (ObId o)) ol) 
      ))
    (map (function t -> mk_bterm [] t) tl)


let iterm_term t = mk_term iterm_op [mk_bterm [] t]
let iterm_bterms bterms = mk_term iterm_op bterms
let ivoid_op = (mk_nuprl5_op [make_param (Token "!void")])
let ivoid_term = mk_term ivoid_op []

let ivoid_term_p t = 
  match dest_term t with
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
  map Mbterm.print_term tl;

 raise (Nuprl5_Exception ((String.concat " " sl),(imessage_term ("NuprlLightLibrary" :: sl) oids tl)))

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
  |_ -> 
	( () (* TODO dump error to stdout *)
	; handler (itext_term "Unexpected Nuprl Light failure"))

let unwind_error body unwind = 
  try body ()
  with 
    e -> (unwind ()); raise e



let parameters_of_term t =
  match dest_term t with
   { term_op = op; term_terms = _}
   -> match dest_op op with
    { op_name = _; op_params = parms } -> parms

let operator_of_term t =
  match dest_term t with
   { term_op = op; term_terms = _} -> op

let bound_terms_of_term t =
  match dest_term t with
   { term_op = _; term_terms = bterms} -> bterms

let term_of_unbound_term bterm = 
  match dest_bterm bterm with
    { bvars = []; bterm = t }
    -> t
  | _ -> error ["unbound"; "bound"] [] [(mk_term iterm_op [bterm])]

let unbound_bterm_p bterm = 
  match dest_bterm bterm with
    { bvars = []; bterm = t }
    -> true
  | _ -> false


let parameter_of_carrier p t =
  match dest_term t with
    { term_op = o; term_terms = []} 
     -> (match dest_op o with
	  { op_name = opname; op_params = [p'; c] } when (parmeq p p' & nuprl5_opname_p opname)
	    -> c
	|_ -> error ["term"; "carrier"; "op"] [] [t; mk_term (mk_nuprl5_op [p]) []])
  |_ -> error ["term"; "carrier"; "subterms"] [] [t]

let parameters_of_carrier p t =
  match dest_term t with
    { term_op = o; term_terms = []} 
     -> (match dest_op o with
	  { op_name = opname; op_params = p':: r } when (parmeq p p' & nuprl5_opname_p opname)
	    -> r
	|_ -> error ["term"; "carrier"; "op"] [] [t; mk_term (mk_nuprl5_op [p]) []])
  |_ -> error ["term"; "carrier"; "subterms"] [] [t]

let token_parameter_to_string p =
  match dest_param p with
    Token s -> s
  |_ -> error ["parameter"; "token"; "not"] [] []


let number_of_inatural_term t =
  match dest_param (parameter_of_carrier inatural_parameter t) with
    Number (Num.Int n) -> n
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
    Token s -> s
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
    Token s -> s
  |_ -> error ["parameter"; "token"] [] []

let dest_int_param p =
  match dest_param p with
    Number (Num.Int n) -> n
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

let istamp_parameter = make_param (Token "!stamp" )
let istamp_op parms = mk_nuprl5_op (istamp_parameter :: parms)

exception InvalidStampTerm of term

let term_to_stamp t = 
  match dest_term t with
   { term_op = op;
     term_terms = [] } 
    -> (match dest_op op with
	{ op_name = opname; 
	  op_params = [istamp; pseq; ptime; ptseq; ppid] }
	    when (nuprl5_opname_p opname & parmeq istamp istamp_parameter)
         ->
	(match dest_param ppid with Token pid ->
	(match dest_param ptseq with Number (Num.Int tseq) -> 
	(match dest_param pseq with Number (Num.Int seq) ->
	       (* print_string "tts "; *)
           {term = t;
            process_id = pid;
            transaction_seq = tseq;
            seq = seq;
            time = (try (destruct_time_parameter ptime)
		    with Invalid_argument "destruct_time_parameter_b"
				-> error ["stamp"; "term"; "invalid"; "timeb"] [] [t]
			| Invalid_argument "destruct_time_parameter_c"
				-> error ["stamp"; "term"; "invalid"; "timec"] [] [t]
			| Invalid_argument "destruct_time_parameter_d"
				-> error ["stamp"; "term"; "invalid"; "timed"] [] [t]
			| Invalid_argument "destruct_time_parameter_e"
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
  { process_id = pid1; transaction_seq = tseq1 }
  { process_id = pid2; transaction_seq = tseq2 } -> 
    pid1 = pid2 & tseq1 = tseq2


let transaction_less = fun
  { term = term1; process_id = pid1; seq = seq1; time = time1 }
  { term = term2; process_id = pid2; seq = seq2; time = time2 } -> 

    if not (stringeq pid1 pid2) then error ["stamp"; "less"; "incomparable"] [] [term1; term2]
    else if (eq_num time1 time2) then seq1 < seq2
         else (lt_num time1 time2)

let get_inet_addr =
	let {h_addr_list=l} = gethostbyname (gethostname ())
	in l.(0)

type stamp_data = {mutable count : int; pid : string}

(* TODO pid should include inet addr and time as well as process id to insure uniqueness *)
let stamp_data = 
	{ count = 0
	; pid = String.concat "_"
			[ string_of_inet_addr get_inet_addr
			; string_of_int (getpid())
			; string_of_int (time())
			]
	}

let make_stamp pid tseq seq time =
	{ term = (mk_term (istamp_op
				[ make_param (Number (Num.Int seq))
   				; make_time_parameter time
				; make_param (Number (Num.Int tseq))
   				; make_param (Token pid)
   				])
			[])
	; process_id = pid
	; transaction_seq = tseq
	; seq = seq
	; time = time
	}

let equal_stamps_p a b =
 a.process_id = b.process_id
 & a.transaction_seq = b.transaction_seq
 & a.seq = b.seq
 & (eq_num a.time b.time)

let new_stamp () = 
  stamp_data.count <- stamp_data.count + 1;
  make_stamp stamp_data.pid stamp_data.count stamp_data.count (num_of_int (time()))

let get_stamp () = 
  make_stamp stamp_data.pid stamp_data.count stamp_data.count (num_of_int (time()))

let sequence () = 
  stamp_data.count <- stamp_data.count + 1;
  stamp_data.count

let itransaction_id_parameter = make_param (Token "!transaction_id")
let itransaction_id_op pl = mk_nuprl5_op (itransaction_id_parameter :: pl)

let tid () =
    (mk_term
      (itransaction_id_op
		[ make_param (Number (Num.Int (sequence())))
		; make_param (Token  stamp_data.pid)
		])
     [])

let tideq s t = 
 opeq (operator_of_term s) (operator_of_term t)


(* expect Fatal error: uncaught exception Incomparable_Stamps
   should try other tests and make outcome more apparent ie print test ok
 *)
let test () = 
 let s1 = (make_stamp "goo" 2 1 (num_of_int 2))
 and s2 = (make_stamp "moo" 1 2 (num_of_int 2))
 and s3 = (make_stamp "goo" 2 2 (num_of_int 3))
   in (in_transaction_p s3 s1) & 
      (transaction_less s1 s3) &
      (transaction_less s3 s2) 
;;


let icons_op = (mk_nuprl5_op [make_param (Token "!cons")])
let icons_term op h t = mk_term op [mk_bterm [] h; mk_bterm [] t]

let hd_of_icons_term iop t =
  match dest_term t with
    { term_op = op; term_terms = [l; r] } when opeq op iop
       ->  term_of_unbound_term l
    |_ -> error ["icons"; "not"] [] [t]

let tl_of_icons_term iop t =
  match dest_term t with
    { term_op = op; term_terms = [l; r] } when opeq op iop
       ->  term_of_unbound_term r
    |_ -> error ["icons"; "not"] [] [t]


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
  match dest_term t with
    { term_op = op; term_terms = [] } when opeq op iop
       -> acc
    | { term_op = op; term_terms = [] } when not (opeq op iop)
       -> (f t) :: acc
    | { term_op = op; term_terms = [l; r] } when ((opeq op iop) & (unbound_bterm_p l) & (unbound_bterm_p r))
       -> aux (term_of_unbound_term l) (aux (term_of_unbound_term r) acc)
    |_ -> (f t) :: acc  
  in
 aux t []

let map_isexpr_by_op iop f t =
 let rec aux t =
  match dest_term t with
    { term_op = op; term_terms = [] } when opeq op iop
       -> ()
    | { term_op = op; term_terms = [] } when not (opeq op iop)
       -> (f t); ()
    | { term_op = op; term_terms = [l; r] } when ((opeq op iop) & (unbound_bterm_p l) & (unbound_bterm_p r))
       -> aux (term_of_unbound_term l); aux (term_of_unbound_term r); ()
    |_ -> (f t); ()
  in
 aux t

let map_isexpr_to_list f t = map_isexpr_to_list_by_op icons_op f t

let isome_op = (mk_nuprl5_op [make_param (Token "!some")])
let isome_term t = mk_term isome_op [mk_bterm [] t]

let ioption_term tt =
 match tt with
   None -> ivoid_term
 | Some t -> isome_term t

let option_of_ioption_term t =
  if ivoid_term_p t
     then None
     else  match dest_term t with
              { term_op = op; term_terms = [s] } when opeq op isome_op
               ->  Some (term_of_unbound_term s)
	      |_ -> error ["isome"; "not"] [] [t]

let iproperty_parameter = make_param (Token "!property")
let iproperty_term name_prop =
  mk_term (mk_nuprl5_op [iproperty_parameter; make_param (Token (fst name_prop))])
	  [mk_bterm [] (snd name_prop)]

 
let string_of_token_parameter p =
  match dest_param p with
    Token s -> s
  |_ -> error ["parameter"; "token"; "not"; ""] [] []

let property_of_iproperty_term pt =
  match dest_term pt with 
    { term_op = pto; term_terms = [prop] } 
    -> (match dest_op pto with
	{ op_name = po; op_params = [iprop; name] } when (nuprl5_opname_p po 
							  & parmeq iprop iproperty_parameter)
	  -> (string_of_token_parameter name, term_of_unbound_term prop)
	|_ -> error ["iproperty"; "op"; "not"; ""] [] [pt])
    |_ -> error ["iproperty"; "term"; "not"; ""] [] [pt]







