
open Unix
open List
open Utils
open Opname
open Term
open Object_id
open BigInt
open Nuprl5

(*
 * common terms
 *)

let mk_nuprl5_op pl = mk_op nuprl5_opname pl

let imessage_parameter = make_param (Token "!message")
let imessage_op parms = mk_nuprl5_op (imessage_parameter :: parms)

(* !natural{n} *)
let inatural_parameter = make_param (Token "!natural")
let inatural_op p = mk_nuprl5_op [inatural_parameter; p]
let inatural_term i = mk_term (inatural_op (make_param (Number i))) []

(* !token{t} *)
let itoken_parameter = make_param (Token "!token")
let itoken_op p =  mk_nuprl5_op [itoken_parameter; p]
let itoken_term s = mk_term (itoken_op (make_param (Token s))) []

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
let ivoid_term = mk_term (mk_nuprl5_op [make_param (Token "!void")]) []

(*
 * failure
 *)

exception Nuprl5_Exception of (string * term)

let error sl oids tl = 
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
  | _ -> 
	( () (* TODO dump error to stdout *)
	; handler (itext_term "Unexpected Nuprl Light failure"))


let parameters_of_term t =
  match dest_term t with
   { term_op = op; term_terms = _}
   -> match dest_op op with
    { op_name = _; op_params = parms } -> parms

let term_of_unbound_term bterm = 
  match dest_bterm bterm with
    { bvars = []; bterm = t }
    -> t
  | _ -> error ["unbound"; "bound"] [] [(mk_term iterm_op [bterm])]


let parameter_of_carrier p t =
  match dest_term t with
    { term_op = o; term_terms = []} 
     -> (match dest_op o with
	  { op_name = opname; op_params = [p'; c] } when (p = p' & opname = nuprl5_opname)
	    -> c
	|_ -> error ["term"; "carrier"; "op"] [] [t; mk_term (mk_nuprl5_op [p]) []])
  |_ -> error ["term"; "carrier"; "subterms"] [] [t]

let token_parameter_to_string p =
  match dest_param p with
    Token s -> s
  |_ -> error ["parameter"; "token"; "not"] [] []


let number_of_inatural_term t =
  match dest_param (parameter_of_carrier inatural_parameter t) with
    Number n -> n
  |_ -> error ["term"; "!text"; "parameter type"] [] [t]

let string_of_itext_term t =
  match dest_param (parameter_of_carrier itext_parameter t) with
    String s -> s
  |_ -> error ["term"; "!text"; "parameter type"] [] [t]

let oid_of_ioid_term t =
  match dest_param (parameter_of_carrier ioid_parameter t) with
    ObId o -> o
  |_ -> error ["term"; "!oid"; "parameter type"] [] [t]


(*
 * stamps
 *)

(* todo change time from int to time parameter type. *)

type stamp = {term: term;
	      process_id: string;
	      transaction_seq: int;
	      seq: int;
	      time: bigint
	      }


let istamp_parameter = make_param (Token "!stamp" )
let istamp_op parms = mk_nuprl5_op (istamp_parameter :: parms)

exception InvalidStampTerm of term

let term_to_stamp t = 
  match dest_term t with
   { term_op = op;
     term_terms = [] } 
    -> (match dest_op op with
	{ op_name = opname; 
	  op_params = [istamp; ppid; ptseq; pseq; ptime] }
	    when (opname = nuprl5_opname & istamp = istamp_parameter)
         ->
	(match dest_param ppid with String pid ->
	(match dest_param ptseq with Number tseq -> 
	(match dest_param pseq with Number seq -> 
         {term = t; process_id = pid; transaction_seq = tseq; seq = seq; time = destruct_time_parameter ptime}
	| _ -> error ["stamp"; "term"; "invalid"; "sequence"] [] [t])
	| _ -> error ["stamp"; "term"; "invalid"; "transaction"] [] [t])
	| _ -> error ["stamp"; "term"; "invalid"; "pid"] [] [t])
       | _ -> error ["stamp"; "term"; "invalid"; "op"] [] [t])
   | _ -> error ["stamp"; "term"; "invalid"; "term"] [] [t]

let stamp_to_term stamp = stamp.term

let in_transaction_p = fun
  { process_id = pid1; transaction_seq = tseq1 }
  { process_id = pid2; transaction_seq = tseq2 } -> 
    pid1 = pid2 & tseq1 = tseq2


let transaction_less = fun
  { term = term1; process_id = pid1; seq = seq1; time = time1 }
  { term = term2; process_id = pid2; seq = seq2; time = time2 } -> 
    if not (pid1 = pid2) then error ["stamp"; "less"; "incomparable"] [] [term1; term2]
    else if (bequal time1 time2) then seq1 < seq2
         else (blt time1 time2)
 
type stamp_data = {mutable count : int; pid : string}

(* TODO pid should include inet addr and time as well as process id to insure uniqueness *)
let stamp_data = 
	{ count = 0
	; pid = String.concat "::"
			[ string_of_inet_addr inet_addr_any
			; string_of_int (getpid())
			; string_of_int (time ())
			]
	}

let make_stamp pid tseq seq time = 
	{ term = (mk_term (istamp_op
				[ make_param (String pid)
   				; make_param (Number tseq)
   				; make_param (Number seq)
   				; make_time_parameter time
				])
			[])
	; process_id = pid
	; transaction_seq = tseq
	; seq = seq
	; time = time
	}

let new_stamp () = 
  stamp_data.count <- stamp_data.count + 1;
  make_stamp stamp_data.pid stamp_data.count stamp_data.count (BigInt.mk_bint (time()))

let get_stamp () = 
  make_stamp stamp_data.pid stamp_data.count stamp_data.count (BigInt.mk_bint (time()))


let sequence () = 
  stamp_data.count <- stamp_data.count + 1;
  stamp_data.count

let itransaction_id_parameter = make_param (Token "!transaction_id")
let itransaction_id_op pl = mk_nuprl5_op (itransaction_id_parameter :: pl)

let tid () =
  mk_bterm 
    []
    (mk_term
      (itransaction_id_op
		[ make_param (Number (sequence()))
		; make_param (Token  stamp_data.pid)
		])
     [])


(* expect Fatal error: uncaught exception Incomparable_Stamps
   should try other tests and make outcome more apparent ie print test ok
 *)
let test () = 
 let s1 = (make_stamp "goo" 2 1 (BigInt.create 2))
 and s2 = (make_stamp "moo" 1 2 (BigInt.create 2))
 and s3 = (make_stamp "goo" 2 2 (BigInt.create 3))
   in (in_transaction_p s3 s1) & 
      (transaction_less s1 s3) &
      (transaction_less s3 s2) 
;;

