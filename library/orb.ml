
open Unix
open List

open Utils
open Nuprl5
open Link
open Term
open Basic

open Mbterm
open Opname



(*
 * 	Some simplyfying assumtpions FTTB : 
 * 	
 * 	 only one library per Nuprl-Light Process.
 * 	 only connected to a single Library process and a single environment in the process.
 *
 *)

(*
 * 	The environment record represents the connection between a remote environment
 *	and a local one. We assume local is refiner and remote may be a library or editor.
 *	Any eval request contains environment record. Any broadcast is matched against
 *	the broadcast filter ( matches lib stamp and member table types ).
 *	Configure will add to environment.
 *)


(* 	 
 *	environment is local library state's handle to remote environment.
 *	
 *)

type connection =
	{ link	: link
	; orb		: orb			(* local orb containing connection *)
	; ro_address	: string list		(* remote orb *)
	}

and environment = 
	{ connection : connection

	(* remote	*)
	; re_address	: string list		(* to build request wrappers *)

	(* local	*)
	; le_address 	: string list		(* to allow remote to make requests 
						   when req received used to find appropriate eval hook  *)
	; stamp		: stamp			(* local consumer stamp; for start/stop broadcasts *)

	; bhook 	: bound_term list -> unit		(* broadcast hook *)
	; ehook 	: term -> term		(* eval hook *)
	}

and orb =
	{ lo_address	: string list
	; mutable connections	: connection list	(* remote orb *)
	; mutable environments	: environment list	
	}


let orb_open name =
	{ lo_address =	[  "nuprl_light"
			; name
			; "orb"
			; string_of_int (getpid ())
			; string_of_int (time ())
			]
	; connections = []
	; environments = []
	}


(* addr is string list 
   term is !expression term
*)



let ireq_parameter = make_param (Token "!req")
let ireq_op pl = mk_nuprl5_op (ireq_parameter :: pl)
let ireq_term addr t tid = 
  mk_term (ireq_op (make_param (Number (sequence ()))
			:: (make_param (Token "NUPRL5-type"))
			:: (map (function s -> make_param (Token s)) addr)))
	  [mk_bterm [] t; tid]

let irsp_parameter = make_param (Token "!rsp")
let irsp_op p = mk_nuprl5_op [irsp_parameter; p]
let irsp_term seq t =
  mk_term (irsp_op seq)
	  [mk_bterm [] t]

let result_of_irsp_term t =
  match dest_term t with
    { term_op = o;
      term_terms = bterms } 
    -> (match dest_op o with
	{ op_name = ro;
	  op_params = irsp :: ps} when (ro = nuprl5_opname & irsp = irsp_parameter)
	  -> term_of_unbound_term (hd bterms)
	| _ -> error ["orb"; "rsp"; "not"] [] [t])


(* note that token differs one these are two distinct operators *)
let ibroadcast_parameter = make_param (Token "!broadcast")
let ibroadcasts_op = mk_nuprl5_op [make_param (Token "!broadcasts")]

let ifail_parameter = make_param (Token "!fail")
let ifail_op = mk_nuprl5_op [ifail_parameter]
let ifail_term t = mk_term ifail_op [mk_bterm [] t]
let ifail_term_p t =
  match dest_term t with
    { term_op = fo; term_terms = bterms } when fo = ifail_op -> true
  | _ -> false


let ivalue_parameter = make_param (Token "!value")
let ivalue_op = mk_nuprl5_op [ivalue_parameter]
let ivalue_term_p t =
  match dest_term t with
    { term_op = vo; term_terms = bterms } when vo = ivalue_op -> true
  | _ -> false


let iack_parameter = make_param (Token "!ack")
let iack_op = mk_nuprl5_op [iack_parameter]
let iack_term = mk_term iack_op []
let iack_term_p t = 
  match dest_term t with
    { term_op = ao; term_terms = bterms } when ao = iack_op & bterms = [] -> true
  | _ -> false

let messages_of_iresult_term t =
  match dest_term t with
    { term_op = _; term_terms = _ :: msgs } 
       ->  map term_of_unbound_term msgs
    |_ -> error ["orb"; "result"; "messages"] [] [t]

let result_of_iresult_term t = 
  match dest_term t with
    { term_op = _; term_terms = r :: _ } 
       -> term_of_unbound_term r
    |_ -> error ["orb"; "result"; "messages"] [] [t]



(* assumes single environment is present, nfg if more than one env. *)
let orb_broadcast orb t =
  ((hd (orb.environments)).bhook t)
 
    
let local_eval f t =
  unconditional_error_handler 
    (function () -> (f t))
    (function term -> term)

let rec bus_wait c tid ehook = 

  let t = (Link.recv c.link) in
  match dest_term t  with
    { term_op = op;
      term_terms = bterms } 
    ->  (match dest_op op with
          { op_name = opn;
 	    op_params = ib :: ps } when (ib = ibroadcast_parameter & opn = nuprl5_opname)
           -> ( (orb_broadcast c.orb bterms)
             ; bus_wait c tid ehook)
	| { op_name = opn;
	    op_params = ireq :: ps } when (opn = nuprl5_opname & ireq = ireq_parameter)
	  -> (Link.send c.link
	      (if not (tid = (hd (tl (bterms)))) 
		then (irsp_term (hd ps)
			   (ifail_term (imessage_term ["orb"; "req"; "recursive"; "tid"] [] [])))
		else irsp_term (hd ps)
			   (local_eval ehook
					(term_of_unbound_term (hd bterms))))
	     ; bus_wait c tid ehook)
	| _ -> t)


let bus_eval c addr expr tid ehook =
  let link = c.link in

    Link.send link (ireq_term addr expr tid);

    let t = bus_wait c tid ehook in
     result_of_irsp_term t



let iinform_parameter = make_param (Token "!inform")
let iinform_op = mk_nuprl5_op [iinform_parameter]
let iinform_term info = mk_term iinform_op
				[mk_bterm [] info]
let iinform_term_p t =
  match dest_term t with
    { term_op = ino; term_terms = bterms } when ino = iinform_op -> true
  | _ -> false

let info_of_iinform_term t = one_subterm t
 

let ienvironment_address_parameter = make_param (Token "!environment_address")
let ienvironment_address_op pl = mk_nuprl5_op (ienvironment_address_parameter :: pl)
let ienvironment_address_term a =
 mk_term (ienvironment_address_op
	       (map (function s -> make_param (Token s)) a))
	 []

let address_of_ienvironment_address_term t =
  (match dest_term t with
    { term_op = op;
      term_terms =  bts } when bts = []
      ->  (match dest_op op with
	    { op_name = opn;
	      op_params = ienv :: al } when (opn = nuprl5_opname & ienv = ienvironment_address_parameter)
	      ->  (map token_parameter_to_string al)
            |_-> error ["orb"; "term"; "EnvironmentAddress"; "invalid"; "op"] [] [t])
     |_-> error ["orb"; "term"; "EnvironmentAddress"; "invalid"; "subterms"; token_parameter_to_string (hd (parameters_of_term t))] [] [t])



let itable_types_parameter = make_param (Token "!table_types" )
let itable_types_op pl = mk_nuprl5_op (itable_types_parameter :: pl)
let itable_types_term types address =
 mk_term (itable_types_op
		(map (function s -> make_param (Token s)) types))
	 [mk_bterm [] (ienvironment_address_term address)]


let iconfigure_parameter = make_param (Token "!configure")
let iconfigure_op = mk_nuprl5_op [iconfigure_parameter]
let iconfigure_term term =  mk_term iconfigure_op [mk_bterm [] term]

let command_of_iconfigure_term t =
  match dest_term t with
    { term_op = co; term_terms = [bterm] } when co = iconfigure_op 
      -> (match dest_bterm bterm with
	   { bvars = []; bterm = term} -> term
         | _ -> error ["orb"; "term"; "configure"; "subterm"] [] [t])
  | _ -> error ["orb"; "term"; "configure"] [] [t]



let default_ehook t = error ["orb"; "RequestNotExpected"] [] []
	
(* orb-send-configure orb-send-configure-blink *)
let config_send c term =
  let rsp = bus_eval c [] (iconfigure_term term) (tid()) default_ehook in
    if ifail_term_p rsp 
      then error ["orb"; "configure"; "send"; "fail"] [] [term]
      else rsp

let config_send_state c term =
  let rsp = config_send c term in
    if not (iack_term_p rsp)
       then error ["orb"; "configure"; "send"; "state"] [] [term]
       else ()

(* orb-send-request orb-send-request-blink *)
let config_send_request c term = 
  let cmd = command_of_iconfigure_term (config_send c term) in
    if iinform_term_p cmd
      then cmd
      else error ["orb"; "configure"; "send"; "request"] [] [cmd]

let irequest_parameter = make_param (Token "!request")
let irequest_op = mk_nuprl5_op [irequest_parameter]
let irequest_term t = mk_term irequest_op [mk_bterm [] t]

let idisconnect_parameter = make_param (Token "!disconnect")
let idisconnect_op b = mk_nuprl5_op [idisconnect_parameter; make_bool_parameter b]
let idisconnect_term b = mk_term (idisconnect_op b) []

let iconnect_parameter = make_param (Token "!connect")
let iconnect_op localhost sock = 
  mk_nuprl5_op
    [ iconnect_parameter
    ; make_param (Number sock)
    ; make_param (String localhost)
    ]
let iconnect_term localhost sock = mk_term (iconnect_op localhost sock) []


let connect_aux orb host hsock sock =
  let tlink = connect_with_callback host hsock sock in
  let tcon = { link = tlink; orb = orb; ro_address = [] } in
    (try (config_send_state tcon (iinform_term (iconnect_term (gethostname ()) sock)))
      with
        _-> ( config_send_state tcon (iinform_term (idisconnect_term true))
	    ; error ["orb"; "connect"; "fail"] [] []
	    ));

    try (connect_callback tlink) (* returns link if successful *)
      with 
	_-> ( config_send_state tcon (iinform_term (idisconnect_term true))
	    ; error ["orb"; "connect"; "callback"; "fail"] [] []
	    )
          

let connect orb host hsock sock =
  let link = connect_aux orb host hsock sock in
  let tcon = { link = link; orb = orb; ro_address = [] } in
    config_send_state tcon (iinform_term (ienvironment_address_term orb.lo_address));

    let address = address_of_ienvironment_address_term
		    (info_of_iinform_term
		      (config_send_request tcon (irequest_term (ienvironment_address_term [])))) in
    
    let connection =  { link = link; orb = orb; ro_address = address } in
      orb.connections <- (connection :: orb.connections);
      connection

let irevoke_parameter = make_param (Token "!revoke")
let irevoke_op = mk_nuprl5_op [irevoke_parameter]
let irevoke_term t = mk_term irevoke_op [mk_bterm [] t]

let disconnect orb connection =
  (* revoke?? *)

  config_send_state connection (irevoke_term (ienvironment_address_term orb.lo_address));
  config_send_state connection (irevoke_term (ienvironment_address_term connection.ro_address));

  try (config_send_state connection (iinform_term (idisconnect_term false)))
    with _-> ( print_string "Failure in Disconnect, assuming things ok."
	     ; print_newline()
	     );

  orb.connections <- (remove connection orb.connections);

  Link.disconnect connection.link;
  ()

let orb_close orb =
 match orb.connections with
   [] -> ()
 | _ -> error ["orb"; "close"] [] []


(* 
   config_send_state
   config_send_request
	connection_eval 
*)

let iml_parameter = make_param (Token "!ML")
let iml_op pl = mk_nuprl5_op (iml_parameter :: pl)

let iml_woargs_parameter = make_param (Token "!ML_woargs")
let iml_woargs_op pl = mk_nuprl5_op (iml_woargs_parameter :: pl)

let iml_woargs_term result_p term = 
      (mk_term (iml_woargs_op [ make_bool_parameter false
		       ; make_bool_parameter result_p])
		[mk_bterm [] term])

let iml_term result_p term terms = 
 mk_term (iml_op	
		[ make_bool_parameter false
		; make_bool_parameter result_p
		])
         ((mk_bterm [] term) :: map (function t -> mk_bterm [] t) terms)


let iexpression_parameter = make_param (Token "!expression")
let iexpression_op = mk_nuprl5_op [iexpression_parameter]
let iexpression_term term =
 mk_term iexpression_op
	 [mk_bterm [] term]

let iml_expression_term result_p expr args =
 iexpression_term (iml_term result_p expr args)


let icommand_parameter = make_param (Token "!command")
let icommand_op = mk_nuprl5_op [icommand_parameter]

let cmd_of_icommand_term t = 
 match dest_term t with { term_op = o; term_terms = [cmd] } when o = icommand_op
    -> term_of_unbound_term cmd
 |_ -> error ["orb"; "command"; "not"] [] [t]



(* evals in remote orb env. *)
let connection_eval_string c s result_p =
  let result = bus_eval c c.ro_address 
			(iexpression_term (iml_woargs_term result_p (itext_term s)))
			(tid())
			default_ehook in
    if ifail_term_p result    
      then error ["orb"; "connection"; "eval"] [] [result]
      else if ivalue_term_p result
	  then (result_of_iresult_term result)
	  else error ["orb"; "eval"; "value"; "not"] [] [result]

let connection_eval_args c t tl  =
  let result = bus_eval c c.ro_address 
 			(iml_expression_term false t tl)
			(tid ())
			default_ehook in
    ((if (ifail_term_p result)
      then error ["orb"; "connection"; "eval"] [] [result])
    ; ())


let nl0_open_library_environment c lib_id = 
  address_of_ienvironment_address_term
    (connection_eval_string c ("NL0_open_library_environment \"" ^ lib_id ^ "\"") true)
    
let nl0_close_library_environment c addr = 
  (connection_eval_args c
			(itext_term "NL0_close_library_environment") 
			[ienvironment_address_term addr])
    
let nl0_description =
  mk_term (mk_nuprl5_op
		 [make_param (Token "!description"); make_param (Token "NuprlLight")])
	[ mk_bterm [] (inatural_term 0)
	; mk_bterm [] (itoken_term "REFINER")
	]

let istart_op = mk_nuprl5_op [make_param (Token "!start")]

let istart_term t s e d = 
  mk_term istart_op 
	[ mk_bterm [] t
	; mk_bterm [] s
	; mk_bterm [] e
	; mk_bterm [] d
	] 

let broadcasts_of_istart_term s =
  match dest_term s with
    { term_op = o;
      term_terms = [_; _; _; bs] } when o = istart_op
     -> (match dest_term (term_of_unbound_term bs) with
	  { term_op = o';
	    term_terms = bts } when o' = ibroadcasts_op
	     -> bts
	|_ -> ( let {op_name = name} = dest_op o in
	 (*print_term s;*) error ["orb"; "start"; "broadcasts"; "not"] [] [s]))
  |_ -> ( let name = (opname_of_term s) in
	 (*print_term s;*) error ["orb"; "start"; "broadcasts"; "not"; "start"] [] [s])

let start_broadcasts e =
  let t = 
    config_send_request
      e.connection 
      (irequest_term
        (istart_term (itable_types_term [] e.re_address)
		     (stamp_to_term e.stamp)
		     (ienvironment_address_term ((e.connection).orb).lo_address)	
			  (* nfg if we allow mulitple envs *)
		     nl0_description)) 

	in (e.bhook (broadcasts_of_istart_term
 		    (info_of_iinform_term t))); ()

let irevoke_op = mk_nuprl5_op [make_param (Token "!revoke")]
let irevoke_term t = mk_term irevoke_op [mk_bterm [] t]

let stop_broadcasts e =
    config_send_state
      e.connection 
      (irevoke_term
        (istart_term (itable_types_term [] e.re_address)
		     (stamp_to_term e.stamp)
		     (ienvironment_address_term ((e.connection).orb).lo_address)	
			  (* nfg if we allow mulitple envs *)
		     ivoid_term)) 



let open_library_environment connection lib_id bhook ehook =
  let lib_env_address = nl0_open_library_environment connection lib_id in
    let env = 
	{ connection = connection
	; re_address = lib_env_address
	; le_address = []	(* TODO: ?? :: orb_address *)
	; stamp = new_stamp ()
	; bhook = bhook
	; ehook = ehook
	} in
    (connection.orb).environments <- env :: connection.orb.environments;
    start_broadcasts env;
    env


let close_library_environment e =
  stop_broadcasts e;
  nl0_close_library_environment e.connection e.re_address;
  let orb = e.connection.orb in
    orb.environments <- remove e orb.environments;
   ()


(* orb-eval (<expression>) : <result> *)


(* todo *)
let print_message t = 
 prerr_string "There is a term Message.";
 prerr_newline()


(* returns !ack of false result_p
 * else fails or returns value.
 *)
let orb_eval result_p env expr tid ehook= 
 let c = env.connection in
  let result = bus_eval c env.re_address expr tid ehook in
    if ifail_term_p result
      then error ["orb"; "eval"; "fail"] [] [result]
    else if result_p
      then
	((iter print_message (messages_of_iresult_term result));
	if ivalue_term_p result
	  then (result_of_iresult_term result)
	  else error ["orb"; "eval"; "value"; "not"] [] [result])
    else result

           

let eval_string e tid s =
  orb_eval
    false
    e
    (iml_expression_term false (itext_term s) [])
    tid
    default_ehook;
  ()

let eval e tid t =
  orb_eval false e (iml_expression_term false t []) tid default_ehook;
  ()

let eval_args e tid t tl = 
  orb_eval false e (iml_expression_term false t tl) tid default_ehook;
  ()

let eval_string_to_term e tid s =
 orb_eval true e (iml_expression_term true (itext_term s) []) tid default_ehook

let eval_to_term e tid t =
 orb_eval true e (iml_expression_term true t []) tid default_ehook

let eval_args_to_term e tid t tl =
 orb_eval true e (iml_expression_term true t tl) tid default_ehook



(* 
 *	Not sure if this shouldn't be 
 *	... -> (term -> term) -> ... instead of
 *	... -> (term -> unit) -> ...
 *	
 *	Actually keep this and add another set of funcs if need to return a value.
 *	This keep result wrappers (eg !ack) in orb.
 *)

let eval_with_callback e tid f t tl =
  orb_eval false e (iml_expression_term false t tl) 
	   tid
	   (function term -> (f (cmd_of_icommand_term term)); iack_term)
 ; ()	

let eval_to_term_with_callback e tid f t tl =
 (orb_eval true e (iml_expression_term true t tl) 
	  tid
	  (function term -> (f (cmd_of_icommand_term term)); iack_term))


