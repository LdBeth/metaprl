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

open Utils
open Nuprl5
open Link
open Term_sig
open Refiner.Refiner.Term
open Refiner.Refiner.TermType
open Refiner.Refiner.TermOp
open Basic
open Opname

open Mbterm

open Db
open Definition

let _ =
   show_loading "Loading Orb%t"

(*
 * 	Some simplyfying assumtpions FTTB :
 *
 * 	only one library per MetaPRL Process.
 * 	only connected to a single Library process and a single
 *      environment in the process.
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
    { link	: link;
      orb	: orb;			(* local orb containing connection *)
      ro_address  : string list		(* remote orb *)
    }

and environment =
    { connection : connection;
      re_address	: string list;		(* remote address, to build request wrappers *)
      le_address 	: string list;		(* local address, ,to allow remote to make requests
						   when req received used to find appropriate eval hook  *)
      stamp		: stamp;		(* local consumer stamp; for start/stop broadcasts *)
      ehook 	        : (term -> term);	(* eval hook *)
      resources         : (string * termtable) list
    }

and orb =
	{ lo_address	: string list;
	  mutable connections	: connection list;	(* remote orb *)
	  mutable environments	: environment list
	}

let resource env string = assoc string env.resources

let orb_open name =
   { lo_address = [ "metaprl";
		    name;
		    "orb";
		    string_of_int (getpid ());
		    string_of_int (Stdlib.truncate (time ()))
		  ];
     connections = [];
     environments = []
   }

let token s = Token (mk_opname s nil_opname)

let ireq_parameter = make_param (token "!req")
let ireq_op pl = mk_nuprl5_op (ireq_parameter :: pl)
let ireq_term seq addr t tid =
  mk_term (ireq_op (make_param (Number (Lm_num.num_of_int seq))
		    :: (make_param (token "CONFIG"(*NUPRL5-type*)))
		    :: (map (function s -> make_param (token s)) addr)))
    [mk_bterm [] t(*; mk_bterm [] tid*)]

let irsp_parameter = make_param (token "!rsp")
let irsp_op p = mk_nuprl5_op [irsp_parameter; p]
let irsp_term seq t =
  mk_term (irsp_op seq)
    [mk_bterm [] t]

let result_of_irsp_term t =
  match dest_term t with
    { term_op = o; term_terms = bterms } ->
      (match dest_op o with
	{ op_name = ro; op_params = irsp :: ps} when
	  (nuprl5_opname_p ro && parmeq irsp irsp_parameter) ->
	    term_of_unbound_term (hd bterms)
      | _ -> error ["orb"; "rsp"; "not"] [] [t])

let seq_of_irsp_term t =
  match dest_term t with
    { term_op = o; term_terms = bterms} ->
      (match dest_op o with
	{ op_name = ro;
	  op_params = irsp :: iseq :: ps} when (nuprl5_opname_p ro && parmeq irsp irsp_parameter)
	-> dest_int_param iseq
      | _ -> error ["orb"; "rsp"; "not"] [] [t])

let ibroadcasts_parameter = make_param (token "!broadcasts")
let broadcasts_of_ibroadcasts_term t =
  match dest_term t with
    { term_op = o; term_terms = bterms } ->
      (match dest_op o with
	{ op_name = ro;
	  op_params = ib :: tp :: ps} when
	    (nuprl5_opname_p ro && parmeq ib ibroadcasts_parameter) ->
	      if (ps = [] || not (destruct_bool_parameter (hd ps)))
	      then (tl bterms)
	      else (tl (tl bterms))
      | _ -> error ["orb"; "broadcasts"; "not"] [] [t])

let transaction_stamp_of_ibroadcasts_term t =
  match dest_term t with
    { term_op = o; term_terms = bterms } ->
      (match dest_op o with
	{ op_name = ro;
	  op_params = ib :: tp :: ps} when (nuprl5_opname_p ro && parmeq ib ibroadcasts_parameter)
	-> term_to_stamp (term_of_unbound_term (hd bterms))
      | _ -> error ["orb"; "broadcasts"; "stamp"; "not"] [] [t])

let auto_commit_of_ibroadcasts_term t =
  match dest_term t with
    { term_op = o; term_terms = bterms } ->
      (match dest_op o with
	{ op_name = ro;
	  op_params = ib :: tp :: ps} when (ro = nuprl5_opname && ib = ibroadcasts_parameter)
	-> if (not (nullp ps) && (destruct_bool_parameter (hd ps)))
	then Some (term_to_stamp (term_of_unbound_term (hd (tl bterms))))
	else None
      | _ -> error ["orb"; "broadcasts"; "commit"; "not"] [] [t])

let ifail_parameter = make_param (token "!fail")
let ifail_op = mk_nuprl5_op [ifail_parameter]
let ifail_term t = mk_term ifail_op [mk_bterm [] t]
let ifail_term_p t =
  match dest_term t with
    { term_op = fo; term_terms = bterms } when opeq fo ifail_op -> true
  | _ -> false

let ivalue_parameter = make_param (token "!value")
let ivalue_op = mk_nuprl5_op [ivalue_parameter]
let ivalue_term_p t =
  match dest_term t with
    { term_op = vo; term_terms = bterms } when opeq vo ivalue_op -> true
  | _ -> false

let ivalue_term t = mk_term ivalue_op [mk_bterm [] t]

let iack_parameter = make_param (token "!ack")
let iack_op = mk_nuprl5_op [iack_parameter]
let iack_term = mk_term iack_op []
let iack_term_p t =
  match dest_term t with
    { term_op = ao; term_terms = bterms } when opeq ao iack_op && nullp bterms -> true
  | _ -> false

let messages_of_iresult_term t =
  match dest_term t with
    { term_op = _; term_terms = _ :: msgs }
       ->  map term_of_unbound_term msgs
  | _ -> error ["orb"; "result"; "messages"] [] [t]

let result_of_iresult_term t =
  match dest_term t with
    { term_op = _; term_terms = r :: _ }
       -> term_of_unbound_term r
  | _ -> error ["orb"; "result"; "messages"] [] [t]

let ipassport_param = make_param (token "!passport")

(*
 * pull desc and bcast
 * find table from bcast type
 * apply to table
 *)
let broadcast_eval env tstamp commit_stamp bcasts =
  print_string "num bcasts "; print_string (string_of_int (List.length bcasts));  print_newline();
  map (function bipass ->
    print_string ".";
    let ipass = term_of_unbound_term bipass in
	(*Mbterm.print_term ipass; print_newline(); *)
    match dest_term ipass with
      { term_op = pop; term_terms = [stamp; desc; bcast] }
      -> (match dest_op pop with
	{ op_name = opn; op_params = [id; ttype] } when parmeq id ipassport_param
	-> (try apply_broadcast
	    (resource env (dest_token_param ttype))
	    (term_of_unbound_term bcast)
	    (term_of_unbound_term desc)
	    tstamp (*(term_to_stamp (term_of_unbound_term stamp))*)
	    commit_stamp
	with Not_found -> print_string "Broadcast for unknown table ignored. ";
	  print_string (dest_token_param ttype); print_newline(); ()
	| e -> (print_string "ap bcast failed"; raise e))
      | _ -> error ["term"; "!passport"; "op"] [] [ipass]
	 )
    | _ -> error ["term"; "!passport"] [] [ipass]
      )
    bcasts

let orb_broadcast env t =
  broadcast_eval env
    (transaction_stamp_of_ibroadcasts_term t)
    (auto_commit_of_ibroadcasts_term t)
    (broadcasts_of_ibroadcasts_term t)

let icommand_parameter = make_param (token "!command")
let icommand_op = mk_nuprl5_op [icommand_parameter]
let icommand_term t = mk_term icommand_op [mk_bterm [] t]

(*
let cmd_of_icommand_term t =
 match dest_term t with { term_op = o; term_terms = [cmd] } when opeq o icommand_op
    -> term_of_unbound_term cmd
 |_ -> error ["orb"; "command"; "not"] [] [t]

*)
let cmd_of_icommand_term t =
 match dest_term t with { term_op = o; term_terms = [cmd] }
    -> (match dest_op o with
      { op_name = opn; op_params = cmdparm :: oppl } when (parmeq cmdparm icommand_parameter)
      -> term_of_unbound_term cmd
    |_ -> error ["orb"; "command"; "op"; "not"] [] [t])
 |_ -> error ["orb"; "command"; "not"] [] [t]

let local_eval f t =
  (print_string "local_eval";
  unconditional_error_handler
    (function () -> (ivalue_term (f (cmd_of_icommand_term t))))
    (function term -> ifail_term term))

(*
let local_eval_new f t =
  unconditional_error_handler
    (function () ->
     (match dest_term t with {term_op = { op_name = opn; op_params = p :: oppl} ; term_terms = bts }
	-> if parmeq p icommand_parameter then (ivalue_term (f (cmd_of_icommand_term t)))
       	   else if parmeq p iexpression_parameter
    (function term -> ifail_term term)
*)

let imsg_parameter = make_param (token "!msg")

let rec bus_wait c tid ehook =
  let t = (Link.recv c.link) in
  match dest_term t  with
    { term_op = op;
      term_terms = bterms }
    -> (match dest_op op with
      { op_name = opn;
 	op_params = ib :: ps } when (parmeq ib ibroadcasts_parameter && nuprl5_opname_p opn)
      -> ((try
	(special_error_handler
	   (function () -> let _ = orb_broadcast (hd c.orb.environments) t in ())
	   (fun s t ->
	     print_string "broadcast failed xyz"
	       ; print_newline()
	       ; print_string s
	       ; print_newline()
	       ; Mbterm.print_term t
	       ; print_newline()))
      with Not_found -> (print_string "broadcast failed notfound"; print_newline())
      | _-> (print_string "broadcast failed abc"; print_newline()))
	       (*
		   (try ((orb_broadcast (hd c.orb.environments) t); ())
		   with Not_found -> (print_string "broadcast failed notfound"; print_newline())
		   | _-> (print_string "broadcast failed"; print_newline()))
		*)
		(* above assumes single environment is present, nfg if more than one env. *)
            ; bus_wait c tid ehook)
    | { op_name = opn;
	op_params = ireq :: ps } when (nuprl5_opname_p opn && parmeq ireq ireq_parameter)
      -> (match tid with
	None ->
	  (Link.send c.link
	     (irsp_term (hd ps)
		(local_eval ehook
		   (term_of_unbound_term (hd bterms))))
	     ; ivoid_term)

      | Some ttid ->
	  (Link.send c.link
	     (if not (tideq ttid (term_of_unbound_term (hd (tl (bterms)))))
	     then (print_string "not tideq"; irsp_term (hd ps)
		     (ifail_term (imessage_term ["orb"; "req"; "recursive"; "tid"] [] [])))
	     else irsp_term (hd ps)
		 (local_eval ehook
		    (term_of_unbound_term (hd bterms))))
	     ; bus_wait c tid ehook))
    | { op_name = opn;
	op_params = imsg :: ps } when (nuprl5_opname_p opn && parmeq imsg imsg_parameter)
      -> ( Mbterm.print_term t
	    ; bus_wait c tid ehook)
    | _ -> t)

(* presence of tid has connotation to lib. mainly that the lib eval is non-local.
   But evals to join lib env are local thus it needs to be optional
   another connotation might be that there is a transaction.
   could be a good place to check for liveness.
 *)
let bus_eval c addr expr tid ehook =
  let link = c.link in
  let seq = sequence () in

  Link.send link (ireq_term seq addr expr tid);
  let t = bus_wait c (Some tid) ehook in
  if not (inteq seq (seq_of_irsp_term t))
  then (Mbterm.print_term t; Mbterm.print_term expr; error ["bus"; "eval"; "sequence"] [] [t])
  else result_of_irsp_term t

let iinform_parameter = make_param (token "!inform")
let iinform_op = mk_nuprl5_op [iinform_parameter]
let iinform_term info = mk_term iinform_op [mk_bterm [] info]
let iinform_term_p t =
  match dest_term t with
    { term_op = ino; term_terms = bterms } when opeq ino iinform_op -> true
  | _ -> false

let info_of_iinform_term t = one_subterm t
let ienvironment_address_parameter = make_param (token "!environment_address")
let ienvironment_address_op pl = mk_nuprl5_op (ienvironment_address_parameter :: pl)
let ienvironment_address_term a =
 mk_term (ienvironment_address_op
	    (map (function s -> make_param (token s)) a))
    []

let address_of_ienvironment_address_term t =
  (match dest_term t with
    { term_op = op;
      term_terms =  bts } when nullp bts
    -> (match dest_op op with
      { op_name = opn;
	op_params = ienv :: al } when (nuprl5_opname_p opn && parmeq ienv ienvironment_address_parameter)
      ->  (map token_parameter_to_string al)
    |_-> error ["orb"; "term"; "EnvironmentAddress"; "invalid"; "op"] [] [t])
  |_-> error ["orb"; "term"; "EnvironmentAddress"; "invalid"; "subterms"; token_parameter_to_string (hd (parameters_of_term t))] [] [t])

let itable_types_parameter = make_param (token "!table_types" )
let itable_types_op pl = mk_nuprl5_op (itable_types_parameter :: pl)
let itable_types_term types address =
  mk_term (itable_types_op
	     (map (function s -> make_param (token s)) types))
    [mk_bterm [] (ienvironment_address_term address)]

let iconfigure_parameter = make_param (token "!configure")
let iconfigure_op = mk_nuprl5_op [iconfigure_parameter]
let iconfigure_term term =  mk_term iconfigure_op [mk_bterm [] term]

let command_of_iconfigure_term t =
  match dest_term t with
    { term_op = co; term_terms = [bterm] } when opeq co iconfigure_op
      -> (match dest_bterm bterm with
	   { bvars = []; bterm = term} -> term
         | _ -> error ["orb"; "term"; "configure"; "subterm"] [] [t])
  | _ -> error ["orb"; "term"; "configure"] [] [t]

let default_ehook t = error ["orb"; "RequestNotExpected"] [] []

(* orb-send-configure orb-send-configure-blink *)
let config_send c term =
  let rsp = bus_eval c [] (iconfigure_term term) ivoid_term default_ehook in
  if ifail_term_p rsp then
    error ["orb"; "configure"; "send"; "fail"] [] [term]
  else rsp

let config_send_state c term =
  let rsp = config_send c term in
  if not (iack_term_p rsp) then
    error ["orb"; "configure"; "send"; "state"] [] [term]
  else ()

(* orb-send-request orb-send-request-blink *)
let config_send_request c term =
  let cmd = command_of_iconfigure_term (config_send c term) in
    if iinform_term_p cmd
      then cmd
    else error ["orb"; "configure"; "send"; "request"] [] [cmd]

let irequest_parameter = make_param (token "!request")
let irequest_op = mk_nuprl5_op [irequest_parameter]
let irequest_term t = mk_term irequest_op [mk_bterm [] t]


let itokens_term toks =
    let param_list = List.append [(make_param (token "!tokens"))] (List.map
	(function s -> make_param (token s)) toks) in
    mk_term (mk_nuprl5_op param_list) []

let ilink_describe_environment_term address description =
    mk_term (mk_nuprl5_op [(make_param (token "!link_describe_environment"))])
      [(mk_bterm [] address); (mk_bterm [] description)]

let iconnect_environments_term source dest =
    mk_term (mk_nuprl5_op [(make_param (token "!connect_environments"))])
      [(mk_bterm [] source); (mk_bterm [] dest)]

let ilink_environment_properties_term addr prop=
  mk_term (mk_nuprl5_op
	     [(make_param (token "!link_environment_properties"))])
    [(mk_bterm [] addr); (mk_bterm [] prop)]

let ibool_term b =
  mk_term (mk_nuprl5_op
	     [(make_param (token "!bool"));
	       (make_bool_parameter b)]) []

let orb_mini_set_idle connection addr b =
  config_send_state connection
	   (iinform_term
	      (ilink_environment_properties_term
		 (itokens_term addr)
		 (Basic.iproperty_term ("idle", (ibool_term b)))))

let orb_mini_connect_environments connection remotea locala =
    config_send_state connection
	   (iinform_term
		 (iconnect_environments_term (itokens_term locala)
		    (itokens_term remotea)))

let orb_mini_describe connection remotea locala locald =
  let remoted = info_of_iinform_term
      (config_send_request connection
	 (irequest_term
	    (iinform_term
	       (ilink_describe_environment_term (itokens_term remotea)
		  (ivoid_term))))) in

  config_send_state connection
       (iinform_term
	  (ilink_describe_environment_term (itokens_term locala)
	     locald));
  remoted

let idisconnect_parameter = make_param (token "!disconnect")
let idisconnect_op b = mk_nuprl5_op [idisconnect_parameter; make_bool_parameter b]
let idisconnect_term b = mk_term (idisconnect_op b) []

(* unused
let iconnect_parameter = make_param (token "!connect")
let iconnect_op localhost sock =
  mk_nuprl5_op
    [ iconnect_parameter;
      make_param (Number (Lm_num.num_of_int sock));
      make_param (String localhost)
    ]

let iconnect_term localhost sock = mk_term (iconnect_op localhost sock) []
*)

let ilink_encoding_term encoding =
  mk_term (mk_nuprl5_op
    [ (make_param (token "!link_encoding"));
      (make_param (token encoding))
    ]) []

let connect_aux orb host rport =
   let tlink = Link.connect_with_callback host rport in
   let tcon = { link = tlink; orb = orb; ro_address = [] } in
   print_term (iinform_term (ilink_encoding_term "MATHBUS"));
   (try (config_send_state tcon (iinform_term (ilink_encoding_term "MATHBUS")))
   with
   _ -> (config_send_state tcon (iinform_term (idisconnect_term true));
   error ["orb"; "connect"; "fail"] [] []
   ));
  tlink

let db_pathname = ref "/home/nuprl/nuprl5/NuPrlDB"

let connect orb name host rport =
  db_init !db_pathname true;
  let tlink = connect_aux orb host rport in
  let tcon = { link = tlink; orb = orb; ro_address = [] } in

  let address = address_of_ienvironment_address_term
      (info_of_iinform_term
	 (config_send_request tcon
	 (irequest_term (iinform_term (ienvironment_address_term [name]))))) in

  let connection =  { link = tlink; orb = orb; ro_address = address } in
  orb.connections <- (connection :: orb.connections);
  config_send_state connection (iinform_term (ienvironment_address_term ["JPROVER"]));
  connection

let irevoke_parameter = make_param (token "!revoke")
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

let iml_parameter = make_param (token "!ML")
let iml_op pl = mk_nuprl5_op (iml_parameter :: pl)

let iml_woargs_parameter = make_param (token "!ML_woargs")
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


let iexpression_parameter = make_param (token "!expression")
let iexpression_op = mk_nuprl5_op [iexpression_parameter]
let iexpression_term term =
 mk_term iexpression_op
	 [mk_bterm [] term]

let iml_expression_term result_p expr args =
 if nullp args
    then iexpression_term (iml_woargs_term result_p expr)
    else iexpression_term (iml_term result_p expr args)

(* evals in remote orb env. *)
let connection_eval_string c s result_p =
  let result = bus_eval c c.ro_address
			(iexpression_term (iml_woargs_term result_p (itext_term s)))
			ivoid_term
			default_ehook in
    if ifail_term_p result
      then error ["orb"; "connection"; "eval"; "string"] [] [result]
      else if ivalue_term_p result
	  then (result_of_iresult_term result)
	  else (if result_p then error ["orb"; "connection"; "eval"; "string"; "value"; "not"] [] [result]
			    else result)

let connection_eval_args c t tl  =
  let result = bus_eval c c.ro_address
 			(iml_expression_term true t tl)
			ivoid_term
			default_ehook in
    if (ifail_term_p result)
      then error ["orb"; "connection"; "eval"; "args"] [] [result]
      else if ivalue_term_p result
	  then (result_of_iresult_term result)
	  else (Mbterm.print_term result
		; Mbterm.print_term t
	  	; error ["orb"; "connection"; "eval"; "args"; "value"; "not"] [] [result])



(* open/close save/restore join/leave *)

(* open *)
let library_environment_new c tag =
  address_of_ienvironment_address_term
    (connection_eval_string c ("ienvironment_address_term (new_library `" ^ tag ^ "`)") true)

(* join *)
(* unused
let library_environment_join c tags =
  address_of_ienvironment_address_term
    (connection_eval_args c
	(itext_term
	  "\\l. (ienvironment_address_term (library_open
	   (tags_of_ienvironment_address_term (hd l))))")
	[ienvironment_address_term tags]
	)
*)

(* restore *)
let library_environment_restore c stamp_string =
  address_of_ienvironment_address_term
    (connection_eval_string c ("ienvironment_address_term (open_environment \"" ^ stamp_string ^ "\")") true)



let library_environment_close c addr =
  string_of_istring_term
    (connection_eval_args c
	(itext_term
	 "\\args. istring_term (hd (close_environment (tags_of_ienvironment_address_term (hd args)) false ))")
	[ienvironment_address_term addr])

let metaprl_description_term =
  mk_term (mk_nuprl5_op
		 [make_param (token "!description"); make_param (token "metaprl")])
	[ mk_bterm [] (inatural_term 0)
	; mk_bterm [] (list_to_ilist_map itoken_term ["REFINER"; "ObjectIdDAG"])
	]

let jprover_description_term =
  mk_term (mk_nuprl5_op
		 [make_param (token "!description"); make_param (token "metaprl")])
	[ mk_bterm [] (inatural_term 0)
	; mk_bterm [] (itoken_term "JPROVER")
	]

let current_description_term = ref metaprl_description_term

let istart_op = mk_nuprl5_op [make_param (token "!start")]

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
      term_terms = [_; _; _; bs] } when opeq o istart_op
     -> (term_of_unbound_term bs)
  |_ ->
	 (*print_term s;*) error ["orb"; "start"; "broadcasts"; "not"; "start"] [] [s]

let start_broadcasts e =
  let t =
    config_send_request
      e.connection
      (irequest_term
        (istart_term (itable_types_term (map fst e.resources) e.re_address)
		     (stamp_to_term e.stamp)
		     (ienvironment_address_term ((e.connection).orb).lo_address)
			  (* nfg if we allow mulitple envs *)
		     !current_description_term))

	in  print_string "start_broadcasts : ";
		let _ = orb_broadcast e (broadcasts_of_istart_term
                            (info_of_iinform_term t))
      in ()

let irevoke_op = mk_nuprl5_op [make_param (token "!revoke")]
let irevoke_term t = mk_term irevoke_op [mk_bterm [] t]

let stop_broadcasts e =
    config_send_state
      e.connection
      (irevoke_term
        (istart_term (itable_types_term (map fst e.resources) e.re_address)
		     (stamp_to_term e.stamp)
		     (ienvironment_address_term ((e.connection).orb).lo_address)
			  (* nfg if we allow mulitple envs *)
		     ivoid_term))

let open_library_environment connection lib_id ehook =
  let lib_env_address = library_environment_new connection lib_id in
    let env =
	{ connection = connection
	; re_address = lib_env_address
	; le_address = []	(* TODO: ?? :: orb_address *)
	; stamp = new_stamp ()
	; ehook = ehook
	; resources = [("TERMS", make_termtable())]
	} in
    (connection.orb).environments <- env :: connection.orb.environments;
    start_broadcasts env;
    env


let join_library_environment con mneumonic ehook =

  let _ = orb_mini_describe con con.ro_address mneumonic !current_description_term in (* we ignore remote description now *)
  orb_mini_connect_environments con con.ro_address mneumonic;
  orb_mini_set_idle con mneumonic false;
  let env =
	{ connection = con
	; re_address = con.ro_address
	; le_address = []
	; stamp = new_stamp ()
	; ehook = ehook
	; resources = [("TERMS", make_termtable())]
	} in
    (con.orb).environments <- env :: con.orb.environments;
    (* start_broadcasts env; *)
  env

let restore_library_environment connection sstamp ehook =
  let lib_env_address = library_environment_restore connection sstamp in
    let env =
	{ connection = connection
	; re_address = lib_env_address
	; le_address = []	(* TODO: ?? :: orb_address *)
	; stamp = new_stamp ()
	; ehook = ehook
	; resources = [("TERMS", make_termtable())]
	} in
    (connection.orb).environments <- env :: connection.orb.environments;
    start_broadcasts env;
    env

let close_library_environment e =
  stop_broadcasts e;
  (let s = library_environment_close e.connection e.re_address in
    (let orb = e.connection.orb in
      orb.environments <- remove e orb.environments);
    s)

let leave_library_environment e =
  stop_broadcasts e;
  (* at them moment no-op but when le_address has meaning then should revoke *)
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
    else if (iack_term_p result)
      then result
    else (Mbterm.print_term result; result)



let eval_string e tid s =
  let _ =
    orb_eval
      false
      e
      (iml_expression_term false (itext_term s) [])
      tid
      default_ehook
  in ()

let eval e tid t =
  let _ = orb_eval false e (iml_expression_term false t []) tid default_ehook
  in ()

let eval_args e tid t tl =
  let _ = orb_eval false e (iml_expression_term false t tl) tid default_ehook
  in ()

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
 *	This keeps result wrappers (eg !ack) in orb.
 *)


let itransaction_parameter = make_param (token "!transaction")
let itransaction_term b = mk_term (mk_nuprl5_op [itransaction_parameter; make_bool_parameter b]) []

let eval_callback checkpointp e tid f =
  let _ =
    orb_eval false e (icommand_term (itransaction_term checkpointp))
	   tid
	   (function term ->
		  (f term) ; iack_term)
  in ()

let with_fail_protect g f =
  let a = null_oref ()
  and err = null_oref() in

  try (
    g (function b ->
         let _ = oref_set a
	            (try f b
	             with e -> let _ = oref_set err e in raise e)
         in ()
      );

    oref_val a)

  with e ->
	if oref_p err
	   then raise (oref_val err)
	   else raise e

(*
let eval_with_callback e tid f t =
 orb_eval false e (iml_expression_term false t [])
	   tid
	   (function term ->
		(f (cmd_of_icommand_term term)); iack_term)
 ; ()

let eval_to_term_with_callback e tid f t =
 (orb_eval true e (iml_expression_term true t [])
	  tid
	  (function term -> (f (cmd_of_icommand_term term)); iack_term))

let eval_args_with_callback e tid f t tl =
  orb_eval false e (iml_expression_term false t tl)
	   tid
	   (function term -> (f (cmd_of_icommand_term term)); iack_term)
 ; ()

let eval_args_to_term_with_callback e tid f t tl =
 (orb_eval true e (iml_expression_term true t tl)
	  tid
	  (function term -> (f (cmd_of_icommand_term term)); iack_term))

*)



let quit_loop = oref false

let iquit_loop_op = mk_nuprl5_op [ make_param (token "!quit_loop")]

let quit_loop_term_p t =
  match dest_term t with
    { term_op = qlo; term_terms = bterms } when opeq qlo iquit_loop_op -> true
  | _ -> false


let quit_hook ehook =
 function t ->
  if quit_loop_term_p t
     then (let _ = oref_set quit_loop true in ivoid_term)
     else ehook t


let orb_req_loop env =

  while (not (oref_val quit_loop))
  do let _ = bus_wait env.connection None
                      (quit_hook env.ehook)
     in ()
  done
