
open Utils
open List
open Term
open Basic
(*open Tent *)
open Orb
open Definition

type transaction_type = SAVE | RESTORE | LOCAL | REMOTE

type library =
	{ mutable transactions : transaction list
	; environment 	: environment
	(*
	; lemmas : table
	; rewrites : table
	*)
	}

and transaction =
	{ library	: library
	; tbegin	: stamp
	; tent_collect	: unit (* tent *) list
	; ttype		: transaction_type
	; tid		: bound_term
	}


type connection = Orb.connection

let orbr = null_oref ()

let init_orb () = 
  if (not (oref_p orbr))
     then (oref_set orbr (orb_open "foo"); () )
     else ()

let connect remote_host remote_port local_port =
 init_orb();
 Orb.connect (oref_val orbr) remote_host remote_port local_port 

let disconnect c = Orb.disconnect (oref_val orbr) c

let lib_open c =
	{ transactions = []
	; environment =
	     open_library_environment
		c
		""
		(fun ts acs b -> 
		  match b with
		      [] -> ()
		      |_ -> (error ["library"; "BroadcastsNotCurrentlySupported"] [] [iterm_bterms b]))
		(* todo ivoid_term is kludge. need to find a type for error that allows this??? *)
		(function t -> (error ["library"; "LocalEvalNotCurrentlySupported"] [] [t]))
	}

(*
 * NL0_save () -> <stamp-term{begin}>
 *)

let temp_lib_open c s =
	{ transactions = []
	; environment =
	     open_library_environment
		c
		s	
		(fun ts acs b -> 
		  match b with
		      [] -> ()
		      |_ -> (error ["library"; "BroadcastsNotCurrentlySupported"] [] [iterm_bterms b]))
		(* todo ivoid_term is kludge. need to find a type for error that allows this??? *)
		(function t -> (error ["library"; "LocalEvalNotCurrentlySupported"] [] [t]))
	}

let lib_join c tags =
	{ transactions = []
	; environment =
	     join_library_environment
		c
		tags	
		(fun ts acs b -> 
		  match b with
		      [] -> ()
		      |_ -> (error ["library"; "BroadcastsNotCurrentlySupported"] [] [iterm_bterms b]))
		(* todo ivoid_term is kludge. need to find a type for error that allows this??? *)
		(function t -> (error ["library"; "LocalEvalNotCurrentlySupported"] [] [t]))
	}


let save l f =
  let s = null_oref 
  and e = l.environment
  and tid = tid () in
    string_of_itext_term
      (eval_to_term_with_callback
	e 
	tid
	(function term -> 
          (let transaction = 
		{ library = l
		; tbegin = term_to_stamp term
		; tent_collect = []
		; ttype = SAVE
		; tid = tid
		} in
            (f transaction)))
	(itext_term "NL0_transaction ()"))    


(* TODO: FTTB, this is a callback to lib, however oids should be cached locally, ie oid table 
 *  or lib-table with oids.
 *)

let oid_export transaction oid =
  (if not (transaction.ttype = SAVE)
     then error ["library"; "OidExport"] [oid] []);

  string_of_itext_term
    (eval_args_to_term
      transaction.library.environment
      transaction.tid
      (itext_term "NL0_oid_export") [(ioid_term oid)])

(* TODO: FTTB this is done in two steps : 1st restore lib, second start a transaction
 *  to import oids. Later either do in one step or as for export second step is local.
 *)

let restore c checkpoint f =
  (* restore lib. *)
  let lib = 
	{ transactions = []
	; environment = 
	    open_library_environment
		c
		checkpoint
		(fun ts acs b -> 
		  error ["library"; "restore"; "BroadcastsNotCurrentlySupported"]
			[] [iterm_bterms b]) 
		(function t -> 
		  error ["library"; "restore"; "LocalEvalNotCurrentlySupported"] [] [t])
	} 
  and tid = tid() in

    (* import *)
    (eval_with_callback
      lib.environment
      tid
      (function term -> 
	(let transaction =	 
		{ library = lib
		; tbegin = term_to_stamp term
		; tent_collect = []
		; ttype = RESTORE
		; tid = tid
		} in
	  (f transaction)))
      (itext_term "NL0_transaction ()"));
      lib


let oid_import transaction s =
  (if not (transaction.ttype = RESTORE)
     then error ["library"; "OidImport"] [] [itext_term s]);

  oid_of_ioid_term
    (eval_args_to_term
      transaction.library.environment
      transaction.tid
      (itext_term "NL0_oid_import") [(itext_term s)])


let lib_close lib =
 close_library_environment lib.environment

let temp_lib_close lib =
 close_library_environment lib.environment

let lib_leave lib =
 leave_library_environment lib.environment


let with_transaction lib f =
  let result = null_oref ()
  and tid = tid() in

  ((eval_with_callback
    lib.environment
    tid
    (function t -> 
      ( ( oref_set result 
	  (f	{ library = lib
		; tbegin = term_to_stamp t
		; tent_collect = []
		; ttype = REMOTE
		; tid = tid
		}))
	)
      ; ())
    (itext_term "NL0_transaction ()"));

  oref_val result)


let with_local_transaction lib f =
  f	{ library = lib
	; tbegin = new_stamp()
	; tent_collect = []
	; ttype = LOCAL
	; tid = tid ()
	}



let require_remote_transaction t =
 if not (t.ttype = REMOTE) then error ["library"; "transaction"; "local"] [] [];
 ()
 
let iml_cons_op = (mk_nuprl5_op [make_param (Token "!ml_text_cons")])
let iml_cons_term = icons_term iml_cons_op



(* 
 todo whoever calls commit needs to add tent to collect queue
*)


 
(* tree notes.
 *
 *  root objects not collectable all others are.
 *  objects created by create command are not collectable.
 *
 *  in order to prevent cycles by concurrent transactions when one directory is inserted in
 *  another the path to the root of the dir being inserted into must be locked.
 *
 *  we can prevent dup roots by locking all roots at make_root or equivalently by having
 *  special root object which contains all roots and locking that.
 *)


(*
 val make_root		: transaction -> string -> object_id
 val remove_root	: transaction -> object_id -> unit

 val make_directory	: transaction -> object_id -> string -> object_id
 val remove_directory	: transaction -> object_id -> string -> unit

 val nuprl_string_ap	: term (* f *) -> string -> term (* f *)
*)


(* there are several possible methodologies for calling nuprl5 functions. 
 *   - One is to code cororallies for each nuprl-light function which unmarshalls
 *     predictable args and then calls appropriate v5 function.
 *   - Another method is to write abstract v5 unmarshall funcs and then in
 *     nuprl-light construct text to call unmarshall funcs. Nl funcs 
 *	which construct unmarshall call will do marshall as arg as well.
 *	This is less error prone then first method.
 *  - third method is to make abstraction which expands to call. Avoids
 *    marshalling of non-term args and shortens data transmitted.
 *  - fourth is to orthogonal where you send some index to a function 
 *    lookup table. This avoids eval in nuprl5 and would be most efficient.
 *
 * first doesn't have too many pluses. second is nice while developing.
 * last would be best for stable commands. third might be useful as intermediate step.
 *)

(* todo : desire an iml_parens wrap then cons("String_ap"; wrap(f)) *)
let lparen_itext = itext_term "("
let rparen_itext = itext_term ")"
let wrap_parens t = iml_cons_term lparen_itext (iml_cons_term t rparen_itext)

let null_ap_itext = itext_term "Null_ap "
let string_ap_itext = itext_term "String_ap "
let token_ap_itext = itext_term "Token_ap "
let oid_ap_itext = itext_term "Oid_ap "
let term_ap_itext = itext_term "Term_ap "

let null_ap term = (wrap_parens (iml_cons_term null_ap_itext term), [])
let string_ap m s =  (wrap_parens (iml_cons_term string_ap_itext (fst m)), (istring_term s) :: snd m)
let oid_ap m oid  =  (wrap_parens (iml_cons_term oid_ap_itext (fst m)), (ioid_term oid) :: snd m)
let token_ap m s =  (wrap_parens (iml_cons_term token_ap_itext (fst m)), (itoken_term s) :: snd m)
let term_ap m t =  (wrap_parens (iml_cons_term term_ap_itext (fst m)), t :: snd m)

let object_id_ap = oid_ap

let make_ap remote_termto toterm =
 let unmarshall_ap_itext = itext_term remote_termto in
  (fun m v -> 
    wrap_parens (iml_cons_term unmarshall_ap_itext (fst m)), (toterm v) :: snd m)


let abstract_prefix_itext = (itext_term "\l. ")
let abstract_suffix_itext = (itext_term " l")

let with_abstract f g =
  wrap_parens (iml_cons_term abstract_prefix_itext
			     (iml_cons_term f 
					    (wrap_parens (iml_cons_term g abstract_suffix_itext))))


let oid_return_itext = itext_term "ioid_term "
let oid_return m = (with_abstract oid_return_itext (fst m), (snd m))

let string_return_itext = itext_term "istring_term "
let string_return m = (with_abstract string_return_itext (fst m), (snd m))

let token_return_itext = itext_term "(\t. istring_term (tok_to_string t))  "
let token_return m = (with_abstract token_return_itext (fst m), (snd m))

let eval_m t m =
  require_remote_transaction t;
  eval_args t.library.environment t.tid
				(fst m)
				(snd m)
				
let eval_m_to_term t m =
  require_remote_transaction t;
  eval_args_to_term t.library.environment t.tid
				(fst m)
				(snd m)
				
let eval = eval_m
let eval_to_term = eval_m_to_term
let eval_to_string t m = string_of_istring_term (eval_m_to_term t (string_return m))
let eval_to_token t m = string_of_istring_term (eval_m_to_term t (token_return m))
let eval_to_object_id t m = oid_of_ioid_term (eval_m_to_term t (oid_return m))



let make_eval remote_toterm termto =
  let marshall_result_itext = (itext_term remote_toterm) in
  (fun t m ->
       termto (eval_to_term t 
		(with_abstract marshall_result_itext
			       (fst m),
		 (snd m))))

let term_to_properties t = map_isexpr_to_list property_of_iproperty_term t
let properties_to_term props = list_to_ilist_map iproperty_term props

let activate_ap = null_ap (itext_term "activate ")
let activate t oid = eval t (oid_ap activate_ap oid)

let deactivate_ap = null_ap (itext_term "activate ")
let deactivate t oid = eval t (oid_ap activate_ap oid)

let allow_ap = null_ap (itext_term "allow_collection ")
let allow_collection t oid = eval t (oid_ap allow_ap oid)

let disallow_ap = null_ap (itext_term "disallow_collection ")
let disallow_collection t oid = eval t (oid_ap disallow_ap oid)



let create_ap = null_ap (itext_term "NL0_create ")
let create t ttype init_term init_props =
  eval_to_object_id t
    (term_ap 
      (term_ap
	(token_ap create_ap ttype)
	(ioption_term init_term))
      (properties_to_term init_props))

let delete_strong_ap = null_ap (itext_term "delete_strong ")
let delete_strong t oid =
  eval t (oid_ap delete_strong_ap oid)

let delete_ap = null_ap (itext_term "allow_collection ")
let delete t oid =
  eval t (oid_ap delete_ap oid)
  
let put_term_ap = null_ap (itext_term "put_term ")
let put_term t oid term =
  eval t (term_ap (oid_ap put_term_ap oid) term)

let get_term_ap = null_ap (itext_term "get_term ")
let get_term t oid = 
  eval_to_term t (oid_ap get_term_ap oid)

let put_property_ap = null_ap (itext_term "put_property ")
let put_property t oid s term =
  eval t 
    (term_ap (token_ap (oid_ap put_property_ap oid) s) term)

let remove_property_ap = null_ap (itext_term "remove_property ")
let remove_property t oid s =
  eval t 
    (token_ap (oid_ap remove_property_ap oid) s)

let get_property_ap = null_ap (itext_term "get_property ")
let get_property t oid s =
  eval_to_term t 
    (token_ap (oid_ap get_property_ap oid) s)


let put_properties_ap = null_ap (itext_term "\oid iprops. set_properties oid (term_to_property_list iprops) ")
let put_properties t oid props =
  eval t
    (term_ap (oid_ap put_properties_ap oid) 
	(properties_to_term props))

let get_properties_ap = null_ap (itext_term "\oid . property_list_to_term (get_properties oid) ")
let get_properties t oid =
  term_to_properties
    (eval_to_term t
      (oid_ap put_properties_ap oid))


let make_root_ap = null_ap (itext_term "dag_make_root ")
let make_root t name =
  eval_to_object_id t
    (token_ap make_root_ap name)

let remove_root_ap = null_ap (itext_term "dag_remove_root ")
let remove_root t oid =
  eval t
    (oid_ap remove_root_ap oid)


let make_directory_ap = null_ap (itext_term "dag_make_directory ")
let make_directory t oid s =
  eval_to_object_id t 
    (token_ap
      (oid_ap make_directory_ap oid)
      s)

let remove_directory_ap = null_ap (itext_term "dag_remove_directory ")
let remove_directory t oid s =
  eval t 
    (token_ap
      (oid_ap remove_directory_ap oid)
      s)


let make_leaf_ap = null_ap (itext_term "dag_make_leaf ")
let make_leaf t oid s =
  eval_to_object_id t 
    (token_ap
      (oid_ap make_leaf_ap oid)
      s)

let remove_leaf_ap = null_ap (itext_term "dag_remove_leaf ")
let remove_leaf t oid s =
  eval t 
    (token_ap
      (oid_ap remove_leaf_ap oid)
      s)

let insert_ap = null_ap (itext_term "dag_insert")
let insert t parent name oid =
  eval t 
    (oid_ap
      (token_ap (oid_ap insert_ap parent)
		name)
      oid)

let insert_leaf_ap = null_ap (itext_term "dag_insert_leaf")
let insert_leaf t parent name typ data =
  eval_to_object_id t 
    (term_ap
      (token_ap
	(token_ap (oid_ap insert_leaf_ap parent)
		  name)
	typ)
      data)

let roots t =
  Definition.roots (resource t.library.environment "TERMS") t.tbegin

let directory_p t oid =
  Definition.directory_p (resource t.library.environment "TERMS") t.tbegin oid

let children t oid =
  Definition.directory_children (resource t.library.environment "TERMS") t.tbegin oid

let root t name = (assoc name (roots t))


