
open Utils
open List
open Term
open Basic
(*open Tent *)
open Orb

(* open Table *)

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


let with_transaction lib f =
  let result = null_oref ()
  and tid = tid() in

  ((eval_with_callback
    lib.environment
    tid
    (function t -> 
      (
	( oref_set result 
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
 
let create t ttype init_term init_props =
 require_remote_transaction t;
 oid_of_ioid_term (eval_args_to_term t.library.environment t.tid
			 	(itext_term "NL0_create")
				[ (itoken_term ttype)
				; ioption_term init_term
				; (list_to_ilist_map (function p ->
							(iproperty_term p))
						     init_props)
				]) 


let delete_strong t oid =
  require_remote_transaction t;
  (eval_args t.library.environment t.tid
		(itext_term "NL0_delete_strong")
		[ (ioid_term oid) ])

let put_term t oid term =
  require_remote_transaction t;
  (eval_args t.library.environment t.tid
		(itext_term "NL0_put_term")
		[ (ioid_term oid)
		; term
		])
  
let get_term t oid =
  require_remote_transaction t;
  (eval_args_to_term t.library.environment t.tid
		(itext_term "NL0_get_term")
		[ (ioid_term oid)
		])
  

let put_property l oid s term =
  (eval_args l.environment (tid ())
		(itext_term "NL0_put_prop")
		[ (ioid_term oid)
		; (itext_term s)
		; term
		])
  
let get_property l oid s =
  (eval_args_to_term l.environment (tid ())
		(itext_term "NL0_get_prop")
		[ (ioid_term oid)
		; (itext_term s)
		])
  


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



let create_top t ttype =
 require_remote_transaction t;
 oid_of_ioid_term (eval_args_to_term t.library.environment t.tid
				(itext_term "NL0_create_top")
				[ (itext_term ttype)
				])


