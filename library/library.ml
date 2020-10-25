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

open Utils
open List
open Term_sig
open Refiner.Refiner.Term
open Refiner.Refiner.TermType
open Basic
open Nuprl5
open Orb
open Opname

let _ =
   show_loading "Loading Library%t"

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
	; tid		: term
	; mutable cookie	: string
        ; mutable liveness	: bool
        (* dependencies *)
	}

let orbr = null_oref ()

let init_orb () =
  if (not (oref_p orbr)) then
    (let _ = oref_set orbr (orb_open "foo") in () )
  else ()

let connect servername remote_host remote_port =
 init_orb();
 Orb.connect (oref_val orbr) servername remote_host remote_port

let disconnect c = Orb.disconnect (oref_val orbr) c

(*	test_ehook
 *
 *	  - add : term -> term -> term list
 *
 *
 *)

let faux_mbs bterms =
  let term = term_of_unbound_term (hd bterms) in
  print_newline();
  print_string "mbs";
  print_newline();
  term

(* unused
let faux_ascii bterms =
  let persist = term_of_unbound_term (hd bterms) and
      data = term_of_unbound_term (hd (tl bterms)) in
  let s, y = Db.stamp_and_type_of_idata_persist_term persist in
  let data' = Db.db_read s y in
  print_newline();
  if alpha_equal data data' then print_string "+" else print_string "-" ;
  print_newline();
  data'
*)

let faux_ascii_quick bterms =
  let persist = term_of_unbound_term (hd bterms) in
  let s, y = Db.stamp_and_type_of_idata_persist_term persist in
  let data = Db.db_read s y in
  print_newline();
  print_string "faq";
  print_newline();
  data

(* unused
let faux_ascii_file bterms =
  let persist = term_of_unbound_term (hd bterms) in
  let s, y = Db.stamp_and_type_of_idata_persist_term persist in
  let data = Db.db_read s y in
  print_newline();
  print_string "faf";
  print_newline();
  let filename = "/amd/noon/y/nuprl/nuprll/nuprl-light/library/mbnode.txt" in
  Mbterm.write_node_to_file (Mbterm.mbterm_of_term data) filename;
  print_newline();
  print_string "aw";
  print_newline();
  itoken_term filename
*)

let faux_refine bterms =
  let seq = term_of_unbound_term (hd bterms) in
  print_newline();
  Mbterm.print_term seq;
  print_newline();
  list_to_ilist [seq; seq]

let test_do_add bterms =
  let r =
    inatural_term
      (fold_left (fun i j -> i + j)
	 0
	 (map (function bt -> number_of_inatural_term (term_of_unbound_term bt)) bterms)) in
  print_newline();
  Mbterm.print_term r;
  print_newline();
  r

let token s = Token (mk_opname s nil_opname)

let test_add_op = mk_nuprl5_op [ make_param (token "!test_add")]
let faux_refiner_op = mk_nuprl5_op [ make_param (token "!faux_refine")]
let refiner_op = mk_nuprl5_op [ make_param (token "!refine")]
let faux_ascii_op = mk_nuprl5_op [ make_param (token "!faux_ascii")]
let faux_mbs_op = mk_nuprl5_op [ make_param (token "!faux_mbs")]

let test_ehook t =
  match dest_term t with
    {term_op = op; term_terms = bterms } when opeq op test_add_op
    -> test_do_add bterms
  | {term_op = op; term_terms = bterms } when opeq op faux_refiner_op
    ->  faux_refine bterms
  | {term_op = op; term_terms = bterms } when opeq op refiner_op
    ->  faux_refine bterms
  | {term_op = op; term_terms = bterms } when opeq op faux_ascii_op
    ->  faux_ascii_quick bterms
  | {term_op = op; term_terms = bterms } when opeq op faux_mbs_op
    ->  faux_mbs bterms
  | _ -> error ["eval"; "op"; "unrecognized"] [] [t]

(* unused
let error_ehook t = (error ["library"; "LocalEvalNotCurrentlySupported"] [] [t])
*)

let lib_new c s =
  { transactions = [];
    environment = open_library_environment c s test_ehook
  }

let join c tags =
  { transactions = [];
    environment =
    join_library_environment c tags test_ehook
  }

let join_eval c tags ehook =
  { transactions = [];
    environment = join_library_environment c tags ehook
  }

(* TODO: FTTB this is done in two steps : 1st restore lib, second start a transaction
 *  to import oids. Later either do in one step or as for export second step is local.
 *)

let icallback_param =  make_param (token "!callback")
(* unused
let icallback_op =  mk_nuprl5_op [icallback_param]
*)

let cookie_of_icallback_term t =
  match dest_term t with
    { term_op = op; term_terms = [s] }
    ->  (match dest_op op with
      { op_name = opn; op_params = [icp; ckp] } when (nuprl5_opname_p opn && parmeq icp icallback_param)
      -> (match dest_param ckp with
	String s -> s
      |_ -> error ["icallback"; "not"; "param"] [] [t])
    |_ -> error ["icallback"; "not"; "op"] [] [t])
  |_ -> error ["icallback"; "not"; "term"] [] [t]

let stamp_of_icallback_term t =
  match dest_term t with
    { term_op = op; term_terms = [s] }
    -> (match dest_op op with
      { op_name = opn; op_params = (icp :: rest) }
      when (nuprl5_opname_p opn && parmeq icp icallback_param)
      ->  (term_of_unbound_term s)
    |_ -> error ["icallback"; "not"; "op"] [] [t])
  |_ -> error ["icallback"; "not"; "term"] [] [t]

let restore c cookie f =
  (* restore lib. *)
  let lib =
    { transactions = [];
      environment =
      restore_library_environment
	c
	cookie
	(function t ->
	  error ["library"; "restore"; "LocalEvalNotCurrentlySupported"] [] [t])
    }
  and tid = tid() in

    (* import *)
  (eval_callback
     false
     lib.environment
     tid
     (function t ->
       let transaction =
	 { library = lib
	     ; tbegin = term_to_stamp (stamp_of_icallback_term t)
	     ; tent_collect = []
	     ; ttype = RESTORE
	     ; tid = tid
	     ; cookie = cookie
 	     ; liveness = true
	 } in
       (f transaction)));
  lib

let with_transaction_aux lib ttype checkp f =
  let tid = tid() in

  with_fail_protect
    (function g ->
      eval_callback checkp lib.environment tid
	(function t ->
          g { library = lib
		; tbegin = term_to_stamp (stamp_of_icallback_term t)
		; tent_collect = []
		; ttype = ttype
		; tid = tid
		; cookie = cookie_of_icallback_term t
 		; liveness = true
	    }))

    (function t ->
      (let result = (f t) in t.liveness <- false; result))

let with_transaction lib f =
  with_transaction_aux lib REMOTE false f

let save l f =
  with_transaction_aux l SAVE true
    (function t ->
      f t;
      t.cookie)

(*
  let tid = tid () in

    with_fail_protect
      (function g ->
        eval_callback true l.environment tid
	  (function t ->
            g { library = l
		; tbegin = term_to_stamp (stamp_of_icallback_term t)
		; tent_collect = []
		; ttype = SAVE
		; tid = tid
		; cookie = cookie_of_icallback_term t
 		; liveness = true
		}))

	(function t ->
	    f t;
	    t.cookie)
*)
(* TODO: FTTB, this is a callback to lib, however oids should be cached locally, ie oid table
 *  or lib-table with oids.
 *)

let lib_close lib = close_library_environment lib.environment
let leave lib = leave_library_environment lib.environment

(*
 transactions active type of thing will prevent transaction from being used outside of
 with_transaction.
 *)

(*
  let tid = tid() in

    with_fail_protect
      (function g ->
	eval_callback false lib.environment tid
	   (function t ->
             g { library = lib
		; tbegin = term_to_stamp (stamp_of_icallback_term t)
		; tent_collect = []
		; ttype = REMOTE
		; tid = tid
		; cookie = ""
 		; liveness = true
		}))
	(function t -> f t)

*)

let with_local_transaction lib f = with_transaction lib f

(* needs fixin! stamps at lookup incomparable.
  f	{ library = lib
	; tbegin = new_stamp()
	; tent_collect = []
	; ttype = LOCAL
	; tid = tid ()
	; cookie = ""
	; liveness = true
	}
*)

let require_live_transaction t =
 if (not t.liveness) then  error ["library"; "transaction"; "dead"] [] []; ()

let require_remote_transaction t =
  require_live_transaction t;
  if (t.ttype = LOCAL) then error ["library"; "transaction"; "local"] [] [];
  ()

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

(* Marshall/Unmarshall *)

(* there are several possible methodologies for calling nuprl5 functions.
 *   - One is to code cororallies for each nuprl-light function which unmarshalls
 *     predictable args and then calls appropriate v5 function.
 *   - Another method is to write abstract v5 unmarshall funcs and then in
 *     nuprl-light construct text to call unmarshall funcs. Mp funcs
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

let iml_cons_op = (mk_nuprl5_op [make_param (token "!ml_text_cons")])
let iml_cons_term = icons_term iml_cons_op

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

let abstract_prefix_itext = (itext_term "\\l. ")
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

let server_loop lib = orb_req_loop lib.environment

let oid_export_ap = null_ap (itext_term "oid_export ")
let oid_import_ap = null_ap (itext_term "oid_import ")

let oid_export transaction oid =
  (if not (transaction.ttype = SAVE) then
    error ["library"; "OidExport"] [oid] []);
  eval_to_string transaction
    (oid_ap (token_ap oid_export_ap transaction.cookie) oid)
    (* (itext_term "\args. istring_term (object_id_to_string (TtoO (hd args)))") [(ioid_term oid)] *)

let oid_import transaction s =
  (if not (transaction.ttype = RESTORE) then
    error ["library"; "OidImport"; "restore"] [] [itext_term s]);
  eval_to_object_id transaction
    (string_ap (token_ap oid_import_ap transaction.cookie) s)

let term_to_properties t = map_isexpr_to_list property_of_iproperty_term t
let properties_to_term props = list_to_ilist_map iproperty_term props

let activate_ap = null_ap (itext_term "activate ")
let activate t oid = eval t (oid_ap activate_ap oid)

let deactivate_ap = null_ap (itext_term "deactivate ")
let deactivate t oid = eval t (oid_ap deactivate_ap oid)

let allow_ap = null_ap (itext_term "allow_collection ")
let allow_collection t oid = eval t (oid_ap allow_ap oid)

let disallow_ap = null_ap (itext_term "disallow_collection ")
let disallow_collection t oid = eval t (oid_ap disallow_ap oid)

let create_ap = null_ap (itext_term "create_with_some_term ")
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

let put_properties_ap = null_ap (itext_term "\\oid iprops. set_properties oid (term_to_property_list iprops) ")
let put_properties t oid props =
  eval t
    (term_ap (oid_ap put_properties_ap oid)
       (properties_to_term props))

let get_properties_ap = null_ap (itext_term "\\oid . property_list_to_term (get_properties oid) ")
let get_properties t oid =
  term_to_properties
    (eval_to_term t
      (oid_ap get_properties_ap oid))

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
let make_leaf t oid name ttype =
  eval_to_object_id t
    (token_ap (token_ap
      (oid_ap make_leaf_ap oid)
      name) ttype)

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

let ninsert_leaf_ap = null_ap (itext_term "dag_ninsert_leaf")
let ninsert_leaf t parent name typ data =
  eval_to_object_id t
    (term_ap
      (token_ap
	(token_ap (oid_ap ninsert_leaf_ap parent)
		  name)
	typ)
      data)

let resource t s =
 require_live_transaction t;
 Orb.resource t.library.environment s

let roots t =
  Definition.roots (resource t "TERMS") t.tbegin

let directory_p t oid =
  Definition.directory_p (resource t "TERMS") t.tbegin oid

let children t oid =
  Definition.directory_children (resource t "TERMS") t.tbegin oid

let root t name =
 try (assoc name (roots t))
 with Not_found -> error ["root";  "Not_found"; name] [][]

let child t oid name =
  let l = (children t oid) in
  try assoc name l
  with Not_found -> error ["child"; "Not_found"; name] [oid][]

let descendent t oid names =
  let rec aux oid names =
    if nullp names then
      oid
    else try aux (assoc (hd names) (children t oid)) (tl names)
    with Not_found -> error ("child" :: "Not_found" :: names) [oid] []
  in aux oid names
