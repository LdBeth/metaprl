(*
 * This file is part of Nuprl-Light, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/index.html for information on Nuprl,
 * OCaml, and more information about this system.
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

(*
 * This implements a filesystem interface to the library.
 *)



open Refiner.Refiner.Term
open Refiner.Refiner.TermType
open Refiner.Refiner.Refine
open Basic

module Shell = Shell.Shell (Shell_p4.ShellP4)

open Shell

open Utils
open Library

open Printf
open Nl_debug
open Nl_num

let _ =
   if !debug_load then
      eprintf "Loading Library_eval%t" eflush

let library = null_oref()
let connection = null_oref()

exception LibraryException of string

let library_close () =
 if oref_p library
    then (leave (oref_val library);
	  disconnect (oref_val connection);
	  oref_nullify library
	 )
    else raise (LibraryException "Close: No library open.")

let lib_open_eval env ehook host localport remoteport =

 (* eprintf "%s %d %d %t" host localport remoteport eflush; *)

 if oref_p library
    then raise (LibraryException "Open: Library already open.")
    else ( oref_set library (join_eval  (oref_set connection (connect host localport remoteport))
					[env]
					ehook
					)
	 ; at_exit (function () -> if oref_p library then library_close()))
 ; ()

(*
 * 
 * NL cmd syntax
 *
 * !nl_list_root{}() returns !tok_cons 
 * !nl_list_module{}(tok_cons) returns !cons{}(!cons(!token{thm:t}(); sequent); !cons ...
 * !nl_thm_create{}(!msequent) returns !void()  
 * !nl_create_set{}(!tok_cons; sequent) returns !void()  
 * !nl_set_goal{}(!tok_cons; sequent) returns !void()  
 * !nl_lookup_proof{}(symaddr) returns !nl_prf(goal; tactic; children; extras) g and ex are sequent, ch are nl_prf
 * !nl_refine{}(symaddr; address; tactic) returns !nl_ref(goal; subgoals; extras) g, subs and ex are sequent terms
 * !nl_undo{}(symb) returns !nl_prf(goal; tactic; children; extras) 
 * 
 *)


let nl_list_root_op = mk_nuprl5_op [ make_param (Token "!nl_list_root")]
let nl_list_module_op = mk_nuprl5_op [ make_param (Token "!nl_list_module")]
let nl_create_op = mk_nuprl5_op [ make_param (Token "!nl_create")]
let nl_create_set_op = mk_nuprl5_op [ make_param (Token "!nl_create_set")]
let nl_set_goal_op = mk_nuprl5_op [ make_param (Token "!nl_set_goal")]
let nl_lookup_op = mk_nuprl5_op [ make_param (Token "!nl_lookup_proof")]
let nl_refine_op = mk_nuprl5_op [ make_param (Token "!nl_refine")]
let nl_undo_op = mk_nuprl5_op [ make_param (Token "!nl_undo")]
let nl_node_op = mk_nuprl5_op [ make_param (Token "!nl_node")]

let refiner_op = mk_nuprl5_op [ make_param (Token "!refine")]

let refine_req_p t = opeq refiner_op (operator_of_term t)

let current_symaddr = ref []

let refine_args t =
  match dest_term t with
    {term_op = op; term_terms = goal :: tac :: r } when (opeq op refiner_op)

    -> (term_of_unbound_term goal, term_of_unbound_term tac)

  | _ -> error ["eval"; "op"; "unrecognized"] [] [t]

let iinf_sequent_op = mk_nuprl5_op [make_param (Token "!inf_sequent"); Basic.inil_parameter]
let iinf_sequent_term hyp term = mk_term iinf_sequent_op [(mk_bterm [] hyp); (mk_bterm [""] term)]

let nl_prf_term g tac s e =
  mk_term (mk_nuprl5_op [make_param (Token "!nl_prf")])
    [(mk_bterm [] g); (mk_bterm [] tac); (mk_bterm [] s); (mk_bterm [] e)]

let nl_ref_term g s e =
  mk_term (mk_nuprl5_op [make_param (Token "!nl_ref")])
    [(mk_bterm [] g); (mk_bterm [] s); (mk_bterm [] e)]

let inatural_cons_op = (mk_nuprl5_op [make_param (Token "!natural_cons")])
let int_list_of_term t =
  map_isexpr_to_list_by_op inatural_cons_op number_of_inatural_term t
    
let string_list_of_term t =
  map_isexpr_to_list_by_op icons_op string_of_itoken_term t

let inl_msequent_op = (mk_nuprl5_op [make_param (Token "!nl_msequent")])
let inl_msequent_term a g = mk_term inl_msequent_op [(mk_bterm [] a); (mk_bterm [] g)]
    
let msequent_to_term mseq = 
  let (goal, hyps) = dest_msequent mseq 
  in inl_msequent_term (list_to_ilist hyps) goal 

let ipair_op = (mk_nuprl5_op [make_param (Token "!pair")])
let ipair_term h t = mk_term ipair_op [(mk_bterm [] h); (mk_bterm [] t)] 
  
let list_of_ints length =
  let rec ll len =
  if len = 0 then [] 
   else len::(ll (len - 1))
 in List.rev (ll length)

open List

let refine_ehook rhook =
  (function t ->
    (* used in undo and lookup cmds *)
    let rec visit_proof root_il =
	let (s, goal, subgoals, extras) = edit_node root_il
	in
	(match s with Some ss -> 
       let length = List.length subgoals
	      in let vps i = 
		 visit_proof (i :: root_il) in
		(nl_prf_term (msequent_to_term goal) (itext_term ss) 
		    (list_to_ilist (List.map vps (list_of_ints length))) 
		    (list_to_ilist (List.map msequent_to_term extras)))
      | None -> (nl_prf_term (msequent_to_term goal) ivoid_term inil_term 
		(list_to_ilist (List.map msequent_to_term extras))))	 
       in 
  (match dest_term t with
    {term_op = op; term_terms = goal :: tac :: r } when (opeq op refiner_op) -> 
      list_to_ilist (rhook (term_of_unbound_term goal) (term_of_unbound_term tac))
  	
  | {term_op = op; term_terms = symaddr :: addr :: tac :: r } when (opeq op nl_refine_op) ->
      (let l = string_list_of_term (term_of_unbound_term symaddr)
      in if l = !current_symaddr then () else Shell.edit_cd_thm (hd (tl l)) (hd (tl (tl l)));
      eprintf "%s %t" (string_of_itext_term (term_of_unbound_term tac)) eflush;
      let (gg, subgoals, extras) = 
      	Shell.edit_refine (int_list_of_term (term_of_unbound_term addr))
  	  (string_of_itext_term (term_of_unbound_term tac)) in
      nl_ref_term (msequent_to_term gg)
      	(list_to_ilist (List.map msequent_to_term subgoals)) 
	   (list_to_ilist (List.map msequent_to_term extras)))

  (* node not needed at command call level*)
  (*| {term_op = op; term_terms = addr :: tac :: r } when (opeq op nl_node_op) -> 
      let (goal, subgoals, extras) = 
      	Shell.edit_refine (int_list_of_term (term_of_unbound_term addr))
  	  (string_of_itext_term tac) 
      in
      icons_term (msequent_to_term goal)
      	(icons_term (list_to_ilist (map msequent_to_term subgoals)) 
	   (list_to_ilist (List.map msequent_to_term extras)))
   *)

  | {term_op = op; term_terms = symaddr :: r } when (opeq op nl_undo_op) ->
      (let l = string_list_of_term (term_of_unbound_term symaddr)
      in 
      if l = !current_symaddr then () else Shell.edit_cd_thm (hd (tl l)) (hd (tl (tl l)));
      Shell.edit_undo ();  
      visit_proof [])

  | {term_op = op; term_terms = [] } when (opeq op nl_list_root_op) ->
      list_to_ilist (map itoken_term (Shell.edit_list_modules ()))

  | {term_op = op; term_terms = symaddr :: r } when (opeq op nl_list_module_op) ->
      let name = (hd (map_isexpr_to_list string_of_itoken_term
			(term_of_unbound_term symaddr)))
      in 
      let f x = (Shell.edit_cd_thm name x;
		let (s, goal, subgoals, extras) = Shell.edit_node [] in
		ipair_term (itoken_term x) (msequent_to_term goal))
      in list_to_ilist (List.map f (Shell.edit_list_module name))

  (*| {term_op = op; term_terms = symaddr :: goal :: r} when (opeq op nl_set_goal_op) ->
      let l = string_list_of_term (term_of_unbound_term symaddr)
      in 
      (if l = !current_symaddr then () else Shell.edit_cd_thm (hd (tl l)) (hd (tl (tl l)));
       Shell.edit_set_goal (term_to_msequent goal); ivoid_term)
 
  | {term_op = op; term_terms = symaddr :: r} when (opeq op nl_thm_create_op) ->
      let l = string_list_of_term (term_of_unbound_term symaddr)
      in 
      (Shell.edit_create_thm (hd (tl l)) (hd (tl (tl l))); ivoid_term)
 *)
 (* cd not needed at command call level*)
 (*
  | {term_op = op; term_terms = symaddr :: r} when (opeq op nl_cd_thm_op) ->
      let l = string_list_of_term (term_of_unbound_term symaddr)
      in 
      (current_symaddr := l; Shell.edit_cd_thm (hd (tl l)) (hd (tl (tl l))); ivoid_term)
  *)  
  | {term_op = op; term_terms = mname :: name :: r} when (opeq op nl_create_op) ->
     (Shell.edit_create_thm (string_of_itoken_term (term_of_unbound_term mname))
			(string_of_itoken_term (term_of_unbound_term name));
     ivoid_term)
  
  | {term_op = op; term_terms = symaddr :: r} when (opeq op nl_lookup_op) ->
      let l = string_list_of_term (term_of_unbound_term symaddr)
      in 
      (if l = !current_symaddr then () 
      else Shell.edit_cd_thm (hd l) (hd (tl l));
      visit_proof [])

  | _ -> error ["eval"; "op"; "unrecognized"] [] [t]))



let library_open_eval name rhook =

  if not (oref_p library)
     then let host = Sys.getenv "NUPRLLIB_HOST"
	  and port = int_of_string (Sys.getenv "NUPRLLIB_PORT")
	  in
	  lib_open_eval name (refine_ehook rhook) host port (port+2)
 ; ()


let library_loop_eval () =

	let lib = oref_val library in

	(with_transaction lib
	   (function t ->
		(eval t
		 (null_ap (itext_term "\l. inform_message nil ``NuprlLight Loop Start`` nil")))))

	; server_loop lib


let library_open_and_loop_eval name rhook =

  library_open_eval name rhook;

  (unwind_error
     (function () -> library_loop_eval ();
	library_close ())
     (function () -> library_close ()))


let faux_refine g t =
    print_newline();
    Mbterm.print_term g;
    Mbterm.print_term t;
    print_newline();
    [g; g]
