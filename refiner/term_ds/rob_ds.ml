(* The unification of terms with bindings based on the ideas of
 * Robinson's  unification algorithm and lasy convertion into dag representation
 *------------------------------------------------------------
 *
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/index.html for information on Nuprl,
 * OCaml, and more information about this system.
 *
 * Copyright (C) 1998 Alexey Nogin, Cornell University
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
 * Author: Vladimir Krupski
 *)

INCLUDE "refine_error.mlh"

open Printf
open Mp_debug
open String_set

open Refine_error_sig
open Term_ds_sig
open Term_ds

module TermSubstRob
(Term : TermDsSig
        with type level_exp_var = TermType.level_exp_var
        with type level_exp = TermType.level_exp
        with type param = TermType.param
        with type operator = TermType.operator
        with type term = TermType.term
        with type term_core = TermType.term_core
        with type bound_term = TermType.bound_term
        with type esequent = TermType.esequent
        with type seq_hyps = TermType.seq_hyps
        with type seq_goals = TermType.seq_goals
        with type hypothesis = TermType.hypothesis

        with type level_exp_var' = TermType.level_exp_var'
        with type level_exp' = TermType.level_exp'
        with type object_id = TermType.object_id
        with type param' = TermType.param'
        with type operator' = TermType.operator'
        with type term' = TermType.term'
        with type bound_term' = TermType.bound_term'

        with type term_subst = TermType.term_subst)
(RefineError : RefineErrorSig
               with type level_exp = TermType.level_exp
               with type param = TermType.param
               with type term = TermType.term
               with type bound_term = TermType.bound_term)
=
struct

   open RefineError
   open TermType
   open Term

   type term = TermType.term
   type param = TermType.param
   type term_subst = TermType.term_subst

(*
*   Nodes with children relation (field children) give a tree.
*   Sack contains links which make a DAG from it. Sack also contains
*   looping links for variable nodes which means that this variable
*   is not eliminated yet; these loops shouldn't be followed (in DAG).
*)
   type node= nodeknown ref
   and nodeknown={mutable nodeterm:term;
                  mutable children:childrentype;
                  mutable bindings:caseofb;
                  mutable bound:bool}
   and childrentype=ChildrenDelayed
                  | ChildrenKnown of node list
   and caseofb =  BindingPoint of bnode list list
                | BoundOc of (node*int*int)
                | Constant
                | BindingNone
                | BindingDelayed of brunch_bindings_delayed
   and bnode = dummy ref
   and dummy = Dummy
   and sacktype = (string, node) Hashtbl.t
(*
*  bbd:brunch_bindings_delayed will store the delayed bindings in
*  the current brunch of the tree -- in order to fill in the field
*  caseofb = (BoundOc ???) on the leafs later
*)
   and brunch_bindings_delayed = (string * (node*int*int)) list




let rec cr_dummylist l =
   match l with
      []->[]
    | h::t ->(ref Dummy)::cr_dummylist t

let bbd_diff l n i =
   let rec bbd_d ll j =
      match ll with
         [] -> []
       | h::t -> (h , (n,i,j))::(bbd_d t (j+1))
   in bbd_d l 0

(* val destruct: (bound_term list) -> brunch_bindings_delayed -> node -> int ->unit  *)

let rec destruct l bbd n i =
    match l with
     [] -> [],[]
   | hh::tt ->let b0,ch0 = destruct tt bbd n (i+1) in
              let b=(cr_dummylist hh.bvars)::b0 in
              let ch=(ref {nodeterm=hh.bterm;
                           children=ChildrenDelayed;
                           bindings=BindingDelayed  ((bbd_diff hh.bvars n i)@ bbd);
                           bound=false}
                     )::ch0
              in b,ch

(*  One step of conversion from term to DAG.
*   val calc_node : node -> sacktype -> unit
*   Almost free when the node is already calculated!
*   We suppose the invariants:
*      1) bindings!=BindingDelayed _ ==>
*          children!=ChildrenDelayed and bound is correct;
*      2) sack does not contain links to a node with
*         bindings = BindingDelayed _ .
*)
let calc_node n  sack consts =
   match (!n).bindings with
      BindingDelayed bbd ->
      begin
      match bbd with
         h::t -> ( (!n).bound <- true )
       | _ ->();
      match get_core (!n).nodeterm with
         FOVar x ->if List.mem_assoc x bbd then
                      begin
                      (!n).bindings <- BoundOc (List.assoc x bbd);
                      (!n).children <- ChildrenKnown []
                      end
                   else let unlinked = not (Hashtbl.mem sack x)
                        in
                        if unlinked && (StringSet.mem consts x) then
                           begin
                           (!n).bindings <- Constant;
                           (!n).children <- ChildrenKnown []
                           end
                        else
                           begin
                           (!n).bindings <- BindingNone;
                           (!n).children <- ChildrenKnown [];
                           if unlinked then Hashtbl.add sack x n
                           end
(* GOAL: lasy initialization of stack: for every free variable name x
*  we need a node n with (x,n) in sack; it may be a looping edge as
*  we did here. *)
       | Term t -> begin
                   let b,ch = destruct t.term_terms bbd n 0
                   in
                   (!n).bindings <- (match b with
                                        [] -> BindingNone
                                      | _  -> BindingPoint b);
                   (!n).children <- ChildrenKnown ch
                   end;
       | _ -> REF_RAISE ( RefineError("unify_rob", StringError "Fail to convert term"))
      end
    | _ ->()

(* we have to deal with looping edges in sack too !! *)
let rec follow_links n sack consts = calc_node n sack consts;
   match (!n).bindings with
      BindingNone ->
      (match get_core (!n).nodeterm with
          FOVar x ->let nn = Hashtbl.find sack x in
                    if n==nn then n
                    else (follow_links nn sack consts)
        | _ -> n)
    | _ -> n


let is_bvar_n n = match (!n).bindings with
                    BoundOc _ -> true
                  | _ -> false

let is_const_n n = match (!n).bindings with
                    Constant  -> true
                  | _ -> false

let is_fvar_n n = (is_var_term (!n).nodeterm) &&
                  (not (is_bvar_n n)) &&
                  (not (is_const_n n))

let links_fvar_n n sack consts = is_fvar_n (follow_links n sack consts)

let fvarstr_n n sack consts =
   let nn = follow_links n sack consts in
   dest_var (!nn).nodeterm

let is_fsymb_n n = (not (is_var_term (!n).nodeterm)) ||
                   is_const_n n

let links_fsymb_n n sack consts  = is_fsymb_n (follow_links n sack consts)

let fsymboper_n n sack consts =
   let nn = follow_links n sack consts in
   match get_core (!nn).nodeterm with
      FOVar x -> (mk_op var_opname [Var x]) (* for constsnts *)
    | Term t -> t.term_op
    | _ ->REF_RAISE ( RefineError("unify_rob", StringError "Fail to get oper_n"))

let succs n sack consts =
   let nn = follow_links n sack consts in
      match (!nn).children with
         ChildrenKnown l -> l
       | _ -> raise generic_refiner_exn

(* substfree checks whether the link (v,n) can be added to sack.
*  The problem is to check that the node n is not inside the bound
*  part of the DAG, i.e. there is no bound occurances under n which
*  refer to binding points not under n. We should add a special boolean
*  field to nodeknown type which will mark the bound part of DAG.
*)
(*
let substfree v n sack consts =
   let is_link = Hashtbl.mem sack v in
   let rec free v n sack consts =
      match (!n).bindings with
         BindingDelayed _ ->

   in free v n sack consts
*)
end     (* end TermSubstRob  *)









