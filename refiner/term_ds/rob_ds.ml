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
*   A node n is bound if there is a node in DAG below it representing
*   some occurense of a bound variable with the binding point not
*   below n (field bound in nodeknown). There souldn't be links to bound
*   nodes in sack.
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

(* node equality *)
let eq_n m n = (!m == !n)

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

let is_var_free v t = StringSet.mem (free_vars_set t) v

(*  One step of conversion from term to DAG.
*   val calc_node : node -> sacktype -> unit
*   Almost free when the node is already calculated!
*   We suppose the invariants:
*      1) bindings!=BindingDelayed _ ==>
*          children!=ChildrenDelayed and bound is correct;
*      2) sack does not contain links to a node with
*         bindings = BindingDelayed _ ;
*      3) sack does not contain links to bound nodes.
*)
let calc_node n  sack consts =
   match (!n).bindings with
      BindingDelayed bbd ->
      begin
      (!n).bound <- (let f w = is_var_free (fst w) (!n).nodeterm
                     in
                     List.exists f bbd
                    );
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
                   (is_const_n n)

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

let successors n sack consts = (*links followed twice*)
   let nn = follow_links n sack consts in
      match (!nn).children with
         ChildrenKnown l ->
            List.map (function x -> follow_links x sack consts) l
       | _ -> raise generic_refiner_exn

let is_link v sack consts = (* (Hashtbl.mem sack v) && seems to be =true *)
   let n=Hashtbl.find sack v in
   (is_fsymb_n n) ||
   ((is_fvar_n n) && (not ((fvarstr_n n sack consts)=v)))

let is_var_not_link_n n sack consts=
   (is_fvar_n n) && ((follow_links n sack consts)==n)

(* strl_followlink: (string list) -> sack ->StringSet.t -> (node list)
*  returnes nodelist without repetitions
*)

module H_nodeknown =
struct
   type t = nodeknown
   let equal  = (==)
   let hash = Hashtbl.hash
end
(* Hashtbl  with keys: node ; should be calculated !*)
module Hashtbl_n = Hashtbl.Make(H_nodeknown)

(* follows links and removes repeated elements (up to eq_n)
*  from the node list
*)
let filt_nodelist l sack consts =
   let s = Hashtbl_n.create 23 in
   let rec filt = function
      [] ->
         []
    | h::t -> let n=follow_links h sack consts in
              if Hashtbl_n.mem s (!n) then (filt t)
              else (Hashtbl_n.add s (!n) true; n::(filt t))
   in
   filt l

(* outputs as filt_nodelist but for varnamelists *)
let filt_strlist l sack consts =
   let s = Hashtbl_n.create 23 in
   let rec filt = function
      [] ->
         []
    | h::t ->if Hashtbl.mem sack h then
                let n=follow_links (Hashtbl.find sack h) sack consts
                in
                if Hashtbl_n.mem s (!n) then (filt t)
                else (Hashtbl_n.add s (!n) 0;
                      n::(filt t)
                     )
             else filt t
   in
   filt l

(* calculates the conjunction of p(x) where x ranges all the nodes of
*  the list (filt_strlist l sack consts). When v \in l is not a link
*  then the corresponding conjunct is omitted. Uses external
*  pmemo :Hashtbl_n.t which stores some values of p(x) with keys (!x).
*)
let forall_filt_strlist l sack consts p pmemo=
   let rec filt = function
      [] ->
         true
    | h::t ->if Hashtbl.mem sack h then
                let n=follow_links (Hashtbl.find sack h) sack consts
                in
                if Hashtbl_n.mem pmemo (!n) then
                     (Hashtbl_n.find pmemo (!n)) && (filt t)
                else (let value = p n in
                      Hashtbl_n.add pmemo (!n) value;
                      value && (filt t)
                     )
             else filt t
   in
   filt l


(* calculates the conjunction of p(x) where x ranges all the nodes of
*  the list (filt_nodelist l sack consts). Uses external
*  pmemo :Hashtbl_n.t which stores some values of p(x) with keys (!x).
*)
let forall_filt_nodelist l sack consts p pmemo=
   let rec filt = function
      [] ->
         true
    | h::t -> let n=follow_links h sack consts in
              if Hashtbl_n.mem pmemo (!n) then
               (Hashtbl_n.find pmemo (!n)) && (filt t)
              else (let value = p n in
                    Hashtbl_n.add pmemo (!n) value;
                    value && (filt t)
                   )
   in
   filt l

(* calculates the conjunction of p(x) where x ranges all the nodes of
*  the list (filt_nodelist l sack consts). Without hashtbl, so
*  no additional allocations but slow. Should be used when a list is short.
*)
(*
let forall_filt_nodelist0 l sack consts p=
   let rec eq_mem x = function
      [] ->
         false
    | h::t ->((eq_n x h) || (eq_mem x t))
   in
   let rec filt = function
      [] ->
         true
    | h::t -> let n=follow_links h sack consts in
              (( (eq_mem n t) || (p n) ) && (filt t))
   in
   filt l
*)

(* substfree checks whether the link (v,n) can be added to sack.
*  The problem is to check that the node n is not inside the bound
*  part of the DAG, i.e. there is no bound occurances under n which
*  refer to binding points not under n. We have a special
*  field bound:bool in nodeknown type which marks the bound part of the DAG.
*  We want to
*  1) allow to add to sack a link (v,n) if n is "not calculated" node
*    (in order to implement most lasy conversion) but calculate it when it is added,
*     so in sack all nodes are calculated;
*  2) not allow to add (v,n) when n is "calculated" variable node with
*    n!=(follow_link n sack consts);
*  3) not allow to add (v,n) when (v,nn) is in sack and nn represents
*     something different from v (i.e. not to redefine links) or when n represents v.
*
* sfree is a part of it which does the main job but
* in sfree we suppose that v is not a link; it also does not check
* that a node is not bound.
*)


let rec sfree v n sack consts pmemo=
   match (!n).bindings with
      BindingDelayed bbd ->
         let s = free_vars_set (!n).nodeterm in
         let f x y = StringSet.remove (fst y) x in
         let ss = List.fold_left f s bbd in
         let sl = StringSet.elements ss
         in
         (not (StringSet.mem ss v)) &&
         (let p = function
             x -> sfree v x sack consts pmemo
          in
          forall_filt_strlist sl sack consts p pmemo
         )
    | BoundOc _ -> false
    | Constant -> true
    | _ -> ((is_var_not_link_n n sack consts)&&(v!= dest_var (!n).nodeterm)
           ) ||
           ((is_fsymb_n n) &&
            (let p = function  x -> sfree v x sack consts pmemo
             in
             forall_filt_nodelist (succs n sack consts) sack consts p pmemo
            )
           )


let substfree v n sack consts = (not (is_link v sack consts)) &&
   (match (!n).bindings with
       BindingDelayed bbd ->(let f w = is_var_free (fst w) (!n).nodeterm
                             in
                             List.exists f bbd
                            )
     | _ ->not (!n).bound
   )
   && (let pmemo=Hashtbl_n.create 23 in sfree v n sack consts pmemo)



end     (* end TermSubstRob  *)









