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

   type node= nodeknown ref
   and nodeknown={mutable nodeterm:term;
                  mutable children:childrentype;
                  mutable bindings:caseofb}
   and childrentype=ChildrenDelayed
                  | ChildrenKnown of node list
   and caseofb =  BindingPoint of bnode list list
                | BoundOc of (node*int*int)
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


let rec cr_dummylist l = match l with []->[] | h::t ->(ref Dummy)::cr_dummylist t


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
                           bindings=BindingDelayed  ((bbd_diff hh.bvars n i)@ bbd)}
                     )::ch0
              in
              b,ch

(*  A variant with  brunch_bindings_delayed list stored in BindingDelayed
*   val calc_node : node -> sacktype -> unit
*   We suppose the invariant:
*      bindings!=BindingDelayed _ => children!=ChildrenDelayed
*)
   let calc_node n  =
    match (!n).bindings with
     BindingDelayed bbd ->
       begin
       match get_core (!n).nodeterm with
         FOVar x ->(if List.mem_assoc x bbd then
                     (!n).bindings <- BoundOc (List.assoc x bbd)
                    else (!n).bindings <- BindingNone
                   );
                   (!n).children <- ChildrenKnown []
       | Term t -> begin
                   (*calculate (!n).bindings and
                     replace in children ChildrenDelayed by ChildrenKnown:
                   *)
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

let rec nodeoper n sack = calc_node n;
   let trmcore=get_core (!n).nodeterm in
   match trmcore with
     FOVar x ->(try  nodeoper (Hashtbl.find sack x) sack
                with Not_found -> mk_op var_opname [Var x])
   | Term t  ->t.term_op
   | _ -> REF_RAISE ( RefineError("unify_rob", StringError "Fail to get nodeoper"))

end     (* end TermSubstRob  *)


