(*
 * Display all the elements in a particular theory.
 *
 * ----------------------------------------------------------------
 *
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/index.html for information on Nuprl,
 * OCaml, and more information about this system.
 *
 * Copyright C 1998 Jason Hickey, Cornell University
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or at your option any later version.
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
 * Author: Jason Hickey
 * jyh@cs.cornell.edu
 *)

include Itt_theory

open Splay_table
open String_set

open Refiner.Refiner.TermType
open Refiner.Refiner.Term
open Refiner.Refiner.RefineError

open Tacticals
open Base_dtactic
open Base_auto_tactic

open Itt_equal
open Itt_logic
open Itt_struct

(*
 * Problem.
 *)
interactive pigeon2 'H : :
   sequent ['ext] { 'H;
             x11: univ[1:l];
             x12: univ[1:l];
             x21: univ[1:l];
             x22: univ[1:l];
             x31: univ[1:l];
             x32: univ[1:l];
             h11: 'x11 => "not"{.'x21 or 'x31 or 'x12};
             h12: 'x12 => "not"{.'x22 or 'x32 or 'x11};
             h21: 'x21 => "not"{.'x11 or 'x31 or 'x22};
             h22: 'x22 => "not"{.'x12 or 'x32 or 'x21};
             h31: 'x31 => "not"{.'x11 or 'x21 or 'x32};
             h32: 'x32 => "not"{.'x12 or 'x22 or 'x31};
             h1: 'x11 or 'x12;
             h2: 'x21 or 'x22;
             h3: 'x31 or 'x32
             >- void
   }

(*
 * Table for associating pigeon locations with hyp numbers.
 *)
module StringTableBase =
struct
   type elt = string
   type set = StringSet.t
   type data = int

   let union = StringSet.union
   let compare _ s1 s2 = Pervasives.compare s1 s2
   let append l1 l2 = l1 @ l2
end

module PigeonTable = MakeSplayTable (StringTableBase)

(*
 * Step 5: prove a disjunct.
 *)
let rec prove_disjunct pigeons p =
   let t = Sequent.concl p in
      (if is_or_term t then
          (selT 1 (dT 0) thenMT prove_disjunct pigeons thenWT autoT)
          orelseT (selT 2 (dT 0) thenMT prove_disjunct pigeons thenWT autoT)
       else
          try
             nthHypT (PigeonTable.find pigeons (dest_var t))
          with
             Not_found ->
                raise (RefineError ("prove_disjunct", StringError "disjunct not provable"))) p
(*
 * Step 4: prove one of the negations.
 *)
let rec prove_negation pigeons i p =
   ((dT i thenT prove_disjunct pigeons) orelseT prove_negation pigeons (pred i)) p

(*
 * Step 3: forward chain through the possible pigeon locations.
 *)
let rec forward_chain pigeons i p =
   let _, t = Sequent.nth_hyp p i in
      (if is_implies_term t then
         let t, _ = dest_implies t in
            try
               let j = PigeonTable.find pigeons (dest_var t) in
                  dT i thenLT [nthHypT j; forward_chain pigeons (pred i)]
            with
               Not_found ->
                  forward_chain pigeons (pred i)
      else if is_univ_term t then
         idT
      else
         forward_chain pigeons (pred i)) p

(*
 * Step 2: collect the pigeon placements.
 *)
let rec collect_pigeons pigeons i p =
   let _, t = Sequent.nth_hyp p i in
      if is_var_term t then
         collect_pigeons (PigeonTable.add pigeons (dest_var t) i) (pred i) p
      else
         pigeons

(*
 * Step 1: decompose all the disjunctions.
 *)
let rec decompose_disjuncts i p =
   let _, t = Sequent.nth_hyp p i in
      (if is_or_term t then
          dT i thenT decompose_disjuncts i
       else if is_var_term t then
          decompose_disjuncts (pred i)
       else
          idT) p

(*
 * Put it all together.
 *)
let prove3T pigeons p =
   prove_negation pigeons (Sequent.hyp_count p) p

let prove2T p =
   let length = Sequent.hyp_count p in
   let pigeons = collect_pigeons (PigeonTable.create StringSet.empty) length p in
      (forward_chain pigeons length thenT prove3T pigeons) p

let proveT p =
   let length = Sequent.hyp_count p in
      (decompose_disjuncts length thenT prove2T) p

(*
 * -*-
 * Local Variables:
 * Caml-master: "editor.top"
 * End:
 * -*-
 *)
