(*
 * These are the basic rewriting operations.
 *
 * We execute the operations outside the refiner.
 * After the refinement is done, we construct the
 * rewrite tree.
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
 * Copyright (C) 1998 Jason Hickey, Cornell University
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
 * Author: Jason Hickey
 * jyh@cs.cornell.edu
 *)

open Mp_debug
open Printf

open Refiner.Refiner.TermType
open Refiner.Refiner.Term
open Refiner.Refiner.TermSubst
open Refiner.Refiner.RefineError

open Tactic_boot_sig
open Tactic_boot
open Rewrite_boot
open Sequent_boot

(*
 * Debug statement.
 *)
let _ =
   show_loading "Loading Tacticals%t"

let debug_conv =
   create_debug (**)
      { debug_name = "conv";
        debug_description = "display conversion operation";
        debug_value = false
      }

let debug_reduce =
   create_debug (**)
      { debug_name = "reduce";
        debug_description = "display reductions";
        debug_value = false
      }

module Conversionals =
struct
   type env = TacticInternalType.env
   type conv = TacticInternalType.conv
   type tactic_arg = TacticInternalType.tactic_arg
   type tactic = TacticInternalType.tactic

   let env_term = RewriteInternal.env_term
   let env_goal = RewriteInternal.env_goal
   let env_arg = RewriteInternal.env_arg
   let get_conv = Sequent.get_conv_arg

   let prefix_andthenC = RewriteInternal.prefix_andthenC
   let prefix_orelseC = RewriteInternal.prefix_orelseC
   let addrC = RewriteInternal.addrC
   let clauseC = RewriteInternal.clauseC
   let idC = RewriteInternal.idC
   let foldC = RewriteInternal.foldC
   let makeFoldC = RewriteInternal.makeFoldC
   let cutC = RewriteInternal.cutC
   let funC = RewriteInternal.funC

   (************************************************************************
    * SEARCH                                                               *
    ************************************************************************)

   (*
    * Failure.
    *)
   let failC err =
      funC (fun _ -> raise (RefineError ("failC", StringError err)))

   let failWithC (name, err) =
      funC (fun _ -> raise (RefineError (name, err)))

   (*
    * Trial.
    *)
   let tryC rw =
      prefix_orelseC rw idC

   (*
    * First subterm that works.
    *)
   let someSubC conv =
      let someSubCE env =
         let t = env_term env in
         let count = subterm_count t in
         let rec subC i =
            if i = count then
               failWithC ("subC", StringError "all subterms failed")
            else
               prefix_orelseC (addrC [i] conv) (subC (i + 1))
         in
            subC 0
      in
         funC someSubCE

   (*
    * Apply to all subterms.
    *)
   let allSubC conv =
      let allSubCE conv env =
         let t = env_term env in
         let count = subterm_count t in
         let rec subC conv count i =
            if i = count then
               idC
            else
               prefix_andthenC (addrC [i] conv) (subC conv count (i + 1))
         in
            subC conv count 0
      in
         funC (allSubCE conv)

   (*
    * Outermost terms.
    * HigherC has been moved into the refiner
    * for efficiency.
    *)
   let higherC = RewriteInternal.higherC

   let rw conv clause =
      RewriteInternal.rw conv 0 clause

   let rwh conv i =
      RewriteInternal.rw (higherC conv) 0 i

   let rwc conv i j =
      RewriteInternal.rw conv i j

   let rwch conv i j =
      RewriteInternal.rw (higherC conv) i j

   (*
    * Apply to leftmost-innermost term.
    *)
   let rec lowerC rw =
      let lowerCE e =
         prefix_orelseC (someSubC (lowerC rw)) rw
      in
         funC lowerCE

   (*
    * Apply to all terms possible from innermost to outermost.
    *)
   let rec sweepUpC rw =
      let sweepUpCE e =
         prefix_andthenC (allSubC (sweepUpC rw)) (tryC rw)
      in
         funC sweepUpCE

   let rec sweepDnC rw =
      let sweepDnCE e =
         prefix_andthenC (tryC rw) (allSubC (sweepDnC rw))
      in
         funC sweepDnCE

   (*
    * Use the first conversion that works.
    *)
   let rec firstC = function
      [conv] ->
         conv
    | conv :: t ->
         prefix_orelseC conv (firstC t)
    | [] ->
         raise (RefineError ("firstC", StringError "empty argument list"))

   (*
    * Repeat the conversion until nothing more happens.
    *)
   let repeatC conv =
      let repeatCE env =
         let rec repeat t env =
            let t' = env_term env in
               if alpha_equal t t' then
                  idC
               else
                  prefix_andthenC conv (tryC (funC (repeat t')))
         in
         let t = env_term env in
            prefix_andthenC conv (funC (repeat t))
      in
         funC repeatCE

   let rec repeatForC i conv =
      let repeatForCE env =
         if i = 0 then
            idC
         else
            prefix_andthenC conv (repeatForC (i - 1) conv)
      in
         funC repeatForCE
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
