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
 * Author: Jason Hickey <jyh@cs.cornell.edu>
 * Modified By: Aleksey Nogin <nogin@cs.caltech.edu>
 *)

open Lm_debug
open Lm_printf

open Refiner.Refiner
open Refiner.Refiner.TermAddr
open Refiner.Refiner.TermSubst
open Refiner.Refiner.RefineError

open Tactic_boot
open Rewrite_boot
open Sequent_boot

open Tacticals_boot

(*
 * Debug statement.
 *)
let _ =
   show_loading "Loading Conversionals_boot%t"

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
   module ConversionalsTypes = TacticInternalType
   open ConversionalsTypes
   type env = TacticInternalType.env

   let env_term = Rewrite.env_term
   let env_arg = Rewrite.env_arg

   let prefix_thenC = Rewrite.prefix_thenC
   let prefix_orelseC = Rewrite.prefix_orelseC
   let addrC = Rewrite.addrC
   let addrLiteralC = Rewrite.addrLiteralC
   let idC = Rewrite.idC
   let foldC = Rewrite.foldC
   let makeFoldC = Rewrite.makeFoldC
   let cutC = Rewrite.cutC
   let funC = Rewrite.funC
   let termC = Rewrite.termC
   let allSubC = Rewrite.allSubC
   let prefix_thenTC = Rewrite.prefix_thenTC

   (************************************************************************
    * SEARCH                                                               *
    ************************************************************************)

   (*
    * Failure.
    *)

   let failWithC s =
      funC (fun _ -> raise (RefineError ("failWithC", StringError s)))

   let failC  =  failWithC "Fail"

   (*
    * Trial.
    *)
   let tryC rw =
      prefix_orelseC rw idC

   let ifEqualC term conv1 conv2 =
      let if_alpha_eq t = if alpha_equal t term then conv1 else conv2
      in termC if_alpha_eq

   let replaceUsingC term conv =
      ifEqualC term (*then*) conv (*else*) failC

   let progressC conv =
      let prgC term =
         prefix_thenC conv  (ifEqualC term (*then*) failC (*else*) idC)
      in termC prgC
   (*
    * First subterm that works. This is similar to Rewrite_boot's allSubC.
    *)
   let someSubC =
      let failC = funC (fun _ -> raise (RefineError ("subC", StringError "all subterms failed"))) in
      let someSubCE conv env =
         let addrs = subterm_addresses (env_term env) in
            List.fold_left (fun conv' addr -> prefix_orelseC (addrLiteralC addr conv) conv') failC addrs
      in
         fun conv -> funC (someSubCE conv)

   let allSubThenC conv1  conv2 =
       prefix_thenC (progressC (allSubC (tryC conv1))) conv2

   (*
    * Outermost terms.
    * HigherC has been moved into the refiner
    * for efficiency.
    *)
   let higherC = Rewrite.higherC

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
         prefix_thenC (allSubC (sweepUpC rw)) (tryC rw)
      in
         funC sweepUpCE

   let rec sweepDnC rw =
      let sweepDnCE e =
         prefix_thenC (tryC rw) (allSubC (sweepDnC rw))
      in
         funC sweepDnCE

   (*
    * These are the same but don't allow failure.
    *)
   let rec sweepUpFailC rw =
      let sweepUpCE e =
         prefix_thenC (allSubC (sweepUpFailC rw)) rw
      in
         funC sweepUpCE

   let rec sweepDnFailC rw =
      let sweepDnCE e =
         prefix_thenC rw (allSubC (sweepDnFailC rw))
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
    * Apply all to all terms the first conversion that works
    *)
   let applyAllC convs =
      higherC (sweepUpC (firstC convs))

   (*
    * Repeat the conversion until nothing more happens.
    *)
   let whileProgressC conv =
      let repeatCE env =
         let rec repeat t env =
            let t' = env_term env in
               if alpha_equal t t' then
                  idC
               else
                  prefix_thenC conv (funC (repeat t'))
         in
         let t = env_term env in
            prefix_thenC conv (funC (repeat t))
      in
         funC repeatCE

   (*
    * Repeat the conversion until fails nothing more happens.
    *)
   let repeatC conv = whileProgressC (tryC conv)

   (*
    * Repeat the conversion until fails.
    *)
   let untilFailC conv =
      let rec aux env =
         (tryC (prefix_thenC conv (funC aux)))
      in
         funC aux

   let rec repeatForC i conv =
      if i < 0 then
         raise (Invalid_argument "repeatForC: the argument should be not negative")
      else
         let repeatForCE env =
            if i = 0 then
               idC
            else
               prefix_thenC conv (repeatForC (i - 1) conv)
         in
            funC repeatForCE



   let rwc conv assum clause =
      Tacticals.funT
      (fun p -> Rewrite.rw conv assum (Sequent.assum_clause_addr p assum clause))

   let empty_addr = TermAddr.make_address []

   let rwcAll conv assum =
      Rewrite.rw (allSubC conv) assum empty_addr

   let rw conv clause =
      rwc conv 0 clause

   let rwAll conv   =
      rwcAll conv 0

   let rwAllAll conv   =
      Tacticals.prefix_thenT Tacticals.removeHiddenLabelT
      (Tacticals.prefix_thenMT (Tacticals.onAllMAssumT (rwcAll conv)) (rwAll conv))

   let rwh conv = rw (higherC conv)
   let rwch conv = rwc (higherC conv)
   let rwhAll conv = rwAll (higherC conv)
   let rwchAll conv = rwcAll (higherC conv)
   let rwhAllAll conv = rwAllAll (higherC conv)

   let rwa convs = rw (applyAllC convs)
   let rwca convs = rwc (applyAllC convs)
   let rwaAll convs = rwAll (applyAllC convs)
   let rwcaAll convs = rwcAll (applyAllC convs)
   let rwaAllAll convs = rwAllAll (applyAllC convs)

   (*
    * Conversionals.
    *)
   let create_iform = Rewrite.create_iform
   let apply_rewrite = Rewrite.apply_rewrite
   let rewrite_of_pre_rewrite = Rewrite.rewrite_of_pre_rewrite
   let redex_and_conv_of_rw_annotation = Rewrite.redex_and_conv_of_rw_annotation
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
