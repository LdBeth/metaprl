(*
 * This is the basic rewrite data type.
 * A file with this name is required for every theory.
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

open Printf
open Mp_debug

open Refiner.Refiner
open Refiner.Refiner.Term
open Refiner.Refiner.TermMan
open Refiner.Refiner.TermAddr
open Refiner.Refiner.RefineError
open Refiner.Refiner.Rewrite
open Refiner.Refiner.Refine

open Tactic_boot_sig
open Tactic_boot
open Sequent_boot
open Tacticals_boot.Tacticals

let debug_rewrite = load_debug "rewrite"

module Rewrite =
struct
   open TacticInternalType

   (************************************************************************
    * TYPES                                                                *
    ************************************************************************)

   type env = TacticInternalType.env
   type conv = TacticInternalType.conv
   type tactic = TacticInternalType.tactic
   type tactic_arg = TacticInternalType.tactic_arg

   (************************************************************************
    * IMPLEMENTATION                                                       *
    ************************************************************************)

   (*
    * Get the term of the environment.
    *)
   let env_term (arg, addr) =
      term_subterm (Sequent.goal arg) addr

   let env_term_subterm_count (arg, addr) =
      term_subterm_count (Sequent.goal arg) addr

   let env_arg (arg, addr) =
      arg

   (*
    * Get the sequent that we are matching against.
    *)
   let env_goal (arg, _) =
      Sequent.goal arg

   (*
    * Create a conversion from a basic rewrite.
    * This function is required by filter_prog.
    *)
   let rewrite_of_rewrite rw =
      RewriteConv rw

   (*
    * Create a conversion from a conditional rewrite.
    * This function is required by filter_prog.
    *)
   let rewrite_of_cond_rewrite crw args =
      CondRewriteConv (crw args)

   (*
    * Combine two lissts of conversion.
    * Note if the adjacent conversion can be combined.
    *)
   let combine rw_f crw_f make clist1 clist2 =
      match Flist.last clist1, Flist.first clist2 with
         RewriteConv rw1, RewriteConv rw2 ->
            let rw = RewriteConv (fun s t -> rw_f rw1 rw2 s t) in
               if Flist.singleton clist1 & Flist.singleton clist2 then
                  rw
               else
                  make (Flist.append_skip clist1 rw clist2)
       | CondRewriteConv crw1, CondRewriteConv crw2 ->
            let crw = CondRewriteConv (fun s v t -> crw_f crw1 crw2 s v t) in
               if Flist.singleton clist1 & Flist.singleton clist2 then
                  crw
               else
                  make (Flist.append_skip clist1 crw clist2)
       | _ ->
            make (Flist.append clist1 clist2)

   let compose clist1 clist2 =
      combine andthenrw candthenrw (fun l -> ComposeConv l) clist1 clist2

   let choose clist1 clist2 =
      combine orelserw corelserw (fun l -> ChooseConv l) clist1 clist2

   let prefix_thenC conv1 conv2 =
      let clist1 =
         match conv1 with
            ComposeConv clist1 ->
               clist1
          | _ ->
               Flist.create conv1
      in
      let clist2 =
         match conv2 with
            ComposeConv clist2 ->
               clist2
          | _ ->
               Flist.create conv2
      in
         compose clist1 clist2

   let prefix_orelseC conv1 conv2 =
      let clist1 =
         match conv1 with
            ChooseConv clist1 ->
               clist1
          | _ ->
               Flist.create conv1
      in
      let clist2 =
         match conv2 with
            ChooseConv clist2 ->
               clist2
          | _ ->
               Flist.create conv2
      in
         choose clist1 clist2

   (*
    * No action.
    *)
   let idC = IdentityConv

   (*
    * Function conversion needs an argument.
    *)
   let funC f =
      FunConv f

   let termC f = funC (fun e -> (f (env_term e)))

   (*
    * Apply the conversion at the specified address.
    *)
   let addrC addr =
      let addr = make_address addr in
         (function
            RewriteConv rw ->
               RewriteConv (rwaddr addr rw)
          | CondRewriteConv crw ->
               CondRewriteConv (crwaddr addr crw)
          | conv ->
               AddressConv (addr, conv))

   let clauseC clause rw =
      ClauseConv (clause, rw)

   (*
    * Apply the conversion at the highest addresses.
    *)
   let higherC = function
      RewriteConv rw ->
         RewriteConv (rwhigher rw)
    | CondRewriteConv crw ->
         CondRewriteConv (crwhigher crw)
    | conv ->
         HigherConv conv

   let allSubC conv =
      let allSubCE conv env =
         let count = env_term_subterm_count env in
         let rec subC conv count i =
            if i = count then
               idC
            else
               prefix_thenC (addrC [i] conv) (subC conv count (i + 1))
         in
            subC conv count 0
      in
         funC (allSubCE conv)

   let higherLC rw =
      let rec higherCE rw env =
         (prefix_orelseC rw (allSubC (funC (higherCE rw))))
      in
         funC (higherCE rw)

   (*
    * Reverse the conversion at the specified address.
    *)
   let foldC t conv =
      FoldConv (t, conv)

   (*
    * Build a fold conversion from the contractum
    * and the unfolding conversion.
    *)
   let makeFoldC contractum conv =
      let fold_aux = function
         RewriteConv rw ->
            let mseq = mk_msequent contractum [] in
            let tac = rwtactic 0 rw in
               begin
                  (* Apply the unfold conversion *)
                  match Refine.refine any_sentinal tac mseq with
                     [redex], _ ->
                        (* Unfolded it, so create a rewrite that reverses it *)
                        let redex, _ = dest_msequent redex in
                        let rw' = term_rewrite Strict ([||], [||]) [redex] [contractum] in
                        let doCE env =
                           match apply_rewrite rw' ([||], [||], []) (env_term env) [] with
                              [contractum], _ ->
                                 FoldConv (contractum, conv)
                            | _ ->
                                 raise (RefineError ("Rewrite_type.fold", StringTermError ("rewrite failed", redex)))
                        in
                           FunConv doCE
                   | _ ->
                        raise (RefineError ("Rewrite_type.fold", StringTermError ("fold failed", contractum)))
               end
       | _ ->
            raise (RefineError ("Rewrite_type.fold", StringError "can't fold nontrivial rewrites"))
      in
         Refine_exn.print Dform.null_base fold_aux conv

   (*
    * Cut just replaces the term an generates a rewrite
    * subgoal.
    *)
   let cutC t =
      CutConv t

   (*
    * root: address of the clause
    * rel: offset into the term
    * addr: compose_addrress root rel
    *)
   let rec apply clause addr conv p =
      match conv with
         RewriteConv rw ->
            if !debug_rewrite then
               eprintf "Rewrite_type.apply: Rewrite%t" eflush;
            Tactic.tactic_of_rewrite clause (rwaddr addr rw) p
       | CondRewriteConv crw ->
            if !debug_rewrite then
               eprintf "Rewrite_type.apply: CondRewrite%t" eflush;
            Tactic.tactic_of_cond_rewrite clause (crwaddr addr crw) p
       | ComposeConv clist ->
            if !debug_rewrite then
               eprintf "Rewrite_type.apply: Compose%t" eflush;
            composeT clause addr (Flist.tree_of_list clist) p
       | ChooseConv clist ->
            if !debug_rewrite then
               eprintf "Rewrite_type.apply: Choose%t" eflush;
            chooseT clause addr (Flist.tree_of_list clist) p
       | AddressConv (addr', conv) ->
            let addr = compose_address addr addr' in
               if !debug_rewrite then
                  eprintf "Rewrite_type.apply: Address %s%t" (string_of_address addr') eflush;
               apply clause addr conv p
       | ClauseConv (clause, conv) ->
            apply clause addr conv p
       | IdentityConv ->
            if !debug_rewrite then
               eprintf "Rewrite_type.apply: Identity%t" eflush;
            TacticInternal.idT p
       | FunConv f ->
            if !debug_rewrite then
               eprintf "Rewrite_type.apply: Fun%t" eflush;
            apply clause addr (f (p, addr)) p
       | HigherConv conv ->
            if !debug_rewrite then
               eprintf "Rewrite_type.apply: Higher%t" eflush;
            apply clause addr (higherLC conv) p
       | FoldConv (t, conv) ->
            if !debug_rewrite then
               eprintf "Rewrite_type.apply: Fold%t" eflush;
            (prefix_thenLT (rwcutT clause addr t) [addHiddenLabelT "main"; solveCutT addr conv]) p
       | CutConv t ->
            if !debug_rewrite then
               eprintf "Rewrite_type.apply: Cut%t" eflush;
            rwcutT clause addr t p

   and composeT clause addr tree p =
      match tree with
         Flist.Empty ->
            idT p
       | Flist.Leaf conv ->
            apply clause addr conv p
       | Flist.Append (tree1, tree2) ->
            (prefix_then_OnFirstT (composeT clause addr tree1) (composeT clause addr tree2)) p

   and chooseT clause addr tree p =
      match tree with
         Flist.Empty ->
            idT p
       | Flist.Leaf conv ->
            apply clause addr conv p
       | Flist.Append (tree1, tree2) ->
            (prefix_orelseT (chooseT clause addr tree1) (chooseT clause addr tree2)) p

   and rwcutT clause addr t p =
      let goal, hyps = Refine.dest_msequent (Sequent.msequent p) in
      let t' =
         if clause = 0 then
            goal
         else if clause > 0 && clause <= List.length hyps then
            List.nth hyps (pred clause)
         else
            raise (RefineError ("rwcutT", StringIntError ("clause number is out of range", clause)))
      in
      let t' = TermAddr.replace_subterm t' addr t in
         cutT t' p

   and solveCutT addr conv p =
      let len = List.length (snd (Refine.dest_msequent (Sequent.msequent p))) in
         (prefix_thenMT (apply len addr conv) (nthAssumT len)) p

   (*
    * Apply the rewrite.
    *)
   let rw conv assum addr p =
      if !debug_rewrite then
         eprintf "Rewrite start%t" eflush;
      let x = apply assum addr conv p in
         if !debug_rewrite then
            eprintf "Rewrite done%t" eflush;
         x

   (*
    * Create an input form.
    * This is a Relaxed rewrite with no justification.
    *)
   let create_iform name strictp redex contractum =
      let rw = create_input_form (null_refiner name) name strictp redex contractum in
         rewrite_of_rewrite rw

   (*
    * Rewrite a term.
    * No justification.
    *)
   let zero_addr = TermAddr.make_address []

   let apply_rewrite bookmark conv t =
      let bookmark = Mp_resource.find bookmark in
      let arg = TacticInternal.debug_arg bookmark t in
      let tac = apply 0 zero_addr conv in
      let res = fst (TacticInternal.refine tac arg) in
      let goal, _ = Refine.dest_msequent (List.hd res).ref_goal in
         goal
end

(*
 * Internal version exports more functions.
 *)
module RewriteInternal = Rewrite

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
