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
open Lm_debug

open Refiner.Refiner
open Refiner.Refiner.TermAddr
open Refiner.Refiner.RefineError
open Refiner.Refiner.Rewrite
open Refiner.Refiner.Refine

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
   let env_term (arg, i, addr) =
      term_subterm (Sequent.nth_assum arg i) addr

   let env_arg (arg, i, addr) =
      arg

   (*
    * Create a conversion from a basic rewrite.
    * This function is required by filter_prog.
    *)
   let rewrite_of_pre_rewrite rw args =
      match rw, args with
         PrimRW rw, [] -> RewriteConv rw
       | PrimRW _, _ -> raise(Invalid_argument "Rewrite_boot.rewrite_of_pre_rewrite: PrimRW rewrites do not take arguments")
       | CondRW crw, _ -> CondRewriteConv (crw args)

   (*
    * No action.
    *)
   let idC = IdentityConv

   (*
    * Combine two lissts of conversion.
    * Note if the adjacent conversion can be combined.
    *)
   let combine rw_f crw_f make clist1 clist2 =
      match Flist.last clist1, Flist.first clist2 with
         RewriteConv rw1, RewriteConv rw2 ->
            let rw = RewriteConv (rw_f rw1 rw2) in
               if Flist.singleton clist1 & Flist.singleton clist2 then
                  rw
               else
                  make (Flist.append_skip clist1 rw clist2)
       | CondRewriteConv crw1, CondRewriteConv crw2 ->
            let crw = CondRewriteConv (crw_f crw1 crw2) in
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
      if conv1 == idC then conv2 else if conv2 == idC then conv1 else
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
      if conv1 == idC then idC else
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
    * Function conversion needs an argument.
    *)
   let funC f =
      FunConv f

   let termC f = funC (fun e -> (f (env_term e)))

   (*
    * Apply the conversion at the specified address.
    *)
   let addrLiteralC addr = function
      RewriteConv rw ->
         RewriteConv (rwaddr addr rw)
    | CondRewriteConv crw ->
         CondRewriteConv (crwaddr addr crw)
    | conv ->
         AddressConv (addr, conv)

   let addrC addr =
      addrLiteralC (make_address addr)

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

   let allSubC =
      let allSubCE conv env =
         let addrs = subterm_addresses (env_term env) in
            List.fold_left (fun conv' addr -> prefix_thenC (addrLiteralC addr conv) conv') idC addrs
      in
         fun conv -> funC (allSubCE conv)

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
    * Apply a tactic on all the auxillary subgoals
    *)
   let prefix_thenTC conv tac =
      ThenTC (conv, tac)

   (*
    * Build a fold conversion from the contractum and the unfolding conversion.
    * This function is often called from the top level of an .ml file, so we print
    * all the exceptions right away.
    *)
   let makeFoldC contractum = function
      (RewriteConv rw) as conv ->
         let mseq = mk_msequent contractum [] in
         let tac = rwtactic 0 rw in
            begin
               (* Apply the unfold conversion *)
               match
                  begin try
                     Refine.refine any_sentinal tac mseq
                  with RefineError(n, err) ->
                     Refine_exn.stderr_exn "" (RefineError("Rewrite_boot.makeFoldC", PairError("applying rewrite to the term failed", TermError contractum, n, err)))
                  end
               with
                  [redex], _ ->
                     (* Unfolded it, so create a rewrite that reverses it *)
                     let redex, _ = dest_msequent redex in
                     let rw' =
                        try
                           term_rewrite Strict empty_args_spec [redex] [contractum]
                        with RefineError(n, err) ->
                           Refine_exn.stderr_exn "" (RefineError("Rewrite_boot.makeFoldC", PairError("creating a rewrite from term to term failed",TermPairError(redex,contractum), n, err)))
                     in
                     let doCE env =
                        match apply_rewrite rw' empty_args (env_term env) [] with
                           [contractum] ->
                              FoldConv (contractum, conv)
                         | _ ->
                              Refine_exn.stderr_exn "" (RefineError ("Rewrite_boot.makeFoldC", StringTermError ("rewrite failed", redex)))
                     in
                        FunConv doCE
                | _ ->
                     Refine_exn.stderr_exn "" (RefineError ("Rewrite_boot.makeFoldC", StringTermError ("fold failed", contractum)))
            end
    | _ ->
         Refine_exn.stderr_exn "" (RefineError ("Rewrite_boot.makeFoldC", StringTermError("can't fold nontrivial rewrites",contractum)))

   (*
    * Cut just replaces the term an generates a rewrite
    * subgoal.
    *)
   let cutC t =
      CutConv t

   let rec apply assum addr = function
      RewriteConv rw ->
         if !debug_rewrite then
            eprintf "Rewrite_type.apply: Rewrite%t" eflush;
         Tactic.tactic_of_rewrite assum (rwaddr addr rw)
    | CondRewriteConv crw ->
         if !debug_rewrite then
            eprintf "Rewrite_type.apply: CondRewrite%t" eflush;
         Tactic.tactic_of_cond_rewrite assum (crwaddr addr crw)
    | ComposeConv clist ->
         if !debug_rewrite then
            eprintf "Rewrite_type.apply: Compose%t" eflush;
         composeT assum addr (Flist.tree_of_list clist)
    | ChooseConv clist ->
         if !debug_rewrite then
            eprintf "Rewrite_type.apply: Choose%t" eflush;
         chooseT assum addr (Flist.tree_of_list clist)
    | AddressConv (addr', conv) ->
         let addr = compose_address addr addr' in
            if !debug_rewrite then
               eprintf "Rewrite_type.apply: Address %s%t" (string_of_address addr') eflush;
            apply assum addr conv
    | IdentityConv ->
         if !debug_rewrite then
            eprintf "Rewrite_type.apply: Identity%t" eflush;
         TacticInternal.idT
    | FunConv f ->
         if !debug_rewrite then
            eprintf "Rewrite_type.apply: Fun%t" eflush;
         funT (fun p -> apply assum addr (f (p, assum, addr)))
    | HigherConv conv ->
         if !debug_rewrite then
            eprintf "Rewrite_type.apply: Higher%t" eflush;
         apply assum addr (higherLC conv)
    | FoldConv (t, conv) ->
         if !debug_rewrite then
            eprintf "Rewrite_type.apply: Fold%t" eflush;
         (prefix_thenLT (rwcutT assum addr t) [addHiddenLabelT "main"; solveCutT addr conv])
    | CutConv t ->
         if !debug_rewrite then
            eprintf "Rewrite_type.apply: Cut%t" eflush;
         rwcutT assum addr t
    | ThenTC (conv, tac) ->
         if !debug_rewrite then
            printf "Rewrite_type.apply: ThenTC%t" eflush;
         applyThenTC assum addr conv tac

   and composeT assum addr = function
      Flist.Empty ->
         idT
    | Flist.Leaf conv ->
         apply assum addr conv
    | Flist.Append (tree1, tree2) ->
         (prefix_then_OnFirstT (composeT assum addr tree1) (composeT assum addr tree2))

   and chooseT assum addr = function
      Flist.Empty ->
         idT
    | Flist.Leaf conv ->
         apply assum addr conv
    | Flist.Append (tree1, tree2) ->
         (prefix_orelseT (chooseT assum addr tree1) (chooseT assum addr tree2))

   and rwcutT assum addr t =
      funT (fun p ->
      let goal, hyps = Refine.dest_msequent (Sequent.msequent p) in
      let t' =
         if assum = 0 then
            goal
         else if assum > 0 && assum <= List.length hyps then
            List.nth hyps (pred assum)
         else
            raise (RefineError ("rwcutT", StringIntError ("assum number is out of range", assum)))
      in
      let t' = TermAddr.replace_subterm t' addr t in
         cutT t')

   and solveCutT addr conv =
      funT (fun p ->
      let len = List.length (snd (Refine.dest_msequent (Sequent.msequent p))) in
         (prefix_thenMT (apply len addr conv) (nthAssumT len)))

   and applyThenTC_aux tac = function
      [] -> []
    | _ :: tl -> idT :: (List.map (fun _ -> tac) tl)

   and applyThenTC assum addr conv tac =
      prefix_thenFLT (apply assum addr conv) (applyThenTC_aux tac)

   (*
    * Apply the rewrite.
    *)
   let rw conv assum addr =
      apply assum addr conv

   (*
    * Create an input form.
    * This is a Relaxed rewrite with no justification.
    *)
   let create_iform name strictp redex contractum =
      rewrite_of_pre_rewrite (create_input_form (null_refiner name) name strictp redex contractum) []

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

   let redex_and_conv_of_rw_annotation name _ redex _ _ args rw =
      match args with
         [] -> redex, rewrite_of_pre_rewrite rw []
       | _ -> raise (Invalid_argument(name ^ " resource does not support annotations on rewrites that take arguments"))
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
