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
 * See the file doc/htmlman/default.html or visit http://metaprl.org/
 * for more information.
 *
 * Copyright (C) 1998-2006 MetaPRL Group, Cornell University and California
 * Institute of Technology
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
 * Modified by: Aleksey Nogin <nogin@cs.caltech.edu>
 *)
open Lm_debug
open Lm_printf

open Rewrite_sig
open Refiner.Refiner
open Refiner.Refiner.TermAddr
open Refiner.Refiner.RefineError
open Refiner.Refiner.Rewrite
open Refiner.Refiner.Refine

open Opname
open Options_boot
open Tactic_boot
open Sequent_boot
open Tacticals_boot.Tacticals

let debug_rewrite = load_debug "rewrite"
let debug_profile_tactics = load_debug "profile_tactics"

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
   let env_term = snd
   let env_arg = fst

   (*
    * Create a conversion from a basic rewrite.
    * This function is required by filter_prog.
    *)
   let rewrite_of_pre_rewrite rw addrs args =
      match rw, addrs, args with
         PrimRW rw, { arg_ints = [||]; arg_addrs = [||] }, [] -> RewriteConv rw
       | PrimRW _, _, _ -> raise(Invalid_argument "Rewrite_boot.rewrite_of_pre_rewrite: PrimRW rewrites do not take arguments")
       | CondRW crw, _, _ -> CondRewriteConv (crw addrs args)

   (*
    * No action.
    *)
   let idC = IdentityConv

   (*
    * Combine two lists of conversions.
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

   let compose = combine andthenrw candthenrw (fun l -> ComposeConv l)
   let choose = combine orelserw corelserw (fun l -> ChooseConv l)

   let prefix_thenC conv1 conv2 =
      if conv1 == idC then
         conv2
      else if conv2 == idC then
         conv1
      else
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
      if conv1 == idC then
         idC
      else
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

   let tacticC tac =
      TacticConv tac

   let forceC debug conv =
      ForceConv (debug, conv)

   let withOptionC opt option conv =
      WithOptionConv (opt, option, conv)

   let withoutOptionC opt conv =
      WithoutOptionConv (opt, conv)

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

   let allSubC conv =
      AllSubConv conv

   (*
    * Apply the conversion at the highest addresses.
    *)
   let higherC = function
      RewriteConv rw ->
         RewriteConv (rwhigher rw)
  (* XXX: disabled
   *| CondRewriteConv crw ->
   *     CondRewriteConv (crwhigher crw)
   *)
    | (HigherConv _) as conv ->
         conv
    | conv ->
         HigherConv conv

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
                     Refine_exn.stderr_exn (RefineError("Rewrite_boot.makeFoldC", PairError("applying rewrite to the term failed", TermError contractum, n, err)))
                  end
               with
                  [redex], _ ->
                     (* Unfolded it, so create a rewrite that reverses it *)
                     let redex, _ = dest_msequent redex in
                     let rw' =
                        try
                           term_rewrite Rewrite_sig.Strict empty_args_spec [redex] [contractum]
                        with RefineError(n, err) ->
                           Refine_exn.stderr_exn (RefineError("Rewrite_boot.makeFoldC", PairError("creating a rewrite from term to term failed",TermPairError(redex,contractum), n, err)))
                     in
                     let doCE env =
                        match apply_rewrite rw' empty_args (env_term env) [] with
                           [contractum] ->
                              FoldConv (contractum, conv)
                         | _ ->
                              Refine_exn.stderr_exn (RefineError ("Rewrite_boot.makeFoldC", StringTermError ("rewrite failed", redex)))
                     in
                        FunConv doCE
                | _ ->
                     Refine_exn.stderr_exn (RefineError ("Rewrite_boot.makeFoldC", StringTermError ("fold failed", contractum)))
            end
    | _ ->
         Refine_exn.stderr_exn (RefineError ("Rewrite_boot.makeFoldC", StringTermError("can't fold nontrivial rewrites",contractum)))

   (*
    * Cut just replaces the term an generates a rewrite
    * subgoal.
    *)
   let cutC t =
      CutConv t

   let rec apply assum addr t = function
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
         composeT assum addr t (Flist.tree_of_list clist)
    | ChooseConv clist ->
         if !debug_rewrite then
            eprintf "Rewrite_type.apply: Choose%t" eflush;
         chooseT assum addr t (Flist.tree_of_list clist)
    | AddressConv (addr', conv) ->
         let addr = compose_address addr addr' in
         let t =
            match t with
               None -> None
             | Some t -> Some (term_subterm t addr')
         in
            if !debug_rewrite then
               eprintf "Rewrite_type.apply: Address %s%t" (string_of_address addr') eflush;
            apply assum addr t conv
    | IdentityConv ->
         if !debug_rewrite then
            eprintf "Rewrite_type.apply: Identity%t" eflush;
         TacticInternal.idT
    | FunConv f ->
         if !debug_rewrite then
            eprintf "Rewrite_type.apply: Fun%t" eflush;
         funT (fun p ->
            let t = term_subterm (Sequent.nth_assum p assum) addr in
               apply assum addr (Some t) (f (p, t)))
    | WithOptionConv (opt, option, c) ->
         if !debug_rewrite then
            eprintf "Rewrite_type.apply: WithOption@.";
         withOptionT opt option (apply assum addr t c)
    | WithoutOptionConv (opt, c) ->
         if !debug_rewrite then
            eprintf "Rewrite_type.apply: WithOption@.";
         withoutOptionT opt (apply assum addr t c)
    | (HigherConv _ | AllSubConv _) as conv when t = None ->
         funT (fun p -> apply assum addr (Some (term_subterm (Sequent.nth_assum p assum) addr)) conv)
    | (HigherConv conv) as hconv ->
         if !debug_rewrite then
            eprintf "Rewrite_type.apply: Higher%t" eflush;
         prefix_orelseT (apply assum addr t conv) (funT (fun _ -> apply assum addr t (AllSubConv hconv)))
    | AllSubConv conv ->
         if !debug_rewrite then
            eprintf "Rewrite_type.apply: AllSub%t" eflush;
         let t' =
            match t with
               None -> raise(Invalid_argument "Rewrite_boot.apply: internal error")
             | Some t -> t
         in
         (*
          * We assume that applications of conv on subterms will not interfere,
          * so it's OK to reuse the same t even after the success of the first conv
          * makes it outdated.
          *)
         begin match subterm_addresses t' with
            [] -> idT
          | addr' :: addrs ->
               List.fold_left (**)
                  (fun tac addr' ->
                     prefix_then_OnFirstT (apply assum addr t (AddressConv (addr', conv))) tac)
                  (apply assum addr t (AddressConv (addr', conv)))
                  addrs
         end
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
         applyThenTC assum addr t conv tac
    | TacticConv tac ->
         if !debug_rewrite then
            printf "Rewrite_type.apply: TacticConv%t" eflush;
         if assum <> 0 then
            raise (RefineError ("rw", StringError "tacticC conversion can not be applied to an assumption"));
         tac addr
    | ForceConv (debug, conv) ->
         forceT debug (apply assum addr t conv)

   and composeT assum addr t = function
      Flist.Empty ->
         idT
    | Flist.Leaf conv ->
         apply assum addr t conv
    | Flist.Append (tree1, tree2) ->
         (prefix_then_OnFirstT (composeT assum addr t tree1) (composeT assum addr None tree2))

   and chooseT assum addr t = function
      Flist.Empty ->
         idT
    | Flist.Leaf conv ->
         apply assum addr t conv
    | Flist.Append (tree1, tree2) ->
         (prefix_orelseT (chooseT assum addr t tree1) (chooseT assum addr t tree2))

   and rwcutT assum addr t =
      funT (fun p ->
      let goal, assums = Refine.dest_msequent (Sequent.msequent p) in
      let t' =
         if assum = 0 then
            goal
         else if assum > 0 && assum <= List.length assums then
            List.nth assums (pred assum)
         else
            raise (RefineError ("rwcutT", StringIntError ("assum number is out of range", assum)))
      in
      let t' = TermAddr.replace_subterm t' addr t in
         cutT t')

   and solveCutT addr conv =
      funT (fun p ->
      let len = List.length (snd (Refine.dest_msequent (Sequent.msequent p))) in
         (prefix_thenMT (apply len addr None conv) (nthAssumT len)))

   and applyThenTC_aux tac = function
      [] -> []
    | _ :: tl -> idT :: (List.map (fun _ -> tac) tl)

   and applyThenTC assum addr t conv tac =
      prefix_thenFLT (apply assum addr t conv) (applyThenTC_aux tac)

   (*
    * Apply the rewrite.
    *)
   let rw =
      if !debug_profile_tactics then
         fun conv assum addr -> timing_wrap "rw" (apply assum addr None conv)
      else
         fun conv assum addr -> apply assum addr None conv

   (*
    * Create an input form.
    * This is a Relaxed rewrite with no justification.
    *)
   let create_iform name strictp redex contractum =
      rewrite_of_pre_rewrite (create_input_form (null_refiner name) name strictp redex contractum) empty_rw_args []

   let create_ml_iform name f =
      rewrite_of_pre_rewrite (create_ml_rewrite (null_refiner name) name f) empty_rw_args []

   (*
    * Rewrite a term.
    * No justification.
    *)
   let apply_rewrite bookmark conv t =
      let arg = TacticInternal.debug_arg bookmark t in
      let tac = apply 0 null_address None conv in
      let res = fst (TacticInternal.refine tac arg) in
      let goal, _ = Refine.dest_msequent (List.hd res).ref_goal in
         goal

   let redex_and_conv_of_rw_annotation name ?labels rwname redex _ _ addrs args loc rw =
      rule_labels_not_allowed loc labels;
      match addrs, args with
         { spec_ints = [||]; spec_addrs = [||] }, [] -> [redex, rewrite_of_pre_rewrite rw empty_rw_args []]
       | _ -> raise (Invalid_argument (sprintf "%s: rewrite %s: %s resource does not support annotations on rewrites that take arguments" (Simple_print.string_of_loc loc) rwname name))
end

(*
 * -*-
 * Local Variables:
 * End:
 * -*-
 *)
