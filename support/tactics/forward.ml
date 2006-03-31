(*
 * Forward-chaining tactic.  This is very much like an elimination
 * tactic, but we have a check to ensure that progress is being made.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2005-2006 Mojave Group, Caltech
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
 * Author: Jason Hickey @email{jyh@cs.caltech.edu}
 * Modified by: Aleksey Nogin @email{nogin@cs.caltech.edu}
 * @end[license]
 *)
extends Top_tacticals

open Lm_debug
open Lm_printf
open Lm_int_set
open Lm_dag_sig
open Lm_imp_dag

open Term_sig
open Rewrite_sig
open Refiner.Refiner
open Refiner.Refiner.TermType
open Refiner.Refiner.Term
open Refiner.Refiner.TermAddr
open Refiner.Refiner.TermSubst
open Refiner.Refiner.TermMan
open Refiner.Refiner.TermMeta
open Refiner.Refiner.Rewrite
open Refiner.Refiner.RefineError
open Term_match_table
open Term_hash_code
open Simple_print

open Tactic_type
open Tactic_type.Tactic
open Tactic_type.Tacticals
open Options_boot
open Top_conversionals

let debug_forward =
   create_debug (**)
      { debug_name = "forward";
        debug_description = "display forward-chaining operations";
        debug_value = false
      }

(*
 * Arguments to forward-chaining.
 *)
type forward_prec = unit ImpDag.node

type forward_option =
   ForwardArgsOption of (tactic_arg -> term -> term list) * term option
 | ForwardPrec of forward_prec

type forward_info =
   { forward_loc  : MLast.loc;
     forward_prec : forward_prec;
     forward_tac  : int -> tactic
   }

type forward_result = (int -> tactic) -> int -> tactic_arg -> tactic

(*
 * Precedences.
 *)
let dag = ImpDag.create ()

let create_forward_prec before after =
   let node = ImpDag.insert dag () in
      List.iter (fun p -> ImpDag.add_edge dag p node) before;
      List.iter (fun p -> ImpDag.add_edge dag node p) after;
      node

let forward_trivial_prec = create_forward_prec [] []
let forward_normal_prec = create_forward_prec [forward_trivial_prec] []
let forward_max_prec = create_forward_prec [forward_trivial_prec] []

let equal_forward_prec = ImpDag.eq

let forward_precs () =
   ImpDag.sort dag

(************************************************************************
 * Build a term table from the sequent.
 * This is a term set indexed by hash code.
 *)
module type TermTableSig =
sig
   type t

   val empty : t
   val add : t -> term -> t
   val mem : t -> term -> bool
end;;

module TermTable =
struct
   type t

   let empty = IntMTable.empty

   let add table t =
      IntMTable.add table (hash_term t) t

   let mem table t =
      try
         let tl = IntMTable.find_all table (hash_term t) in
            List.exists (fun t' -> alpha_equal t' t) tl
      with
         Not_found ->
            false
end;;

(*
 * Extract the elimination tactic from the table.
 *)
let extract_forward_data =
   (*
    * Build a table from the hyps.
    *)
   let term_table_of_hyps hyps =
      SeqHyp.fold (fun table _ h ->
            match h with
               Hypothesis (_, t) ->
                  TermTable.add table t
             | Context _ ->
                  table) TermTable.empty hyps
   in
   (*
    * Check that only one subgoal is labeled "main" (or "empty")
    *)
   let check_main_fun loc pl =
      let main_count =
         List.fold_left (fun main_count p ->
               match Sequent.label p with
                  "main"
                | "" ->
                     succ main_count
                | _ ->
                     main_count) 0 pl
      in
         if main_count <> 1 then
            raise (RefineForceError ("Forward.check_main",
                                     Printf.sprintf "this rule produced %d subgoals labeled \"main\"
and it should produce exactly one" main_count,
                                     StringError (string_of_loc loc)))
   in
   let checkMainT loc tac =
      subgoalsCheckT (check_main_fun loc) tac
   in
   (* Select the correct precedence *)
   let select pre1 { forward_prec = pre2 } =
      equal_forward_prec pre2 pre1
   in
   (* We assume no progress when there are no new hyps and the concl has not changed. *)
   let not_changed_err = RefineError("forwardChainT", StringError "no progress") in
   (* Merge the new hyps into the list, chacking if any are new *)
   let rec search hyps length new_hyps thin_hyps changed i =
      if i = length then
         new_hyps, thin_hyps, changed
      else
         let new_hyps, thin_hyps, changed =
            match SeqHyp.get hyps i with
               Hypothesis (_, t) ->
                  if TermTable.mem new_hyps t then
                     new_hyps, (i+1) :: thin_hyps, changed
                  else
                     TermTable.add new_hyps t, thin_hyps, true
             | Context _ ->
                  new_hyps, thin_hyps, changed
         in
            search hyps length new_hyps thin_hyps changed (succ i)
   in
   let rec search_tac thinT hyps length new_hyps tac changed i =
      if i = length then
         tac, changed
      else
         match SeqHyp.get hyps i with 
            Hypothesis (_, t) when TermTable.mem new_hyps t ->
               let i = i + 1 in
                  search_tac thinT hyps length new_hyps ((tryT (thinT i)) thenT tac) changed i
          | _ ->
               let i = i + 1 in
                  search_tac thinT hyps length new_hyps (rw simpleReduceC i thenT tac) true i
   in
   (fun tbl ->
      (* Check progress and either abort or do to next tactic in the current table lookup *)
      let rec progress_check thinT precs orig_hyps orig_concl orig_length i cont p =
         let seq = Sequent.explode_sequent_arg p in
         let hyps = seq.sequent_hyps in
         let length = SeqHyp.length hyps in
         let hyps, thin_hyps, changed = search hyps length orig_hyps [] false orig_length in
         let concl = seq.sequent_concl in
            if changed || not (alpha_equal concl orig_concl) then
                  tryOnHypsT thin_hyps thinT thenT (**)
                  funT (step_cont thinT precs hyps concl length i cont)
            else
               raise not_changed_err
      (* Follow-up tactic: thin repeats, simple reduce new hyps *)
      and follow_up thinT precs orig_hyps (orig_concl:term) orig_length i cont p =
         let seq = Sequent.explode_sequent_arg p in
         let hyps = seq.sequent_hyps in
         let length = SeqHyp.length hyps in
            if length < orig_length then
               (* This was a thinning step, no new hyps to post-process *)
               step thinT precs orig_hyps orig_concl length (min i length) p
            else
               let tac, changed = search_tac thinT hyps length orig_hyps idT false orig_length in
                  if changed || not (alpha_equal seq.sequent_concl orig_concl) then
                     tac thenT funT (progress_check thinT precs orig_hyps orig_concl orig_length i cont)
                  else
                     raise not_changed_err
      (* Process a table lookup *)
      and step_cont thinT precs hyps concl length i (cont : forward_info lazy_lookup) p =
         match cont () with
            Some (item, cont) ->
               ((let tac = item.forward_tac in
                  if !debug_forward then checkMainT item.forward_loc (tac i) else tac i)
               thenMT (funT (follow_up thinT precs hyps concl length i cont)))
               orelseT (funT (step_cont thinT precs hyps concl length i cont))
          | None ->
               step thinT precs hyps concl length i p
      (* Process a hyp *)
      and step thinT all_precs hyps concl length i p =
         match all_precs with
            pre :: precs ->
               if i = length then
                  step thinT precs hyps concl length 0 p
               else
                  begin match SeqHyp.get (Sequent.explode_sequent_arg p).sequent_hyps i with
                     Hypothesis(_, t) ->
                        step_cont thinT all_precs hyps concl length (i + 1) (lookup_all tbl (select pre) t) p
                   | Context _ ->
                        step thinT all_precs hyps concl length (i + 1) p
                  end
          | _ ->
               idT
      in
      (fun thinT i p ->
         let { sequent_hyps = hyps;
               sequent_concl = concl
             } = Sequent.explode_sequent_arg p
         in
            step thinT (forward_precs ()) (term_table_of_hyps hyps) concl (SeqHyp.length hyps) i p))

let resource (term * forward_info, forward_result) forward =
   table_resource_info extract_forward_data

(*
 * Get explicit arguments to the elimination rule.
 *)
let rec get_elim_args_arg = function
   ForwardArgsOption (f, arg) :: t ->
      Some (f, arg)
 | _ :: t ->
      get_elim_args_arg t
 | [] ->
      None

let one_rw_arg i =
   { arg_ints = [| i |]; arg_addrs = [||] }

(*
 * Precedence.
 *)
let rec get_prec_arg assums = function
   ForwardPrec pre :: _ ->
      pre
 | _ :: t ->
      get_prec_arg assums t
 | [] ->
      (*
       * If there are no wf subgoals, then we can use at the trivial prec.
       * Otherwise postpone as long as possible.
       *)
      match assums with
         [_] -> forward_trivial_prec
       | _ -> forward_max_prec

(*
 * Process a forward-chaining rule.
 *)
let process_forward_resource_annotation ?(options = []) ?labels name args term_args statement loc pre_tactic =
   if args.spec_addrs <> [||] then
      raise (Invalid_argument (sprintf "elim annotation: %s: context arguments not supported yet" name));
   rule_labels_not_allowed loc labels;

   let assums, goal = unzip_mfunction statement in
      match SeqHyp.to_list (explode_sequent goal).sequent_hyps with
         [Context _; Hypothesis(v, t); Context _] ->
            (*
             * Define a function (term_args i p) that returns the actual
             * term arguments during rule application.
             *)
            let term_args =
               match term_args with
                  [] ->
                     (fun _ _ -> [])
                | _ ->
                     match get_elim_args_arg options with
                        Some (f, arg) ->
                           (* There are some explicit elimination arguments *)
                           let get_arg =
                              match arg with
                                 None ->
                                    (fun p i -> Sequent.nth_hyp p i)
                               | Some arg ->
                                    (match find_subterm t (fun t _ -> alpha_equal t arg) with
                                        addr :: _ ->
                                           (fun p i -> term_subterm (Sequent.nth_hyp p i) addr)
                                      | [] ->
                                           raise (RefineError("intro annotation", (**)
                                                                 StringTermError("term not found in the conclusion", arg))))
                           in
                              (fun i p -> f p (get_arg p i))
                      | None ->
                           (* Get the term arguments from the rule statement *)
                           let length = List.length term_args in
                              (fun _ p ->
                                    let args =
                                       match get_with_args p with
                                          Some args ->
                                             args
                                        | None ->
                                             raise (RefineError (name, StringIntError ("arguments required", length)))
                                    in
                                    let length' = List.length args in
                                       if length' != length then
                                          raise (RefineError (name, StringIntError ("wrong number of arguments", length')));
                                       args)
            in

            (*
             * Get the precedence.
             *)
            let pre = get_prec_arg assums options in

            (*
             * Define the tactic for forward chaining.
             *)
            let tac =
               match args.spec_ints with
                  [| _ |] ->
                     argfunT (fun i p ->
                           if !debug_forward then
                              eprintf "forwardT elim: trying %s%t" name eflush;
                           Tactic_type.Tactic.tactic_of_rule pre_tactic (one_rw_arg i) (term_args i p))
                | _ ->
                     raise (Invalid_argument (sprintf "forwardT: %s: not an elimination rule" name))
            in
            let info =
               { forward_loc  = loc;
                 forward_prec = pre;
                 forward_tac  = tac
               }
            in
               [t, info]
       | _ ->
            raise (Invalid_argument (sprintf "forwardT.improve_elim: %s: must be an elimination rule" name))

let doForwardChainT =
   argfunT (fun thinT p -> Sequent.get_resource_arg p get_forward_resource thinT 0 p)

let doForwardT =
   argfun2T (fun thinT i p ->
      Sequent.get_resource_arg p get_forward_resource thinT ((Sequent.get_pos_hyp_num p i) - 1) p)

(*
 * -*-
 * Local Variables:
 * End:
 * -*-
 *)
