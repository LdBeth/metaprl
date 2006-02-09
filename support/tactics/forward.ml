(*
 * Forward-chaining tactic.  This is very much like an elimination
 * tactic, but we have a check to ensure that progress is being made.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2005 Mojave Group, Caltech
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
 * @email{jyh@cs.caltech.edu}
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

let checkMainT loc tac =
   subgoalsCheckT (check_main_fun loc) tac

(*
 * Extract the elimination tactic from the table.
 *)
let extract_forward_data =
   let rec alliT i (cont : forward_info lazy_lookup) =
      try
         let { forward_loc = loc; forward_tac = tac }, cont = cont () in
            tryT (checkMainT loc (tac i)) thenMT alliT i cont
      with
         Not_found ->
            idT
   in
   let step tbl =
      argfun2T (fun select i p ->
            let t = Sequent.nth_hyp p i in
            let () =
               if !debug_forward then
                  eprintf "forwardT: elim: lookup %s%t" (SimplePrint.short_string_of_term t) eflush
            in
            let tacs =
               try
                  lookup_all tbl select t
               with
                  Not_found ->
                     raise (RefineError ("extract_forward_data", StringTermError ("forwardT doesn't know about", t)))
            in
               alliT i tacs)
   in
      step

let resource (term * forward_info, (forward_info -> bool) -> int -> tactic) forward =
   table_resource_info extract_forward_data

let select_true _ =
   true

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
                                       try get_with_args p with
                                          RefineError _ ->
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

let forward_proof p =
   Sequent.get_resource_arg p get_forward_resource

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
 * Build a table from the hyps.
 *)
let term_table_of_hyps hyps =
   SeqHyp.fold (fun table _ h ->
         match h with
            Hypothesis (_, t) ->
               TermTable.add table t
          | Context _ ->
               table) TermTable.empty hyps

(*
 * Merge the new hyps into the list.
 * Fails if there are no new hyps and the concl has not changed.
 *)
let progress_check orig_hyps orig_concl orig_length p =
   let { sequent_hyps = hyps;
         sequent_concl = concl
       } = explode_sequent (Sequent.goal p)
   in
   let length = SeqHyp.length hyps in
   let rec search new_hyps changed i =
      if i = length then
         new_hyps, changed
      else
         let new_hyps, changed =
            match SeqHyp.get hyps i with
               Hypothesis (_, t) ->
                  if TermTable.mem new_hyps t then
                     new_hyps, changed
                  else
                     TermTable.add new_hyps t, true
             | Context _ ->
                  new_hyps, changed
         in
            search new_hyps changed (succ i)
   in
   let hyps, changed =
      if length <= orig_length then
         orig_hyps, false
      else
         search orig_hyps false orig_length
   in
   let changed = changed || not (alpha_equal concl orig_concl) in
      hyps, concl, length, changed

(*
 * Forward-chain, then repeat if something has changed.
 *)
let rec step forward_tac orig_hyps orig_concl orig_length i depth bound =
   if depth = bound || i = orig_length then
      idT
   else
      let i = succ i in
      let depth = succ depth in
         funT (fun p -> (**)
            (forward_tac i thenMT progress forward_tac orig_hyps orig_concl orig_length i depth bound)
            orelseT step forward_tac orig_hyps orig_concl orig_length i depth bound)

and progress forward_tac orig_hyps orig_concl orig_length i depth bound =
   funT (fun p -> (**)
      let new_hyps, new_concl, new_length, changed = progress_check orig_hyps orig_concl orig_length p in
         if changed then
            step forward_tac new_hyps new_concl new_length i depth bound
         else
            raise (RefineError ("Forward.progress", StringError "no progress")))

(*
 * Single step.
 *)
let single_progress orig_hyps orig_concl orig_length =
   funT (fun p -> (**)
      let _, _, _, changed = progress_check orig_hyps orig_concl orig_length p in
         if changed then
            idT
         else
            raise (RefineError ("Forward.progress", StringError "no progress")))

let single_step forward_tac orig_hyps orig_concl orig_length i =
   funT (fun p -> forward_tac i thenMT single_progress orig_hyps orig_concl orig_length)

(*
 * Describe the original sequent.
 *)
let start_info p =
   let { sequent_hyps = hyps;
         sequent_concl = concl
       } = explode_sequent (Sequent.goal p)
   in
   let len = SeqHyp.length hyps in
   let hyps = term_table_of_hyps hyps in
      len, hyps, concl

let forwardT i =
   funT (fun p ->
         let forward_tac = forward_proof p select_true in
         let len, hyps, concl = start_info p in
         let i = Sequent.get_pos_hyp_num p i in
            single_step forward_tac hyps concl len i)

let forwardChainBoundPrecT pre bound =
   funT (fun p ->
         let forward_tac = forward_proof p pre in
         let len, hyps, concl = start_info p in
            step forward_tac hyps concl len 1 0 bound)

let forwardChainBoundT bound =
   funT (fun p -> (**)
      let rec searchT precs =
         match precs with
            pre1 :: precs ->
               let select { forward_prec = pre2 } =
                  equal_forward_prec pre2 pre1
               in
                  forwardChainBoundPrecT select bound
                  thenMT searchT precs
          | [] ->
               idT
      in
         searchT (forward_precs ()))

let forwardChainT = forwardChainBoundT max_int

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
