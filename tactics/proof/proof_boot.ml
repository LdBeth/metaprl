(*
 * A proof is a collection of inferences, where each inference is
 * a proof step or it is a nested proof.  Each inference
 * has the same goal as a subgoal of a previous inference.
 *
 *                   Goal           status:
 *                    |                bad: one if the proof_items has failed
 *                    |                partial: some incomplete subgoals
 *                    |                asserted: pretend like the proof is complete
 *                    |                complete: all steps have been checked
 *                    |
 *                   Item           proof_item
 *                  / | \
 *                 /  |  \
 *                /   |   \
 *               C1   C2  C3        children
 *              / |   |   | \
 *             /  |   |   |  \
 *            .   .   .   .   .
 *           .    .   .   .    .
 *          SG1  SG2 SG3 SG4  SG5   subgoals
 *
 * We also provide tools for navigation:
 *    1. Get the parent inference
 *    2. Get a subgoal inference
 *    3. Replace a subgoal inference
 *    4. Replace the tactic of the current inference
 *
 * These are functional structures, and they are singly linked from the
 * parents toward the leaves.  Navigation up the tree takes log time.
 *
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
 * Copyright (C) 1999 Jason Hickey, Cornell University
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
 * Modified by: Aleksey Nogin <nogin@cs.cornell.edu>
 *)
open Lm_debug
open Lm_printf
open Lm_printf_rbuffer
open Lm_thread

open Opname
open Refiner.Refiner
open TermMan
open RefineError
open Refine

open Refiner_sig
open Refiner_io

open Lm_rformat
open Dform

open Term_eq_table
open Mp_resource

open Tactic_boot_sig
open Tactic_boot
open Tactic_boot.TacticInternalType
open Tactic_boot.TacticInternal

(*
 * Show that the file is loading.
 *)
let _ =
   show_loading "Loading Proof%t"

let debug_proof =
   create_debug (**)
      { debug_name = "proof";
        debug_description = "show proof operations";
        debug_value = false
      }

let debug_proof_normalize =
   create_debug (**)
      { debug_name = "proof_normalize";
        debug_description = "show proof normalization";
        debug_value = false
      }

let debug_proof_pending =
   create_debug (**)
      { debug_name = "proof_pending";
        debug_description = "show proof Pending operations";
        debug_value = false
      }

type term_io = Refiner_io.TermType.term

module Proof =
struct
   (************************************************************************
    * TYPES                                                                *
    ************************************************************************)

   type tactic_arg = TacticInternalType.tactic_arg
   type tactic = TacticInternalType.tactic
   type extract = TacticInternalType.extract
   type sentinal = TacticInternalType.sentinal
   type raw_attribute = TacticInternalType.raw_attribute
   type raw_attributes = raw_attribute list

   type status =
      StatusBad
    | StatusIncomplete
    | StatusPartial
    | StatusComplete

   (*
    * An address is a integer path.
    * A 0 in the address means a nested proof,
    * and n means child (n - 1) (starting from 0).
    *)
   type address = int list

   type proof = extract

   (*
    * Description of the refinement.
    *)
   type step_expr =
      ExprGoal
    | ExprIdentity
    | ExprUnjustified
    | ExprExtract of arglist
    | ExprCompose of step_expr
    | ExprWrapped of arglist
    | ExprRule of string * MLast.expr

   (*
    * Info about a step of the proof.
    *)
   type step_info =
      { step_goal : proof list;
        step_status : status;
        step_expr : step_expr;
        step_subgoals : proof list list;
        step_extras : proof list;
      }

   (*
    * This is the function that gets called when
    * a proof is changed.
    *)
   type update_fun = proof -> proof

   (*
    * We overload the refinement error to give the location of
    * the error.
    *)
   exception ExtRefineError of string * extract * refine_error
   exception ProofRefineError of string * proof * address * refine_error

   (*
    * Unchanged exception for some operations that want to
    * signal that they did nothing.
    *)
   exception Unchanged

   (************************************************************************
    * BASIC CACHE                                                          *
    ************************************************************************)

   (*
    * A cache is a map: msequent -> extract
    *)
   module CacheBase =
   struct
      type data = extract

      let append = Lm_list_util.unionq
   end

   module Cache = MakeMsequentTable (CacheBase);;

   (*
    * Cache is actually imperative.
    * There is a single cache shared by all threads.
    * NOTE: performance would be improved if we have separate
    * caches (use State.private_val).
    *)
   let cache_entry =
      let default = ref (Cache.empty) in
         State.shared_val "Proof_boot.cache" default

   (************************************************************************
    * PROOF PRINTING                                                       *
    ************************************************************************)

   (*
    * This function is really only used for debugging.
    *)
   let format_proof db buf proof =
      format_string buf "Proof:";
      format_hspace buf;
      format_extract db buf proof

   (*
    * BUG JYH: we should probably make the db type abstract,
    * construct this triple in the Dform module.
    *)
   let print_ext ext =
      let buf = Lm_rformat.new_buffer () in
      let db = "prl", find_dftable Mp_resource.top_bookmark, null_state in
         format_extract db buf ext;
         format_newline buf;
         prerr_rbuffer buf;
         flush stderr

   (************************************************************************
    * NORMALIZATION                                                        *
    ************************************************************************)

   let rec count_leaves = function
      Goal _ | Identity _ -> 1
    | Unjustified (_, leaves)
    | Extract (_, leaves, _) -> List.length leaves
    | Wrapped (_, goal) -> count_leaves goal
    | Compose ci -> count_leaves_aux ci.comp_subgoals
    | RuleBox ri -> count_leaves_aux ri.rule_subgoals
    | Pending f -> count_leaves (f ())
    | Locked ext -> count_leaves ext

   and count_leaves_aux = function
      goal :: subgoals -> count_leaves goal + count_leaves_aux subgoals
    | [] -> 0

   let rec all_identity = function
      [] -> true
    | Identity _ :: tl -> all_identity tl
    | _ -> false

   (* Replace the current goal with an equivalent one *)
   let rec replace_goal node goal =
      match node with
         Goal _ | RuleBox _ | Pending _ ->
            raise (Invalid_argument "Proof_boot.replace_goal")
       | Unjustified (g,sgs) ->
            if g==goal then node else Unjustified (goal,sgs)
       | Extract (g,sgs,ext) ->
            if g==goal then node else Extract (goal,sgs,ext)
       | Wrapped (args,ext) ->
            let res = replace_goal ext goal in
            if res == ext then node else Wrapped (args,res)
       | Compose ci ->
            let ext = ci.comp_goal in
            let res = replace_goal ext goal in
            if res == ext then node else
            Compose {
               comp_status = ci.comp_status;
               comp_goal = res;
               comp_subgoals = ci.comp_subgoals;
               comp_leaves = ci.comp_leaves;
               comp_extras = ci.comp_extras }
       | Locked ext ->
            let res = replace_goal ext goal in
            if res == ext then node else Locked res
       | Identity g ->
            if g==goal then node else Identity goal

   (* Replace subgoals with equivalent ones *)

   let rec replace_list gs = function
      [] -> gs, []
    | (sg :: sgs) as allx ->
         begin match gs with
            sg':: gs ->
               let gs,sgs' = replace_list gs sgs in
               if sg == sg' && sgs == sgs' then gs, allx else gs, sg'::sgs'
          | _ -> raise (Invalid_argument "Proof_boot.replace_list")
         end

   let rec replace_subg_aux gs node =
      match node with
         Goal _ | RuleBox _ | Pending _ ->
            if !debug_proof_normalize then begin
               eprintf "Proof_boot.replace_subg_aux: found Goal _ | RuleBox _ | Pending _ :\n";
               print_ext node
            end;
            raise (Invalid_argument "Proof_boot.replace_subg_aux")
       | Unjustified (g,sgs) ->
            let gs, res = replace_list gs sgs in
            if res==sgs then gs,node else gs, Unjustified (g,res)
       | Extract (g,sgs,ext) ->
            let gs, res = replace_list gs sgs in
            if res==sgs then gs,node else gs, Extract (g,res,ext)
       | Wrapped (args,ext) ->
            let gs, res = replace_subg_aux gs ext in
            if res==ext then gs, node else gs, Wrapped (args,res)
       | Compose ci ->
            let exts = ci.comp_subgoals in
            let gs,res = replace_subg_list gs exts in
            if res==exts then gs,node else
            gs, Compose {
               comp_status = ci.comp_status;
               comp_goal = ci.comp_goal;
               comp_subgoals = res;
               comp_leaves = LazyLeavesDelayed;
               comp_extras = ci.comp_extras }
       | Locked ext ->
            let gs,res = replace_subg_aux gs ext in
            if res == ext then gs, node else gs, Locked res
       | Identity g ->
            begin match gs with
               res::gs ->
                  if g==res then gs, node else gs, Identity res
             | _ -> raise (Invalid_argument "Proof_boot.replace_subg_aux")
            end

   and replace_subg_list gs = function
      [] -> gs, []
    | (sg :: sgs) as allx ->
         let gs,sg' = replace_subg_aux gs sg in
         let gs,sgs' = replace_subg_list gs sgs in
         if sg == sg' && sgs == sgs' then gs, allx else gs, sg'::sgs'

   let rec dest_ids = function
      [] -> []
    | Identity g :: tl -> g :: (dest_ids tl)
    | _ -> raise (Invalid_argument "Proof_boot.dest_ids")

   let replace_subg ext sgs =
      if !debug_proof_normalize then begin
         eprintf "replace_subg called on:\n";
         print_ext ext
      end;
      match replace_subg_aux (dest_ids sgs) ext with
         [], ext -> ext
       | _ -> raise (Invalid_argument "Proof_boot.replace_subg")

   let rec normalize ext = match ext with
      Pending f ->
         let pext = f () in
         if !debug_proof_pending then begin
            eprintf "Normalizing Pending:\n";
            print_ext pext
         end;
         let next = normalize pext in
            if (pext != next) then begin
               if !debug_proof_pending then begin
                  eprintf "Normalized Pending to:\n";
                  print_ext next
               end;
               next
            end else begin
               if !debug_proof_pending then eprintf "Normalizing left Pending unchanged!\n";
               ext
            end
    | Wrapped (l,e) ->
         let e' = normalize e in
         if (e == e') then ext else Wrapped (l,e')
    | Compose ci ->
         let c_goal = normalize ci.comp_goal in
         let c_subgs = normalize_list ci.comp_subgoals in begin
            match c_goal with
               Identity goal ->
                  begin match c_subgs with
                     [subg] -> replace_goal subg goal
                   | _ ->
                        print_ext ext;
                        raise (Invalid_argument "Proof_boot.normalize - this is not supposed to happen")
                  end
             | _ when all_identity c_subgs ->
                  replace_subg c_goal c_subgs
             | Compose ci' ->
                  Compose {
                     comp_status = LazyStatusDelayed;
                     comp_goal = ci'.comp_goal;
                     comp_subgoals = join_subgoals ci'.comp_subgoals c_subgs;
                     comp_leaves = LazyLeavesDelayed;
                     comp_extras = ci.comp_extras @ ci'.comp_extras
                  }
             | RuleBox _ ->
                  print_ext ext;
                  raise (Invalid_argument "Proof_boot.normalize - this is not supposed to happen")
             | _ ->
                  if (c_goal==ci.comp_goal) && (c_subgs == ci.comp_subgoals) then ext else
                  Compose { ci with comp_goal = c_goal; comp_subgoals = c_subgs }
            end
    | RuleBox ri ->
         if not ri.rule_extract_normalized then begin
            if !debug_proof_normalize then begin
               eprintf "Normalizing RuleBox's rule_extract:\n";
               print_ext ri.rule_extract;
            end;
            let res = normalize ri.rule_extract in
            if !debug_proof_normalize then begin
               if res == ri.rule_extract then eprintf "Normalization left it unchanged!%t" eflush
               else begin
                  eprintf "Normalized to:\n";
                  print_ext res
               end
            end;
            ri.rule_extract <- res;
            ri.rule_extract_normalized <- true
         end;
         ext
    | _ -> ext

   and normalize_list l = Lm_list_util.smap normalize l

   and join_subgoals sg1 sg2 =
      if all_identity sg1 then sg2 else
         join_subgoals_aux sg2 sg1

   and join_subgoals_aux sgs = function
      [] ->
         if sgs != [] then raise (Invalid_argument "Proof_boot.join_subgoals_aux") else []
    | hd :: tl ->
         let sghd,sgtl = split_subgoals (count_leaves hd) sgs in
         let c = Compose {
            comp_status = LazyStatusDelayed;
            comp_goal = hd;
            comp_subgoals = sghd;
            comp_leaves = LazyLeavesDelayed;
            comp_extras = []
         } in
            (normalize c) :: (join_subgoals_aux sgtl tl)

   (* this is just a counting excersize *)
   and split_subgoals i l =
      if i=0 then [],l else match l with
       | hd'::tl' ->
            let sghd,sgtl = split_subgoals (pred i) tl' in
               (hd'::sghd),sgtl
       | [] -> raise (Invalid_argument "Proof_boot.split_subgoals")

   (************************************************************************
    * BASIC NAVIGATION AND DESTRUCTION                                     *
    ************************************************************************)

   (*
    * Make a proof of an extract term.
    *)
   let create goal =
      Goal goal

   (*
    * Raise an error because the address is invalid.
    *)
   let raise_select_error proof node raddr i =
      raise (ProofRefineError ("select", proof, List.rev raddr, StringIntError ("illegal address", i)))

   (*
    * Choose a child by address.
    *)
   let select_subgoal proof node raddr goal subgoals extras i =
      if i < 0 then
         raise_select_error proof node raddr i
      else if i = 0 then
         goal
      else
         let i' = pred i in
         let len = List.length subgoals in
            if i' < len then
               List.nth subgoals i'
            else
               let i' = i' - len in
                  if i' < List.length extras then
                     List.nth extras i'
                  else
                     raise_select_error proof node raddr i

   let rec select_child proof node raddr i =
      match node with
         Goal _
       | Extract _
       | Identity _
       | Unjustified _ ->
            raise_select_error proof node raddr i
       | Compose { comp_goal = goal; comp_subgoals = subgoals; comp_extras = extras } ->
            if i = 0 then select_child proof goal raddr 0
            else select_subgoal proof node raddr goal subgoals extras i
       | Wrapped (_, goal) ->
            if i = 0 then
               goal
            else
               raise_select_error proof node raddr i
       | RuleBox ri ->
            if (i=0) && (not ri.rule_extract_normalized) then ignore(normalize node);
            select_subgoal proof node raddr ri.rule_extract ri.rule_subgoals ri.rule_extras i
       | Pending f ->
            select_child proof (f ()) raddr i
       | Locked ext ->
            select_child proof ext raddr i

   (*
    * Go the a particular path.
    *)
   let rec index_ext proof node raddr = function
      i :: tl ->
         index_ext proof (select_child proof node raddr i) (i :: raddr) tl
    | [] ->
         node

   let index_exn = RefineError ("Proof_boot.index", StringError "there is no proof node with such address")

   let index proof path =
      try index_ext proof proof [] path with
         ProofRefineError _ -> raise index_exn

   (************************************************************************
    * DESTRUCTORS                                                          *
    ************************************************************************)

   (*
    * Is the proof a leaf?
    *)
   let rec is_leaf_ext = function
      Goal _ ->
         true
    | Wrapped (_, ext) ->
         is_leaf_ext ext
    | _ ->
         false

   let is_leaf pf addr =
      is_leaf_ext (index pf addr)

   (*
    * Get the goal of the extract.
    *)
   let rec goal_ext = function
      Goal t
    | Identity t
    | Unjustified (t, _)
    | Extract (t, _, _) ->
         t
    | Compose { comp_goal = ext }
    | Wrapped (_, ext)
    | RuleBox { rule_extract = ext }
    | Locked ext ->
         goal_ext ext
    | Pending f ->
         goal_ext (f ())

   let goal pf addr =
      goal_ext (index pf addr)

   (*
    * Remove duplicates in a list of tactic args.
    * This is a quadratic algorithm, but the number
    * of leaves is usually small so its not worth doing
    * something smarter.
    *)
   let rec search_arg arg = function
      hd :: tl ->
         tactic_arg_alpha_equal hd arg || search_arg arg tl
    | [] ->
         false

   let rec remove_duplicates found = function
      hd :: tl ->
         if search_arg hd found then
            remove_duplicates found tl
         else
            hd :: remove_duplicates (hd :: found) tl
    | [] ->
         []

   (*
    * Concatenate the subgoals of the proof.
    * This returns a list of tactic_args
    *)
   let rec leaves_ext goal =
      let leaves =
         match goal with
            Goal t
          | Identity t ->
               [t]
          | Unjustified (_, leaves)
          | Extract (_, leaves, _) ->
               leaves
          | Wrapped (_, goal) ->
               leaves_ext goal
          | Compose ({ comp_leaves = leaves; comp_subgoals = subgoals } as comp) ->
               begin
                  match leaves with
                     LazyLeavesDelayed ->
                        let leaves = collect_leaves subgoals in
                           comp.comp_leaves <- LazyLeaves leaves;
                           leaves
                   | LazyLeaves leaves ->
                        leaves
               end
          | RuleBox ({ rule_leaves = leaves; rule_subgoals = subgoals } as info) ->
               begin
                  match leaves with
                     LazyLeavesDelayed ->
                        let leaves = collect_leaves subgoals in
                           info.rule_leaves <- LazyLeaves leaves;
                           leaves
                   | LazyLeaves leaves ->
                        leaves
               end
          | Pending f ->
               leaves_ext (f ())
          | Locked ext ->
               leaves_ext ext
      in
         if !debug_proof then begin
            let db = "prl", find_dftable Mp_resource.top_bookmark, null_state in
            let buf = Lm_rformat.new_buffer () in
               format_hzone buf;
               format_string buf "Leaves of";
               format_space buf;
               format_szone buf;
               format_extract db buf goal;
               format_ezone buf;
               format_space buf;
               print_ext goal;
               format_space buf;
               format_string buf "are";
               format_space buf;
               List.iter (format_arg db buf) leaves;
               format_ezone buf;
               prerr_rbuffer buf;
               eprintf "%t" eflush
         end;
         remove_duplicates [] leaves

   and collect_leaves = function
      [goal] ->
         leaves_ext goal
    | goal :: subgoals ->
         leaves_ext goal @ collect_leaves subgoals
    | [] ->
         []

   (*
    * Get the status of the current step.
    *)
   let translate_status = function
      LazyStatusBad ->
         StatusBad
    | LazyStatusPartial ->
         StatusPartial
    | LazyStatusIncomplete
    | LazyStatusDelayed ->
         StatusIncomplete
    | LazyStatusComplete ->
         StatusComplete

   let rec status_ext = function
      Goal _ ->
         LazyStatusPartial
    | Identity _
    | Extract _ ->
         LazyStatusComplete
    | Unjustified _ ->
         LazyStatusIncomplete
    | Compose ({ comp_status = status; comp_goal = goal; comp_subgoals = subgoals } as info) ->
         if status = LazyStatusDelayed then
            let status = compute_status goal subgoals in
               info.comp_status <- status;
               status
         else
            status
    | Wrapped (_, ext)
    | Locked ext ->
         status_ext ext
    | RuleBox ({ rule_status = status; rule_extract = goal; rule_subgoals = subgoals } as info) ->
         if status = LazyStatusDelayed then
            let status = compute_status goal subgoals in
               info.rule_status <- status;
               status
         else
            status
    | Pending f ->
         status_ext (f ())

   and compute_status goal subgoals =
      compute_status_subgoals (status_ext goal) subgoals

   and compute_status_subgoals status = function
      goal :: subgoals ->
         begin
            match status, status_ext goal with
               LazyStatusBad, _
             | _, LazyStatusBad ->
                  LazyStatusBad
             | LazyStatusIncomplete, _
             | _, LazyStatusIncomplete ->
                  compute_status_subgoals LazyStatusIncomplete subgoals
             | _, LazyStatusPartial ->
                  compute_status_subgoals LazyStatusPartial subgoals
             | LazyStatusPartial, LazyStatusComplete
             | LazyStatusComplete, LazyStatusComplete ->
                  compute_status_subgoals status subgoals
             | LazyStatusDelayed, _
             | _, LazyStatusDelayed ->
                  raise (Invalid_argument "Proof.compute_status_subgoals")
         end
    | [] ->
         status

   let status pf addr =
      translate_status (status_ext (index pf addr))

   (**********************************************************************
    * MAP FUNCTIONS                                                        *
    ************************************************************************)

   (*
    * Address error during replacement.
    *)
   let raise_replace_error proof node raddr i =
      raise (ProofRefineError ("replace", proof, List.rev raddr, StringIntError ("illegal address", i)))

   let norm_goal goal =
      let mseq = TermNorm.normalize_msequent goal.ref_goal in
         if mseq == goal.ref_goal then goal else {goal with ref_goal = mseq}

   (*
    * Replace the current goal of a node.
    *)
   let rec replace_goal_ext goal = function
      Goal _ ->
         Goal goal
    | RuleBox rb ->
         RuleBox { rb with
            rule_status = LazyStatusDelayed;
            rule_extract_normalized = true;
            rule_extract = replace_goal_ext goal rb.rule_extract
         }
    | node ->
         Unjustified (goal, leaves_ext node)

   (*
    * Match the subgoal list with the existing subgoals.
    * Split them into children and extras that don't match up.
    * Be careful not to change the arguments if not necessary.
    *)
   let match_subgoals_general find_leaf =
      let rec filter_subgoals = function
         Goal _ :: subgoals ->
            filter_subgoals subgoals
       | subgoal :: subgoals ->
            (goal_ext subgoal, subgoal) :: filter_subgoals subgoals
       | [] ->
            []
      in
      let filter_extra = function
         (_, Goal _) ->
            None
       | (_, arg) ->
            Some arg
      in
      let rec collect leaves subgoals =
         match leaves with
            leaf :: leaves ->
               let subgoal, subgoals = find_leaf leaf subgoals in
               let subgoals, extras = collect leaves subgoals in
                  subgoal :: subgoals, extras
          | [] ->
               [], Lm_list_util.some_map filter_extra subgoals
      in
         (fun leaves subgoals extras ->
               collect leaves (filter_subgoals (subgoals @ extras)))

   let make_goal goal = Goal (norm_goal goal)

   let rec find_leaf compare leaf = function
      (goal, subgoal) as h :: subgoals ->
         if compare goal leaf then
            subgoal, subgoals
         else
            let subgoal, subgoals = find_leaf compare leaf subgoals in
               subgoal, h :: subgoals
    | [] ->
         make_goal leaf, []

   let match_subgoals = match_subgoals_general (find_leaf tactic_arg_alpha_equal)

   let tactic_arg_match arg1 arg2 =
      let g1 = msequent_goal arg1.ref_goal in
      let g2 = msequent_goal arg2.ref_goal in
      try
         let e1 = explode_sequent g1 in
         let e2 = explode_sequent g2 in
         begin try
            ignore(Match_seq.match_hyps e1 e2);
            true
         with RefineError _ ->
            ignore(Match_seq.match_hyps e2 e1);
            true
         end
      with RefineError _ ->
         false

   let find_leaf_guess leaf subgoals =
      match find_leaf tactic_arg_alpha_equal leaf subgoals with
         Goal _, _ ->
            begin match
               match find_leaf tactic_arg_match leaf subgoals with
                  Goal _, _ -> find_leaf tactic_arg_alpha_equal_concl leaf subgoals
                | answer -> answer
            with
               RuleBox _ as node , subgoals ->
                  replace_goal_ext (norm_goal leaf) node, subgoals
             | answer -> answer
            end
       | answer ->
            answer

   let update_subgoals = match_subgoals_general find_leaf_guess

   (*
    * Replace a child at the given index.
    * Recompute matches among the children.
    *)
   let insert_subgoal node i subgoals extras =
      let len = List.length subgoals in
         if i < len then
            Lm_list_util.replace_nth i node subgoals, extras
         else
            subgoals, Lm_list_util.replace_nth (i - len) node extras

   let replace_subgoal proof node raddr (goal : extract) (subgoals : extract list) (extras : extract list) i (node' : extract) =
      if i < 0 then
         raise_replace_error proof node raddr i
      else if i = 0 then
         let subgoals', extras' = match_subgoals (leaves_ext node') subgoals extras in
            node', subgoals', extras', node' == goal && Lm_list_util.compare_eq subgoals subgoals' && Lm_list_util.compare_eq extras extras'
      else
         let subgoals', extras' = insert_subgoal node' (pred i) subgoals extras in
         let subgoals', extras' = match_subgoals (leaves_ext goal) subgoals' extras' in
            goal, subgoals', extras', Lm_list_util.compare_eq subgoals subgoals' && Lm_list_util.compare_eq extras extras'

   let replace_subterm proof node raddr goal subgoals i = function
      Goal goal' ->
         if i < 0 || i > List.length subgoals then
            raise_replace_error proof node raddr i
         else if i = 0 then
            goal', subgoals, goal' == goal
         else
            let subgoal = List.nth subgoals i in
               goal, Lm_list_util.replace_nth i goal' subgoals, goal' == subgoal
    | _ ->
         raise_replace_error proof node raddr i

   (*
    * Replace a child at the given index.
    *
    * Return ((locked : bool) * (pending : bool) * (node : extract)),
    * where pending is true if a Pending node has been replaced,
    * and locked is true if a Locked node has been encountered,
    * and node is the new node.
    *)
   let rec replace_child proof node raddr i (node' : extract) =
      match node with
         Goal _
       | Identity _ ->
            raise_replace_error proof node raddr i
       | Extract (goal, subgoals, _)
       | Unjustified (goal, subgoals) ->
            let goal, subgoals, unchanged = replace_subterm proof node raddr goal subgoals i node' in
            let node =
               if unchanged then
                  node
               else
                  Unjustified (goal, subgoals)
            in
               false, false, node
       | Compose { comp_goal = goal; comp_subgoals = subgoals; comp_extras = extras } -> begin
            match node' with
               RuleBox _ ->
                  raise_replace_error proof node raddr i
             | _ ->
                  let goal, subgoals, extras, unchanged = replace_subgoal proof node raddr goal subgoals extras i node' in
                  let node =
                     if unchanged then
                        node
                     else
                        Compose {
                           comp_status = LazyStatusDelayed;
                           comp_goal = goal;
                           comp_subgoals = subgoals;
                           comp_extras = extras;
                           comp_leaves = LazyLeavesDelayed
                        }
                  in
                     false, false, node
            end
       | Wrapped (label, node'') ->
            if i = 0 then
               let node =
                  if node' == node'' then
                     node
                  else
                     Wrapped (label, node')
               in
                  false, false, node
            else
               raise_replace_error proof node raddr i
       | RuleBox { rule_expr = expr;
                   rule_string = text;
                   rule_tactic = tac;
                   rule_extract_normalized = normal;
                   rule_extract = goal;
                   rule_subgoals = subgoals;
                   rule_extras = extras
         } ->
            let new_goal, subgoals, extras, unchanged = replace_subgoal proof node raddr goal subgoals extras i node' in
            let node =
               if unchanged then
                  node
               else
                  RuleBox { rule_status = LazyStatusDelayed;
                            rule_expr = expr;
                            rule_tactic = tac;
                            rule_string = text;
                            rule_extract_normalized = normal && (goal==new_goal);
                            rule_extract = new_goal;
                            rule_subgoals = subgoals;
                            rule_leaves = LazyLeavesDelayed;
                            rule_extras = extras
                  }
            in
               false, false, node
       | Pending f ->
            let _, _, node = replace_child proof (f ()) raddr i node' in
               false, true, node
       | Locked node ->
            let _, pending, node = replace_child proof node raddr i node' in
               true, pending, Locked node

   (*
    * Map a function along the nodes in the path.
    *)
   let rec map_path_ext proof node raddr results f = function
      i :: t ->
         map_path_ext proof (select_child proof node raddr i) (i :: raddr) (f proof node :: results) f t
    | [] ->
         List.rev (f proof node :: results)

   let map_path f proof path =
      map_path_ext proof proof [] [] f path

   (*
    * Fold a function along the path from innermost to outermost,
    * recomputing the proof.
    *)
   let rec fold_up_proof_ext (proof : proof) (node : extract) (raddr : address) (arg : extract) = function
      i :: t ->
         let locked, post, node' = fold_up_proof_ext proof (select_child proof node raddr i) (i :: raddr) arg t in
         let locked', post', node = replace_child proof node raddr i node' in
            locked || locked', post || post', node
    | [] ->
         false, false, arg

   let fold_up_proof node pf addr =
      fold_up_proof_ext pf pf [] node addr

   let fold_proof postf pf addr node =
      if !debug_proof then
         eprintf "Proof_boot.fold_proof [%a]%t" print_int_list addr eflush;
      let locked_flag, post_flag, proof = fold_up_proof node pf addr in
         if !debug_proof then
            eprintf "Proof_boot.fold_proof: post=%b locked=%b%t" post_flag locked_flag eflush;
         if post_flag || not locked_flag then
            postf proof
         else
            proof

   (*
    * Sweep a function over the tactic_args in the tree.
    *)
   let rec map_tactic_arg_ext (f : tactic_arg -> tactic_arg) node =
      match node with
         Goal arg ->
            Goal (f arg)
       | Identity arg ->
            Identity (f arg)
       | Unjustified (goal, subgoals)
       | Extract (goal, subgoals, _) ->
            Unjustified (f goal, List.map f subgoals)
       | Compose { comp_goal = goal; comp_subgoals = subgoals; comp_extras = extras } ->
            Compose {
               comp_status = LazyStatusDelayed;
               comp_goal = map_tactic_arg_ext f goal;
               comp_subgoals = List.map (map_tactic_arg_ext f) subgoals;
               comp_extras = List.map (map_tactic_arg_ext f) extras;
               comp_leaves = LazyLeavesDelayed
               }
       | Wrapped (label, goal) ->
            Wrapped (label, map_tactic_arg_ext f goal)
       | RuleBox { rule_expr = expr;
                   rule_string = text;
                   rule_tactic = tac;
                   rule_extract_normalized = normal;
                   rule_extract = goal;
                   rule_subgoals = subgoals;
                   rule_extras = extras
         } ->
            let new_goal = map_tactic_arg_ext f goal in
            RuleBox { rule_status = LazyStatusDelayed;
                      rule_expr = expr;
                      rule_string = text;
                      rule_tactic = tac;
                      rule_extract_normalized = normal && (new_goal==goal);
                      rule_extract = new_goal;
                      rule_subgoals = List.map (map_tactic_arg_ext f) subgoals;
                      rule_leaves = LazyLeavesDelayed;
                      rule_extras = List.map (map_tactic_arg_ext f) extras
               }
       | Pending g ->
            map_tactic_arg_ext f (g ())
       | Locked ext ->
            Locked (map_tactic_arg_ext f ext)

   (************************************************************************
    * NAVIGATION                                                           *
    ************************************************************************)

   (*
    * Get the status along the path from the root.
    * The last component of the path may not be valid.
    *)
   let path_status pf addr =
      try map_path (fun proof node -> translate_status (status_ext node)) pf addr with
         (ProofRefineError _) as exn ->
            if addr = [] then
               raise exn;
            let addr, _ = Lm_list_util.split_last addr in
               map_path (fun proof node -> translate_status (status_ext node)) pf addr

   (*
    * Set the goal of the current node.
    *)
   let set_goal_ext node mseq =
      replace_goal_ext { (goal_ext node) with ref_goal = mseq } node

   let set_goal postf pf addr mseq =
      let node = set_goal_ext (index pf addr) mseq in
         fold_proof postf pf addr node

   (*
    * Paste an alternate proof at this location.
    *)
   let paste postf to_proof addr from_proof =
      let to_node = index to_proof addr in
      let from_node = set_goal_ext from_proof (goal_ext to_node).ref_goal in
         fold_proof postf to_proof addr from_node

   (*
    * Copy a proof from one location to another.
    *)
   let copy postf pf from_addr to_addr =
      paste postf pf to_addr (index pf from_addr)

   (*
    * Make the current subgoal an assumption.
    *)
   let make_assum_arg goal i arg =
      let goal, hyps = Refine.dest_msequent arg.ref_goal in
      let hyps = Lm_list_util.insert_nth i goal hyps in
        { arg with ref_goal = Refine.mk_msequent goal hyps }

   let make_assum postf pf addr =
      (* Add the goal as an assumption to all proof nodes *)
      let goal, _ = Refine.dest_msequent (goal pf addr).ref_goal in
      let _, hyps = Refine.dest_msequent (goal_ext pf).ref_goal in
      let len = List.length hyps in
      let pf = map_tactic_arg_ext (make_assum_arg goal len) pf in
      let node = index_ext pf pf [] addr in
         fold_proof postf pf addr node

   (************************************************************************
    * CACHE & CACHED NAVIGATION                                            *
    ************************************************************************)

   (*
    * Add an extract to the cache.
    *)
   let set_cache ext =
      let mseq = (goal_ext ext).ref_goal in
         State.write cache_entry (fun cache ->
               cache := Cache.add !cache mseq ext)

   (*
    * Add a new Pending node to the cache.
    * Remember the proof in case the goal changes.
    *)
   let post f addr =
      if !debug_proof then
         eprintf "Posting proof%t" eflush;
      let proof = f () in
      let node = index proof addr in
      let old_goal = (goal_ext node).ref_goal in
      let old_proof = ref proof in
      let old_node = ref node in
      let compute_ext () =
         try
            let new_proof = f () in
            if !old_proof==new_proof then !old_node else
               let new_node = index new_proof addr in
               if msequent_alpha_equal (goal_ext new_node).ref_goal old_goal then
                  begin
                     if !debug_proof_pending then begin
                        eprintf "Replacing old :\n";
                        print_ext !old_node;
                        eprintf "with new :\n";
                        print_ext new_node
                     end;
                     old_node := new_node;
                     old_proof := new_proof;
                     new_node
                  end
               else
                  !old_node
         with
            ProofRefineError _
          | ExtRefineError _ ->
               !old_node
      in
      let ext = Pending compute_ext in
         set_cache ext;

         (* Eliminate all enclosing Pending nodes *)
         fold_proof (fun proof -> proof) proof addr (Locked node)

   (*
    * Fetch all the entries in the cache that are possible for this
    * tactic_arg.
    *)
   let get_cache_ext proof =
      try
         State.read cache_entry (fun cache -> Cache.find_all !cache (goal_ext proof).ref_goal)
      with
         Not_found ->
            []

   let get_cache pf addr =
      get_cache_ext (index pf addr)

   (*
    * For the goal, append all cached proofs.
    *)
   let proof_goal cache goal =
      goal :: if cache then get_cache_ext goal else []

   (*
    * Remove duplicates in the subgoals, and append cached proofs.
    *)
   let proof_subgoals =
      let rec collect found = function
         subgoal :: subgoals ->
            if search_arg subgoal found then
               collect found subgoals
            else
               [Goal subgoal] :: collect (subgoal :: found) subgoals
       | [] ->
            []
      in
         collect []

   (*
    * Get the list of subgoals with cached entries.
    * Make sure extras are in the cache.
    *)
   let proof_subgoals_extras cache subgoals extras =
      let _ = List.iter set_cache extras in
      let subgoals =
         List.map (fun subgoal -> subgoal :: if false && cache then get_cache_ext subgoal else []) subgoals
      in
         subgoals, extras

   (*
    * Expressions.
    *)
   let string_of_opname opname =
      let rec collect_opname cap = function
         [t] -> cap t
       | h :: t ->
            (collect_opname String.capitalize t) ^ "." ^ cap h
       | [] ->
            "<nil opname>"
      in
         collect_opname (fun x -> x) (dest_opname opname)

   let make_extract_expr ext =
      let arglist =
         match Refine.describe_extract ext with
            EDRule (opname, addrs, params) ->
               let addrs = Array.of_list (List.map (fun addr -> IntArg addr) addrs) in
               let params = Array.of_list (List.map (fun t -> TermArg t) params) in
               let name = [|StringArg (string_of_opname opname)|] in
                  GeneralArgList (Array.concat [name; addrs; params])
          | EDRewrite ->
               NoneArgList "<rewrite>"
          | EDCondREwrite ->
               NoneArgList "<conditional-rewrite>"
          | EDComposition ->
               StringStringArgList ("<tactic>", "thenT", "<tactic>")
          | EDNthHyp i ->
               IntArgList ("nthAssumT", i)
          | EDCut t ->
               TermArgList ("cutT", t)
          | EDIdentity ->
               NoneArgList "<identity>"
      in
         ExprExtract arglist

   let null_address = TermAddr.make_address []

   (*
    * Describe all the parts of this step.
    *)
   let rec info_ext compose_flag = function
      Goal goal as node ->
         { step_goal = node :: get_cache_ext node;
           step_status = StatusPartial;
           step_expr = ExprGoal;
           step_subgoals = [];
           step_extras = []
         }
    | Identity goal ->
         let proofs = proof_goal false (Goal goal) in
            { step_goal = proofs;
              step_status = StatusComplete;
              step_expr = ExprIdentity;
              step_subgoals = [proofs];
              step_extras = []
            }
    | Unjustified (goal, subgoals) ->
         { step_goal = proof_goal false (Goal goal);
           step_status = StatusIncomplete;
           step_expr = ExprUnjustified;
           step_subgoals = proof_subgoals subgoals;
           step_extras = []
         }
    | Extract (goal, subgoals, ext) ->
         { step_goal = proof_goal false (Goal goal);
           step_status = StatusComplete;
           step_expr = make_extract_expr ext;
           step_subgoals = proof_subgoals subgoals;
           step_extras = []
         }
    | Compose { comp_goal = goal; comp_subgoals = subgoals; comp_extras = extras } ->
         let new_subgoals, extras = proof_subgoals_extras false subgoals extras in
         let status = translate_status (status_ext goal) in
         if compose_flag then
            let goal = info_ext false goal in
               { step_goal = goal.step_goal;
                 step_status = status;
                 step_expr = ExprCompose goal.step_expr;
                 step_subgoals = new_subgoals;
                 step_extras = extras
               }
         else
            { step_goal = proof_goal false goal;
              step_status = status;
              step_expr = ExprCompose ExprIdentity;
              step_subgoals = List.flatten (List.map (fun goal -> (info_ext false goal).step_subgoals) subgoals);
              step_extras = extras
            }
    | Wrapped (label, ext) ->
         let info = info_ext false ext in
            { info with step_expr = ExprWrapped label }
    | RuleBox { rule_extract_normalized = false } as node ->
         info_ext compose_flag (normalize node)
    | RuleBox { rule_expr = expr;
                rule_string = text;
                rule_extract_normalized = true;
                rule_extract = goal;
                rule_subgoals = subgoals;
                rule_extras = extras
      } ->
         let subgoals, extras = proof_subgoals_extras true subgoals extras in
            { step_goal = proof_goal true goal;
              step_status = translate_status (status_ext goal);
              step_expr = ExprRule (text, expr ());
              step_subgoals = subgoals;
              step_extras = extras
            }
    | Pending f ->
         info_ext compose_flag (f ())
    | Locked ext ->
         info_ext compose_flag ext

   let info pf addr =
      let info = info_ext true (index pf addr) in
         if !debug_proof then
            eprintf "Got info_ext%t" eflush;
         info

   let rec find_subgoal_aux p addr node arg =
      let test ext = tactic_arg_alpha_equal arg (goal_ext ext) in
      if test node then
         if addr = [] then
            []
         else
            let addr = fst (Lm_list_util.split_last addr) in
               find_subgoal_aux p addr (index p addr) arg
      else
         let test_subgoal ext = List.exists (tactic_arg_alpha_equal arg) (leaves_ext ext) in
         let rec comp_aux rb addr goal subgoals =
            if List.exists test subgoals then
               aux (if rb then 0::addr else addr) goal
            else
               let i = Lm_list_util.find_item test_subgoal subgoals in
                  aux ((i+1)::addr) (List.nth subgoals i)
         and aux addr = function
            Goal _
          | Extract _
          | Identity _
          | Unjustified _  as ext ->
               if addr = [] then [] else (List.rev (List.tl addr))
          | Wrapped (_, ext) ->
               aux (0::addr) ext
          | Pending f ->
               aux addr (f ())
          | Locked ext ->
               aux addr ext
          | RuleBox ri as ext ->
               if not ri.rule_extract_normalized then ignore (normalize ext);
               comp_aux true addr ri.rule_extract ri.rule_subgoals
          | Compose ci ->
               comp_aux false addr ci.comp_goal ci.comp_subgoals
         in
            aux [] node

   let find_subgoal p addr i =
      let node = index p addr in
         if i = 0 then
            find_subgoal_aux p addr node (goal_ext node)
         else
            let l =
               try List.nth (info_ext true node).step_subgoals (i - 1)
               with Failure _ ->
                  raise (Invalid_argument "Proof_boot.find_subgoal")
            in
               find_subgoal_aux p addr node (goal_ext (List.hd l))

   (************************************************************************
    * UPDATES                                                              *
    ************************************************************************)

   (*
    * Replace the current proof step.
    *)
   let replace_step_subgoals step subgoals' extras' =
      let leaves = leaves_ext step.rule_extract in
      let subgoals, extras = match_subgoals leaves (step.rule_subgoals @ subgoals') (step.rule_extras @ extras') in
         { step with
           rule_status = LazyStatusDelayed;
           rule_subgoals = subgoals;
           rule_leaves = LazyLeavesDelayed;
           rule_extras = extras
         }

   let rec replace_step_rule node step =
      match node with
         Goal _
       | Identity _
       | Unjustified _
       | Extract _ ->
            step
       | Compose { comp_subgoals = subgoals'; comp_extras = extras' }
       | RuleBox { rule_subgoals = subgoals'; rule_extras = extras' } ->
            replace_step_subgoals step subgoals' extras'
       | Wrapped (label, node') ->
            replace_step_rule node' step
       | Pending f ->
            replace_step_rule (f ()) step
       | Locked ext ->
            replace_step_rule ext step

   let refine postf pf addr text expr tac =
      let node = index pf addr in
      let subgoals, ext = TacticInternal.refine tac (goal_ext node) in
      let info =
         { rule_status = LazyStatusDelayed;
           rule_expr = (fun () -> expr);
           rule_string = text;
           rule_tactic = (fun () -> tac);
           rule_extract_normalized = false;
           rule_extract = ext;
           rule_subgoals = List.map make_goal subgoals;
           rule_leaves = LazyLeavesDelayed;
           rule_extras = []
         }
      in
      let info = replace_step_rule node info in
      let ext = RuleBox info in
         set_cache ext;
         fold_proof postf pf addr ext

   (************************************************************************
    * GLOBAL PROOF OPERATIONS                                              *
    ************************************************************************)

   (*
    * "Clean" up the proof by removing all extras.
    *)
   let rec clean_extras_ext node =
      match node with
         Goal _
       | Identity _
       | Unjustified _
       | Extract _ ->
            node
       | Wrapped (label, node) ->
            Wrapped (label, clean_extras_ext node)
       | Compose { comp_status = status;
                   comp_goal = goal;
                   comp_subgoals = subgoals;
                   comp_leaves = leaves
         } ->
            Compose { comp_status = status;
                      comp_goal = clean_extras_ext goal;
                      comp_subgoals = List.map clean_extras_ext subgoals;
                      comp_leaves = leaves;
                      comp_extras = []
            }
       | RuleBox ri ->
            RuleBox {
               ri with
               rule_extract = clean_extras_ext ri.rule_extract;
               rule_subgoals = List.map clean_extras_ext ri.rule_subgoals;
               rule_extras = [];
            }
       | Pending f ->
            clean_extras_ext (f ())
       | Locked node ->
            Locked (clean_extras_ext node)

   let clean postf pf addr =
      fold_proof postf pf addr (clean_extras_ext (index pf addr))

   (*
    * Squash the proof by removing all extracts.
    * Compositions also get squashed, but we take care to preserve
    * any paths that lead to rule boxes.
    *)
   let rec squash_check_ext = function
      Goal _
    | Identity _
    | Unjustified _
    | Extract _ ->
         ()
    | Wrapped (_, node)
    | Locked node ->
         squash_check_ext node
    | Compose ci ->
         squash_check_ext ci.comp_goal;
         List.iter squash_check_ext ci.comp_subgoals;
         List.iter squash_check_ext ci.comp_extras
    | Pending f ->
         squash_check_ext (f ())
    | RuleBox _ ->
         raise (Invalid_argument "Proof_boot.squash_ext: the proof is ill-formed: a RuleBox proof contains a RuleBox inside")

   let rec squash_kill_ext = function
      (Goal _| Identity _ | Unjustified _) as node ->
         node
    | Extract (goal, subgoals, _) ->
         Unjustified (goal, subgoals)
    | Pending f ->
         squash_kill_ext (f ())
    | Locked node ->
         squash_kill_ext node
    | node ->
         squash_check_ext node;
         Unjustified (goal_ext node, leaves_ext node)

   let rec squash_ext = function
      (Goal _ | Identity _ | Unjustified _ | Extract _) as node ->
         squash_kill_ext node
    | Wrapped (label, node) ->
         Wrapped (label, squash_ext node)
    | Compose { comp_goal = goal;
                comp_subgoals = subgoals;
                comp_extras = extras
      } as node ->
         Compose { comp_status = LazyStatusDelayed;
                   comp_goal = squash_ext goal;
                   comp_subgoals = List.map squash_ext subgoals;
                   comp_extras = List.map squash_ext extras;
                   comp_leaves = LazyLeaves (leaves_ext node)
         }
    | RuleBox { rule_string = text;
                rule_expr = expr;
                rule_tactic = tac;
                rule_extract = goal;
                rule_subgoals = subgoals;
                rule_extras = extras;
                rule_leaves = leaves
      } ->
         RuleBox { rule_status = LazyStatusDelayed;
                   rule_string = text;
                   rule_expr = expr;
                   rule_tactic = tac;
                   rule_extract_normalized = false;
                   rule_extract = squash_kill_ext goal;
                   rule_subgoals = List.map squash_ext subgoals;
                   rule_extras = List.map squash_ext extras;
                   rule_leaves = leaves
         }
    | Pending f ->
         squash_ext (f ())
    | Locked node ->
         Locked (squash_ext node)

   let squash postf pf addr =
      fold_proof postf pf addr (squash_ext (index pf addr))

   (************************************************************************
    * PROOF CHECKING                                                       *
    ************************************************************************)

   (*
    * Re-expand all the rule boxes.
    *)
   let rec expand_ext exn_wrapper = function
         Goal _
       | Identity _
       | Unjustified _
       | Extract _ as node ->
            node
       | Compose { comp_goal = goal; comp_subgoals = subgoals; comp_extras = extras } ->
            let goal = expand_ext exn_wrapper goal in
            let subgoals = List.map (expand_ext exn_wrapper) subgoals in
            let extras = List.map (expand_ext exn_wrapper) extras in
            let subgoals, extras = match_subgoals (leaves_ext goal) subgoals extras in
               Compose { comp_status = LazyStatusDelayed;
                         comp_goal = goal;
                         comp_subgoals = subgoals;
                         comp_extras = extras;
                         comp_leaves = LazyLeavesDelayed
               }
       | Wrapped (label, goal) ->
            Wrapped (label, expand_ext exn_wrapper goal)
       | RuleBox { rule_expr = expr;
                   rule_string = text;
                   rule_extract_normalized = normal;
                   rule_extract = goal;
                   rule_tactic = tac;
                   rule_subgoals = subgoals;
                   rule_extras = extras
         } ->
            let t = goal_ext goal in
            let new_goal =
               try exn_wrapper (fun () -> snd (TacticInternal.refine (tac ()) t)) ()
               with RefineError _ -> goal
            in
            let leaves = leaves_ext new_goal in
            let subgoals, extras = update_subgoals leaves subgoals extras in
            let subgoals = List.map (expand_ext exn_wrapper) subgoals in
            let extras = List.map (expand_ext exn_wrapper) extras in
            let subgoals, extras = match_subgoals leaves subgoals extras in
               RuleBox { rule_status = LazyStatusDelayed;
                         rule_expr = expr;
                         rule_string = text;
                         rule_tactic = tac;
                         rule_extract_normalized = normal && (goal==new_goal);
                         rule_extract = new_goal;
                         rule_subgoals = subgoals;
                         rule_leaves = LazyLeavesDelayed;
                         rule_extras = extras
               }
       | Pending f ->
            expand_ext exn_wrapper (f ())
       | Locked ext ->
            expand_ext exn_wrapper ext

   let expand postf exn_wrapper pf addr =
      fold_proof postf pf addr (expand_ext exn_wrapper (index pf addr))

   let rec refiner_extract_of_proof = function
      Goal _ | Unjustified _ ->
         raise(RefineError("Proof_boot.refiner_extract_of_proof", StringError "The proof is incomplete or unexpanded"))
    | Wrapped(_,ext) | Locked ext -> refiner_extract_of_proof ext
    | Extract(_,_,re) -> re
    | Compose{ comp_goal = goal; comp_subgoals = subgoals } ->
         Refine.compose (refiner_extract_of_proof goal) (List.map refiner_extract_of_proof subgoals)
    | RuleBox{ rule_extract = goal; rule_subgoals = subgoals } ->
         (* In a RuleBox, several identical subgoals could be compressed into one, *)
         (* so we need to be careful.                                              *)
         let ext = refiner_extract_of_proof goal in
         let real_subgoals = subgoals_of_extract ext in
            Refine.compose ext (List.map (find_mseq_extract subgoals) real_subgoals)
    | Pending f ->
         refiner_extract_of_proof (f ())
    | Identity goal ->
         TacticInternal.identity goal

   and find_mseq_extract subgoals mseq =
      match subgoals with
       | hd::tl ->
            if msequent_alpha_equal mseq (goal_ext hd).ref_goal then
               refiner_extract_of_proof hd
            else
               find_mseq_extract tl mseq
       | [] ->
            raise(RefineError("Proof_boot.refiner_extract_of_proof", StringError "Invalide rule box"))

   (************************************************************************
    * CONVERSIONS                                                          *
    ************************************************************************)

   (*
    * An io proof is a proof, but the function parts have been removed.
    * We do not hash-cons it.
    *)
   type simple_tactic_arg =
      { simp_goal : msequent;
        simp_label : string;
        simp_attributes : attribute_info
      }

   type io_proof =
      IOGoal of simple_tactic_arg
    | IOUnjustified of simple_tactic_arg * simple_tactic_arg list
    | IOWrapped of arglist * io_proof
    | IOCompose of io_compose_info
    | IORuleBox of io_rule_info
    | IOIdentity of simple_tactic_arg

   and io_compose_info =
      { io_comp_status : lazy_status;
        io_comp_goal : io_proof;
        io_comp_subgoals : io_proof list;
        io_comp_extras : io_proof list
      }

   and io_rule_info =
      { io_rule_status : lazy_status;
        io_rule_string : string;
        io_rule_goal : io_proof;
        io_rule_subgoals : io_proof list;
        io_rule_extras : io_proof list
      }

   (*
    * Convert to an io proof.
    * For speed, we marshal the full tactic arg.  But some of the
    * raw attributes, and the sentinal will be invalid when we read it
    * back in.
    *)
   let io_proof_of_proof squash _ _ proof =
      let parents = ref [] in
      let rec make_tactic_arg_sq squash arg =
         try
            List.assq arg !parents
         with
            Not_found ->
               let { ref_goal = goal;
                     ref_label = label;
                     ref_attributes = attrs
                   } = arg
               in
               let attr =
                  let attrs = squash_attributes attrs in
                  if squash then begin
                     if attrs <> empty_attribute then
                        eprintf "Warning: Proof_boot.io_proof_of_proof: unexpected attribute list. If the .prla file is old, ignore this warning\n";
                     empty_attribute
                 end else attrs
               in let arg' =
                  { simp_goal = goal;
                    simp_label = label;
                    simp_attributes = attr
                  }
               in
                  parents := (arg, arg') :: !parents;
                  arg'
         and make_tactic_arg arg = make_tactic_arg_sq false arg
         and make_tactic_arg_squash arg = make_tactic_arg_sq true arg
      in
      let rec convert arg =
         if !debug_proof then begin
            eprintf "IO convertion of:\n";
            print_ext arg
         end;
         let res = match arg with
            Goal arg ->
               IOGoal (make_tactic_arg arg)
          | Unjustified (goal, subgoals) ->
               IOUnjustified (make_tactic_arg_squash goal, List.map make_tactic_arg_squash subgoals)
          | Extract (goal, subgoals, _) ->
               IOUnjustified (make_tactic_arg goal, List.map make_tactic_arg subgoals)
          | Wrapped (args, node) ->
               IOWrapped (args, convert node)
          | Compose { comp_status = status;
                      comp_goal = goal;
                      comp_subgoals = subgoals;
                      comp_extras = extras
            } ->
               IOCompose { io_comp_status = status;
                           io_comp_goal = convert goal;
                           io_comp_subgoals = List.map convert subgoals;
                           io_comp_extras = List.map convert extras
               }
          | RuleBox { rule_status = status;
                      rule_string = text;
                      rule_extract = goal;
                      rule_subgoals = subgoals;
                      rule_extras = extras
            } ->
               IORuleBox {
                  io_rule_status = status;
                  io_rule_string = text;
                  io_rule_goal = convert goal;
                  io_rule_subgoals = List.map convert subgoals;
                  io_rule_extras = List.map convert extras }
          | Pending f ->
               convert (f ())
          | Locked node ->
               convert node
          | Identity arg ->
               IOIdentity (make_tactic_arg arg)
         in
            if !debug_proof then
               eprintf "\\__ IO convertion done.\n%t" eflush;
            res
      in
         (* "Update" the proof by forcing computation of status and leaf nodes. *)
         ignore (status_ext proof);
         ignore (leaves_ext proof);
         convert (if squash then squash_ext proof else proof)

   (*
    * Convert from an io proof.
    *)
   let lazy_apply f x =
      let cell = ref None in
      let f () =
         match !cell with
            None ->
               let p = f x in
                  cell := Some p;
                  p
          | Some x ->
               x
      in
         f

   let proof_of_io_proof raw_attributes sentinal bookmark parse eval node =
      let parents = ref [] in
      let rec make_tactic_arg arg =
         try
            List.assq arg !parents
         with
            Not_found ->
               let { simp_goal = goal;
                     simp_label = label;
                     simp_attributes = args
                   } = arg
               in
               let args = update_attributes args raw_attributes in
               let arg' =
                  { ref_goal = goal;
                    ref_label = label;
                    ref_attributes = args;
                    ref_bookmark = bookmark;
                    ref_sentinal = sentinal
                  }
               in
                  parents := (arg, arg') :: !parents;
                  arg'
      in
      let rec convert = function
         IOGoal arg ->
            Goal (make_tactic_arg arg)
       | IOUnjustified (goal, subgoals) ->
            Unjustified (make_tactic_arg goal, List.map make_tactic_arg subgoals)
       | IOWrapped (args, node) ->
            Wrapped (args, convert node)
       | IOCompose { io_comp_goal = goal;
                     io_comp_subgoals = subgoals;
                     io_comp_extras = extras
         } ->
            Compose { comp_status = LazyStatusDelayed;
                      comp_goal = convert goal;
                      comp_subgoals = List.map convert subgoals;
                      comp_leaves = LazyLeavesDelayed;
                      comp_extras = List.map convert extras
            }
       | IORuleBox { io_rule_string = text;
                     io_rule_goal = goal;
                     io_rule_subgoals = subgoals;
                     io_rule_extras = extras
         } ->
            let expr = lazy_apply parse text in
            let tactic = lazy_apply (fun () -> eval (expr ())) () in
               RuleBox { rule_status = LazyStatusDelayed;
                         rule_string = text;
                         rule_expr = expr;
                         rule_tactic = tactic;
                         rule_extract_normalized = false;
                         rule_extract = convert goal;
                         rule_subgoals = List.map convert subgoals;
                         rule_leaves = LazyLeavesDelayed;
                         rule_extras = List.map convert extras
               }
       | IOIdentity arg ->
            Identity (make_tactic_arg arg)
      in
         convert node

   (*
    * Some simple operations on IO proofs.
    *)
   let rec status_of_io_proof = function
      IOGoal _
    | IOUnjustified _
    | IOIdentity _ ->
         StatusPartial
    | IOWrapped (_, node) ->
         status_of_io_proof node
    | IOCompose { io_comp_status = status }
    | IORuleBox { io_rule_status = status } ->
         translate_status status

   (*
    * Count up the number of nodes.
    *)
   let rec node_count_of_io_proof_node rules nodes = function
      IOGoal _
    | IOUnjustified _
    | IOIdentity _ ->
         rules, succ nodes
    | IOWrapped (_, node) ->
         node_count_of_io_proof_node rules (succ nodes) node
    | IOCompose { io_comp_goal = goal;
                  io_comp_subgoals = subgoals
      } ->
         let rules, nodes = node_count_of_io_proof_node rules (succ nodes) goal in
            node_count_of_io_subgoals rules nodes subgoals
    | IORuleBox { io_rule_goal = goal;
                  io_rule_subgoals = subgoals
      } ->
         let rules, nodes = node_count_of_io_proof_node (succ rules) (succ nodes) goal in
            node_count_of_io_subgoals rules nodes subgoals

   and node_count_of_io_subgoals rules nodes = function
      node :: tl ->
         let rules, nodes = node_count_of_io_proof_node rules nodes node in
            node_count_of_io_subgoals rules nodes tl
    | [] ->
         rules, nodes

   let node_count_of_io_proof proof =
      node_count_of_io_proof_node 0 0 proof

   (*
    * Conversion to terms.
    *)
   module ProofTerm (ToTerm : RefinerSig) =
   struct
      module Convert = Proof_term_boot.ProofTerm (ToTerm);;

      let to_term parse eval proof =
         Convert.to_term parse eval (goal_ext proof) proof

      let of_term args sentinal bookmark parse eval t =
         Convert.of_term args sentinal bookmark parse eval t

      let convert = Convert.convert
      let revert = Convert.revert

      let status_of_term = Convert.status_of_term
      let node_count_of_term = Convert.node_count_of_term
   end

   module ProofTerm_std = ProofTerm (Refiner.Refiner);;
   module ProofTerm_io = ProofTerm (Refiner_io);;

   (*
    * Term conversions.
    *)
   let to_term = ProofTerm_std.to_term
   let of_term = ProofTerm_std.of_term

   (*
    * Convert the IO proof.
    *)
   let term_of_io_proof parse eval proof =
      ProofTerm_std.to_term parse eval (proof_of_io_proof [] Tactic.null_sentinal empty_bookmark parse eval proof)

   let io_proof_of_term parse eval term =
      io_proof_of_proof true [] Tactic.null_sentinal (ProofTerm_std.of_term [] Tactic.null_sentinal empty_bookmark parse eval term)

   (*
    * Convert the IO proof
    *)
   let term_io_of_io_proof parse eval proof =
      ProofTerm_io.to_term parse eval (proof_of_io_proof [] Tactic.null_sentinal empty_bookmark parse eval proof)

   let io_proof_of_term_io parse eval term =
      let proof = ProofTerm_io.of_term [] Tactic.null_sentinal empty_bookmark parse eval term in
         io_proof_of_proof true [] Tactic.null_sentinal proof

   (************************************************************************
    * PROOF OPERATIONS                                                     *
    ************************************************************************)

   (*
    * Get the total node count.
    * Return the total number of nodes and the number of rule boxes.
    *)
   let rec node_count_ext (rcount, ncount) = function
      Goal _
    | Identity _
    | Unjustified _
    | Extract _ ->
         rcount, succ ncount
    | Wrapped (label, ext) ->
         node_count_ext (rcount, succ ncount) ext
    | Compose { comp_goal = goal; comp_subgoals = subgoals } ->
         node_count_subgoals_ext (node_count_ext (rcount, succ ncount) goal) subgoals
    | RuleBox ri as node ->
         if not ri.rule_extract_normalized then ignore (normalize node);
         node_count_subgoals_ext (node_count_ext (succ rcount, succ ncount) ri.rule_extract) ri.rule_subgoals
    | Pending f ->
         node_count_ext (rcount, ncount) (f ())
    | Locked ext ->
         node_count_ext (rcount, ncount) ext

   and node_count_subgoals_ext counts = function
      subgoal :: subgoals ->
         node_count_subgoals_ext (node_count_ext counts subgoal) subgoals
    | [] ->
         counts

   let node_count = node_count_ext (0, 0)

   (*
    * Kreitz the tree into a single node.
    * This only work on the outermost rule boxes.
    *)
   let loc = Lexing.dummy_pos, Lexing.dummy_pos

   let rec kreitz_ext =
      let rec concat_text = function
         [text, _, _, _] ->
            text
       | (text, _, _, _) :: subnodes ->
            text ^ "; " ^ concat_text subnodes
       | [] ->
            ""
      in
      let rec concat_ast = function
         (_, e, _, _) :: tl ->
            (<:expr< $lid:"::"$ $e$ $concat_ast tl$ >>)
       | [] ->
            (<:expr< [] >>)
      in
      let rec concat_subgoals = function
         (_, _, _, subgoals) :: tl ->
            subgoals @ concat_subgoals tl
       | [] ->
            []
      in function
         Goal _
       | Identity _
       | Unjustified _
       | Extract _
       | Wrapped _
       | Compose _ as node ->
            "idT", (<:expr< $lid: "idT"$ >>), TacticInternal.idT, [goal_ext node]
       | Pending f ->
            kreitz_ext (f ())
       | Locked ext ->
            kreitz_ext ext
       | RuleBox { rule_expr = expr;
                   rule_string = text;
                   rule_tactic = tac;
                   rule_subgoals = subgoals
         } ->
            let subnodes = List.map kreitz_ext subgoals in
            let text = sprintf "%s thenLT [%s]" text (concat_text subnodes) in
            let expr = (<:expr< $lid: "prefix_thenLT"$ $expr ()$ $concat_ast subnodes$ >>) in
            let tac = prefix_thenLT (tac ()) (List.map (fun (_, _, tac, _) -> tac) subnodes) in
            let subgoals = concat_subgoals subnodes in
               text, expr, tac, subgoals

   let kreitz postf pf addr =
      let node = index pf addr in
      let text, expr, tac, subgoals = kreitz_ext node in
      let info =
         { rule_status = LazyStatusDelayed;
           rule_expr = (fun () -> expr);
           rule_string = text;
           rule_tactic = (fun () -> tac);
           rule_extract_normalized = true;
           rule_extract = Unjustified (goal_ext node, subgoals);
           rule_subgoals = List.map (fun t -> Goal t) subgoals;
           rule_leaves = LazyLeavesDelayed;
           rule_extras = []
         }
      in
         fold_proof postf pf addr (RuleBox info)

end

(*
 * -*-
 * Local Variables:
 * Caml-master: "editor.top"
 * End:
 * -*-
 *)
