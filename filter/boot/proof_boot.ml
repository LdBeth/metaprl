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

open Printf
open Mp_debug
open Weak_memo

open Opname
open Refiner.Refiner
open TermType
open Term
open TermOp
open TermMan
open TermSubst
open RefineError
open Refine
open Refine_exn

open Refiner_sig
open Refiner_io

open Rformat
open Dform
open Dform_print

open Term_eq_table
open Mp_resource

open Tactic_boot
open Tactic_boot.TacticType
open Tactic_boot.TacticInternalType
open Tactic_boot.TacticInternal
open Sequent_boot

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
   type attribute = TacticType.attribute
   type attributes = (string * attribute) list
   type arglist = TacticType.arglist
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

   (*
    * The proof is just an address into an extract.
    * Invariant: pf_node = index pf_root pf_address
    *)
   type proof =
      { pf_root : extract;
        pf_address : address;
        pf_node : extract
      }

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
   exception ProofRefineError of string * proof * refine_error

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

      let append = List_util.unionq
   end

   module Cache = MakeMsequentTable (CacheBase);;

   (*
    * Cache is actually imperative.
    *)
   let cache = ref (Cache.empty)

   let cache_lock = Mutex.create ()

   (************************************************************************
    * PROOF PRINTING                                                       *
    ************************************************************************)

   let format_proof db buf proof =
      let rec format_addr buf = function
         [i] ->
            format_int buf i
       | i :: tl ->
            format_int buf i;
            format_char buf ';';
            format_addr buf tl
       | [] ->
            ()
      in
         format_string buf "Proof: [";
         format_addr buf proof.pf_address;
         format_string buf "]";
         format_hspace buf;
         format_extract db buf proof.pf_root;
         format_hspace buf;
         format_string buf "Node:";
         format_hspace buf;
         format_extract db buf proof.pf_node

   let print_ext ext =
      let buf = Rformat.new_buffer () in
         format_extract !debug_base buf ext;
         format_newline buf;
         print_to_channel default_width buf stderr;
         flush stderr

   (************************************************************************
    * NORMALIZATION                                                        *
    ************************************************************************)

   let rec count_leaves = function
      Goal _ | Identity _ | ExtractRewrite _ -> 1
    | Unjustified (_, leaves) -> List.length leaves
    | Extract (_, leaves, _) -> List.length leaves
    | ExtractCondRewrite (_, leaves, _, _) -> List.length leaves
    | ExtractNthHyp _ -> 0
    | ExtractCut _ -> 2
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
       | ExtractRewrite (g,sg,addr,ext) ->
            if g==goal then node else ExtractRewrite (goal,sg,addr,ext)
       | ExtractCondRewrite (g,sgs,addr,ext) ->
            if g==goal then node else ExtractCondRewrite (goal,sgs,addr,ext)
       | ExtractNthHyp (g,i) ->
            if g==goal then node else ExtractNthHyp (goal,i)
       | ExtractCut (g,t,sg1,sg2) ->
            if g==goal then node else ExtractCut (goal,t,sg1,sg2)
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
       | ExtractRewrite (g,sg,addr,ext) ->
            begin match gs with
               res::gs ->
                  if res==sg then gs,node else gs, ExtractRewrite (g,res,addr,ext)
             | _ -> raise (Invalid_argument "Proof_boot.replace_subg_aux")
            end
       | ExtractCondRewrite (g,sgs,addr,ext) ->
            let gs, res = replace_list gs sgs in
            if res==sgs then gs,node else gs, ExtractCondRewrite (g,res,addr,ext)
       | ExtractNthHyp _ ->
            gs,node
       | ExtractCut (g,t,sg1,sg2) ->
            begin match gs with
               res1::res2::gs ->
                  if res1==sg1 && res2==sg2 then gs,node else gs, ExtractCut (g,t,res1,res2)
             | _ -> raise (Invalid_argument "Proof_boot.replace_subg_aux")
            end
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
                        raise (Invalid_argument "Proof_boot.normalize - nogin: according to my understanding, this is not supposed to happen")
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
                  raise (Invalid_argument "Proof_boot.normalize - nogin: according to my understanding, this is not supposed to happen")
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

   and normalize_list l = List_util.smap normalize l

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
      let ext = Goal goal in
         { pf_root = ext;
           pf_address = [];
           pf_node = ext
         }

   (*
    * Main proof node.
    *)
   let root { pf_root = root } =
      { pf_root = root;
        pf_address = [];
        pf_node = root
      }

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

   let is_leaf pf =
      is_leaf_ext pf.pf_node

   (*
    * Get the goal of the extract.
    *)
   let rec goal_ext = function
      Goal t ->
         t
    | Identity t ->
         t
    | Unjustified (t, _) ->
         t
    | Extract (t, _, _) ->
         t
    | ExtractRewrite (t, _, _, _) ->
         t
    | ExtractCondRewrite (t, _, _, _) ->
         t
    | ExtractNthHyp (t, _) ->
         t
    | ExtractCut (t, _, _, _) ->
         t
    | Compose { comp_goal = ext } ->
         goal_ext ext
    | Wrapped (_, ext) ->
         goal_ext ext
    | RuleBox { rule_extract = ext } ->
         goal_ext ext
    | Pending f ->
         goal_ext (f ())
    | Locked ext ->
         goal_ext ext

   let goal proof =
      goal_ext proof.pf_node

   (*
    * Compute the parent sets.
    *)
   let rec get_parents arg =
      match arg.ref_parent with
         ParentLazy parent ->
            let set = get_parents parent in
            let set = ParentTable.add set parent.ref_goal parent in
               arg.ref_parent <- ParentSet (parent, set);
               set
       | ParentSet (_, set) ->
            set
       | ParentNone ->
            ParentTable.empty

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
            Goal t ->
               [t]
          | Identity t ->
               [t]
          | Unjustified (_, leaves) ->
               leaves
          | Extract (_, leaves, _) ->
               leaves
          | ExtractRewrite (_, leaf, _, _) ->
               [leaf]
          | ExtractCondRewrite (_, leaves, _, _) ->
               leaves
          | ExtractNthHyp _ ->
               []
          | ExtractCut (_, _, leaf1, leaf2) ->
               [leaf1; leaf2]
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
            let buf = Rformat.new_buffer () in
            format_hzone buf;
            format_string buf "Leaves of";
            format_space buf;
            format_szone buf;
            format_extract !debug_base buf goal;
            format_ezone buf;
            format_space buf;
            print_ext goal;
            format_space buf;
            format_string buf "are";
            format_space buf;
            List.iter (format_arg !debug_base buf) leaves;
            format_ezone buf;
            print_to_channel default_width buf stderr;
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

   let leaves proof =
      leaves_ext proof.pf_node

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
    | Extract _
    | ExtractRewrite _
    | ExtractCondRewrite _
    | ExtractNthHyp _
    | ExtractCut _ ->
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
    | Wrapped (_, ext) ->
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
    | Locked ext ->
         status_ext ext

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

   let status proof =
      translate_status (status_ext proof.pf_node)

   (**********************************************************************
    * MAP FUNCTIONS                                                        *
    ************************************************************************)

   (*
    * Raise an error because the address is invalid.
    *)
   let raise_select_error { pf_root = root; pf_address = address } node raddr i =
      let proof =
         { pf_root = root;
           pf_address = address @ List.rev raddr;
           pf_node = node
         }
      in
         raise (ProofRefineError ("select", proof, StringIntError ("illegal address", i)))

   (*
    * Choose a child by address.
    *)
   let select_subgoal proof node raddr goal subgoals extras i =
      if i < 0 then
         raise_select_error proof node raddr i
      else if i = 0 then
         goal
      else
         let i = pred i in
         let len = List.length subgoals in
            if i < len then
               List.nth subgoals i
            else
               let i = i - len in
                  if i < List.length extras then
                     List.nth extras i
                  else
                     raise_select_error proof node raddr i

   let rec select_child proof node raddr i =
      match node with
         Goal _
       | Extract _
       | ExtractRewrite _
       | ExtractCondRewrite _
       | ExtractNthHyp _
       | ExtractCut _
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
    * Address error during replacement.
    *)
   let raise_replace_error { pf_root = root; pf_address = address } node raddr i =
      let proof =
         { pf_root = root;
           pf_address = address @ List.rev raddr;
           pf_node = node
         }
      in
         raise (ProofRefineError ("replace", proof, StringIntError ("illegal address", i)))

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
               [], List_util.some_map filter_extra subgoals
      in
         (fun leaves subgoals extras ->
               collect leaves (filter_subgoals (subgoals @ extras)))

   let rec find_leaf compare leaf = function
      (goal, subgoal) as h :: subgoals ->
         if compare goal leaf then
            subgoal, subgoals
         else
            let subgoal, subgoals = find_leaf compare leaf subgoals in
               subgoal, h :: subgoals
    | [] ->
         Goal leaf, []

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
               RuleBox rb, subgoals -> RuleBox { rb with rule_extract = Goal leaf}, subgoals
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
            List_util.replace_nth i node subgoals, extras
         else
            subgoals, List_util.replace_nth (i - len) node extras

   let replace_subgoal proof node raddr (goal : extract) (subgoals : extract list) (extras : extract list) i (node' : extract) =
      if i < 0 then
         raise_replace_error proof node raddr i
      else if i = 0 then
         let subgoals', extras' = match_subgoals (leaves_ext node') subgoals extras in
            node', subgoals', extras', node' == goal && List_util.compare_eq subgoals subgoals' && List_util.compare_eq extras extras'
      else
         let subgoals', extras' = insert_subgoal node' (pred i) subgoals extras in
         let subgoals', extras' = match_subgoals (leaves_ext goal) subgoals' extras' in
            goal, subgoals', extras', List_util.compare_eq subgoals subgoals' && List_util.compare_eq extras extras'

   let replace_subterm proof node raddr goal subgoals i = function
      Goal goal' ->
         if i < 0 || i > List.length subgoals then
            raise_replace_error proof node raddr i
         else if i = 0 then
            goal', subgoals, goal' == goal
         else
            let subgoal = List.nth subgoals i in
               goal, List_util.replace_nth i goal' subgoals, goal' == subgoal
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
       | Extract (goal, subgoals, ext) ->
            let goal, subgoals, unchanged = replace_subterm proof node raddr goal subgoals i node' in
            let node =
               if unchanged then
                  node
               else
                  Unjustified (goal, subgoals)
            in
               false, false, node
       | ExtractRewrite (goal, subgoal, _, _) ->
            let goal, subgoals, unchanged = replace_subterm proof node raddr goal [subgoal] i node' in
            let node =
               if unchanged then
                  node
               else
                  Unjustified (goal, subgoals)
            in
               false, false, node
       | ExtractCondRewrite (goal, subgoals, _, _) ->
            let goal, subgoals, unchanged = replace_subterm proof node raddr goal subgoals i node' in
            let node =
               if unchanged then
                  node
               else
                  Unjustified (goal, subgoals)
            in
               false, false, node
       | ExtractNthHyp (goal, _) ->
            let goal, subgoals, unchanged = replace_subterm proof node raddr goal [] i node' in
            let node =
               if unchanged then
                  node
               else
                  Unjustified (goal, subgoals)
            in
               false, false, node
       | ExtractCut (goal, _, leaf1, leaf2) ->
            let goal, subgoals, unchanged = replace_subterm proof node raddr goal [leaf1; leaf2] i node' in
            let node =
               if unchanged then
                  node
               else
                  Unjustified (goal, subgoals)
            in
               false, false, node
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
      map_path_ext proof proof.pf_node [] [] f path

   let map_root_path f proof =
      map_path f (root proof) proof.pf_address

   (*
    * Fold a function along the path from outermost to innermost.
    *)
   let rec fold_down_ext proof node raddr arg f = function
      i :: t ->
         fold_down_ext proof (select_child proof node raddr i) (i :: raddr) (f proof node arg) f t
    | [] ->
         arg

   let fold_down f arg proof path =
      fold_down_ext proof proof.pf_node [] arg f path

   let fold_down_root f arg proof =
      fold_down f arg (root proof) proof.pf_address

   (*
    * Fold a function along the path from innermost to outermost.
    *)
   let rec fold_up_ext proof node raddr arg f = function
      i :: t ->
         f proof node (fold_up_ext proof (select_child proof node raddr i) (i :: raddr) arg f t)
    | [] ->
         f proof node arg

   let fold_up f arg proof path =
      fold_up_ext proof proof.pf_node [] arg f path

   let fold_up_root f arg proof =
      fold_up f arg (root proof) proof.pf_address

   (*
    * Fold a function along the path from innermost to outermost,
    * recomputing the proof.
    *)
   let rec fold_up_proof_ext (proof : proof) (node : extract) (raddr : int list) (arg : extract) (f : proof -> extract -> extract) = function
      i :: t ->
         let locked, post, node' = fold_up_proof_ext proof (select_child proof node raddr i) (i :: raddr) arg f t in
         let locked', post', node = replace_child proof node raddr i (f proof node') in
            locked || locked', post || post', node
    | [] ->
         false, false, arg

   let fold_up_proof f node proof =
      fold_up_proof_ext (root proof) proof.pf_root [] node f proof.pf_address

   let fold_proof postf proof node =
      let _ =
         if !debug_proof then
            let { pf_root = root; pf_address = address } = proof in
               eprintf "Proof_boot.fold_proof %a%t" print_int_list address eflush;
               print_ext root;
               print_ext node
      in
      let locked_flag, post_flag, root = fold_up_proof (fun _ node -> node) node proof in
      let proof =
         { pf_root = root;
           pf_address = proof.pf_address;
           pf_node = node
         }
      in
         if !debug_proof then
            eprintf "Proof_boot.fold_proof: post=%b locked=%b%t" post_flag locked_flag eflush;
         if post_flag || not locked_flag then
            postf proof
         else
            proof

   (*
    * Sweep a function up the proof tree.
    *)
   let rec sweep_up_ext proof (f : proof -> extract -> extract) node =
      match node with
         Goal _
       | Identity _
       | Unjustified _
       | Extract _
       | ExtractRewrite _
       | ExtractCondRewrite _
       | ExtractNthHyp _
       | ExtractCut _ ->
            f proof node
       | Compose { comp_goal = goal; comp_subgoals = subgoals; comp_extras = extras } ->
            let goal = sweep_up_ext proof f goal in
            let subgoals = List.map (sweep_up_ext proof f) subgoals in
            let extras = List.map (sweep_up_ext proof f) extras in
            let subgoals, extras = match_subgoals (leaves_ext goal) subgoals extras in
               Compose {
                  comp_status = LazyStatusDelayed;
                  comp_goal = goal;
                  comp_subgoals = subgoals;
                  comp_extras = extras;
                  comp_leaves = LazyLeavesDelayed
               }
       | Wrapped (label, goal) ->
            Wrapped (label, sweep_up_ext proof f goal)
       | RuleBox { rule_expr = expr;
                   rule_string = text;
                   rule_tactic = tac;
                   rule_extract_normalized = normal;
                   rule_extract = goal;
                   rule_subgoals = subgoals;
                   rule_extras = extras
         } ->
            let new_goal = sweep_up_ext proof f goal in
            let subgoals = List.map (sweep_up_ext proof f) subgoals in
            let extras = List.map (sweep_up_ext proof f) extras in
            let subgoals, extras = match_subgoals (leaves_ext new_goal) subgoals extras in
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
       | Pending g ->
            sweep_up_ext proof f (g ())
       | Locked ext ->
            Locked (sweep_up_ext proof f ext)

   let sweep_up postf f proof =
      let node = sweep_up_ext proof f proof.pf_node in
         fold_proof postf proof node

   (*
    * Sweep a function over the tactic_args in the tree.
    *)
   let rec map_tactic_arg_ext (f : tactic_arg -> tactic_arg) node =
      match node with
         Goal arg ->
            Goal (f arg)
       | Identity arg ->
            Identity (f arg)
       | Unjustified (goal, subgoals) ->
            Unjustified (f goal, List.map f subgoals)
       | Extract (goal, subgoals, ext) ->
            Unjustified (f goal, List.map f subgoals)
       | ExtractRewrite (goal, subgoal, addr, ext) ->
            Unjustified (f goal, [f subgoal])
       | ExtractCondRewrite (goal, subgoals, addr, ext) ->
            Unjustified (f goal, List.map f subgoals)
       | ExtractNthHyp (arg, i) ->
            ExtractNthHyp (f arg, i)
       | ExtractCut (goal, hyp, cut_lemma, cut_then) ->
            ExtractCut (f goal, hyp, f cut_lemma, f cut_then)
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

   let map_tactic_arg postf f proof =
      let node = map_tactic_arg_ext f proof.pf_node in
         fold_proof postf proof node

   (************************************************************************
    * EXTRACT UNFOLDING                                                    *
    ************************************************************************)

   (*
    * Faster map2 with a normal exception.
    *)
   let rec map2 f l1 l2 =
      match l1, l2 with
         h1 :: t1, h2 :: t2 ->
            f h1 h2 :: map2 f t1 t2
       | _ ->
            raise (RefineError ("Proof_boot.map2", StringError "proof is ill-formed"))

   (*
    * Build a tactic arg from an msequent returned from the extract.
    *)
   let replace_msequent arg goal =
      { arg with ref_goal = goal }

   let replace_msequent_goal arg goal' =
      let goal, hyps = dest_msequent arg.ref_goal in
      let goal = mk_msequent goal' hyps in
         { arg with ref_goal = goal }

   let replace_msequent_addr arg addr goal' =
      if TermAddr.is_null_address addr then
         replace_msequent_goal arg goal'
      else
         let goal, hyps = dest_msequent arg.ref_goal in
         let goal = TermAddr.replace_subterm goal addr goal' in
         let goal = mk_msequent goal hyps in
            { arg with ref_goal = goal }

   let cut_goal arg hyp =
      let goal, hyps = dest_msequent arg.ref_goal in
      let goal = mk_msequent goal (hyps @ [hyp]) in
         { arg with ref_goal = goal }

   (*
    * Get the goals and subgoals of the Refine.extract and
    * build an extract.
    *)
   let extract_of_refine_extract goal ext =
      Extract (replace_msequent goal (Refine.goal_of_extract ext),
               List.map (replace_msequent goal) (Refine.subgoals_of_extract ext),
               ext)

   let extract_of_rewrite_extract goal addr ext =
      ExtractRewrite (replace_msequent_addr goal addr (Refine.goal_of_rw_extract ext),
                      replace_msequent_addr goal addr (Refine.subgoal_of_rw_extract ext),
                      addr,
                      ext)

   let extract_of_cond_rewrite_extract goal addr ext =
      ExtractCondRewrite (replace_msequent_addr goal addr (Refine.goal_of_crw_extract ext),
                          List.map (fun (addr, t) -> replace_msequent_addr goal addr t) (Refine.subgoals_of_crw_extract ext),
                          addr,
                          ext)

   let extract_failure_exn = RefineError ("Proof_boot.unfold_extract", StringError "extract is atomic")
   let rw_extract_failure_exn = RefineError ("Proof_boot.unfold_rw_extract", StringError "extract is atomic")
   let crw_extract_failure_exn = RefineError ("Proof_boot.unfold_crw_extract", StringError "extract is atomic")
   let index_exn = RefineError ("Proof_boot.index", StringError "there is no proof node with such address")

   (*
    * Unfold the Refine.extract into a normal extract.
    *)
   let unfold_extract goal subgoals ext =
      if !debug_proof then
         eprintf "Proof_boot.unfold_extract%t" eflush;
      match Refine.dest_extract ext with
         AtomicExtract _ ->
            raise extract_failure_exn
       | RewriteExtract (goal', ext, subgoal') ->
            begin
               match subgoals with
                  [subgoal] ->
                     ExtractRewrite (replace_msequent goal goal',
                                     replace_msequent subgoal subgoal',
                                     TermAddr.make_address [],
                                     ext)
                | _ ->
                     raise extract_failure_exn
            end
       | CondRewriteExtract (goal', ext, subgoals') ->
            ExtractCondRewrite (replace_msequent goal goal',
                                map2 replace_msequent subgoals subgoals',
                                TermAddr.make_address [],
                                ext)
       | ComposeExtract (ext, extl) ->
            Compose {
               comp_status = LazyStatusDelayed;
               comp_goal = extract_of_refine_extract goal ext;
               comp_subgoals = List.map (extract_of_refine_extract goal) extl;
               comp_leaves = LazyLeavesDelayed;
               comp_extras = []
            }
       | NthHypExtract (goal', i) ->
            begin
               match subgoals with
                  [] ->
                     ExtractNthHyp (replace_msequent goal goal', i)
                | _ ->
                     raise extract_failure_exn
            end
       | CutExtract { cut_goal = cut_goal;
                      cut_hyp = hyp;
                      cut_lemma = cut_lemma;
                      cut_then = cut_then
         } ->
            begin
               match subgoals with
                  [subgoal1; subgoal2] ->
                     ExtractCut (goal,
                                 hyp,
                                 replace_msequent subgoal1 cut_lemma,
                                 replace_msequent subgoal2 cut_then)
                | _ ->
                     raise extract_failure_exn
            end

   (*
    * Flatten a higherC rewrite.
    *)
   let flatten_higher get_goal get_subgoal make_goal goal addr =
      let rec search addr' t ext exts results t' =
         if alpha_equal t t' then
            let addr' = TermAddr.make_address (List.rev addr') in
            let addr'' = TermAddr.compose_address addr addr' in
            let ext =
               make_goal (replace_msequent_addr goal addr'' t) (**)
                  (replace_msequent_addr goal addr'' (get_subgoal ext))
                  addr''
                  ext
            in
               if !debug_proof then
                  eprintf "Proof_boot.flatten_higher: addr=%s rel=%s final=%s%t" (**)
                     (TermAddr.string_of_address addr)
                     (TermAddr.string_of_address addr')
                     (TermAddr.string_of_address addr'')
                     eflush;
               exts, ext :: results

         else if is_var_term t then
            ext :: exts, results

         else
            let rec search' index t ext exts results = function
               bterm :: bterms ->
                  let { bterm = t' } = dest_bterm bterm in
                  let exts, results = search (index :: addr') t ext exts results t' in
                     begin
                        match exts with
                           ext :: exts ->
                              search' (succ index) (get_goal ext) ext exts results bterms
                         | [] ->
                              [], results
                     end
             | [] ->
                  exts, results
            in
               search' 0 t ext exts results (dest_term t).term_terms
      in
      let rec compose = function
         [ext] ->
            ext
       | ext :: exts ->
            Compose {
               comp_status = LazyStatusDelayed;
               comp_goal = ext;
               comp_subgoals = [compose exts];
               comp_leaves = LazyLeavesDelayed;
               comp_extras = []
            }
       | [] ->
            Identity goal
      in
      let collect exts goal' =
         match exts with
            ext :: exts ->
               compose (snd (search [] (get_goal ext) ext exts [] goal'))
          | [] ->
               Identity goal
      in
         collect

   (*
    * Reverse the extract.
    * The reversal becomes a cut.
    *)
   let reverse_extract get_goal make_ext goal subgoal addr ext =
      let goal_term, hyps = dest_msequent goal.ref_goal in
      let rw_goal = get_goal ext in
      let hyp = TermAddr.replace_subterm goal_term addr rw_goal in
      let cut_lemma = replace_msequent_goal goal hyp in
      let cut_then = cut_goal goal hyp in
      let ext_cut = ExtractCut (goal, hyp, cut_lemma, cut_then) in
      let ext_lemma = make_ext cut_lemma goal addr ext in
      let comp_lemma =
         Compose {
            comp_status = LazyStatusDelayed;
            comp_goal = ext_cut;
            comp_subgoals = [ExtractNthHyp (goal, List.length hyps)];
            comp_leaves = LazyLeavesDelayed;
            comp_extras = []
         }
      in
         Compose {
            comp_status = LazyStatusDelayed;
            comp_goal = ext_cut;
            comp_subgoals = [Goal cut_lemma; comp_lemma];
            comp_leaves = LazyLeavesDelayed;
            comp_extras = []
         }

   (*
    * Unfold the rewrite.
    *)
   let make_rw_extract goal subgoal addr ext =
      ExtractRewrite (goal, subgoal, addr, ext)

   let unfold_rw_extract goal subgoal addr ext =
      if !debug_proof then
         eprintf "Proof_boot.unfold_rw_extract: begin%t" eflush;
      let ext =
         match Refine.dest_rw_extract ext with
            AtomicRewriteExtract _ ->
               if !debug_proof then
                  eprintf "Proof_boot.unfold_rw_extract: atomic%t" eflush;
               raise rw_extract_failure_exn
          | ReverseRewriteExtract ext ->
               if !debug_proof then
                  eprintf "Proof_boot.unfold_rw_extract: reverse%t" eflush;
               reverse_extract Refine.goal_of_rw_extract make_rw_extract goal subgoal addr ext
          | ComposeRewriteExtract (ext1, ext2) ->
               if !debug_proof then
                  eprintf "Proof_boot.unfold_rw_extract: compose%t" eflush;
               Compose {
                  comp_status = LazyStatusDelayed;
                  comp_goal = extract_of_rewrite_extract goal addr ext1;
                  comp_subgoals = [extract_of_rewrite_extract goal addr ext2];
                  comp_leaves = LazyLeavesDelayed;
                  comp_extras = []
               }
          | AddressRewriteExtract (goal', addr', ext, subgoal') ->
               if !debug_proof then
                  eprintf "Proof_boot.unfold_rw_extract: address: %s/%s%t" (**)
                     (TermAddr.string_of_address addr)
                     (TermAddr.string_of_address addr')
                     eflush;
               ExtractRewrite (goal,
                               subgoal,
                               TermAddr.compose_address addr addr',
                               ext)
          | HigherRewriteExtract (goal', exts, subgoal') ->
               if !debug_proof then
                  eprintf "Proof_boot.unfold_rw_extract: higher: %s%t" (TermAddr.string_of_address addr) eflush;
               flatten_higher Refine.goal_of_rw_extract Refine.subgoal_of_rw_extract make_rw_extract goal addr exts goal'
      in
         if !debug_proof then
            eprintf "Proof_boot.unfold_rw_extract: done%t" eflush;
         ext

   (*
    * Unfold the conditional rewrite.
    *)
   let make_crw_extract goal _ addr ext =
      ExtractCondRewrite (goal,
                          List.map (fun (addr, t) -> replace_msequent_addr goal addr t) (Refine.subgoals_of_crw_extract ext),
                          addr,
                          ext)

   let unfold_crw_extract goal subgoal addr ext =
      if !debug_proof then
         eprintf "Proof_boot.unfold_crw_extract%t" eflush;
      match Refine.dest_crw_extract ext with
         AtomicCondRewriteExtract _ ->
            raise crw_extract_failure_exn
       | ReverseCondRewriteExtract ext ->
            reverse_extract Refine.goal_of_crw_extract make_crw_extract goal subgoal addr ext
       | ComposeCondRewriteExtract (ext1, ext2) ->
            Compose {
               comp_status = LazyStatusDelayed;
               comp_goal = extract_of_cond_rewrite_extract goal addr ext1;
               comp_subgoals = [extract_of_cond_rewrite_extract goal addr ext2];
               comp_leaves = LazyLeavesDelayed;
               comp_extras = []
            }
       | AddressCondRewriteExtract (goal', addr', ext, subgoal') ->
            ExtractCondRewrite (replace_msequent_addr goal addr goal',
                                List.map (fun (addr, t) -> replace_msequent_addr goal addr t) (Refine.subgoals_of_crw_extract ext),
                                TermAddr.compose_address addr addr',
                                ext)
       | HigherCondRewriteExtract (goal', exts, subgoal') ->
            flatten_higher Refine.goal_of_crw_extract Refine.subgoal_of_crw_extract make_crw_extract goal addr exts goal'

   (************************************************************************
    * NAVIGATION                                                           *
    ************************************************************************)

   (*
    * Return the absolute address of the current proof.
    *)
   let address proof =
      proof.pf_address

   (*
    * Get the status along the path from the root.
    * The last component of the path may not be valid.
    *)
   let path_status proof =
      let addr = proof.pf_address in
      let proof = root proof in
         try map_path (fun proof node -> translate_status (status_ext node)) proof addr with
            (ProofRefineError _) as exn ->
               if addr = [] then
                  raise exn;
               let addr, _ = List_util.split_last addr in
                  map_path (fun proof node -> translate_status (status_ext node)) proof addr

   (*
    * If the current node is an extract, unfold it.
    *)
   let unfold_ext node =
      match node with
         Goal _
       | Identity _
       | Unjustified _
       | ExtractNthHyp _
       | ExtractCut _
       | Compose _
       | Wrapped _
       | RuleBox _
       | Pending _
       | Locked _ ->
            node

       | Extract (goal, subgoals, ext) ->
            unfold_extract goal subgoals ext
       | ExtractRewrite (goal, subgoal, addr, ext) ->
            unfold_rw_extract goal subgoal addr ext
       | ExtractCondRewrite (goal, subgoals, addr, ext) ->
            unfold_crw_extract goal subgoals addr ext

   let unfold postf proof =
      fold_proof postf proof (unfold_ext proof.pf_node)

   (*
    * Go the a particular path.
    *)
   let rec index_ext proof node raddr = function
      i :: tl ->
         index_ext proof (select_child proof node raddr i) (i :: raddr) tl
    | [] ->
         node

   let index proof path =
      let { pf_root = root;
            pf_address = addr;
            pf_node = node
          } = proof
      in
         try
            let node = index_ext proof node [] path in
               { pf_root = root;
                 pf_address = addr @ path;
                 pf_node = node
               }
         with ProofRefineError _ -> raise index_exn

   (*
    * Child operation is a simplified index.
    *)
   let child proof i =
      index proof [i]

   (*
    * Parent of node performs search from the root.
    *)
   let parent proof =
      match proof.pf_address with
         [] ->
            proof
       | addr ->
            let addr, _ = List_util.split_last addr in
               index (root proof) addr

   (*
    * Set the goal of the current node.
    *)
   let set_goal_ext node mseq =
      let goal = goal_ext node in
      let arg = Goal { goal with ref_goal = mseq } in
      match node with
         RuleBox ri ->
            RuleBox { ri with
               rule_status = LazyStatusDelayed;
               rule_extract_normalized = true;
               rule_extract = arg;
               rule_leaves = LazyLeavesDelayed;
            }
       | _ ->
         arg

   let set_goal postf proof mseq =
      let node = set_goal_ext proof.pf_node mseq in
         fold_proof postf proof node

   (*
    * Copy a proof from one location to another.
    *)
   let copy postf proof from_addr to_addr =
      let { pf_address = address;
            pf_node = node
          } = proof
      in
      let from_node = index_ext proof node [] from_addr in
      let to_proof = index proof to_addr in
      let goal = goal proof in
      let from_node = set_goal_ext from_node goal.ref_goal in
      let to_proof = fold_proof postf to_proof from_node in
         index to_proof address

   (*
    * Paste an alternate proof at this location.
    *)
   let paste postf to_proof from_proof =
      let goal = goal to_proof in
      let from_node = set_goal_ext from_proof.pf_node goal.ref_goal in
         fold_proof postf to_proof from_node

   (*
    * Make the current subgoal an assumption.
    *)
   let make_assum_arg goal i arg =
      let goal, hyps = Refine.dest_msequent arg.ref_goal in
      let hyps = List_util.insert_nth i goal hyps in
        replace_msequent arg (Refine.mk_msequent goal hyps)

   let make_assum postf proof =
      (* Add the goal as an assumption to all proof nodes *)
      let mseq = (goal proof).ref_goal in
      let goal, _ = Refine.dest_msequent mseq in
      let mseq = (goal_ext proof.pf_root).ref_goal in
      let _, hyps = Refine.dest_msequent mseq in
      let len = List.length hyps in
      let root = map_tactic_arg_ext (make_assum_arg goal len) proof.pf_root in

      (* Replace the current node with ExtractNthHyp *)
      let address = proof.pf_address in
      let node = index_ext proof root [] address in
      let proof =
         { pf_root = root;
           pf_address = address;
           pf_node = node
         }
      in
         fold_proof postf proof node

   (************************************************************************
    * CACHE & CACHED NAVIGATION                                            *
    ************************************************************************)

   (*
    * Add an extract to the cache.
    *)
   let set_cache ext =
      let mseq = (goal_ext ext).ref_goal in
         Mutex.lock cache_lock;
         cache := Cache.add !cache mseq ext;
         Mutex.unlock cache_lock

   (*
    * Add a new Pending node to the cache.
    * Remember the proof in case the goal changes.
    *)
   let post f =
      if !debug_proof then
         eprintf "Posting proof%t" eflush;
      let proof = f () in
      let addr = proof.pf_address in
      let old_goal = (goal proof).ref_goal in
      let old_proof = ref proof in
      let compute_ext () =
         try
            let new_proof = f () in
            let proof =
               if !old_proof.pf_root==new_proof.pf_root then !old_proof else
                  let new_proof = index (root new_proof) addr in
                  if msequent_alpha_equal (goal new_proof).ref_goal old_goal then
                     begin
                        if !debug_proof_pending then begin
                           eprintf "Replacing old :\n";
                           print_ext !old_proof.pf_node;
                           eprintf "with new :\n";
                           print_ext new_proof.pf_node
                        end;
                        old_proof := new_proof;
                        new_proof
                     end
                  else
                     !old_proof
            in
               proof.pf_node
         with
            ProofRefineError _
          | ExtRefineError _ ->
               (!old_proof).pf_node
      in
      let ext = Pending compute_ext in
         set_cache ext;

         (* Eliminate all enclosing Pending nodes *)
         fold_proof (fun proof -> proof) proof (Locked proof.pf_node)

   (*
    * Compute all the extracts along the path to the root.
    * This is slow, so don't do this very often.
    *)
   let compute_path_set proof =
      let add_extract proof ext set =
         let goal = goal_ext ext in
            Cache.add set goal.ref_goal ext
      in
         fold_down_root add_extract Cache.empty proof

   (*
    * Remove extracts that occur in the parent set.
    *)
   let rec prune_cycles parents found = function
      ext :: tl ->
         let goal = goal_ext ext in
         let possible = Cache.find_all parents goal.ref_goal in
            if List.memq ext possible || List.memq ext found then
               prune_cycles parents found tl
            else
               prune_cycles parents (ext :: found) tl
    | [] ->
         found

   (*
    * Fetch all the entries in the cache that are possible for this
    * tactic_arg.  We have to check that the extracs do not cause
    * cycles in the proof.
    *)
   let get_cache proof =
      try
         let goal = goal proof in
         let exts = Cache.find_all !cache goal.ref_goal in
         let parents = compute_path_set proof in
         let nodes = prune_cycles parents [] exts in
            List.map (fold_proof (fun proof -> proof) proof) nodes
      with
         Not_found ->
            []

   (*
    * For the goal, append all cached proofs.
    *)
   let proof_goal cache proof goal =
      let { pf_root = root; pf_address = address } = proof in
         { pf_root = root;
           pf_address = address @ [0];
           pf_node = goal
         } :: if cache then get_cache proof else []

   (*
    * Remove duplicates in the subgoals, and append cached proofs.
    *)
   let proof_subgoals proof subgoals =
      let { pf_root = root; pf_address = address } = proof in
      let rec collect i found = function
         subgoal :: subgoals ->
            if search_arg subgoal found then
               collect (succ i) found subgoals
            else
               let proof =
                  { pf_root = root;
                    pf_address = address @ [i];
                    pf_node = Goal subgoal
                  }
               in
                  proof :: collect (succ i) (subgoal :: found) subgoals
       | [] ->
            []
      in
      let subgoals = collect 1 [] subgoals in
         List.map (fun subgoal -> [subgoal]) subgoals

   (*
    * Get the list of subgoals with cached entries.
    * Make sure extras are in the cache.
    *)
   let proof_subgoals_extras cache proof subgoals extras =
      let { pf_root = root; pf_address = address } = proof in
      let rec wrap i = function
         subgoal :: subgoals ->
            let proof =
               { pf_root = root;
                 pf_address = address @ [i];
                 pf_node = subgoal
               }
            in
               proof :: wrap (succ i) subgoals
       | [] ->
            []
      in
      let _ = List.iter set_cache extras in
      let subgoals = wrap 1 subgoals in
      let extras = wrap (succ (List.length subgoals)) extras in
      let subgoals = List.map (fun subgoal -> subgoal :: if cache then get_cache subgoal else []) subgoals in
         subgoals, extras

   (*
    * Expressions.
    *)
   let string_of_opname opname =
      let rec collect_opname caps = function
         [t] ->
            if caps then String.capitalize t else t
       | h :: t ->
            collect_opname true t ^ "." ^ (if caps then String.capitalize h else h)
       | [] ->
            "<nil opname>"
      in
         collect_opname false (dest_opname opname)

   let make_extract_expr ext =
      let arglist =
         match Refine.dest_extract ext with
            AtomicExtract { just_addrs = addrs;
                            just_names = names;
                            just_params = params;
                            just_refiner = opname
            } ->
               let addrs = Array.map (fun addr -> StringArg (TermAddr.string_of_address addr)) addrs in
               let names = Array.map (fun name -> StringArg name) names in
               let params = Array.of_list (List.map (fun t -> TermArg t) params) in
               let name = [|StringArg (string_of_opname opname)|] in
                  GeneralArgList (Array.concat [name; addrs; names; params])

          | RewriteExtract _ ->
               NoneArgList "<rewrite>"
          | CondRewriteExtract _ ->
               NoneArgList "<conditional-rewrite>"
          | ComposeExtract _ ->
               StringStringArgList ("<tactic>", "thenT", "<tactic>")
          | NthHypExtract (_, i) ->
               IntArgList ("nthAssumT", i)
          | CutExtract { cut_hyp = hyp } ->
               TermArgList ("cutT", hyp)
      in
         ExprExtract arglist

   let null_address = TermAddr.make_address []

   let make_rw_extract_expr addr ext =
      let arglist =
         match Refine.dest_rw_extract ext with
            AtomicRewriteExtract (_, opname, _) ->
               if addr = null_address then
                  NoneArgList (string_of_opname opname)
               else
                  StringStringArgList ("addrC", TermAddr.string_of_address addr, string_of_opname opname)
          | ReverseRewriteExtract _ ->
               StringArgList ("foldC", "<rewrite>")
          | ComposeRewriteExtract _ ->
               StringStringArgList ("<rewrite>", "thenC", "<rewrite>")
          | AddressRewriteExtract (_, addr', _, _) ->
               StringStringArgList ("addrC", TermAddr.string_of_address (TermAddr.compose_address addr addr'), "<rewrite>")
          | HigherRewriteExtract _ ->
               StringArgList ("higherC", "<rewrite>")
      in
         ExprExtract arglist

   let make_crw_extract_expr addr ext =
      let arglist =
         match Refine.dest_crw_extract ext with
            AtomicCondRewriteExtract { cjust_names = names;
                                       cjust_params = params;
                                       cjust_refiner = opname
            } ->
               let names = Array.map (fun name -> StringArg name) names in
               let params = Array.of_list (List.map (fun t -> TermArg t) params) in
               let name = [|StringArg (string_of_opname opname)|] in
                  GeneralArgList (Array.concat [name; names; params])
          | ReverseCondRewriteExtract _ ->
               StringArgList ("foldC", "<conditional-rewrite>")
          | ComposeCondRewriteExtract _ ->
               StringStringArgList ("<conditional-rewrite>", "thenC", "<conditional-rewrite>")
          | AddressCondRewriteExtract (_, addr', _, _) ->
               StringStringArgList ("addrC", TermAddr.string_of_address (TermAddr.compose_address addr addr'), "<conditional-rewrite>")
          | HigherCondRewriteExtract _ ->
               StringArgList ("higherC", "<conditional-rewrite>")
      in
         ExprExtract arglist

   let make_nth_hyp_expr i =
      ExprExtract (IntArgList ("nthAssumT", i))

   let make_cut_expr hyp =
      ExprExtract (TermArgList ("cutT", hyp))

   (*
    * Describe all the parts of this step.
    *)
   let rec info_ext compose_flag proof node =
      match node with
         Goal goal ->
            { step_goal = proof :: get_cache proof;
              step_expr = ExprGoal;
              step_subgoals = [];
              step_extras = []
            }
       | Identity goal ->
            let proofs = proof_goal false proof (Goal goal) in
               { step_goal = proofs;
                 step_expr = ExprIdentity;
                 step_subgoals = [proofs];
                 step_extras = []
               }
       | Unjustified (goal, subgoals) ->
            { step_goal = proof_goal false proof (Goal goal);
              step_expr = ExprUnjustified;
              step_subgoals = proof_subgoals proof subgoals;
              step_extras = []
            }
       | Extract (goal, subgoals, ext) ->
            { step_goal = proof_goal false proof (Goal goal);
              step_expr = make_extract_expr ext;
              step_subgoals = proof_subgoals proof subgoals;
              step_extras = []
            }
       | ExtractRewrite (goal, subgoal, addr, ext) ->
            { step_goal = proof_goal false proof (Goal goal);
              step_expr = make_rw_extract_expr addr ext;
              step_subgoals = proof_subgoals proof [subgoal];
              step_extras = []
            }
       | ExtractCondRewrite (goal, subgoals, addr, ext) ->
            { step_goal = proof_goal false proof (Goal goal);
              step_expr = make_crw_extract_expr addr ext;
              step_subgoals = proof_subgoals proof subgoals;
              step_extras = []
            }
       | ExtractNthHyp (goal, i) ->
            { step_goal = proof_goal false proof (Goal goal);
              step_expr = make_nth_hyp_expr i;
              step_subgoals = [];
              step_extras = []
            }
       | ExtractCut (goal, hyp, subgoal1, subgoal2) ->
            { step_goal = proof_goal false proof (Goal goal);
              step_expr = make_cut_expr hyp;
              step_subgoals = proof_subgoals proof [subgoal1; subgoal2];
              step_extras = []
            }
       | Compose { comp_goal = goal; comp_subgoals = subgoals; comp_extras = extras } ->
            let new_subgoals, extras = proof_subgoals_extras false proof subgoals extras in
            if compose_flag then
               let goal = info_ext false proof goal in
                  { step_goal = goal.step_goal;
                    step_expr = ExprCompose goal.step_expr;
                    step_subgoals = new_subgoals;
                    step_extras = extras
                  }
            else
               { step_goal = proof_goal false proof goal;
                 step_expr = ExprCompose ExprIdentity;
                 step_subgoals = List.flatten (List.map (fun goal -> (info_ext false proof goal).step_subgoals) subgoals);
                 step_extras = extras
               }
       | Wrapped (label, ext) ->
            let { step_goal = goal;
                  step_subgoals = subgoals;
                  step_extras = extras
                } = info_ext false proof ext
            in
               { step_goal = goal;
                 step_expr = ExprWrapped label;
                 step_subgoals = subgoals;
                 step_extras = extras
               }
       | RuleBox { rule_extract_normalized = false } ->
            info_ext compose_flag proof (normalize node)
       | RuleBox { rule_expr = expr;
                   rule_string = text;
                   rule_extract_normalized = true;
                   rule_extract = goal;
                   rule_subgoals = subgoals;
                   rule_extras = extras
         } ->
            let subgoals, extras = proof_subgoals_extras true proof subgoals extras in
               { step_goal = proof_goal true proof goal;
                 step_expr = ExprRule (text, expr ());
                 step_subgoals = subgoals;
                 step_extras = extras
               }
       | Pending f ->
            info_ext compose_flag proof (f ())
       | Locked ext ->
            info_ext compose_flag proof ext

   let info proof =
      let info = info_ext true proof proof.pf_node in
         if !debug_proof then
            eprintf "Got info_ext%t" eflush;
         info

   (************************************************************************
    * UPDATES                                                              *
    ************************************************************************)

   (*
    * Replace the current proof step.
    *)
   let replace_step_subgoals step subgoals' extras' =
      let { rule_expr = expr;
            rule_string = text;
            rule_tactic = tac;
            rule_extract_normalized = normal;
            rule_extract = goal;
            rule_subgoals = subgoals;
            rule_extras = extras
          } = step
      in
      let leaves = leaves_ext goal in
      let subgoals, extras = match_subgoals leaves (subgoals @ subgoals') (extras @ extras') in
         { rule_status = LazyStatusDelayed;
           rule_expr = expr;
           rule_string = text;
           rule_tactic = tac;
           rule_extract_normalized = normal;
           rule_extract = goal;
           rule_subgoals = subgoals;
           rule_leaves = LazyLeavesDelayed;
           rule_extras = extras
         }

   let rec replace_step_rule proof node step =
      match node with
         Goal _
       | Identity _
       | Unjustified _
       | Extract _
       | ExtractRewrite _
       | ExtractCondRewrite _
       | ExtractNthHyp _
       | ExtractCut _ ->
            step
       | Compose { comp_subgoals = subgoals'; comp_extras = extras' } ->
            replace_step_subgoals step subgoals' extras'
       | Wrapped (label, node') ->
            replace_step_rule proof node' step
       | Pending f ->
            replace_step_rule proof (f ()) step
       | Locked ext ->
            replace_step_rule proof ext step
       | RuleBox { rule_subgoals = subgoals'; rule_extras = extras' } ->
            replace_step_subgoals step subgoals' extras'

   let refine postf proof text expr tac =
      let subgoals, ext = TacticInternal.refine tac (goal proof) in
      let info =
         { rule_status = LazyStatusDelayed;
           rule_expr = (fun () -> expr);
           rule_string = text;
           rule_tactic = (fun () -> tac);
           rule_extract_normalized = false;
           rule_extract = ext;
           rule_subgoals = List.map (fun goal -> Goal goal) subgoals;
           rule_leaves = LazyLeavesDelayed;
           rule_extras = []
         }
      in
      let info = replace_step_rule proof.pf_node proof.pf_node info in
      let ext = RuleBox info in
         set_cache ext;
         fold_proof postf proof ext

   (************************************************************************
    * GLOBAL PROOF OPERATIONS                                              *
    ************************************************************************)

   (*
    * "Update" the proof by forcing computation of status and
    * leaf nodes.
    *)
   let update proof =
      ignore (status_ext proof.pf_root);
      ignore (leaves_ext proof.pf_root)

   (*
    * "Clean" up the proof by removing all extras.
    *)
   let rec clean_extras_ext node =
      match node with
         Goal _
       | Identity _
       | Unjustified _
       | Extract _
       | ExtractRewrite _
       | ExtractCondRewrite _
       | ExtractNthHyp _
       | ExtractCut _ ->
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
       | RuleBox { rule_status = status;
                   rule_string = text;
                   rule_expr = expr;
                   rule_tactic = tactic;
                   rule_extract_normalized = normal;
                   rule_extract = goal;
                   rule_subgoals = subgoals;
                   rule_leaves = leaves
         } ->
            RuleBox { rule_status = status;
                      rule_string = text;
                      rule_expr = expr;
                      rule_tactic = tactic;
                      rule_extract_normalized = normal;
                      rule_extract = clean_extras_ext goal;
                      rule_subgoals = List.map clean_extras_ext subgoals;
                      rule_leaves = leaves;
                      rule_extras = []
            }
       | Pending f ->
            clean_extras_ext (f ())
       | Locked node ->
            Locked (clean_extras_ext node)

   let clean postf proof =
      fold_proof postf proof (clean_extras_ext proof.pf_node)

   (*
    * Squash the proof by removing all extracts.
    * Compositions also get squashed, but we take care to preserve
    * any paths that lead to rule boxes.
    *)
   let rec squash_ext node =
      match node with
         Goal _
       | Identity _
       | Unjustified _
       | ExtractNthHyp _
       | ExtractCut _ ->
            false, node
       | Extract (goal, subgoals, _) ->
            false, Unjustified (goal, subgoals)
       | ExtractRewrite (goal, subgoal, _, _) ->
            false, Unjustified (goal, [subgoal])
       | ExtractCondRewrite (goal, subgoals, _, _) ->
            false, Unjustified (goal, subgoals)
       | Wrapped (label, node) ->
            let (flag, node') as res = squash_ext node in
               if flag then flag, Wrapped (label, node')
               else res
       | Compose { comp_goal = goal;
                   comp_subgoals = subgoals;
                   comp_extras = extras
         } ->
            let flag, goal' = squash_ext goal in
            let flag, subgoals' = squash_subgoals_ext flag subgoals in
            let flag, extras' = squash_subgoals_ext flag extras in
            let leaves = leaves_ext node in
            let flag = flag || extras' <> [] in
            let node' =
               if flag then
                  Compose { comp_status = LazyStatusDelayed;
                            comp_goal = goal';
                            comp_subgoals = subgoals';
                            comp_extras = extras';
                            comp_leaves = LazyLeaves leaves
                  }
               else
                  let leaves = leaves_ext node in
                     Unjustified (goal_ext goal', leaves)
            in
               flag, node'
       | RuleBox { rule_string = text;
                   rule_expr = expr;
                   rule_tactic = tac;
                   rule_extract = goal;
                   rule_subgoals = subgoals;
                   rule_extras = extras;
                   rule_leaves = leaves
         } ->
            true, RuleBox { rule_status = LazyStatusDelayed;
                            rule_string = text;
                            rule_expr = expr;
                            rule_tactic = tac;
                            rule_extract_normalized = false;
                            rule_extract = snd (squash_ext goal);
                            rule_subgoals = List.map (fun goal -> snd (squash_ext goal)) subgoals;
                            rule_extras = List.map (fun goal -> snd (squash_ext goal)) extras;
                            rule_leaves = leaves
            }
       | Pending f ->
            squash_ext (f ())
       | Locked node ->
            let flag, node = squash_ext node in
               flag, Locked node

   and squash_subgoals_ext flag = function
      goal :: subgoals ->
         let flag', goal' = squash_ext goal in
         let flag'', subgoals' = squash_subgoals_ext (flag || flag') subgoals in
            flag'', goal' :: subgoals'
    | [] ->
         flag, []

   let squash postf proof =
      fold_proof postf proof (snd (squash_ext proof.pf_node))

   (************************************************************************
    * PROOF CHECKING                                                       *
    ************************************************************************)

   (*
    * Ask the refiner for the extract term.
    *)
   let term_of_proof refiner proof terms =
      term_of_extract refiner proof.pf_node terms

   (*
    * Re-expand all the rule boxes.
    *)
   let rec expand_ext dforms proof node =
      match node with
         Goal _
       | Identity _
       | Unjustified _
       | Extract _
       | ExtractRewrite _
       | ExtractCondRewrite _
       | ExtractNthHyp _
       | ExtractCut _ ->
            node
       | Compose { comp_goal = goal; comp_subgoals = subgoals; comp_extras = extras } ->
            let goal = expand_ext dforms proof goal in
            let subgoals = List.map (expand_ext dforms proof) subgoals in
            let extras = List.map (expand_ext dforms proof) extras in
            let subgoals, extras = match_subgoals (leaves_ext goal) subgoals extras in
               Compose { comp_status = LazyStatusDelayed;
                         comp_goal = goal;
                         comp_subgoals = subgoals;
                         comp_extras = extras;
                         comp_leaves = LazyLeavesDelayed
               }
       | Wrapped (label, goal) ->
            Wrapped (label, expand_ext dforms proof goal)
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
               try Filter_exn.print dforms (fun () -> snd (TacticInternal.refine (tac ()) t)) () with
                  RefineError _ ->
                     goal
            in
            let leaves = leaves_ext new_goal in
            let subgoals, extras = update_subgoals leaves subgoals extras in
            let subgoals = List.map (expand_ext dforms proof) subgoals in
            let extras = List.map (expand_ext dforms proof) extras in
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
            expand_ext dforms proof (f ())
       | Locked ext ->
            expand_ext dforms proof ext

   let expand postf dforms proof =
      fold_proof postf proof (expand_ext dforms proof.pf_node proof.pf_node)

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
        simp_parent : simple_tactic_arg option;
        simp_attributes : attribute_info
      }

   type io_proof =
      IOGoal of simple_tactic_arg
    | IOUnjustified of simple_tactic_arg * simple_tactic_arg list
    | IOExtractNthHyp of simple_tactic_arg * int
    | IOExtractCut of simple_tactic_arg * term * simple_tactic_arg * simple_tactic_arg
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
                     ref_parent = parent;
                     ref_attributes = attrs
                   } = arg
               in
               let parent =
                  match parent with
                     ParentNone ->
                        None
                   | ParentLazy parent ->
                        Some (make_tactic_arg parent)
                   | ParentSet (parent, _) ->
                        Some (make_tactic_arg parent)
               in
               let attr =
                  let attrs = squash_attributes attrs in
                  if squash then begin
                     if attrs <> empty_attribute then begin
                        let buf = Rformat.new_buffer () in
                        format_arg !debug_base buf arg;
                        eprintf "Warning: Proof_boot.io_proof_of_proof: an attribute list in unjustified node was non-empty before IO:\n%t%t"
                           (print_to_channel default_width buf) eflush
                     end;
                     empty_attribute
                 end else attrs
               in let arg' =
                  { simp_goal = goal;
                    simp_label = label;
                    simp_parent = parent;
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
          | ExtractRewrite (goal, subgoal, _, _) ->
               IOUnjustified (make_tactic_arg goal, [make_tactic_arg subgoal])
          | ExtractCondRewrite (goal, subgoals, _, _) ->
               IOUnjustified (make_tactic_arg goal, List.map make_tactic_arg subgoals)
          | ExtractNthHyp (goal, i) ->
               IOExtractNthHyp (make_tactic_arg goal, i)
          | ExtractCut (goal, term, cut_lemma, cut_then) ->
               IOExtractCut (make_tactic_arg goal,
                             term,
                             make_tactic_arg cut_lemma,
                             make_tactic_arg cut_then)
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
               eprintf "\__ IO convertion done.\n%t" eflush;
            res
      in
         update proof;
         convert (if squash then snd (squash_ext proof.pf_node) else proof.pf_node)

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
                     simp_parent = parent;
                     simp_attributes = args
                   } = arg
               in
               let args = update_attributes args raw_attributes in
               let parent =
                  match parent with
                     None ->
                        ParentNone
                   | Some parent ->
                        ParentLazy (make_tactic_arg parent)
               in
               let arg' =
                  { ref_goal = goal;
                    ref_label = label;
                    ref_parent = parent;
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
       | IOExtractNthHyp (goal, i) ->
            ExtractNthHyp (make_tactic_arg goal, i)
       | IOExtractCut (goal, term, cut_lemma, cut_then) ->
            ExtractCut (make_tactic_arg goal, term, make_tactic_arg cut_lemma, make_tactic_arg cut_then)
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
            let tactic = lazy_apply (fun text -> eval (parse text)) text in
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
      let node = convert node in
         { pf_root = node;
           pf_address = [];
           pf_node = node
         }

   (*
    * Some simple operations on IO proofs.
    *)
   let rec status_of_io_proof = function
      IOGoal _
    | IOUnjustified _
    | IOExtractNthHyp _
    | IOExtractCut _
    | IOIdentity _ ->
         StatusPartial
    | IOWrapped (_, node) ->
         status_of_io_proof node
    | IOCompose { io_comp_status = status } ->
         translate_status status
    | IORuleBox { io_rule_status = status } ->
         translate_status status

   (*
    * Count up the number of nodes.
    *)
   let rec node_count_of_io_proof_node rules nodes = function
      IOGoal _
    | IOUnjustified _
    | IOExtractNthHyp _
    | IOExtractCut _
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
         Convert.to_term parse eval (goal proof) (proof.pf_node)

      let of_term args sentinal bookmark parse eval t =
         let ext = Convert.of_term args sentinal bookmark parse eval t in
            { pf_root = ext;
              pf_address = [];
              pf_node = ext
            }

      let convert = Convert.convert
      let revert = Convert.revert

      let status_of_term = Convert.status_of_term
      let node_count_of_term = Convert.node_count_of_term
   end

   module ProofTerm_std = ProofTerm (Refiner.Refiner);;
   module ProofTerm_io = ProofTerm (Refiner_io);;

   (*
    * HACK!
    * We hack the recognition of io_proof type for now.
    * A term_io proof has two fields, and the tag is always zero.
    *)
   let term_io_proof_hack proof =
      let t = Obj.repr proof in
         if Obj.size t = 2 && Obj.tag t = 0 then
            begin
               eprintf "Proof_boot: converting term_io style proof%t" eflush;
               true
            end
         else
            false

   let io_proof_hack proof =
      let parse text =
         raise (Invalid_argument "Proof_boot.proof_hack.parse")
      in
      let eval expr =
         raise (Invalid_argument "Proof_boot.proof_hack.eval")
      in
         if term_io_proof_hack proof then
            let proof = ProofTerm_io.of_term [] Tactic.null_sentinal empty_bookmark parse eval ((Obj.magic proof) : term_io) in
               io_proof_of_proof true parse eval proof
         else
            proof

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
    | Extract _
    | ExtractRewrite _
    | ExtractCondRewrite _
    | ExtractNthHyp _
    | ExtractCut _ ->
         rcount, succ ncount
    | Wrapped (label, ext) ->
         node_count_ext (rcount, succ ncount) ext
    | Compose { comp_goal = goal; comp_subgoals = subgoals } ->
         node_count_subgoals_ext (node_count_ext (rcount, succ ncount) goal) subgoals
    | RuleBox { rule_extract = goal; rule_subgoals = subgoals } ->
         node_count_subgoals_ext (node_count_ext (succ rcount, succ ncount) goal) subgoals
    | Pending f ->
         node_count_ext (rcount, ncount) (f ())
    | Locked ext ->
         node_count_ext (rcount, ncount) ext

   and node_count_subgoals_ext counts = function
      subgoal :: subgoals ->
         node_count_subgoals_ext (node_count_ext counts subgoal) subgoals
    | [] ->
         counts

   let node_count { pf_node = node } =
      if !debug_proof then begin
         eprintf "Counting nodes of:\n";
         print_ext node
      end;
      node_count_ext (0, 0) node

   (*
    * Kreitz the tree into a single node.
    * This only work on the outermost rule boxes.
    *)
   let loc = 0, 0

   let rec kreitz_ext proof node =
      match node with
         Goal _
       | Identity _
       | Unjustified _
       | Extract _
       | ExtractRewrite _
       | ExtractCondRewrite _
       | ExtractNthHyp _
       | ExtractCut _
       | Wrapped _
       | Compose _ ->
            "idT", (<:expr< $lid: "idT"$ >>), TacticInternal.idT, [goal_ext node]
       | Pending f ->
            kreitz_ext proof (f ())
       | Locked ext ->
            kreitz_ext proof ext
       | RuleBox { rule_expr = expr;
                   rule_string = text;
                   rule_tactic = tac;
                   rule_subgoals = subgoals
         } ->
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
            in
            let subnodes = List.map (kreitz_ext proof) subgoals in
            let text = sprintf "%s thenLT [%s]" text (concat_text subnodes) in
            let expr = (<:expr< $lid: "prefix_thenLT"$ $expr ()$ $concat_ast subnodes$ >>) in
            let tac = prefix_thenLT (tac ()) (List.map (fun (_, _, tac, _) -> tac) subnodes) in
            let subgoals = concat_subgoals subnodes in
               text, expr, tac, subgoals

   let kreitz postf proof =
      let text, expr, tac, subgoals = kreitz_ext proof.pf_node proof.pf_node in
      let info =
         { rule_status = LazyStatusDelayed;
           rule_expr = (fun () -> expr);
           rule_string = text;
           rule_tactic = (fun () -> tac);
           rule_extract_normalized = true;
           rule_extract = Unjustified (goal_ext proof.pf_node, subgoals);
           rule_subgoals = List.map (fun t -> Goal t) subgoals;
           rule_leaves = LazyLeavesDelayed;
           rule_extras = []
         }
      in
         fold_proof postf proof (RuleBox info)

end

(*
 * -*-
 * Local Variables:
 * Caml-master: "editor.top"
 * End:
 * -*-
 *)
