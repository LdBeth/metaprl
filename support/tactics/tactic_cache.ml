(*
 * This module defines a "forward-chaining" cache that performs
 * forward -chaining, as well as collecting statistics about
 * hyp usage.  The module is expressed in terms of inherited
 * and synthesized attributes.
 *
 * Notes:
 *
 * "Worlds" are a technique that we use for incremental chaining.
 * Inferences are relative to a collection of assumptions ("hypotheses")
 * that together form a "world."  When assumptions are added
 * in the course of refinement, new worlds are created, and when
 * the user backs out of a proof, either because it failed, or
 * because it is complete, hypotheses are removed to form
 * more general worlds.
 *
 * The issues that have to be solved for incremental computation:
 * When new hypotheses are added, more things can be derived
 * by forward chaining.  Howver, any further inferences that are
 * derived in the old worlds should remain valid in those worlds,
 * so that if we back out, the inferences are preserved.
 *
 * Constraint: backout is common, and it chould be cheap.
 *    Also it should be easy to recover if an inference is
 *    backed out and later restored.  Call this "recovery".
 * Tradeoff: if backout is cheap, then lookup and insertion
 *    becomes expensive.
 *
 * The solution that we use is to associate refinement
 * and forward inference with worlds.  Each world contains the
 * inferences it has made by forward chaining, and when backout
 * removes a world, it also removes all the forward inferences.
 *
 * There is more discussion of the algorithm just before the
 * type definition for 'a extract.
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
open Lm_debug
open Lm_symbol
open Lm_printf
open Refiner.Refiner.Term
open Refiner.Refiner.TermType
open Refiner.Refiner.TermMan
open Refiner.Refiner.TermShape
open Refiner.Refiner.TermSubst
open Refiner.Refiner.Rewrite
open Refiner.Refiner.Refine

(*
 * Debug statement.
 *)
let _ =
   show_loading "Loading Tactic_cache%t"

(************************************************************************
 * TYPES                                                                *
 ************************************************************************)

(*
 * A forward-chaining rule.
 * The justification (which is probably going to be a tactic),
 * takes the indices of the hyps as arguments, takes
 * the names of the results, and produces an 'a (which is
 * probably a tactic).
 *)
type 'a frule =
   { fc_ants : term list;
     fc_concl : term list;
     fc_just : 'a
   }

(*
 * Similar back-chaining rule.
 *)
type 'a brule =
   { bc_concl : term;
     bc_ants : (term list * term) list;
     bc_just : 'a
   }

(*
 * A proof has a forward and a backward component.
 *)
type 'a proof =
   ForeTactics of (int list * 'a) list
 | BackTactic of ('a * 'a proof list)
 | NthHyp of int
 | SeqTactic of 'a proof list

(*
 * The fcache is a collection of compiled rules.
 * The plates are templates for the arguments.
 * If there is a wild entry, it contains a
 * list of indices of arguments that are wildcards
 * (variables), and the rewrite converts the args
 * to their wildcard entries.
 *)
type 'a fcache_info =
   { finfo_plates : shape list;
     finfo_wild : (int list * rewrite_rule) option;
     finfo_rw : rewrite_rule;
     finfo_just : 'a
   }

(*
 * A backward info entry has a template for the goal,
 * and a rewrite to produce the subgoals.
 *)
type 'a bcache_info =
   { binfo_plate : shape;
     binfo_rw : term -> (term list * term) list;
     binfo_just : 'a
   }

(*
 * The cache is a list of these entries.
 *)
type 'a cache =
   CacheNil
 | CacheForward of 'a fcache_info * 'a cache
 | CacheBackward of 'a bcache_info * 'a cache

(*
 * Inferences are used to list the results from forward chaining.
 * A world is a hypothesis list, and each world contains
 * a list of inferences that have been computed
 * using forward chaining rules.  An extract contains
 * a world, which then contains the results of forward chaining.
 *
 * An inference is a fact, either because it is a
 * hypothesis, or because it has been derived by
 * forward chaining.  The info contains the justification,
 * the inferences that were used as antecedents, and
 * the total list of results.  A particular inference
 * is a specific result.
 *)
type 'a inference_info =
   { inf_values : term list;
     inf_just : 'a;
     inf_args : 'a inference list;
     inf_world : 'a world
   }

and 'a hyp_info =
   { hyp_world : 'a world }

and 'a inf_info =
   Inference of 'a inference_info
 | Hyp of 'a hyp_info

and 'a inference =
   { inf_name : var;
     inf_value : term;
     inf_hash : shape;
     inf_info : 'a inf_info
   }

(*
 * Goals are used to list the results of backward chaining.
 * They represent one vertex in the or-and tree of backward
 * chaining (which is type 'a goalnode below).
 *
 *    goal_concl: this is the term to be proved
 *    goal_assums: these are extra hypotheses that can be
 *       assumed at this point the in the backward chaining
 *    goal_subgoals: the subgoals are set when backwward
 *       chaining has been applied successfully to this goal.
 *       The subgoals are either unexplored, or they are a list of
 *       possible subgoal combinations (meaning that they represent
 *       an or-branch).
 *)
and 'a subgoal =
   { sub_goals : 'a goal list;
     sub_just : 'a
   }

and 'a subgoals =
   Unexplored
 | Unsolvable
 | Subgoals of 'a subgoal list

and assumption =
   { assum_term : term;
     assum_hash : shape
   }

and 'a goal =
   { goal_concl : term;
     goal_hash : shape;
     goal_assums : assumption list;
     mutable goal_subgoals : 'a subgoals
   }

(*
 * The search DAG is used for constructing trees of goals
 * for backward chaining.  The nodes are matched with the
 * inferences produced by forward chaining.
 *
 * The search DAG is a bi-directional DAG of goals, with
 * a flag indicating if the goal is satisfied.  The goal
 * can either be satisfied by forward chaining, or it
 * can be derived from subgoals.
 *
 * node_goal: the goal to be proved
 * node_world: the world (assumptions) to prove it in
 * node_children: or-and list of nodes to justify this node
 *    each entry in the list corresponds to
 *    an entry in node_goal.goal_subgoals.
 *
 * node_status: whether this node has been proved:
 *    Unproved: hasn't been explored
 *    Unprovable: can't explore this goal
 *    Derivable: can be derived from proof of subgoals
 *    Primitive: provable by forward chaining
 *    Derived: proof by backward chaining
 *)
and 'a node_status =
   Unproved
 | Unprovable
 | Derivable
 | Derived
 | Primitive of 'a inference

and 'a goal_node =
   { node_goal : 'a goal;
     node_world : 'a world;
     mutable node_children : 'a goal_node list list;
     mutable node_status : 'a node_status
   }

(*
 * A "world" summarizes the results of forward chaining.
 * Each world represents a hypothesis list, and each hyp
 * lists the results that have been obtained through forward chaining.
 *)
and 'a world_info =
   { world_hyp : 'a inference;
     world_prev : 'a world;
     mutable world_infs : 'a inference list
   }

and 'a world =
   Hypothesis of 'a world_info
 | Empty

(*
 * Forward chaining table.
 * The flookup_table maps terms into possible arguments to rules.
 *
 * When a turn is given to forward chaining, a hypothesis
 * is selected.  We look for all the possible forward chaining
 * rules in the table, and try them all.
 *)
type 'a flookup_table = (shape, (int * 'a fcache_info) list) Hashtbl.t

(*
 * Backward chaining table.
 * A backward table maps goals into a list of all the possible
 * backward chaining rules that can be applied.
 *)
type 'a blookup_table = (shape, 'a bcache_info list) Hashtbl.t

(*
 * This table is used during forward chaining.
 * Whenever an inference is made, that inference is
 * checked for possible matches with forward rules.
 * This table pairs up the forward rules with possible
 * arguments, and there are a combinatorial number
 * of instances of the forward rule.  The argument
 * shape list is the shape list for the forward rule.
 *
 * This info table lists possible instantiations of rules.
 * Each rule contains a list of possible instantiations,
 * where the instantation is expressed as a vector of
 * possible arguments, where the arguments are a list
 * of inferences that are derived from the relevant hyps.
 * The argument lists are updated destructively.
 *)
type 'a info_table =
   (shape list, ('a fcache_info * 'a inference list array) list) Hashtbl.t

(*
 * Table of terms.  Each term can be satisfied by an inference,
 * and it may also be listed as a goal.
 *)
type 'a term_entry =
   { entry_infs : 'a inference list;
     entry_goals : 'a goal list
   }

type 'a term_table = (shape, 'a term_entry) Hashtbl.t

(*
 * Backward chaining tree table.
 * This maps goals to nodes in the DAG.
 * This will coded to have the type:
 *    'a goal -> 'a goal_node
 *)
type 'a bchain_table = (shape, 'a goal_node list) Hashtbl.t

(*
 * Summary of data structures:
 *    We keep a summary of the sequent.
 *    ext_hyps contains the list of hyps in the sequent,
 *    and ext_goal contains the conclusion.  We don't want to
 *    be doing substitution on the hyps all the time, so the actual
 *    free variables in the hyps are saved in ext_names.  The names that
 *    are used externally are saved in ext_gnames.  The ext_used list
 *    is a list of the hyps that have been referenced.  This list is
 *    used during synthesis to garbage collect unused hyps.  The
 *    world representation of the hyps is saved in ext_world.
 *
 *    The goal is saved in ext_goal, and the backchaining tree is
 *    save in ext_node.  During backchaining, we need to map subgoals
 *    to nodes that we have explored ('a goal -> 'a goal_node), and this
 *    is saved in ext_goals.
 *
 *    The ext_ftable is used to provide a function from hypotheses
 *    to possible forward rules that can be applied.  From ext_ftable
 *    we produce a function (term -> (int * 'a fcache_info) list), where the
 *    number that is returned is the hypothesis from the rule that
 *    may apply.
 *
 *    The ext_btable is used to provide a function from conclusions
 *    to possible backward rules that might be applied.  We produce
 *    a function (term -> 'a bcache_info list), of rules that may apply.
 *
 *    When a term is produced by chaining, we need to see if it has
 *    ever occurred during a chaining operation before.  The ext_terms
 *    gives us a function to find previous occurrences.
 *       For instance, suppose we produce a result during forward chaining,
 *       If this term was ever produced by forward chaining previously,
 *          there is no need to continue.
 *       If the term was ever produced by backward chaining,
 *          we can go to the backward instance and satisfy it.
 *
 *       Otherwise, suppose a goal is produced during backward chaining.
 *       If the term was ever produced during forward chaining,
 *          we can satisfy the goal.
 *       If the term was ever produced during backward chaining,
 *          we stop, because the search is already in progress.
 *
 *    The ext_fqueue is used to list the new inferences that have been
 *    produced by forward chaining.  These are available for further
 *    chaining.  The ext_index counts the total number of inferences,
 *    and we use the value to compute new variable names.
 *
 *    The ext_worlds is the list of worlds we have _ever_ seen.
 *    This is used during backout to recover states that we may
 *    have forgotten.
 *
 *    The ext_rules is used for finding possible matches for
 *    forward chaining.  When an inference is made, we find
 *    all the forward rules that might be able to use it,
 *    and we add the inference to ext_rules as a possible
 *    match to be explored.
 *
 * Algorithm:
 *    The hyps and the goal can be modified with add_hyp, del_hyp,
 *    and set_goal.  When a hyp is added, we look to see if we have
 *    ever inferred it.  If we have, then we just add it as the new
 *    world.  Otherwise, we construct a new world, add it hyps,
 *    and add a new inference to ext_fqueue.
 *
 *    When a hyp is deleted, we remove the hyp, but we allow
 *    pending forward inferences to continue on the assumption
 *    that they will be useful on other branches of the proof.
 *    In the conclusion, we prune the searh tree that uses the
 *    hyp or any that follow it.
 *
 *    When the goal is changed we have to reset the search tree.
 *
 *    Backchaining search produces an or-and tree, and the
 *    ext_bchain function explores this tree.  It is a value,
 *    and it is changed each time backchaining produces a new
 *    goal.  The new function searches the current goal, then
 *    tries the old ext_bchain.  This produces a simple-minded
 *    depth-first search in the backchaining tree.  TODO: Would like
 *    to make this paramterizable at some point.
 *
 *    If a chaining action is requested, we alternate between
 *    backchaining and forechaining.
 *       During forechaining, we pick an inference off the
 *       ext_fqueue, and we find all possible rules from
 *       ext_ftable, and we apply all of them in turn.
 *          For each rule, this means that we must find an
 *          instance that applies, which means that we have
 *          to find all hyps that apply.  We do an exhaustive
 *          search through all the inferences we have made in
 *          the current world.  If we find a potential instance,
 *          we apply the forward rule.  If it fails, drop it.
 *          If it succeeds, it produces new results.  We assign
 *          a new inference to each one of them.  We check if
 *          they have been seen before, and if not we add them
 *          to ext_fqueue.
 *
 *      During backchaining, we find all back rules that
 *      potentially apply to the current goal, and we try
 *      them in turn.
 (          For each rule, if it applies, it produces
 *          a list of subgoals that we have to prove.
 *          For each subgoal, we check if it has been
 *          found by forward chaining.  If so, we mark
 *          it as proved.  If it has occurred, before
 *          during backchaining, we link it to the
 *          previous occurrence.  If it has never been
 *          seen before, we construct a new back search
 *          that explores this case.
 *
 * Variable summary:
 *    Static lookup tables:
 *       ext_ftable: A hashtable of the rules for forward chaining.
 *          The hashtable maps terms to antecedents of rules,
 *          so that when a new fact arrives, it is easy to
 *          tell how the new fact can be used.
 *       ext_btable: A hashtable of the rules for backward chaining
 *
 *    Tables to optimize inferencing:
 *       ext_terms: a table mapping terms to all inferences
 *           and goals having the same term.
 *       ext_rules: a table containg possible instations of
 *           the forward chaining rules.
 *
 *    Hyp info.  These lists all have the length ext_hcount.
 *       ext_hcount: number of hyps
 *       ext_names: the internal names of the hyps
 *       ext_gnames: the external names of the hyps
 *       ext_hyps: the actual hyp list, with free vars from ext_names
 *
 *    For hyp records:
 *       ext_used: a list of hyps that have been referenced.
 *
 *    For forward chaining:
 *       ext_index: number of inferences ever made
 *       ext_fqueue: a list of inferences available for forward chaining
 *
 *    For backward chaining:
 *       ext_goal: the goal in the current world
 *       ext_node: the node corresponding to this goal
 *       ext_bchain: the function that incrementally performs back chaining.
 *       ext_goals: the table mapping goals to nodes in the goal tree.
 *
 *    All the worlds ever seen are also recorded.
 *       ext_world: the current world
 *       ext_worlds: all worlds investigated so far.
 *
 *    All info particular to the current world is stored
 *    in the extract directly.  All info that is common
 *    to all worlds is stored in ext_base.
 *)
type 'a chain =
   {  (* Rule tables *)
      ext_ftable : 'a flookup_table;
      ext_btable : 'a blookup_table;
      ext_terms : 'a term_table;
      ext_rules : 'a info_table;

      (* Forward chaining *)
      mutable ext_index : int;
      mutable ext_fqueue : 'a inference list;

      (* Possible worlds *)
      mutable ext_worlds : 'a world list
   }

type 'a extract =
   {  (* Assumptions *)
      ext_used : 'a world list;
      ext_hcount : int;
      ext_names : var list;
      ext_gnames : var list;
      ext_hyps : 'a world list;

      (* Backward chaining *)
      ext_goal : 'a goal option;
      ext_node : 'a goal_node option;
      ext_goals : 'a bchain_table;
      ext_bchain : 'a extract -> bool;

      (* Current world *)
      ext_world : 'a world;

      (* Chaining base *)
      ext_base : 'a chain
   }

(*
 * A synthesis just contains the handles of the used hyps.
 *)
type 'a synthesis =
   { syn_extract : 'a extract;
     syn_used : 'a world list
   }

(*
 * This type is used to summarize the result of a world search.
 * See find_world_extension.
 *)
type 'a find =
   FindNone
 | FindPrev of 'a world_info
 | FindNext of 'a world

(*
 * A stack is used to backward chain using an or-and tree.
 *)
type 'a goal_item =
   AndBranch
 | OrBranch
 | Node of 'a goal_node
 | RootNode of 'a goal_node

(************************************************************************
 * BASIC UTILITIES                                                      *
 ************************************************************************)

(*
 * Construct a new name from the base.
 *)
let mk_var_name { ext_base = { ext_index = index } as base } =
   base.ext_index <- index + 1;
   Lm_symbol.add ("`" ^ (string_of_int index))

(*
 * We use this term to stand for contexts.
 *)
declare context

let context_term = << context >>

(*
 * Functional record update.
 *)
let set_used
    { ext_used = used;
      ext_hcount = hcount;
      ext_names = names;
      ext_gnames = gnames;
      ext_hyps = hyps;
      ext_goal = goal;
      ext_node = node;
      ext_goals = goals;
      ext_bchain = bchain;
      ext_world = world;
      ext_base = base
    } used' =
   { ext_used = used';
     ext_hcount = hcount;
     ext_names = names;
     ext_gnames = gnames;
     ext_hyps = hyps;
     ext_goal = goal;
     ext_node = node;
     ext_goals = goals;
     ext_bchain = bchain;
     ext_world = world;
     ext_base = base
   }

let set_gnames
    { ext_used = used;
      ext_hcount = hcount;
      ext_names = names;
      ext_gnames = gnames;
      ext_hyps = hyps;
      ext_goal = goal;
      ext_node = node;
      ext_goals = goals;
      ext_bchain = bchain;
      ext_world = world;
      ext_base = base
    } gnames' =
   { ext_used = used;
     ext_hcount = hcount;
     ext_names = names;
     ext_gnames = gnames';
     ext_hyps = hyps;
     ext_goal = goal;
     ext_node = node;
     ext_goals = goals;
     ext_bchain = bchain;
     ext_world = world;
     ext_base = base
   }

(*
 * Get the world for any kind of inference.
 *)
let world_of_inf { inf_info = info } =
   match info with
      Inference { inf_world = world }
    | Hyp { hyp_world = world } ->
         world

(*
 * World naming.
 *)
let name_of_world = function
   Hypothesis { world_hyp = { inf_name = name } } ->
      name
 | Empty ->
      raise (Invalid_argument "name_of_world")

(*
 * Add an inference to a world.
 *)
let push_world_inf world inf =
   match world with
      Hypothesis w ->
         w.world_infs <- inf :: w.world_infs
    | Empty ->
         raise (Invalid_argument "push_world_inf")

(*
 * Push a new inference onto the forward chaining queue
 * so that we will check it for further inferencing.
 *)
let push_extract_inf { ext_base = base } inf =
   base.ext_fqueue <- inf :: base.ext_fqueue

(************************************************************************
 * HASHTABLE IMPLEMENTATIONS                                            *
 ************************************************************************)

(*
 * Equality of goals.
 * Don't compare subgoals.
 *)
let eq_goal
    { goal_concl = t;
      goal_hash = hash;
      goal_assums = assums
    }
    { goal_concl = t';
      goal_hash = hash';
      goal_assums = assums'
    } =
   let rec aux = function
      ({ assum_term = t; assum_hash = hash }::tl),
      ({ assum_term = t'; assum_hash = hash' }::tl') ->
         if hash = hash' & alpha_equal t t' then
            aux (tl, tl')
         else
            false
    | [], [] ->
         true
    | _ ->
         false
   in
      hash = hash' & alpha_equal t t' & aux (assums, assums')

(*
 * Determine if a world is a prefix of another.
 * Useful for looking up inferences.
 *)
let is_child_of_parent child parent =
   let rec aux world =
      if world == parent then
         true
      else
         match world with
            Hypothesis { world_prev = prev } ->
               aux prev
          | Empty ->
               false
   in
      aux child

(*
 * Determine if a world is an extension by the given assumptions.
 *)
let is_child_with_assumptions child assums parent =
   let rec aux world = function
      { assum_term = t; assum_hash = hash } :: tl ->
         begin
            match world with
               Hypothesis { world_hyp = { inf_value = t'; inf_hash = hash' };
                            world_prev = prev
               } ->
                  if hash = hash' & alpha_equal t t' then
                     aux prev tl
                  else
                     false
             | Empty ->
                  false
         end
    | [] ->
         world == parent
   in
      aux child assums

(*
 * Provide a 'a goal -> 'a goal_node interface to a 'a bchain_table.
 * Not idempotent.
 *)
let bset_tbl_node tbl node =
   let { node_goal = { goal_hash = hash } } = node in
   let nodes =
      try Hashtbl.find tbl hash with
         Not_found ->
            []
   in
      Hashtbl.remove tbl hash;
      Hashtbl.add tbl hash (node :: nodes)

let bset_node { ext_goals = tbl } node =
   bset_tbl_node tbl node

let bget_nodes { ext_goals = tbl } ({ goal_hash = hash } as goal) =
   let rec collect = function
      ({ node_goal = goal' } as node)::tl ->
         if goal' == goal then
            node :: collect tl
         else
            collect tl
    | [] ->
         []
   in
   let l =
      try Hashtbl.find tbl hash with
         Not_found ->
            []
   in
      collect l

(*
 * This gets just one node with the given world,
 * extended by the assumptions in the goal.
 *)
let bget_node extract world ({ goal_assums = assums } as goal) =
   let nodes = bget_nodes extract goal in
   let test { node_world = world' } =
      is_child_with_assumptions world' assums world
   in
      Lm_list_util.find test nodes

(*
 * Provide an interface to flookup_table of type
 *    term -> (int * 'a fcache_info * 'a inference list array) list
 *
 * Not idempotent.
 *
 * When an inference is added to the possible instances,
 * using fset_insts, a summary of the new combinations
 * is returned, and the insance list is updated.
 *)
let fset_entry (ftable : 'a flookup_table) hash (entry : int * 'a fcache_info) =
   let entries =
      try Hashtbl.find ftable hash with
         Not_found ->
            []
   in
      Hashtbl.remove ftable hash;
      Hashtbl.add ftable hash (entry :: entries)

let fget_entry { ext_base = { ext_ftable = ftable; ext_rules = rules } } { inf_hash = hash } =
   let aux (i, ({ finfo_plates = plates } as rl)) =
      let test (rule', _) =
         rule' == rl
      in
      let rules = Hashtbl.find rules plates in
      let _, insts = Lm_list_util.find test rules in
         i, rl, insts
   in
   let l = Hashtbl.find ftable hash in
      List.map aux l

let fset_insts inf (i, _, insts) =
   let len = Array.length insts in
   let rec explore j =
      if j = len then
         []
      else if j = i then
         [inf] :: explore (j + 1)
      else
         insts.(j) :: explore (j + 1)
   in
      insts.(i) <- inf :: insts.(i);
      explore 0

let init_insts (rules : 'a info_table) ({ finfo_plates = plates } as rl) =
   let len = List.length plates in
   let entry = Array.create len [] in
   let entries =
      try Hashtbl.find rules plates with
         Not_found ->
            []
   in
      Hashtbl.remove rules plates;
      Hashtbl.add rules plates ((rl, entry) :: entries)

(*
 * Provide a rough
 * 'a goal -> 'a bcache_info list interface for blookup_table.
 * Not idempotent.
 *)
let bset_entry (tbl : 'a blookup_table) ({ binfo_plate = hash } as entry) =
   let entries =
      try Hashtbl.find tbl hash with
         Not_found ->
            []
   in
      Hashtbl.remove tbl hash;
      Hashtbl.add tbl hash (entry::entries)

let bget_entries { ext_base = { ext_btable = tbl } } { goal_hash = hash } =
   Hashtbl.find tbl hash

(*
 * Provide an interface to the terms table.
 * Not idempotent.
 *)
let set_inf { ext_base = { ext_terms = terms } } ({ inf_hash = hash } as inf) =
   let { entry_infs = infs; entry_goals = goals } =
      try Hashtbl.find terms hash with
         Not_found ->
            { entry_infs = []; entry_goals = [] }
   in
      Hashtbl.remove terms hash;
      Hashtbl.add terms hash { entry_infs = inf :: infs; entry_goals = goals }

let set_goal { ext_base = { ext_terms = terms } } ({ goal_hash = hash } as goal) =
   let { entry_infs = infs; entry_goals = goals } =
      try Hashtbl.find terms hash with
         Not_found ->
            { entry_infs = []; entry_goals = [] }
   in
      Hashtbl.remove terms hash;
      Hashtbl.add terms hash { entry_infs = infs; entry_goals = goal::goals }

(*
 * Inferences are found by their term and their world must include
 * the previous inference's world.
 *)
let find_inf { ext_base = { ext_terms = terms } } world t hash =
   let test ({ inf_value = t' } as inf) =
      let world' = world_of_inf inf in
         alpha_equal t t' & is_child_of_parent world world'
   in
   let { entry_infs = infs } = Hashtbl.find terms hash in
      Lm_list_util.find test infs

let inf_is_already_known extract world t hash =
   try
      let _ = find_inf extract world t hash in
         true
   with
      Not_found ->
         false

let find_goal { ext_base = { ext_terms = terms } } ({ goal_hash = hash } as goal) =
   let { entry_goals = goals } = Hashtbl.find terms hash in
      Lm_list_util.find (eq_goal goal) goals

(*
 * Find the goalnodes that match the inference.
 *)
let find_nodes
    ({ ext_base = { ext_terms = terms }; ext_goals = btable } as extract)
    ({ inf_value = t; inf_hash = hash } as inf) =
   let world = world_of_inf inf in
   let rec filter_goals = function
      ({ goal_concl = t' } as goal)::tl ->
         if alpha_equal t t' then
            goal :: filter_goals tl
         else
            filter_goals tl
    | [] ->
         []
   in
   let rec filter_nodes = function
      ({ node_world = world' } as node)::tl ->
         if is_child_of_parent world world' then
            node :: filter_nodes tl
         else
            filter_nodes tl
    | [] ->
         []
   in
   let { entry_goals = goals } = Hashtbl.find terms hash in
   let goals' = filter_goals goals in
   let nodes = Lm_list_util.flat_map (bget_nodes extract) goals' in
      filter_nodes nodes

(*
 * Find an inference that proves the current goal.
 * The Hashtbl.find operation will *always* work because the
 * goal itself is in the table.
 *)
let find_inf_for_node
    { ext_base = { ext_terms = terms } }
    { node_goal = { goal_concl = t; goal_hash = hash }; node_world = world } =
   let rec aux = function
      ({ inf_value = t' } as inf)::tl ->
         let world' = world_of_inf inf in
            if alpha_equal t t' & is_child_of_parent world world' then
               Some inf
            else
               aux tl
    | [] ->
         None
   in
   let { entry_infs = infs } = Hashtbl.find terms hash in
      aux infs

let is_provable extract goal =
   match find_inf_for_node extract goal with
      Some _ ->
         true
    | None ->
         false

(*
 * See if a goal is already known.
 *)
let hash_goal { ext_base = { ext_terms = terms } }
    ({ goal_concl = t; goal_hash = hash } as goal) =
   let { entry_goals = goals } = Hashtbl.find terms hash in
      Lm_list_util.find (eq_goal goal) goals

(************************************************************************
 * UTILITIES                                                            *
 ************************************************************************)

(*
 * Find the wild terms, and mix them in with
 * the normal terms.  The nargs is a list of
 * indices of the wildcard entries.
 *)
let mix_wild nargs terms wilds =
   let rec aux terms wild i = function
      j::t ->
         begin
            if i = j then
               match wilds with
                  whd::wtl ->
                     whd::(aux terms wtl (i + 1) t)
                | [] ->
                     raise (Invalid_argument "mix_wild")
            else
               match terms with
                  thd::ttl ->
                     thd::(aux ttl wilds (i + 1) t)
                | [] ->
                     raise (Invalid_argument "mix_wild")
         end
    | [] ->
         terms
   in
      aux terms wilds 0 nargs

(*
 * Find a world that can be used as an extension of the
 * current one.  The world can be used if the worlds it
 * depends on are all ancestors of this world.  We return
 * three cases:
 *    FindNext world: an exact match is found
 *    FindPrev world: this world depends on a world
 *       that is an ancestor of the current world
 *    FindNone: no appropriate world was found
 *)
let find_world_extension { ext_base = { ext_worlds = worlds } } world t hash =
   let rec search newest = function
      ((Hypothesis world') as next)::tl ->
         let { world_hyp = { inf_value = t'; inf_hash = hash' };
               world_prev = prev
             } = world'
         in
            if hash = hash' & alpha_equal t t' then
               if prev == world then
                  FindNext next
               else if is_child_of_parent world prev then
                  match newest with
                     None ->
                        search (Some world') tl
                   | Some world'' ->
                        if is_child_of_parent prev world''.world_prev then
                           search (Some world') tl
                        else
                           search newest tl
               else
                  search newest  tl
            else
               search newest tl
    | _::tl ->
         search newest tl
    | [] ->
         match newest with
            Some world ->
               FindPrev world
          | None ->
               FindNone
   in
      search None worlds

(*
 * In this case, find_world_extension returned FindNone.
 * If we can find an inference that characterizes the world,
 * we create the world from the inference.  Otherwise,
 * we create a new world, with a new inference pointing
 * to itself.
 *)
let create_world_with_hyp extract world t hash =
   try
      let inf = find_inf extract world t hash in
         [], Hypothesis ({ world_hyp = inf;
                           world_prev = world;
                           world_infs = []
                         })
   with
      Not_found ->
         let name = mk_var_name extract in
         let rec inf =
            { inf_name = name;
              inf_value = t;
              inf_hash = hash;
              inf_info = Hyp { hyp_world = world' }
            }
         and world' =
            Hypothesis { world_hyp = inf;
                         world_prev = world;
                         world_infs = [inf]
            }
         in
            set_inf extract inf;
            [inf], world'

(*
 * In this case, find_world_extension returned FindAncestor,
 * and we just copy the inferences into a new world.
 *)
let copy_world world world' =
   let { world_hyp = inf; world_infs = infs } = world' in
      Hypothesis { world_hyp = inf;
                   world_prev = world;
                   world_infs = infs
      }

(*
 * Find or build a world that is an extension of the current one.
 * Add newly generated inferences to the forward chaining queue.
 *)
let build_world_with_hyp ({ ext_base = base } as extract) world t hash =
   match find_world_extension extract world t hash with
      FindNext world' ->
         world'
    | FindPrev world' ->
         let world' = copy_world world world' in
            base.ext_worlds <- world' :: base.ext_worlds;
            world'
    | FindNone ->
         let infs, world' = create_world_with_hyp extract world t hash in
            base.ext_worlds <- world' :: base.ext_worlds;
            base.ext_fqueue <- infs @ base.ext_fqueue;
            world'

(*
 * Construct the world from the old world and a list of new assumptions.
 *)
let build_world_with_assums extract world assums =
   let rec build world = function
      { assum_term = t; assum_hash = hash } :: tl ->
         build (build_world_with_hyp extract world t hash) tl
    | [] ->
         world
   in
      build world assums

(************************************************************************
 * BACKWARD CHAINING                                                    *
 ************************************************************************)

(*
 * Build a subgoal list from a collection of antecendents.
 *)
let construct_subgoals extract just ants =
   let make_assum t =
      { assum_term = t;
        assum_hash = shape_of_term t
      }
   in
   let aux (args, t) =
      let goal =
         { goal_concl = t;
           goal_hash = shape_of_term t;
           goal_assums = List.map make_assum args;
           goal_subgoals = Unexplored
         }
      in
         try hash_goal extract goal with
            Not_found -> goal
   in
      { sub_goals = List.map aux ants;
        sub_just = just
      }

(*
 * Set the subgoals of a goal given a list of bcache_info
 * that may match this goal.
 *)
let set_subgoals extract ({ goal_concl = t } as goal) binfo =
   let rec aux = function
      { binfo_rw = rw; binfo_just = just }::tl ->
         begin
            try
               let ants = rw t in
               let _, subgoals' = aux tl in
                  true, (construct_subgoals extract just ants) :: subgoals'
            with
               _ ->
                  aux tl
         end
    | [] ->
         false, []
   in
   let flag, subgoals = aux binfo in
      if flag then
         goal.goal_subgoals <- Subgoals subgoals
      else
         goal.goal_subgoals <- Unsolvable;
      flag

(*
 * Expand a goal to find its subgoals.
 * It is assumed that the goal is not directly provable
 * from an inference.
 *)
let expand_subgoals extract ({ goal_subgoals = subgoals } as goal) =
   match subgoals with
      Subgoals _ ->
         true
    | Unsolvable ->
         false
    | Unexplored ->
         set_subgoals extract goal (bget_entries extract goal)

(*
 * Construct a node from a subgoal.
 *)
let construct_node extract world ({ goal_assums = assums } as goal) =
   try bget_node extract world goal with
      Not_found ->
         (* Construct a new node *)
         let world' = build_world_with_assums extract world assums in
         let node =
            { node_goal = goal;
              node_world = world';
              node_children = [];
              node_status = Unproved
            }
         in
            bset_node extract node;
            node

(*
 * Set the children of the node from a subgoal list.
 *)
let find_node_children extract { node_world = world } subgoals =
   let construct' = construct_node extract world in
   let aux { sub_goals = goals } =
      List.map construct' goals
   in
      List.map aux subgoals

(*
 * Once the goal has been expanded, expand the node in the tree.
 * In the process, we hash the outgoing subgoals to squash
 * the DAG.
 *)
let set_node_children extract ({ node_goal = { goal_subgoals = subgoals } as goal } as node) =
   match subgoals with
      Unsolvable ->
         node.node_status <- Unprovable
    | Subgoals subgoals' ->
         node.node_status <- Derivable;
         node.node_children <- find_node_children extract node subgoals'
    | Unexplored ->
         raise (Invalid_argument "set_node_children")

(*
 * Expand a backward chain on a specific node in the tree.
 * Assume that node_status is Unproved.
 *)
let expand_node extract ({ node_goal = goal } as node) =
   match find_inf_for_node extract node with
      Some inf ->
         node.node_status <- Primitive inf
    | None ->
         begin
            if expand_subgoals extract goal then
               set_node_children extract node
            else
               node.node_status <- Unprovable
         end

(*
 * Stack operations for searching.
 * Here is and example stack:
 *     ...
 *     Node node1a
 *     Node node2a
 *     AndBranch
 *     Node node1b
 *     Node node2b
 *     Node node3b
 *     AndBranch
 *     Node node1c
 *     OrBranch
 *     Node node1d
 *     ....
 *     Node node1e
 *     RootNode rootnode
 *
 * The nodes directly above and AndBranch must all be true.
 * One of the AndBranches above an OrBranch must be true.
 * The OrBranch belongs to the node directly below it.
 * The Node above the RootNode must be true.
 *
 * Algorithm:
 *     1. If the top is a node:
 *        If it is unprovable, pop to AndBranch, and start the next branch.
 *        If it is proved, pop it.
 *        Otherwise, expand it and place the subgoals on the stack.
 *     2. If the top is an AndBranch:
 *        All the subgoals succeeded, so pop to this node (which is
 *        right below the OrBranch), and note that it is proved.
 *     3. If the top is an OrBranch:
 *        All the subgoals failed, so fail the node right below.
 *     4. If the top is a RootNode, then see if it succeeded.
 *        If so, we are done.
 *        If not, try again, and hope something was proved by
 *        forward chaining.
 *)

(*
 * Push the subgoals of an expanded node.
 *)
let push_derivable_node subgoals fstack =
   let rec aux = function
      subgoals::tl ->
         (List.map (function node -> Node node) subgoals) @ (AndBranch :: aux tl)
    | [] ->
         OrBranch :: !fstack
   in
      fstack := aux subgoals

(* Pop last and-branch *)
let pop_failed_branch fstack =
   let rec aux = function
      AndBranch::t -> t
    | _::t -> aux t
    | [] -> failwith "pop_failed_branch: empty stack"
   in
      fstack := aux !fstack

(*
 * This node succeeded because one of its
 * and-branches succeeded.
 *)
let pop_succeeded_node fstack =
   let rec aux = function
      OrBranch::(Node node)::t ->
         begin
            match node.node_status with
               Derivable ->
                  node.node_status <- Derived;
                  t
             | Derived | Primitive _ ->
                  t
             | Unproved | Unprovable ->
                  failwith "pop_succeeded_node: unprovable"
         end
    | _::t ->
         aux t
    | [] ->
         failwith "pop_succeeded_node: empty stack"
   in
      fstack := aux !fstack

(*
 * This creates a new function to perform backward chaining from a single
 * goal.  The backward chainer checks if the goal is provable.  If so, it
 * modifies the tree node to reflect a primitive proof.  If not, it
 * expands the goal into subgoals.
 *
 * Assume the new ext_goals table is empty.
 *
 * The search function keeps a stack of search functions,
 * and calls them until the stack is empty.
 *)
let new_bchain node =
   (* Search engine *)
   let fstack = ref [RootNode node] in
   let search_node extract ({ node_status = status; node_children = subgoals } as node) =
      match status with
         Unproved ->
            (* Expand subgoals, and restart *)
            expand_node extract node

       | Unprovable ->
            (* Failed node *)
            pop_failed_branch fstack

       | Derivable ->
            (* Push the subgoals *)
            push_derivable_node subgoals fstack

       | Derived | Primitive _ ->
            (* This node is provable *)
            let _ = Lm_ref_util.pop fstack in
               ()

   in
   let search extract =
      (* Process the top goal *)
      match List.hd !fstack with
         Node node ->
            (* Expand this node *)
            search_node extract node;
            false

       | AndBranch ->
            (* Success of all items in this branch *)
            pop_succeeded_node fstack;
            false

       | OrBranch ->
            (* All subgoals failed, pop the node, and fail its branch *)
            let _ = Lm_ref_util.pop fstack in
            let _ = pop_failed_branch fstack in
               false

       | RootNode node ->
            (* Is this node successful *)
            begin
               match node.node_status with
                  Unproved
                | Derivable ->
                     (* Try backward chaining *)
                     Lm_ref_util.push (Node node) fstack;
                     false
                | Unprovable ->
                     (* Maybe this will be proved later by forward chaining *)
                     false
                | Derived
                | Primitive _ ->
                     true
            end
   in
      search

(*
 * Construct a new table and bchain.
 *)
let new_goals world goal =
   let tbl = Hashtbl.create 97 in
      match goal with
         Some goal' ->
            let node =
               { node_goal = goal';
                 node_world = world;
                 node_children = [];
                 node_status = Unproved
               }
            in
               bset_tbl_node tbl node;
               Some node, tbl, new_bchain node

       | None ->
            None, tbl, (function _ -> false)

(************************************************************************
 * FORWARD CHAINING                                                     *
 ************************************************************************)

(*
 * Update any of the outstanding goals in the goal tree.
 *)
let update_goalnodes extract inf =
   let status = Primitive inf in
   let aux node =
      node.node_status <- status
   in
   let nodes = find_nodes extract inf in
      List.iter aux nodes

(*
 * Build the results from the values produced
 * during rewriting.
 *)
let build_results ({ ext_base = base } as extract) world root values =
   let rec aux = function
      t::tl ->
         let hash = shape_of_term t in
            if inf_is_already_known extract world t hash then
               aux tl
            else
               let inf =
                  { inf_name = mk_var_name extract;
                    inf_value = t;
                    inf_hash = hash;
                    inf_info = root
                  }
               in
                  push_world_inf world inf;
                  push_extract_inf extract inf;
                  update_goalnodes extract inf;
                  aux tl
    | [] ->
         ()
   in
      aux values

(*
 * Try all combinations of the args.
 *)
let instantiate f world insts =
   let rec aux world l = function
      h::t ->
         let aux' h' =
            let world' = world_of_inf h' in
               if is_child_of_parent world world' then
                  aux world (h'::l) t
               else if is_child_of_parent world' world then
                  aux world' (h'::l) t
               else
                  ()
         in
            List.iter aux' h
    | [] ->
         f world l
   in
      aux world [] insts

(*
 * Try to chain through a particular arglist.
 *)
let try_arglist_normal ({ ext_base = base } as extract) rw just world = function
   [] ->
      ()
 | (arg::args) as all_args ->
      let args' = List.map (function inf -> inf.inf_value) args in
         try
            let values = apply_rewrite rw empty_args arg.inf_value args' in
            let root =
               Inference { inf_values = values;
                           inf_just = just;
                           inf_args = all_args;
                           inf_world = world
               }
            in
               build_results extract world root values
         with
            _ ->
               ()

(*
 * Try to chain through the new item.
 *)
let try_fchain_normal extract world { finfo_rw = rw; finfo_just = just } finst =
   let try_arglist_normal' = try_arglist_normal extract rw just in
      if not (List.mem [] finst) then
         instantiate try_arglist_normal' world (List.rev finst)

(*
 * Try a particular arglist in the wildcard mode.
 * nargs: the indices of the wild args
 * wrw: the rewrite to produce the wildcard hyps
 * rw: the rewrite to produce the results
 * just: the justification
 * args: the facts to be used
 *
 * produce a new extract info
 *)
let try_arglist_wild extract rw wrw nargs just world args =
   match args with
      [] ->
         ()
    | arg::argstl ->
         let terms = List.map (function inf -> inf.inf_value) argstl in
         let term = arg.inf_value in
            try
               let wilds = apply_rewrite wrw empty_args term terms in
               let wargs = List.map (function t -> find_inf extract world t (shape_of_term t)) wilds in
               let values = apply_rewrite rw empty_args term terms in
               let facts = mix_wild nargs args wargs in
               let root =
                  Inference { inf_values = values;
                              inf_just = just;
                              inf_args = facts;
                              inf_world = world
                  }
               in
                  build_results extract world root values
            with
               _ ->
                  ()

(*
 * In wildcard chaining, some of the args are wildcards.
 * A rewrite is supplied to produce what these wildcard arguments
 * should be.
 *)
let try_fchain_wild extract world { finfo_rw = rw; finfo_just = just } (wargs, wrw) finst =
   let try_arglist_wild' = try_arglist_wild extract rw wrw wargs just in
      if not (List.mem [] finst) then
         instantiate try_arglist_wild' world (List.rev finst)

(*
 * Try chaining through a particular new item.
 * Optimize the case where there are no wildcard entries.
 *)
let try_fchain extract world ({ finfo_wild = wild } as info) finst =
   match wild with
      None ->
         try_fchain_normal extract world info finst
    | Some wild ->
         try_fchain_wild extract world info wild finst

(*
 * Try chaining through a particular new fact.
 * We produce all combinations that are available
 * for the forward rule.
 *)
let try_fchaining extract inf info =
   let _, rl, _ = info in
   let finst = fset_insts inf info in
   let world = world_of_inf inf in
      try_fchain extract world rl finst

(*
 * Get something from the queue, and try to chain through it.
 *)
let fchain ({ ext_base = { ext_fqueue = fqueue } as base } as extract) =
   match fqueue with
      inf::tl ->
         base.ext_fqueue <- tl;
         let rules =
            try fget_entry extract inf with
               Not_found ->
                  []
         in
            List.iter (try_fchaining extract inf) rules

    | [] ->
         ()

(************************************************************************
 * CHAIN COMMAND                                                        *
 ************************************************************************)

(*
 * Alternate forward and backward chaining.
 * Return true if a proof is found.
 *)
let chain extract =
   fchain extract;
   extract.ext_bchain extract

(************************************************************************
 * CREATION                                                             *
 ************************************************************************)

(*
 * Cache has empty lists.
 *)
let empty_cache =
   CacheNil

(*
 * Forward chaining rule.
 *)
let compute_wild t =
   let rec aux i = function
      h::t ->
         let terms, wildi, wilds = aux (i + 1) t in
            if is_var_term h then
               terms, i::wildi, h::wilds
            else
               h::terms, wildi, wilds
    | [] ->
         [], [], []
   in
   let terms, wildi, wilds = aux 0 t in
   let terms' = List.rev terms in
   let wilds' =
      if wilds = [] then
         None
      else
         Some (List.rev wildi, term_rewrite Strict empty_args_spec terms' (List.rev wilds))
   in
      terms', wilds'

let add_frule cache { fc_ants = ants; fc_concl = concl; fc_just = just } =
   let args, wild = compute_wild ants in
   let info =
      { finfo_plates = List.map shape_of_term args;
        finfo_wild = wild;
        finfo_rw = term_rewrite Strict empty_args_spec args concl;
        finfo_just = just
      }
   in
      CacheForward (info, cache)

(*
 * Flatten the antecedents for rewriting.
 *)
let rec flatten_ants = function
   (l, t)::tl ->
      l @ [t] @ (flatten_ants tl)
 | [] ->
      []

(*
 * Inverse flatten a term list into an antecedent list.
 *)
let rec simple_ants = function
   ([], _)::tl ->
      simple_ants tl
 | (_::_, _)::_ ->
      false
 | [] ->
      true

let compute_spread_ants ants =
   if simple_ants ants then
      List.map (function x -> [], x)
   else
      let aux (l, _) = List.length l in
      let indices = List.map aux ants in
      let rec aux l = function
         i::t ->
            begin
               if i = 0 then
                  match l with
                     h::t' -> ([], h)::(aux t' t)
                   | [] -> raise (Invalid_argument "spread_ants")
               else
                  match Lm_list_util.split_list i l with
                     ants, h::t' -> (ants, h)::(aux t' t)
                   | _, [] -> raise (Invalid_argument "spread_ants")
            end
       | [] ->
            []
      in
      let spread_ants l =
         aux l indices
      in
         spread_ants

(*
 * Add a rule for backward chaining.
 *)
let add_brule cache { bc_concl = concl; bc_ants = ants; bc_just = just } =
   let flat_ants = flatten_ants ants in
   let rw = term_rewrite Strict empty_args_spec [concl] flat_ants in
   let spread_ants = compute_spread_ants ants in
   let trw t =
      let values = apply_rewrite rw empty_args t [] in
         spread_ants values
   in
   let info =
      { binfo_plate = shape_of_term concl;
        binfo_rw = trw;
        binfo_just = just
      }
   in
      CacheBackward (info, cache)

(************************************************************************
 * EXTRACT CONSTRUCTION                                                 *
 ************************************************************************)

(*
 * Build the forward chaining hash table.
 *)
let insert_ftable_entry ftable info =
   let rec aux i = function
      h::t ->
         fset_entry ftable h (i, info);
         aux (i + 1) t
    | [] ->
         ()
   in
      aux 0 info.finfo_plates

let make_ftable cache =
   let tbl = Hashtbl.create 97 in
      List.iter (insert_ftable_entry tbl) cache;
      tbl

(*
 * Build the backward chaining hash table.
 *)
let make_btable cache =
   let tbl = Hashtbl.create 97 in
      List.iter (bset_entry tbl) cache;
      tbl

(*
 * A default rule table.
 *)
let make_rules info =
   let rules = Hashtbl.create 97 in
      List.iter (init_insts rules) info;
      rules

(*
 * Collect all the forward and backward rules form the cache tree.
 * We keep a record of all the caches seen so we can remove duplicates.
 *)
let extract_cache cache =
   let rec collect caches finfo binfo cache =
      if List.memq cache caches then
         finfo, binfo, caches
      else
         let caches = cache :: caches in
            match cache with
               CacheNil ->
                  finfo, binfo, caches
             | CacheForward (info, cache) ->
                  collect caches (info :: finfo) binfo cache
             | CacheBackward (info, cache) ->
                  collect caches finfo (info :: binfo) cache
   in
   let finfo, binfo, _ = collect [] [] [] cache in
      finfo, binfo

(*
 * Construct the extract.
 *)
let extract cache =
   let finfo, binfo = extract_cache cache in
   let node, goals, bchain = new_goals Empty None in
      {  (* Hyps *)
         ext_used = [];
         ext_hcount = 0;
         ext_names = [];
         ext_gnames = [];
         ext_hyps = [];

         (* Back chaining *)
         ext_goal = None;
         ext_node = node;
         ext_goals = goals;
         ext_bchain = bchain;

         (* Current world *)
         ext_world = Empty;

         (* Chaining base *)
         ext_base =
            { (* Rule tables *)
               ext_ftable = make_ftable finfo;
               ext_btable = make_btable binfo;
               ext_terms = Hashtbl.create 97;
               ext_rules = make_rules finfo;

               (* Forward chaining *)
               ext_index = 0;
               ext_fqueue = [];

               (* Possible worlds *)
               ext_worlds = []
            }
      }

(************************************************************************
 * HYP OPERATIONS                                                       *
 ************************************************************************)

(*
 * Insert a new hyp.
 *)
let add_hyp extract i gname t =
   let { ext_used = used;
         ext_hcount = hcount;
         ext_names = names;
         ext_gnames = gnames;
         ext_hyps = hyps;
         ext_goal = goal;
         ext_world = world;
         ext_base = base
       } = extract
   in
   let t = subst t names (List.map mk_var_term names) in
   let hash = shape_of_term t in
   let world = build_world_with_hyp extract world t hash in
   let name = name_of_world world in
   let i = hcount - i - 1 in
   let node, goals, bchain = new_goals world goal in
      { ext_used = used;
        ext_hcount = hcount + 1;
        ext_names = Lm_list_util.insert_nth i name names;
        ext_gnames = Lm_list_util.insert_nth i gname gnames;
        ext_hyps = Lm_list_util.insert_nth i world hyps;
        ext_goal = goal;
        ext_node = node;
        ext_goals = goals;
        ext_bchain = bchain;
        ext_world = world;
        ext_base = base
      }

(*
 * Delete a hyp and all the hyps after it.
 * Also remove all subsequent hyps.  This won't actually be used
 * very much, since "backout" is usually performed by using
 * a different copy of the extract.  However, explicit backout
 * occurs during thinning.
 *)
let del_hyp
    { ext_used = used;
      ext_hcount = hcount;
      ext_names = names;
      ext_gnames = gnames;
      ext_hyps = hyps;
      ext_goal = goal;
      ext_base = base
    } i =
   if i = 0 then
      (* Remove all hyps *)
      let world = Empty in
      let node, goals, bchain = new_goals world goal in
         { ext_used = [];
           ext_hcount = 0;
           ext_names = [];
           ext_gnames = [];
           ext_hyps = [];
           ext_goal = goal;
           ext_node = node;
           ext_world = world;
           ext_goals = goals;
           ext_bchain = bchain;
           ext_base = base
         }
   else
      (* Find a specific hyp *)
      let i' = hcount - i in
      let rm, hyps' = Lm_list_util.split_list i' hyps in
      let world = List.hd hyps' in
      let node, goals, bchain = new_goals world goal in
      let extract =
         { ext_used = Lm_list_util.subtractq used rm;
           ext_hcount = i;
           ext_names = Lm_list_util.nth_tl i' names;
           ext_gnames = Lm_list_util.nth_tl i' gnames;
           ext_hyps = hyps';
           ext_goal = goal;
           ext_node = node;
           ext_goals = goals;
           ext_bchain = bchain;
           ext_world = world;
           ext_base = base
         }
      in
         extract

(*
 * Reference a particular hyp.
 *)
let ref_hyp ({ ext_hcount = hcount;
               ext_hyps = hyps;
               ext_used = used
             } as extract) i =
   let hyp = List.nth hyps (hcount - i - 1) in
      if List.memq hyp used then
         extract
      else
         set_used extract (hyp::used)

(*
 * Rename a particular hyp.
 *)
let name_hyp extract i gname =
   let { ext_hcount = hcount; ext_gnames = gnames } = extract in
      set_gnames extract (Lm_list_util.replace_nth (hcount - i - 1) gname gnames)

(*
 * Choose a new conclusion.
 * This also resets the backward chaining queue
 * to be only those goals that the current goal depends upon.
 *)
let set_goal extract concl =
   let { ext_used = used;
         ext_hcount = hcount;
         ext_names = names;
         ext_gnames = gnames;
         ext_hyps = hyps;
         ext_world = world;
         ext_base = base
       } = extract
   in
   let t = subst concl gnames (List.map mk_var_term names) in
   let hash = shape_of_term t in
   let goal =
      let goal =
         { goal_concl = t;
           goal_hash = hash;
           goal_assums = [];
           goal_subgoals = Unexplored
         }
      in
         Some (try find_goal extract goal with
                  Not_found ->
                     set_goal extract goal;
                     goal)
   in
   let node, goals, bchain = new_goals world goal in
      { ext_used = used;
        ext_hcount = hcount;
        ext_names = names;
        ext_gnames = gnames;
        ext_hyps = hyps;
        ext_world = world;
        ext_goal = goal;
        ext_node = node;
        ext_goals = goals;
        ext_bchain = bchain;
        ext_base = base
      }

(*
 * Set the sequent all at once.
 * This function performs the following:
 *    1. delete all the hyps using del_hyp
 *    2. add all the hyps using add_hyp
 *    3. set the goal using sewt_goal
 *)
let set_msequent extract seq =
   let rec collect i len extract hyps =
      if i = len then
         extract
      else
         let extract =
            match SeqHyp.get hyps i with
               Term_sig.Hypothesis (name, hyp) ->
                  add_hyp extract i name hyp
             | Term_sig.Context (name, _, _) ->
                  add_hyp extract i name context_term
         in
            collect (i + 1) len extract hyps
   in
   let goal, _ = dest_msequent seq in
      match explode_sequent goal with
         { sequent_hyps = hyps; sequent_concl = concl } ->
            set_goal (collect 0 (SeqHyp.length hyps) (del_hyp extract 0) hyps) concl

(************************************************************************
 * LOOKUP                                                               *
 ************************************************************************)

(*
 * We use this data structure to record the hyps that
 * are available at during the proof.  This would really
 * be a list of inferences, except for some of the results
 * produced by forward chaining, we don't know the particular
 * inference.  Se we settle for the root and index.
 *
 * This is mutable because we want to be able to
 * upgrade Root to Inf in place if it comes along.
 *)
type 'a hyp =
   Root of int * 'a inference_info
 | Inf of 'a inference

type 'a hypref =
   { mutable hyp_info : 'a hyp }

type 'a hyplist =
   { hyp_count : int;
     hyp_hyps : 'a hypref list
   }

(*
 * Produce the initial hyp list from the list of worlds in the extract.
 *)
let init_hyps { ext_hcount = hcount; ext_hyps = hyps } =
   let aux = function
      Hypothesis { world_hyp = inf } ->
         { hyp_info = Inf inf }
    | Empty ->
         raise (Invalid_argument "init_hyps")
   in
      { hyp_count = hcount; hyp_hyps = List.map aux hyps }

(*
 * See of the hyp list contains an inference.
 * First check if the inference exists directly.
 * If that doesn't work, the check if the root for
 * the inference exists, and replace the reference
 * with the inf.
 *)
let hyp_index { hyp_count = hcount; hyp_hyps = hyps } =
   let search_root inf =
      match inf with
         { inf_value = t; inf_info = Inference ({ inf_values = values } as info) } ->
            let index = Lm_list_util.find_indexq t values in
            let rec search i = function
               ({ hyp_info = Root (j, info') } as hyp)::htl ->
                  if info == info' & j = index then
                     begin
                        hyp.hyp_info <- Inf inf;
                        Some i
                     end
                  else
                     search (i - 1) htl
             | _::htl ->
                  search (i - 1) htl
             | [] ->
                  None
            in
               search (hcount - 1) hyps
       | _ ->
            None
   in
   let search_inf inf =
      let rec search i = function
         { hyp_info = Inf inf' }::htl ->
            if inf' == inf then
               Some i
            else
               search (i - 1) htl
       | _::htl ->
            search (i - 1) htl
       | [] ->
            search_root inf
      in
         search (hcount - 1) hyps
   in
      search_inf

let hyp_index_force hyps inf =
   match hyp_index hyps inf with
      Some i -> i
    | None -> raise Not_found

(*
 * Add a rule as an inference.
 *)
let hyp_add_info { hyp_count = hcount; hyp_hyps = hyps }
    ({ inf_values = values } as info) =
   let rec aux i l = function
      _::t ->
         aux (i + 1) ({ hyp_info = Root (i, info) }::l) t
    | [] ->
         { hyp_count = hcount + i; hyp_hyps = l }
   in
      aux 0 hyps values

(*
 * Find the particular branch of the node that succeeded.
 *)
let find_and_branch { node_goal = { goal_subgoals = subgoals }; node_children = children } =
   match subgoals with
      Subgoals subgoals' ->
         let rec search = function
            (children::ctl), ({ sub_just = just }::gtl) ->
               let test { node_status = status } =
                  match status with
                     Primitive _ | Derived -> true
                   | Unproved | Unprovable | Derivable -> false
               in
                  if List.for_all test children then
                     children, just
                  else
                     search (ctl, gtl)
          | _ ->
               raise (Invalid_argument "find_and_branch")
         in
            search (children, subgoals')
    | Unexplored | Unsolvable ->
         raise (Invalid_argument "find_and_branch")

(*
 * Split the infs into those that can be inferred from the current world,
 * and those that must be postponed.
 *)
let split_infs infs world =
   let rec aux = function
      inf::tl ->
         let world' = world_of_inf inf in
         let ninfs, infs' = aux tl in
            if is_child_of_parent world world' then
               inf::ninfs, infs'
            else
               ninfs, inf::infs'
    | [] ->
         [], []
   in
      aux infs

(*
 * To perform forward chaining given the hyp list,
 * keep inferring through roots.  Return the
 * justifications, as well as the new hyp list.
 *)
let rec forechain hyps = function
   inf::infs ->
      begin
         match hyp_index hyps inf with
            Some i ->
               forechain hyps infs
          | None ->
               (* Infer the inf *)
               match inf with
                  { inf_info = Inference ({ inf_just = just; inf_args = args } as info) } ->
                     let l, hyps' = forechain hyps args in
                     let indices = List.map (hyp_index_force hyps') args in
                     let hyps'' = hyp_add_info hyps' info in
                     let l', hyps''' = forechain hyps'' infs in
                        l @ ((indices, just)::l), hyps'''
                | { inf_info = Hyp _ } ->
                     failwith "forechain: hyp_error"
      end

 | [] ->
      [], hyps

(*
 * Backchain from a node, given a collection of infs that
 * must be inferred.  Infer them as soon as possible.
 *)
let rec backchain hyps infs
    ({ node_status = status;
       node_world = world
     } as node) =
   let ninfs, infs' = split_infs infs world in
   let fjust, hyps' = forechain hyps ninfs in
   let children', just = find_and_branch node in
   let bproof = BackTactic (just, List.map (backchain hyps' infs') children') in
   let proof =
      if fjust = [] then
         bproof
      else
         SeqTactic [ForeTactics fjust; bproof]
   in
      proof

(*
 * Derive the backchain tree, and collect the inferences that
 * must be satisfied by forward chaining.
 *)
let collect_infs node =
   let rec search l ({ node_status = status } as node) =
      match status with
         Primitive inf -> inf::l
       | Derived ->
            let children', _ = find_and_branch node in
               List.fold_left search l children'
       | Unproved | Unprovable | Derivable ->
            raise (Invalid_argument "collect")
   in
      search [] node

(*
 * Produce a proof of the goal in the current world.
 * This will be a combination of forward and backward chaining.
 * Assume that there is a proof.
 *
 * This is complicated by the fact that we want
 * to perform forward chaining as soon as possible.
 * For this purpose, we perform two passes:
 *    In the first pass, we collect all the inferences that
 *    should be infered.
 *
 *    In the second pass, we combine the forward,
 *    backward chaining.
 *)
let lookup ({ ext_node = node } as extract) =
   match node with
      Some node' ->
         let hyps = init_hyps extract in
         let infs = collect_infs node' in
            backchain hyps infs node'
    | None ->
         raise (Invalid_argument "lookup")

(************************************************************************
 * SYNTHESIS                                                            *
 ************************************************************************)

(*
 * Find the handles of the hyps being used.
 *)
let synthesize ({ ext_used = used } as extract) syns =
   let rec combine = function
      { syn_used = used' }::t ->
         Lm_list_util.unionq used (combine t)
    | [] ->
         used
   in
      { syn_extract = extract; syn_used = combine syns }

(*
 * Get the used hyp numbers.
 *)
let used_hyps
    { syn_extract = { ext_hyps = hyps; ext_hcount = hcount };
      syn_used = used
    } =
   List.map (function inf -> hcount - (Lm_list_util.find_indexq inf hyps) - 1) used

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
