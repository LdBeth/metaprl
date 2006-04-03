doc <:doc<
   @module[Dtactic]

   The @tactic[dT] tactic is the cornerstone of reasoning in
   most logics; it provides generic application of introduction
   elimination reasoning.  The @hrefmodule[Dtactic] defines a @emph{generic}
   resource that can be used to add introduction and elimination reasoning.
   In addition, it add resource @emph{annotations} that can be used in rule
   definitions to add them automatically to the @tt[dT] resources.

   The @tt[dT] tactic uses two resources.  The @resource[intro] resource
   is used to collect introduction rules; and the @resource[elim] resource
   is used to collect elimination rules.  The components of both resources
   take a term that describes the shape of the goals to which they apply,
   and a tactic to use when goals of that form are recognized.  The
   @hrefresource[elim] takes a tactic of type @code{int -> tactic} (the
   tactic takes the number of the hypothesis to which it applies), and the
   @hrefresource[intro] resource takes a tactic of type @code{tactic}.

   The (@hreftactic[dT] $i$) tactic is a generic tactic that takes the clause number
   of the clause (either hypothesis or conclusion) to ``decompose,'' and it
   applies the most appropriate entry from the resources.

   The resources also allow resource annotations in rule definitions.
   Typically, the annotation is added to explicit introduction or
   elimination rules, like the following:

   $$
   @defrule[and_intro]{
       @tt["{| intro [ ] |}"];
       <<sequent [dummy_arg] { <H> >- 'A }>>@cr
          <<sequent [dummy_arg] { <H> >- 'B }>>;
       <<sequent [dummy_arg] { <H> >- <:doc<A @wedge B>> }>>}
   $$

   Once this rule is defined, an application of the tactic (@hreftactic[dT] 0)
   to a conjunction will result in an application of the @hrefrule[and_intro]
   rule.

   The resource annotations take a list of optional arguments.  The
   @hrefresource[intro] resource takes arguments of the following type:

   @begin[center]
   @begin[verbatim]
   type intro_option =
      SelectOption of int
    | IntroArgsOption of (tactic_arg -> term -> term list) * term option
    | AutoMustComplete
    | CondMustComplete of tactic_arg -> bool
   @end[verbatim]
   @end[center]

   The @tt[SelectOption] is used for rules that require a selection argument.
   For instance, the disjunction introduction rule has two forms for the left
   and right-hand forms.

   $$
   @defrule[or_intro_left]{
      @tt["{| intro [SelectOption 1] |}"];
      <<sequent [dummy_arg] { <H> >- <:doc<B @Type>> }>>
          @cr <<sequent [dummy_arg] { <H> >- 'A }>>;
      <<sequent [dummy_arg] { <H> >- <:doc<A @vee B>> }>>}
   $$

   $$
   @defrule[or_intro_right]{
     @tt["{| intro [SelectOption 2] |}"];
     <<sequent [dummy_arg] { <H> >- <:doc<A @Type>> }>>@cr
        <<sequent [dummy_arg] { <H> >- 'B }>>;
     <<sequent [dummy_arg] { <H> >- <:doc<A @vee B>> }>>}
   $$

   These options require @hreftactic[selT] arguments: the left rule is applied with
   @tt{selT 1 (dT 0)} and the right rule is applied with @tt{selT 2 (dT 0)}.

   The @tt[IntroArgsOption] is used to @emph{infer} arguments to the rule.
   The first argument is a function that should provide an argument list to be used in the
   rule application.  It will be given (a) a @tt[tactic_arg] containing the current proof
   obligation, and (b) a subterm of the conclusion of the goal. The @code{term option}
   entry describes the subterm to be used for the second function argument; @tt[None]
   means that the whole conclusion will be passed in, and @tt[Some] @i[t] (where @i[t]
   is a subterm of the conclusion in the rule specification) means that the corresponding
   subterm will be used.

   The @tt[AutoMustComplete] option can be used to indicate that the
   @hreftactic[autoT] tactic should not use this rule unless it is capable
   of finishing the proof on its own. This option can be used to mark irreversible
   rules that may take a provable goal and produce potentially unprovable
   subgoals. Setting @tt[AutoMustComplete] is equivalent to setting the boolean flag when
   manually adding items to the resource.

   The @tt[CondMustComplete] option is a conditional version of @tt[AutoMustComplete];
   it is used to pass in a predicate controlling when to activate the @tt[AutoMustComplete].

   If an @tt[intro] annotation is used on a rule that has a conclusion of the form
   <<sequent [dummy_arg] { <H>; x: 'T; <J['x]> >- 'C['x] }>> where <<'C['x]>> is not
   a second-order variable, the rule is added to the @hrefresource[intro] with
   the @hreftactic[onSomeHypT] tactical applied to it.

   The @hrefresource[elim] resource options are defined with the following type:

   @begin[center]
   @begin[verbatim]
   type elim_option =
      ThinOption of (int -> tactic)
    | ThinFirst of (int -> tactic)
    | ElimArgsOption of (tactic_arg -> term -> term list) * term option
    | AutoOK
   @end[verbatim]
   @end[center]

   The @tt[ElimArgsOption] provides arguments in the same way as the
   @tt[IntroArgsOption].  The @tt[ThinOption] is an argument that provides an
   optional tactic to ``thin'' the hypothesis after application of the
   elimination rule. The @tt[AutoOK] option specifies that the rule can be used
   by @hreftactic[autoT] on @tt[AutoNormal] level (by default, elim rules will only
   be used by @hreftactic[autoT] on @tt[AutoMustComplete] level). The @tt[ThinFirst]
   option specifies that the rule only makes sense when something actually depends on
   the variable introduced by the given hypothesis; otherwise the hypothesis should be
   simply thinned.

   The @hreftactic[dT] resources are implemented as tables that store
   the term descriptions and tactics for ``decomposition''
   reasoning.  The @hreftactic[dT] tactic selects the most appropriate
   rule for a given goal and applies it.  The @tt{(dT 0)} tactic
   is added to the @hrefresource[auto] resource by default.
   @docoff

   ---------------------------------------------------------------

   @begin[license]

   This file is part of MetaPRL, a modular, higher order
   logical framework that provides a logical programming
   environment for OCaml and other languages.

   See the file doc/htmlman/default.html or visit http://metaprl.org/
   for more information.

   Copyright (C) 1997-2006 MetaPRL Group, Cornell University and
   California Institite of technology

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License
   as published by the Free Software Foundation; either version 2
   of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

   Author: Jason Hickey @email{jyh@cs.caltech.edu}
   Modified by: Aleksey Nogin @email{nogin@cs.cornell.edu}
   @end[license]
>>

doc <:doc<
   @parents
>>
extends Top_tacticals
extends Auto_tactic
extends Browser_resource
doc docoff

open Lm_debug
open Lm_printf

open Opname
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
open Simple_print
open Term_match_table

open Tactic_type
open Tactic_type.Tactic
open Tactic_type.Tacticals

open Options_boot
open Top_options

open Auto_tactic
open Simp_typeinf
open Typeinf
open Browser_resource

let debug_dtactic =
   create_debug (**)
      { debug_name = "dtactic";
        debug_description = "display dT tactic operations";
        debug_value = false
      }

(************************************************************************
 * TYPES                                                                *
 ************************************************************************)

(*
 * The d_tactic uses a term_table to match against terms.
 *)
type intro_option =
   SelectOption of int
 | IntroArgsOption of (tactic_arg -> term -> term list) * term option
 | AutoMustComplete
 | CondMustComplete of (tactic_arg -> bool)

type elim_option =
   ThinOption of (int -> tactic)
 | ThinFirst of (int -> tactic)
 | ElimArgsOption of (tactic_arg -> term -> term list) * term option
 | AutoOK

type intro_item = {
   intro_auto: auto_type;
   intro_select: int option;
   intro_labels: rule_labels;
   intro_fall_through: bool;
   intro_tac: tactic;
   intro_name: string;
}

type elim_item  = rule_labels * bool * (int -> tactic)
type elim_result = (tactic_arg -> int -> tactic) * (tactic_arg -> tactic)

(************************************************************************
 * IMPLEMENTATION                                                       *
 ************************************************************************)

let d_in_auto p =
   match Sequent.get_int_arg p "d_auto" with
      Some 0
    | Some 1 ->
         true
    | _ ->
         false
(*
 * Extract a D tactic from the data.
 * The tactic checks for an optable.
 *)
let extract_elim_data =
   let select_complete options (opts, auto_ok, _) =
      (not auto_ok) && rule_labels_are_allowed options opts
   in
   let select_options options d_in_auto (opts, auto_ok, _) =
      ((not d_in_auto) || auto_ok) && rule_labels_are_allowed options opts
   in
   let rec firstiT i = function
      [] ->
         raise (Invalid_argument "extract_elim_data: internal error")
    | [_, _, tac] ->
         tac i
    | (_, _, tac) :: tacs ->
         tac i orelseT firstiT i tacs
   in
   let eq_exn = RefineError ("dT", StringError "elim rule not suitable for autoT") in
   let nothing_exn = RefineError ("auto_dT", StringError "Nothing appropriate found") in
   let rec num_equal_aux t hyps i =
      if i <= 0 then 0 else
         let i = pred i in
            (num_equal_aux t hyps i) +
            match SeqHyp.get hyps i with
               Hypothesis (_, t') when alpha_equal t t' -> 1
             | _ -> 0
   in
   let num_equal t p =
      let hyps = (TermMan.explode_sequent (Sequent.goal p)).sequent_hyps in
         num_equal_aux t hyps (SeqHyp.length hyps)
   in
   let check_num_equalT n t = funT (fun p ->
      if num_equal t p >= n then raise eq_exn else idT)
   in
      (fun tbl ->
         let rec auto_dT hyps select i p =
            if i = 0 then
               raise nothing_exn
            else
               match SeqHyp.get hyps (i - 1) with
                  Hypothesis (_, t) ->
                     if !debug_dtactic then
                        eprintf "Dtactic: elim: lookup %s%t" (SimplePrint.short_string_of_term t) eflush;
                     begin match lookup_bucket tbl select t with
                        Some tacs ->
                           auto_firstiT hyps select (num_equal t p) t i tacs
                      | None ->
                           auto_dT hyps select (i - 1) p
                     end
                | Context _ ->
                     auto_dT hyps select (i - 1) p
         and auto_firstiT hyps select num t i = function
            [] ->
               funT (auto_dT hyps select (i - 1))
          | [_, _, tac] when i = 1 ->
               tac i thenT check_num_equalT num t
          | (_, _, tac) :: tacs ->
               (tac i thenT check_num_equalT num t) orelseT auto_firstiT hyps select num t i tacs
         in
         let dT p i = 
            let options = get_options p in
            let t = Sequent.nth_hyp p i in
               if !debug_dtactic then
                  eprintf "Dtactic: elim: lookup %s%t" (SimplePrint.short_string_of_term t) eflush;
               match lookup_bucket tbl (select_options options (d_in_auto p)) t with
                  Some tacs ->
                     firstiT i tacs
                | None ->
                     raise (RefineError ("extract_elim_data",
                        StringTermError ("D tactic doesn't know about", t)))
         in
         let auto_dT p =
            let hyps = (Sequent.explode_sequent_arg p).sequent_hyps in
               auto_dT hyps (select_complete (get_options p)) (SeqHyp.length hyps) p
         in
            dT, auto_dT)

let extract_intro_data =
   let select_intro p options in_auto_type item =
      if !debug_dtactic then
         eprintf "Dtactic: intro: potential match found: %s; will test for selection%t" item.intro_name eflush;
      (match in_auto_type, item.intro_auto with
          Some 0, AutoTrivial
        | Some 1, AutoNormal
        | Some 2, AutoComplete
        | None, _ ->
             true
        | _ ->
             false)
      &&
      (match item.intro_select with
          None ->
             true
        | Some i ->
             match get_sel_arg p with
                 Some i' ->
                    i' = i
               | None ->
                    false)
      &&
      (rule_labels_are_allowed options item.intro_labels)
   in
   let rec extract (cont: intro_item lazy_lookup) p =
      match cont () with
         Some (item, cont) ->
            if !debug_dtactic then
               eprintf "Dtactic: intro: found %s%t" item.intro_name eflush;
            if item.intro_fall_through then
               item.intro_tac orelseT (argfunT extract cont)
            else
               item.intro_tac
       | None ->
            let msg =
               match get_sel_arg p with
                  Some _ ->
                     "dT: nothing appropriate found: the select argument may be out of range"
                | None ->
                     (* "dT: nothing appropriate found: the option arguments may not be valid" *)
                     "dT: nothing appropriate found"
            in
               raise (RefineError ("extract_intro_data", StringTermError (msg, (Sequent.concl p))))
   in
      (fun tbl ->
         funT (fun p ->
            let t = Sequent.concl p in
            let options = Sequent.get_option_args p in
               if !debug_dtactic then
                  eprintf "Dtactic: intro: lookup %s%t" (SimplePrint.short_string_of_term t) eflush;
               extract (lookup_all tbl (select_intro p options (Sequent.get_int_arg p "d_auto")) t) p))

(*
 * Options for intro rule.
 *)
let rec get_args_arg = function
   IntroArgsOption (f, arg) :: t ->
      Some (f, arg)
 | _ :: t ->
      get_args_arg t
 | [] ->
      None

let rec get_sel_arg = function
   SelectOption sel :: t ->
      Some sel
 | _ :: t ->
      get_sel_arg t
 | [] ->
      None

let one_rw_arg i =
   { arg_ints = [| i |]; arg_addrs = [||] }

(*
 * Improve the intro resource from a rule.
 *)
let process_intro_resource_annotation ?(options = []) ?labels name args term_args statement loc pre_tactic =
   if args.spec_addrs <> [||] then
      raise (Invalid_argument (sprintf
         "%s: intro annotation: %s: context arguments not supported yet" (string_of_loc loc) name));
   let assums, goal = unzip_mfunction statement in
   let goal = TermMan.explode_sequent goal in
   let t = goal.sequent_concl in
   let term_args_fun =
      match term_args with
         [] ->
            (fun _ -> [])
       | _ ->
            match get_args_arg options with
               Some (f, arg) ->
                  let get_arg =
                     match arg with
                        None ->
                           Sequent.concl
                      | Some arg ->
                        begin match find_subterm t (fun t _ -> alpha_equal t arg) with
                                 addr :: _ ->
                                    (fun p -> term_subterm (Sequent.concl p) addr)
                               | [] ->
                                    raise (RefineError((string_of_loc loc),
                                       StringTermError("intro annotation: term not found in the conclusion", arg)))
                        end
                  in
                     (fun p -> f p (get_arg p))
             | None ->
                  let length = List.length term_args in
                     (fun p ->
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
   let tac =
      match args.spec_ints, SeqHyp.to_list goal.sequent_hyps with
         [||], [Context _] ->
            if term_args = [] then  (* optimization *)
               Tactic_type.Tactic.tactic_of_rule pre_tactic empty_rw_args []
            else
               funT (fun p -> Tactic_type.Tactic.tactic_of_rule pre_tactic empty_rw_args (term_args_fun p))
       | _ ->
            raise (Invalid_argument (sprintf
               "%s: intro annotation: %s: not an introduction rule" (string_of_loc loc) name))
   in
   let sel_opts = get_sel_arg options in
   let option_opts = rule_labels_of_opt_terms labels in
   let item = {
      intro_name = name;
      intro_select = sel_opts;
      intro_labels = option_opts;
      intro_auto = AutoComplete;
      intro_tac = tac;
      intro_fall_through = true;
   } in
   let rec auto_aux = function
      [] ->
         [t, { item with intro_auto = if assums = [] then AutoTrivial else AutoNormal }]
    | AutoMustComplete :: _ ->
         [t, item]
    | CondMustComplete f :: _ ->
         let auto_exn = RefineError ("intro_annotation: " ^ name, StringError "not appropriate in weakAutoT") in
         let tac' =
            funT (fun p -> if f p then raise auto_exn else tac)
         in
            [t, {item with intro_auto = AutoNormal; intro_tac = tac'};
             t, item]
    | _ :: tl ->
         auto_aux tl
   in
      auto_aux options

(*
 * Compile an elimination tactic.
 *)
let rec get_elim_args_arg = function
   ElimArgsOption (f, arg) :: t ->
      Some (f, arg)
 | _ :: t ->
      get_elim_args_arg t
 | [] ->
      None

let process_elim_resource_annotation ?(options = []) ?labels name args term_args statement loc pre_tactic =
   if args.spec_addrs <> [||] then
      raise (Invalid_argument (sprintf
         "%s: elim annotation: %s: context arguments not supported yet" (string_of_loc loc) name));
   let assums, goal = unzip_mfunction statement in
   match SeqHyp.to_list (TermMan.explode_sequent goal).sequent_hyps with
      [ Context _; Hypothesis(v,t); Context _ ] ->
         let term_args =
            match term_args with
               [] ->
                  (fun _ _ -> [])
             | _ ->
                  match get_elim_args_arg options with
                     Some (f, arg) ->
                        let get_arg =
                           match arg with
                              None ->
                                 (fun p i -> Sequent.nth_hyp p i)
                            | Some arg ->
                                 begin match find_subterm t (fun t _ -> alpha_equal t arg) with
                                    addr :: _ ->
                                       (fun p i -> term_subterm (Sequent.nth_hyp p i) addr)
                                 | [] ->
                                       raise (RefineError("intro annotation",
                                          StringTermError("term not found in the conclusion", arg)))
                                 end
                        in
                           (fun i p -> f p (get_arg p i))
                   | None ->
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
         let thinT =
            let rec collect = function
               ThinOption thinT :: _ ->
                  Some thinT
             | _ :: t ->
                  collect t
             | [] ->
                  None
            in
               collect options
         in
         let thinFirstT =
            let rec collect = function
               ThinFirst thinT :: _ ->
                  Some thinT
             | _ :: t ->
                  collect t
             | [] ->
                  None
            in
               collect options
         in
         let tac =
            match args.spec_ints, thinT with
               [| _ |], None ->
                  argfunT (fun i p ->
                     if !debug_dtactic then
                        eprintf "dT elim: trying %s%t" name eflush;
                     Tactic_type.Tactic.tactic_of_rule pre_tactic (one_rw_arg i) (term_args i p))

             | [| _ |], Some thinT ->
                  let rec find_thin_num_aux hyps len i =
                     if i = len then
                        raise (Invalid_argument (sprintf "Dtactic.improve_elim: %s: can not find what to thin in one of the subgoals" name));
                     match SeqHyp.get hyps i with
                        Hypothesis (_, t') when alpha_equal t t' -> i
                      | Hypothesis (v', _) when Lm_symbol.eq v v' -> i
                      | _ -> find_thin_num_aux hyps len (succ i)
                  in
                  let find_thin_num (_,_,assum) =
                     try
                        let hyps = (TermMan.explode_sequent assum).sequent_hyps in
                        find_thin_num_aux hyps (SeqHyp.length hyps) 0
                     with RefineError _ ->
                        raise (Invalid_argument (sprintf "Dtactic.improve_elim: %s: assumptions must be sequents" name))
                  in
                  let thin_nums = List.map find_thin_num assums in
                  let rec check_thin_nums = function
                     [i] -> i
                   | i :: ((i'::_) as tl) when (i=i') -> check_thin_nums tl
                   | [] ->
                        raise (Invalid_argument (sprintf "Dtactic.improve_elim: %s: should not use ThinOption in a rule with no assumptions" name))
                   | _ -> raise (Invalid_argument (sprintf "Dtactic.improve_elim: %s: ThinOption: different assumptions have the eliminated hypothesis in a different place" name))
                  in
                  let thin_incr = (check_thin_nums thin_nums) - 1 in
                  argfunT (fun i p ->
                     if !debug_dtactic then
                        eprintf "dT elim: trying %s%t" name eflush;
                     let tac = Tactic_type.Tactic.tactic_of_rule pre_tactic (one_rw_arg i) (term_args i p)
                     in
                        if get_thinning_arg p then
                           tac thenT tryT (thinT (i + thin_incr))
                        else
                           tac)
             | _ ->
                  raise (Invalid_argument (sprintf "Dtactic: %s: not an elimination rule" name))
         in
         let auto_ok = List.mem AutoOK options in
         let options = rule_labels_of_opt_terms labels in
            begin match thinFirstT with 
               Some thinT when auto_ok ->
                   [t, (options, true, (fun i -> thinT i orelseT tac i))]
             | Some thinT ->
                   [t, (options, true, thinT); t, (options, auto_ok, tac)]
             | None ->
                   [t, (options, auto_ok, tac)]
            end
    | _ ->
         raise (Invalid_argument (sprintf "Dtactic.improve_elim: %s: must be an elimination rule" name))

let wrap_intro ?labels ?(name="wrap_intro") ?select ?(auto=AutoNormal) ?(fall_through=true) tac = {
   intro_name = name;
   intro_select = select;
   intro_labels = rule_labels_of_opt_terms labels;
   intro_auto = auto;
   intro_tac = tac;
   intro_fall_through = fall_through;
}

let mustSelectT = funT (fun p ->
   raise (RefineError ("Dtactic.mustSelectT", StringTermError ("Select (selT) argument required", Sequent.concl p))))

let intro_must_select =
   wrap_intro ~name:"mustSelectT" ~fall_through:false mustSelectT

let mustOptionT = funT (fun p ->
   raise (RefineError ("Dtactic.mustOptionT", StringTermError ("String option (optionT) argument required", Sequent.concl p))))

let intro_must_option =
   ("mustOptionT", None, None, AutoNormal, mustOptionT)

let wrap_elim ?labels tac =
   rule_labels_of_opt_terms labels, false, tac

let wrap_elim_auto_ok ?labels tac =
   rule_labels_of_opt_terms labels, true, tac

(*
 * Resources
 *)
let resource (term * elim_item, elim_result) elim =
   table_resource_info extract_elim_data

let resource (term * intro_item, tactic) intro =
   table_resource_info extract_intro_data

let dT =
   argfunT (fun i p ->
         if i = 0 then
            Sequent.get_resource_arg p get_intro_resource
         else
            fst (Sequent.get_resource_arg p get_elim_resource) p (Sequent.get_pos_hyp_num p i))

let rec dForT i =
   if i <= 0 then
      idT
   else
      dT 0 thenMT dForT (pred i)

(*
 * By default, dT 0 should always make progress.
 *)
let d_prec = create_auto_prec [trivial_prec] [nth_hyp_prec]
let d_elim_prec = create_auto_prec [trivial_prec; d_prec] [reduce_prec]

let d_outside_auto tac = withoutIntT "d_auto" tac

let resource auto += [ {
   auto_name = "dT trivial";
   auto_prec = d_prec;
   auto_tac = withIntT "d_auto" 0 (dT 0);
   auto_type = AutoTrivial;
}; {
   auto_name = "dT";
   auto_prec = d_prec;
   auto_tac = withIntT "d_auto" 1 (dT 0);
   auto_type = AutoNormal;
}; {
   auto_name = "dT elim-simple";
   auto_prec = d_elim_prec;
   auto_tac = withIntT "d_auto" 1 (onSomeHypT dT);
   auto_type = AutoNormal;
}; {
   auto_name = "dT complete";
   auto_prec = d_prec;
   auto_tac = withIntT "d_auto" 2 (dT 0);
   auto_type = AutoComplete;
}; {
   auto_name = "dT elim-complete";
   auto_prec = d_elim_prec;
   auto_tac = funT (fun p -> snd (Sequent.get_resource_arg p get_elim_resource) p);
   auto_type = AutoComplete;
}]

let elim_typeinf t = ElimArgsOption (infer_type_args, Some t)
let intro_typeinf t = IntroArgsOption (infer_type_args, Some t)
let simp_intro_typeinf t = IntroArgsOption (simp_infer_type_args, Some t)
let elim_typeinf_plusone t = ElimArgsOption (infer_type_2args, Some t)
let intro_typeinf_plusone t = IntroArgsOption (infer_type_2args, Some t)

let univ_arg_fun p _ =
   match get_univ_arg p with
      Some t -> [t]
    | None -> raise (RefineError("univ_arg", StringError "univ arg required"))

let elim_univ_arg = ElimArgsOption (univ_arg_fun, None)
let intro_univ_arg = IntroArgsOption (univ_arg_fun, None)

let univ_typeinf_arg p t =
   match get_univ_arg p, get_with_arg p with
      Some u, Some t -> [u; t]
    | Some u, None -> [u; infer_type p t]
    | None, _ -> raise (RefineError("univ_typeinf_arg", StringError "univ arg required"))

let intro_univ_typeinf t = IntroArgsOption (univ_typeinf_arg, Some t)
let elim_univ_typeinf t = ElimArgsOption (univ_typeinf_arg, Some t)

let univ_with_args_fun p _ =
   match get_univ_arg p, get_with_arg p with
      Some u, Some t ->
         [u; t]
    | None, _ ->
         raise (RefineError("univ_with_args", StringError "univ argument required"))
    | _, None ->
         raise (RefineError("univ_with_args", StringError "term argument required"))

let intro_univ_with_args = IntroArgsOption (univ_with_args_fun, None)
let elim_univ_with_args = ElimArgsOption (univ_with_args_fun, None)

(*
 * Add dT 0 to the browser.
 *)
let resource menubar +=
    [<< menuitem["refine", "dT 0", "Command('refine dT 0')"] >>, refine_is_enabled]

(*
 * -*-
 * Local Variables:
 * End:
 * -*-
 *)

