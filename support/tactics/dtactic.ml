doc <:doc<
   @begin[doc]
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
    | ElimArgsOption of (tactic_arg -> term -> term list) * term option
   @end[verbatim]
   @end[center]

   The @tt[ElimArgsOption] provides arguments in the same way as the
   @tt[IntroArgsOption].  The @tt[ThinOption] is an argument that provides an
   optional tactic to ``thin'' the hypothesis after application of the
   elimination rule.

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

   See the file doc/index.html for information on Nuprl,
   OCaml, and more information about this system.

   Copyright (C) 1998 Jason Hickey, Cornell University

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
   @end[doc]
>>

doc <:doc<
   @begin[doc]
   @parents
   @end[doc]
>>
extends Top_tacticals
extends Auto_tactic
extends Browser_resource
doc docoff

open Lm_debug
open Lm_printf

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

open Auto_tactic
open Simp_typeinf
open Typeinf
open Browser_resource

(*
 * Show that the file is loading.
 *)
let _ =
   show_loading "Loading Dtactic%t"

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
 | ElimArgsOption of (tactic_arg -> term -> term list) * term option

type intro_item = string * int option * auto_type * tactic

(************************************************************************
 * IMPLEMENTATION                                                       *
 ************************************************************************)

(*
 * Extract a D tactic from the data.
 * The tactic checks for an optable.
 *)
let extract_elim_data =
   let rec firstiT i = function
      [] -> raise (Invalid_argument "extract_elim_data: internal error")
    | [tac] -> tac i
    | tac :: tacs -> tac i orelseT firstiT i tacs
   in
   (fun tbl ->
   argfunT (fun i p ->
      let t = Sequent.nth_hyp p i in
      if !debug_dtactic then
         eprintf "Dtactic: elim: lookup %s%t" (SimplePrint.short_string_of_term t) eflush;
      let tacs =
         try
            lookup_bucket tbl select_all t
         with
            Not_found ->
               raise (RefineError ("extract_elim_data", StringTermError ("D tactic doesn't know about", t)))
      in firstiT i tacs))

let in_auto p =
   match Sequent.get_int_arg p "d_auto" with
      Some 0 | Some 1 -> true
    | _ -> false

let extract_intro_data =
   let select_intro sel in_auto_type (_, sel', auto_type, _) =
      (match in_auto_type, auto_type with
         Some 0, AutoTrivial
       | Some 1, AutoNormal
       | Some 2, AutoComplete
       | None, _
         -> true
       | _
         -> false)
      &&
      (match sel, sel' with
         _, None -> true
       | Some i, Some i' when i = i' -> true
       | _ -> false)
   in
   let extract (name, _, _, tac) =
      if !debug_dtactic then
         eprintf "Dtactic: intro: found %s%t" name eflush; tac
   in
   (fun tbl ->
   funT (fun p ->
      let t = Sequent.concl p in
      if !debug_dtactic then
         eprintf "Dtactic: intro: lookup %s%t" (SimplePrint.short_string_of_term t) eflush;
      let sel_arg = get_sel_arg p in
      let tacs =
         try lookup_bucket tbl (select_intro sel_arg (Sequent.get_int_arg p "d_auto")) t with
            Not_found ->
               let sel_err =
                  match sel_arg with
                     Some _ -> "out of range"
                   | None -> "required"
               in
                  raise (RefineError ("extract_intro_data", StringTermError ("D tactic doesn't know about or select argument is " ^ sel_err, t)))
      in
         firstT (List.map extract tacs)))

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
let process_intro_resource_annotation name args term_args statement (pre_tactic, options) =
   if args.spec_addrs <> [||] then
      raise (Invalid_argument (sprintf "intro annotation: %s: context arguments not supported yet" name));
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
                                 raise (RefineError("intro annotation",
                                    StringTermError("term not found in the conclusion", arg)))
                           end
                  in
                     (fun p -> f p (get_arg p))
             | None ->
                  let length = List.length term_args in
                     (fun p ->
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
   let tac =
      match args.spec_ints, SeqHyp.to_list goal.sequent_hyps with
         [||], [Context _] ->
            if term_args = [] then  (* optimization *)
               Tactic_type.Tactic.tactic_of_rule pre_tactic empty_rw_args []
            else
               funT (fun p -> Tactic_type.Tactic.tactic_of_rule pre_tactic empty_rw_args (term_args_fun p))
       | [|_|], [ Context _; Hypothesis _; Context _ ] when not (is_so_var_term t) ->
            onSomeHypT (argfunT (fun i p -> Tactic_type.Tactic.tactic_of_rule pre_tactic (one_rw_arg i) (term_args_fun p)))
       | _ ->
            raise (Invalid_argument (sprintf "Dtactic.intro: %s: not an introduction rule" name))
   in
      let sel_opts = get_sel_arg options in
      let rec auto_aux = function
         [] ->
            [t, (name, sel_opts, (if assums = [] then AutoTrivial else AutoNormal), tac)]
       | AutoMustComplete :: _ ->
            [t, (name, sel_opts, AutoComplete, tac)]
       | CondMustComplete f :: _ ->
            let auto_exn = RefineError("intro_annotation: " ^ name, StringError("not appropriate in weakAutoT")) in
            let tac' =
               funT (fun p -> if f p then raise auto_exn else tac)
            in [
               t, (name, sel_opts, AutoNormal, tac');
               t, (name, sel_opts, AutoComplete, tac)
            ]
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

let process_elim_resource_annotation name args term_args statement (pre_tactic, options) =
   if args.spec_addrs <> [||] then
      raise (Invalid_argument (sprintf "elim annotation: %s: context arguments not supported yet" name));
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
                                    try get_with_args p with
                                       RefineError _ ->
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
            [t, tac]
    | _ ->
         raise (Invalid_argument (sprintf "Dtactic.improve_elim: %s: must be an elimination rule" name))

let wrap_intro tac =
   ("wrap_intro", None, AutoNormal, tac)

let mustSelectT = funT (fun p ->
   raise (RefineError("Dtactic.mustSelectT", StringTermError ("Select (selT) argument required", Sequent.concl p))))

let intro_must_select =
   ("mustSelectT", None, AutoNormal, mustSelectT)

(*
 * Resources
 *)
let resource (term * (int -> tactic), int -> tactic) elim =
   table_resource_info extract_elim_data

let resource (term * intro_item, tactic) intro =
   table_resource_info extract_intro_data

let dT =
   argfunT (fun i p ->
   if i = 0 then
      Sequent.get_resource_arg p get_intro_resource
   else
      Sequent.get_resource_arg p get_elim_resource (Sequent.get_pos_hyp_num p i))

let rec dForT i =
   if i <= 0 then
      idT
   else
      dT 0 thenMT dForT (pred i)

(*
 * By default, dT 0 should always make progress.
 *)
let d_prec = create_auto_prec [trivial_prec] [nth_hyp_prec]
let d_elim_prec = create_auto_prec [trivial_prec; d_prec] []

let eq_exn = RefineError ("dT", StringError "elim rule not suitable for autoT")

let rec num_equal_aux t hyps i =
   if i <= 0 then 0 else
   let i = pred i in
      (num_equal_aux t hyps i) +
      match SeqHyp.get hyps i with
         Hypothesis (_, t') when alpha_equal t t' -> 1
       | _ -> 0

let num_equal t p =
   let hyps = (TermMan.explode_sequent (Sequent.goal p)).sequent_hyps in
      num_equal_aux t hyps (SeqHyp.length hyps)

let check_num_equalT n t = funT (fun p ->
   if num_equal t p >= n then raise eq_exn else idT)

let auto_dT =
   argfunT (fun i p ->
      let t = Sequent.nth_hyp p i in
         dT i thenT check_num_equalT (num_equal t p) t)

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
   auto_name = "dT complete";
   auto_prec = d_prec;
   auto_tac = withIntT "d_auto" 2 (dT 0);
   auto_type = AutoComplete;
}; {
   auto_name = "dT elim-complete";
   auto_prec = d_elim_prec;
   auto_tac = onSomeHypT auto_dT;
   auto_type = AutoComplete;
}]

let elim_typeinf t = ElimArgsOption (infer_type_args, Some t)
let intro_typeinf t = IntroArgsOption (infer_type_args, Some t)
let simp_intro_typeinf t = IntroArgsOption (simp_infer_type_args, Some t)
let elim_typeinf_plusone t = ElimArgsOption (infer_type_2args, Some t)
let intro_typeinf_plusone t = IntroArgsOption (infer_type_2args, Some t)
let univ_arg_fun p _ = [get_univ_arg p]
let elim_univ_arg = ElimArgsOption (univ_arg_fun, None)
let intro_univ_arg = IntroArgsOption (univ_arg_fun, None)

(*
 * Add dT 0 to the browser.
 *)
let resource menubar +=
    [<< menuitem["refine", "dT 0", "Command('refine dT 0')"] >>, refine_is_enabled]

(*
 * -*-
 * Local Variables:
 * Caml-master: "editor.run"
 * End:
 * -*-
 *)

