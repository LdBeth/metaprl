doc <:doc< 
   @begin[spelling]
   cutT idT nthAssumT OnFirstT orelseT thenT tryT selT seqT whileProgressT
   @end[spelling]
  
   @begin[doc]
   @module[Top_tacticals]
  
   The @tt[Top_tacticals] module defines the primitive
   tactics and tacticals provided by the @MetaPRL prover.
   @end[doc]
  
   ----------------------------------------------------------------
  
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
   Modified By: Aleksey Nogin @email{nogin@cs.caltech.edu}
  
   @end[license]
>>

open Refiner.Refiner
open Refiner.Refiner.TermType
open Refiner.Refiner.Term
open Refiner.Refiner.TermSubst
open Refiner.Refiner.TermMan
open Refiner.Refiner.RefineError

open Tactic_type

doc <:doc< 
   @begin[doc]
   @parents
   @end[doc]
>>
extends Mptop

doc <:doc< ************************************************************************
   @begin[doc]
   @modsection{Primitive tactics}
  
   @begin[description]
   @item{@tactic[idT];
   The @tt[idT] tactic is the @emph{identity}.
  
   $$
   @rulebox{idT; ;
     <<sequent{ <H> >- 'T}>>;
     <<sequent{ <H> >- 'T}>>}
   $$}
   @end[description]
  
   @docoff
   @end[doc]
>>
let idT = Tacticals.idT

doc <:doc< 
   @begin[doc]
   @begin[description]
   @item{@tactic[cutT];
   The @tt[cutT] tactic implements primitive lemma-instantiation on meta-level by
   allowing one to cut in an assumption.
   
   }
   @end[description]
  
   @docoff
   @end[doc]
>>
let cutT = Tacticals.cutT

doc <:doc< 
   @begin[doc]
   @begin[description]
   @item{@tactic[failT], @tactic[failwithT];
   The @tt[failT] tactic always fails, and the @tt[failWithT] fails
   with a specific message.}
   @end[description]
  
   @docoff
   @end[doc]
>>
let failT = Tacticals.failT
let failWithT = Tacticals.failWithT

doc <:doc< 
   @begin[doc]
   @begin[description]
   @item{@tactic[funT]; When @i[tac] is a function from @tt[tactic_arg] to @tt[tactic]
   (e.g. a function that takes a look at the current goal and chooses the tactic to apply
   based on what is being proven), then @tt[funT] @i[tac] would be the corresponding tactic.}
   @end[description]
  
   @docoff
   @end[doc]
>>
let funT = Tacticals.funT

doc <:doc< 
   @begin[doc]
   @begin[description]
   @item{@tactic[nthAssumT];
   The @tt[nthAssumT] tactic proves a goal by @emph{assumption}.
   Technically, an assumption is a subgoal of the theorem being proved.
   The assumption must be syntactically identical to the goal.
  
   $$
   @rulebox{nthAssumT; i;
    @cdot;
    <<sequent{ <H_1> >- 'T_1}>> @i{(Assumption@space 1)}@cr
    @ldots@cr
    <<sequent{ <H_i> >- 'T_i}>> @i{(Assumption@space @i{i})}@cr
    @ldots@cr
    <<sequent{ <H_n> >- 'T_n}>> @i{(Assumption@space @i{n})}@cr
    @hline
    <<sequent{ <H_i> >- 'T_i}>>}
   $$}
   @end[description]
  
   @docoff
   @end[doc]
>>
let nthAssumT = Tacticals.nthAssumT

doc <:doc< ************************************************************************
   @begin[doc]
   @modsection{Tacticals}
  
   @begin[description]
   @item{@tactic[thenT], @tactic[orelseT];
   There are several tacticals to manage proof search.  The basic
   tacticals are @tt[thenT] and @tt[orelseT], both @emph{infix}
   tacticals.  The tactic @tt{$@i[tac]_1$ thenT $@i[tac]_2$}
   first applies $@i[tac]_1$ to the goal, and then applies $@i[tac]_2$
   to @emph{all} of the subgoals.  It fails if either tactic fails
   on any of the subgoals.  The tactic @tt{$@i[tac]_1$ orelseT $@i[tac]_2$}
   first applies $@i[tac]_1$.  If it succeeds, $@i[tac]_2$ is ignored.
   Otherwise, $@i[tac]_2$ is applied to the original goal.}
  
   @item{@tactic[firstT];
   The @tt[firstT] tactical is a variant of @tt[orelseT].  It takes
   a list of tactics @tt{$[@i[tac]_1; @ldots; @i[tac]_n]$} to be applied
   in order until the first one succeeds.  It fails if all of the argument
   tactics fail.  It is equivalent to (@tt{$@i[tac]_1$ orelseT $@cdots$
   orelseT $@i[tac]_n$}).}
  
   @item{@tactic[seqT], @tactic[seqOnSameConclT];
   The @tt[seqT] is the universal form of the @tt[firstT] tactical.
   The (@tt{seqT $[@i[tac]_1; @ldots; @i[tac]_n]$}) tactic is equivalent to
   (@tt{$@i[tac]_1$ thenT $@cdots$ thenT $@i[tac]_n$}).  The @tt[seqOnSameConclT]
   tactic is the same as @tt[seqT] except that it selects only those subgoals
   that have the same conclusion as the current goal.}
   @end[description]
  
   @docoff
   @end[doc]
>>
let prefix_orelseT = Tacticals.prefix_orelseT
let prefix_andalsoT = Tacticals.prefix_andalsoT
let prefix_orthenT = Tacticals.prefix_orthenT
let firstT = Tacticals.firstT
let prefix_thenT = Tacticals.prefix_thenT
let prefix_thenLT = Tacticals.prefix_thenLT
let seqT = Tacticals.seqT
let seqOnSameConclT = Tacticals.seqOnSameConclT

doc <:doc< 
   @begin[doc]
   @begin[description]
   @item{@tactic[then_OnFirstT],
         @tactic[then_OnLastT],
         @tactic[then_OnSameConclT];
   There are also several tactics that apply to selected subgoals by number.
   The use of these tactics is discouraged in favor of selecting tactics
   by label (discussed below).
  
   The @tt{$@i[tac]_1$ then_OnFirstT $@i[tac]_2$} applies $@i[tac]_1$
   to the goal, and then applies $@i[tac]_2$ to the @emph{first} subgoal
   that is generated.  The @tt[then_OnLastT] selects the last subgoal,
   and the @tt[then_OnSameConclT] tactic chooses the subgoal with the
   same conclusion as the current goal.}
   @end[description]
  
   @docoff
   @end[doc]
>>
let prefix_then_OnFirstT = Tacticals.prefix_then_OnFirstT
let prefix_then_OnLastT = Tacticals.prefix_then_OnLastT
let prefix_then_OnSameConclT = Tacticals.prefix_then_OnSameConclT

doc <:doc< 
   @begin[doc]
   @begin[description]
   @item{@tactic[timingT];
   The @tt[timingT] tactical applies its argument,
   and prints timing information useful for tactic profiling.
  
   $$
   @rulebox{idT; ;
     <<sequent{ <H> >- 'T}>>;
     <<sequent{ <H> >- 'T}>>}
   $$
  
   @code{User time 0.000000; System time 0.000000; Real time 0.001778}}
   @end[description]
  
   @docoff
   @end[doc]
>>
let timingT = Tacticals.timingT

doc <:doc< 
   @begin[doc]
   @begin[description]
   @item{@tactic[tryT], @tactic[completeT];
   The @tt[tryT] tactical applies its argument, and performs
   the identity if the tactic fails.  The tactic (@tt[tryT] @i[tac])
   is equivalent to (@i[tac] @tt{orelseT idT}).}
   @end[description]
  
   @docoff
   @end[doc]
>>
let tryT = Tacticals.tryT
let completeT = Tacticals.completeT

doc <:doc< 
   @begin[doc]
   @begin[description]
   @item{@tactic[progressT];
   The (@tt[progressT] $@i[tac]$) tactic applies its argument and fails
   if either $@i[tac]$ fails, or $@i[tac]$ failed to make ``progress''
   by generating subgoals that differ from the current goal.}
   @end[description]
  
   @docoff
   @end[doc]
>>
let progressT = Tacticals.progressT

doc <:doc< 
   @begin[doc]
   @begin[description]
   @item{@tactic[whileProgressT], @tactic[untilFailT], @tactic[repeatT], @tactic[repeatForT];
   The (@tt[untilFailT] $@i[tac]$) tactic applies the argument $@i[tac]$
   repeatedly to the current goal and all of the generated subgoals until
   an application of $@i[tac]$ fails. This is the same as @tt[REPEAT] tactical in Nuprl.
   Note that the (@tt[untilFailT] $@i[tac]$) tactic
   collects all exceptions generated by $@i[tac]$ and it never fails itself.
  
   The (@tt[whileProgressT] $@i[tac]$) tactic repeatedly executes the given tactic on all subgoals
   while there is a progress. If $@i[tac]$ fails, then @tt[whileProgressT] also fails.
  
  
   The (@tt[repeatT] $@i[tac]$) tactic is equal to  (@tt{whileProgressT tryT} $@i[tac]$).
   It repeats the application of its argument until it fails or no more progress is made.
  
   The (@tt[repeatForT] $i$ $@i[tac]$) repeatedly execute the given tactic  $@i[tac]$ on all subgoals
   until the depth $i$ is reached. If $@i[tac]$ fails, then @tt[repeatForT] also fails.
   }
   @end[description]
  
   @docoff
   @end[doc]
>>
let repeatT = Tacticals.repeatT
let whileProgressT = Tacticals.whileProgressT
let untilFailT = Tacticals.untilFailT
let repeatForT = Tacticals.repeatForT

doc <:doc< ************************************************************************
   @begin[doc]
   @modsection{Tactic arguments}
  
   Tactics and rules may require arguments of various types (such as numbers, strings,
   terms, or even other tactics).  The proof structure allows the proof tree to
   be annotated with these arguments, and the arguments are inherited down
   the proof tree until they are removed.  This allows the proof to act as a
   temporary environment for communication between tactics.
   The following tactics apply
   temporary annotations.
  
   @begin[description]
   @item{@tactic[withT], @tactic[withTypeT], @tactic[withBoolT], @tactic[withIntT];
   {The (@tt[withT] @i{term} @i[tac]) tactic adds a term annotation to the
   tree@; applies @i[tac]@; and then removes the term annotation from all of
   the subgoals.  The @tt[withTypeT] adds a ``type'' annotation (the type is
   expressed as a term).  The @tt[withBoolT] and @tt[withIntT] add Boolean and
   integer annotations.}}
  
   @item{@tactic[selT];
   There is one more tactical that is frequently
   used in the @MetaPRL logics: by convention the @tt[selT] tactical is used to
   ``select'' among several alternate methods of proof.  For example, in proving
   a disjunction (Section @refmodule[Itt_logic]) it is necessary to select the
   branch of the disjunct.
  
   $$
   @rulebox{selT; 2@space (@tt[dT]@space 0);
     <<sequent{ <H>; x: 'T_2; <J> >- <:doc<T_1@space @i{Type}>>}>> @cr
     <<sequent{ <H>; x: 'T_2; <J> >- 'T_2}>>;
     <<sequent{ <H>; x: 'T_2; <J> >- <:doc<T_1 @vee T_2>>}>>}
   $$}
   @end[description]
  
   @docoff
   @end[doc]
>>
let withTermT = Tacticals.withTermT
let withTypeT = Tacticals.withTypeT
let withBoolT = Tacticals.withBoolT
let withIntT = Tacticals.withIntT
let withT = Tacticals.withT
let withTermsT = Tacticals.withTermsT
let atT = Tacticals.atT
let selT = Tacticals.selT
let altT = Tacticals.altT
let thinningT = Tacticals.thinningT
let doNotThinT = thinningT false

doc <:doc< ************************************************************************
   @begin[doc]
   @modsection{Clause selection}
  
   The following tactics are intended for use in a single-conclusion sequent calculus.
   A sequent <<sequent{x: 'T_1; math_cdots; x: 'T_n >- 'C}>> has
   $n + 1$ @emph{clauses}.  The hypotheses are clauses $1, @ldots, n$ and the conclusion
   is clause $0$.
  
   @begin[description]
   @item{@tactic[onClauseT], @tactic[onHypT], @tactic[onConclT];
   The (@tt[onClauseT] $i$ @i[tac]) tactic applies the argument tactic with
   integer argument $i$ (it is equivalent to @i[tac] $i$).  The @tt[onHypT]
   restricts the integer argument to be a valid hypothesis number, and the
   (@tt[onConclT] @i[tac]) tactical applies its argument tactic with
   argument $0$.}
  
   @item{@tactic[onClausesT], @tactic[onHypsT];
   The @tt[onClausesT] and @tt[onHypsT] take a list of clause numbers.
   The (@tt[onClausesT] $[i_1; @cdots; i_n]$ @i[tac]) is equivalent to
   (@tt{@i[tac] $i_1$ thenT $@cdots$ thenT @i[tac] $i_n$ thenT @i[tac] $0$}).  The
   @tt[onHypsT] does the same, but it requires that the indices correspond
   to valid hypothesis numbers.}
  
   @item{@tactic[onAllClausesT], @tactic[onAllHypsT];
   The (@tt[onAllClausesT] @i[tac]) applies the argument tactic to all the clauses,
   including all the hypothesis and the conclusion.  The @tt[onAllHypsT] applies the
   argument only to the hypotheses.}
  
   @item{@tactic[onAllAssumT];
   The (@tt[onAllAssumT] @i[tac]) applies the argument tactic to all the assumptions.}
  
   @item{@tactic[onSomeHypT];
   The (@tt[onSomeHypT] @i[tac]) applies the argument tactic to the
   hypotheses from the last to the first, returning once an application
   succeeds.  The (@tt[onSomeHypT] @i[tac]) is equivalent to
   (@tt{@i[tac] $n$ orelseT $@cdots$ orelseT @i[tac] $1$}).}
   @end[description]
  
   @docoff
   @end[doc]
>>
let onClauseT = Tacticals.onClauseT
let onHypT = Tacticals.onHypT
let onConclT = Tacticals.onConclT
let onClausesT = Tacticals.onClausesT
let onHypsT = Tacticals.onHypsT
let onMClausesT = Tacticals.onMClausesT
let onMHypsT = Tacticals.onMHypsT
let onAllHypsT = Tacticals.onAllHypsT
let onAllClausesT = Tacticals.onAllClausesT
let tryOnHypsT = Tacticals.tryOnHypsT
let tryOnClausesT = Tacticals.tryOnClausesT
let tryOnAllHypsT = Tacticals.tryOnAllHypsT
let tryOnAllClausesT = Tacticals.tryOnAllClausesT
let onAllMHypsT = Tacticals.onAllMHypsT
let onAllAssumT = Tacticals.onAllAssumT
let onAllMAssumT = Tacticals.onAllMAssumT
let tryOnAllMHypsT = Tacticals.tryOnAllMHypsT
let tryOnAllMClausesT = Tacticals.tryOnAllMClausesT
let onSomeAssumT = Tacticals.onSomeAssumT
let onSomeHypT = Tacticals.onSomeHypT

doc <:doc< ************************************************************************
   @begin[doc]
   @modsection{Labels}
  
   Each node in a proof tree has a @emph{label}.  The labels have no logical
   meaning, but they are frequently used to provide an informal description
   of the kind of subgoal.  A label can be any string, but there are three
   commonly used labels: ``@tt[main]'' identifies the main steps of a proof,
   ``@tt[antecedent]'' identifies nodes that are assumptions that have to be
   proved, and ``@tt[wf]'' identifies nodes that require well-formedness reasoning.
  
   @begin[description]
   @item{@tactic[addHiddenLabelT], @tactic[removeHiddenLabelT], @tactic[keepingLabelT];
   There are three tacticals that directly manipulate the label.
   The (@tt[addHiddenLabelT] ``label'') tactic assigns the label to
   the current goal, the @tt[removeHiddenLabelT] tactic assigns
   the label ``main'', and the (@tt[keepingLabelT] $@i[tac]$) applies
   the tactic $@i[tac]$ and assigns the label of the current goal
   to all of the remaining subgoals.  The ``@tt{Hidden}'' is of historical
   significance only@; the labels are hidden only in the sense that they
   have no logical significance.}
  
   @item{@tactic[ifLabT];
   In addition to manipulating the labels, there are several tacticals
   that take advantage of the label to apply tactics selectively.  The
   (@tt[ifLabT] $l$ $@i[tac1]$ $@i[tac2]$) applies the tactic $@i[tac1]$ if
   the current goal has label $l$, otherwise it applies the  tactic $@i[tac2]$.}
  
   @item{@tactic[thenMT], @tactic[thenET], @tactic[thenWT], @tactic[thenAT];
   The (infix) @tt[thenMT], @tt[thenET], and @tt[thenWT] are like the
   @tt[thenT] tactical, except that they apply their second argument
   only to the goals labeled ``@tt[main]'', ``@tt[equality]'', or ``@tt[wf]'' respectively.
   @tt[thenAT] applies its second argument only to the goal @emph{not}
   labeled ``@tt[main]''.}
  
   @item{@tactic[whileProgressMT], @tactic[untilFailMT], @tactic[repeatMT], @tactic[repeatMForT];
   These tactics repeat the argument tactic  only
   on the subgoals labeled ``@tt[main]''.}
   @end[description]
  
   @docoff
   @end[doc]
>>
let addHiddenLabelT = Tacticals.addHiddenLabelT
let removeHiddenLabelT = Tacticals.removeHiddenLabelT
let keepingLabelT = Tacticals.keepingLabelT
let ifLabT = Tacticals.ifLabT
let prefix_thenMT = Tacticals.prefix_thenMT
let prefix_thenMLT = Tacticals.prefix_thenMLT
let prefix_thenAT = Tacticals.prefix_thenAT
let prefix_thenALT = Tacticals.prefix_thenALT
let prefix_thenWT = Tacticals.prefix_thenWT
let prefix_thenET = Tacticals.prefix_thenET
let prefix_thenPT = Tacticals.prefix_thenPT
let repeatMT = Tacticals.repeatMT
let untilFailMT = Tacticals.untilFailMT
let whileProgressMT = Tacticals.whileProgressMT
let repeatMForT = Tacticals.repeatMForT
let seqOnMT = Tacticals.seqOnMT
let completeMT = Tacticals.completeMT
let labProgressT = Tacticals.labProgressT

let thinMatchT thinT assum =
   funT (fun p ->
   let goal = Sequent.goal p in
   let index = Match_seq.match_hyps
      (explode_sequent goal)
      (explode_sequent assum) in
   let rec tac j =
      if j = 0 then idT else
         match index.(pred j) with
            Some _ ->
               tac (pred j)
          | None ->
               thinT j thenT tac (pred j)
   in
      tac (Sequent.hyp_count p))

let nameHypT i v =
   funT (fun p ->
   let v = Var.maybe_new_var_arg p v in
   let i = Sequent.get_pos_hyp_num p i - 1 in
   let goal = Sequent.goal p in
   let eseq = explode_sequent goal in
   let eseq =
      match SeqHyp.get eseq.sequent_hyps i with
         Hypothesis hyp ->
            let map i' hyp' = if i = i' then HypBinding (v, hyp) else hyp' in
               { eseq with sequent_hyps = SeqHyp.mapi map eseq.sequent_hyps }
       | HypBinding (v',hyp) ->
            let vt = mk_var_term v in
            let s1 t = subst1 t v' vt in
            let map i' hyp' =
               if i'<i then hyp'
               else if i=i' then HypBinding(v,hyp)
               else begin match hyp' with
                  Hypothesis hyp' -> Hypothesis (subst1 hyp' v' vt)
                | HypBinding (vv,hyp') -> HypBinding (vv, subst1 hyp' v' vt)
                | Context (vv, ts) -> Context (vv, List.map s1 ts)
               end
            in
               { eseq with sequent_hyps = SeqHyp.mapi map eseq.sequent_hyps; 
                           sequent_goals = SeqGoal.lazy_apply s1 eseq.sequent_goals }
       | _ -> raise(RefineError("nameHypT",StringError "is a context"))
   in
   let goal = mk_sequent_term eseq in
   let a = Sequent.num_assums p + 1 in
      cutT goal thenLT [removeHiddenLabelT; nthAssumT a])

let rec nameHypsT is vs =
   match is, vs with
      [i], [v] -> nameHypT i v
    | i::is, v::vs -> nameHypT i v thenT nameHypsT is vs
    | _ -> raise (Invalid_argument("nameHypsT"))

(*
 * -*-
 * Local Variables:
 * Caml-master: "editor.run"
 * End:
 * -*-
 *)
