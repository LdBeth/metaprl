doc <:doc<
   @module[Top_tacticals]

   The @tt[Top_tacticals] module defines the primitive
   @emph{tactics} and @emph{tacticals} provided by the @MetaPRL prover.

   A @emph{tactic} is a basic building block of proof search in @MetaPRL. When
   a tactic is applied to a current @emph{proof goal}, one of the following
   things can happen:
   @begin[itemize]
   @item{The tactic will @emph{succeed} in fully proving the current goal.}
   @item{The tactic will @emph{succeed} in constructing a partial proof, creating a number
   of new @emph{subgoals}. In case one of the subgoals is identical
   to the original goal, we say that the tactic failed to make progress.}
   @item{The tactic might @emph{fail}, throwing a @tt[RefineError] exception.}
   @end[itemize]

   Whenever a new inference rule (either an axiom or a derived one) is added to the
   @MetaPRL system, a new simple tactic is created with the same name as the rule.
   This new tactic would simply attempt to do a single proof step by applying
   the corresponding inference rule. @emph{Tacticals} allow creation of more complicated
   tactics by combining existing tactics together.
   @docoff

   ----------------------------------------------------------------

   @begin[license]

   This file is part of MetaPRL, a modular, higher order
   logical framework that provides a logical programming
   environment for OCaml and other languages.

   See the file doc/htmlman/default.html or visit http://metaprl.org/
   for more information.

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
extends Perv

open Term_sig
open Refiner.Refiner.TermType
open Refiner.Refiner.Term
open Refiner.Refiner.TermSubst
open Refiner.Refiner.TermMan
open Refiner.Refiner.RefineError

open Tactic_type

(*
 * Dummy argument used for comments.
 *)
doc <:doc<
   @parents
>>
extends Mptop

doc <:doc<
   @modsection{Primitive tactics}

   @begin[description]
   @item{@tactic[idT];
   The @tt[idT] tactic is the @emph{identity}.

   $$
   @rulebox{idT; ;
     <<sequent[dummy_arg]{ <H> >- 'T}>>;
     <<sequent[dummy_arg]{ <H> >- 'T}>>}
   $$}
   @end[description]

   @docoff
>>
let idT = Tacticals.idT

doc <:doc<
   @begin[description]
   @item{@tactic[cutT];
   The @tt[cutT] tactic implements primitive lemma-instantiation on meta-level by
   allowing one to cut in an assumption.

   }
   @end[description]

   @docoff
>>
let cutT = Tacticals.cutT

doc <:doc<
   @begin[description]
   @item{@tactic[failT], @tactic[failwithT];
   The @tt[failT] tactic always fails, and the @tt[failWithT] fails
   with a specific message.}
   @end[description]

   @docoff
>>
let failT = Tacticals.failT
let failWithT = Tacticals.failWithT

doc <:doc<
   @begin[description]
   @item{@tactic[funT]; When @i[tac] is a function from @tt[tactic_arg] to @tt[tactic]
   (e.g. a function that takes a look at the current goal and chooses the tactic to apply
   based on what is being proven), then @tt[funT] @i[tac] would be the corresponding tactic.}
   @end[description]

   @docoff
>>
let funT = Tacticals.funT

doc <:doc<
   @begin[description]
   @item{@tactic[nthAssumT];
   The @tt[nthAssumT] tactic proves a goal by @emph{assumption}.
   Technically, an assumption is a subgoal of the theorem being proved.
   The assumption must be syntactically identical to the goal.

   $$
   @rulebox{nthAssumT; i;
    @cdot;
    <<sequent[dummy_arg]{ <H_1> >- 'T_1}>> @i{(Assumption@space 1)}@cr
    @ldots@cr
    <<sequent[dummy_arg]{ <H_i> >- 'T_i}>> @i{(Assumption@space @i{i})}@cr
    @ldots@cr
    <<sequent[dummy_arg]{ <H_n> >- 'T_n}>> @i{(Assumption@space @i{n})}@cr
    @hline
    <<sequent[dummy_arg]{ <H_i> >- 'T_i}>>}
   $$}
   @end[description]

   @docoff
>>
let nthAssumT = Tacticals.nthAssumT

doc <:doc<
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
   Similarly, the @tt[seqT] tactical is a variant of @tt[thenT].
   The (@tt{seqT $[@i[tac]_1; @ldots; @i[tac]_n]$}) tactic is equivalent to
   (@tt{$@i[tac]_1$ thenT $@cdots$ thenT $@i[tac]_n$}).  The @tt[seqOnSameConclT]
   tactic is the same as @tt[seqT] except that it selects only those subgoals
   that have the same conclusion as the current goal.}
   @end[description]

   @docoff
>>
let prefix_orelseT = Tacticals.prefix_orelseT
let prefix_andalsoT = Tacticals.prefix_andalsoT
let prefix_orthenT = Tacticals.prefix_orthenT
let firstT = Tacticals.firstT
let prefix_thenLT = Tacticals.prefix_thenLT
let prefix_thenFLT = Tacticals.prefix_thenFLT
let seqT = Tacticals.seqT
let seqOnSameConclT = Tacticals.seqOnSameConclT
let prefix_thenT = Tacticals.prefix_thenT

infix orelseT
infix andalsoT
infix orthenT
infix thenLT
infix thenFLT
infix thenT

doc <:doc<
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
   and the @tt[then_OnSameConclT] tactic chooses the subgoals with the
   same conclusion as the current goal.}
   @end[description]

   @docoff
>>
let prefix_then_OnFirstT = Tacticals.prefix_then_OnFirstT
let prefix_then_OnLastT = Tacticals.prefix_then_OnLastT
let prefix_then_OnSameConclT = Tacticals.prefix_then_OnSameConclT

infix then_OnFirstT
infix then_OnLastT
infix then_OnSameConclT

doc <:doc<
   @begin[description]
   @item{@tactic[timingT];
   The @tt[timingT] tactical applies its argument,
   and prints timing information useful for tactic profiling.

   $$
   @rulebox{timingT idT; ;
     <<sequent[dummy_arg]{ <H> >- 'T}>>;
     <<sequent[dummy_arg]{ <H> >- 'T}>>}
   $$

   @code{User time 0.000000; System time 0.000000; Real time 0.001778}}
   @end[description]

   @docoff
>>
let timingT = Tacticals.timingT

doc <:doc<
   @begin[description]
   @item{@tactic[tryT], @tactic[completeT];
   The @tt[tryT] tactical applies its argument, and performs
   the identity if the tactic fails.  The tactic (@tt[tryT] @i[tac])
   is equivalent to (@i[tac] @tt{orelseT idT}). The (@tt[completeT] $@i[tac]$)
   tactic applies its argument and succeeds only when $@i[tac]$ proves the goal
   completely, producing no subgoals.}
   @end[description]

   @docoff
>>
let tryT = Tacticals.tryT
let completeT = Tacticals.completeT
let forceT = Tacticals.forceT

doc <:doc<
   @begin[description]
   @item{@tactic[progressT];
   The (@tt[progressT] $@i[tac]$) tactic applies its argument and fails
   if either $@i[tac]$ fails, or $@i[tac]$ fails to make ``progress''
   by generating subgoals that differ from the current goal.}
   @end[description]

   @docoff
>>
let progressT = Tacticals.progressT

doc <:doc<
   @begin[description]
   @item{@tactic[whileProgressT], @tactic[untilFailT], @tactic[repeatT], @tactic[repeatForT];
   The (@tt[untilFailT] $@i[tac]$) tactic applies the argument $@i[tac]$
   repeatedly to the current goal and all of the generated subgoals until
   an application of $@i[tac]$ fails. This is the same as the @tt[REPEAT] tactical in @Nuprl.
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
>>
let repeatT = Tacticals.repeatT
let whileProgressT = Tacticals.whileProgressT
let untilFailT = Tacticals.untilFailT
let repeatForT = Tacticals.repeatForT

doc <:doc<
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
     <<sequent[dummy_arg]{ <H>; x: 'T_2; <J> >- <:doc< <<'T_1<|H|> >> @space @i{Type}>>}>> @cr
     <<sequent[dummy_arg]{ <H>; x: 'T_2; <J> >- 'T_2<|H|>}>>;
     <<sequent[dummy_arg]{ <H>; x: 'T_2; <J> >- <:doc< <<'T_1<|H|> >> @vee <<'T_2<|H|> >> >>}>>}
   $$}
   @end[description]

   @docoff
>>
let addTermT = Tacticals.addTermT
let addTypeT = Tacticals.addTypeT
let addBoolT = Tacticals.addBoolT
let addIntT = Tacticals.addIntT
let addT = Tacticals.addT

let withTermT = Tacticals.withTermT
let withTypeT = Tacticals.withTypeT
let withBoolT = Tacticals.withBoolT
let withIntT = Tacticals.withIntT
let withT = Tacticals.withT
let withTermsT = Tacticals.withTermsT

let removeTermT = Tacticals.removeTermT
let removeTypeT = Tacticals.removeTypeT
let removeBoolT = Tacticals.removeBoolT
let removeIntT = Tacticals.removeIntT

let withoutTermT = Tacticals.withoutTermT
let withoutTypeT = Tacticals.withoutTypeT
let withoutBoolT = Tacticals.withoutBoolT
let withoutIntT = Tacticals.withoutIntT

let atT = Tacticals.atT
let selT = Tacticals.selT
let altT = Tacticals.altT
let thinningT = Tacticals.thinningT
let doNotThinT = thinningT false

doc <:doc<
   @modsection{Clause selection}

   The following tactics are intended for use in a single-conclusion sequent calculus.
   A sequent <<sequent[dummy_arg]{x: 'T_1; math_cdots; x: 'T_n >- 'C}>> has
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
   (@i[tac] $i_1$ @tt[thenT] $@cdots$ @tt[thenT] @i[tac] $i_n$).  The
   @tt[onHypsT] does the same, but it requires that the indices correspond
   to valid hypothesis numbers.}

   @item{@tactic[onAllClausesT], @tactic[onAllHypsT];
   The (@tt[onAllClausesT] @i[tac]) applies the argument tactic to all the clauses,
   including all the hypotheses and the conclusion.  The @tt[onAllHypsT] applies the
   argument only to the hypotheses.}

   @item{@tactic[onAllAssumT];
   The (@tt[onAllAssumT] @i[tac]) applies the argument tactic to all the assumptions.}

   @item{@tactic[onSomeHypT];
   The (@tt[onSomeHypT] @i[tac]) applies the argument tactic to the
   hypotheses from the last to the first, returning once an application
   succeeds.  The (@tt[onSomeHypT] @i[tac]) is equivalent to
   (@i[tac] $n$ @tt[orelseT] $@cdots$ @tt[orelseT] @i[tac] $1$).}
   @end[description]

   @docoff
>>
let onClauseT = Tacticals.onClauseT
let onHypT = Tacticals.onHypT
let onConclT = Tacticals.onConclT
let onClausesT = Tacticals.onClausesT
let onHypsT = Tacticals.onHypsT
let onMClausesT = Tacticals.onMClausesT
let onMHypsT = Tacticals.onMHypsT
let onAllHypsT = Tacticals.onAllHypsT
let onAllCumulativeHypsT = Tacticals.onAllCumulativeHypsT
let onAllClausesT = Tacticals.onAllClausesT
let tryOnHypsT = Tacticals.tryOnHypsT
let tryOnClausesT = Tacticals.tryOnClausesT
let tryOnAllHypsT = Tacticals.tryOnAllHypsT
let tryOnAllCumulativeHypsT = Tacticals.tryOnAllCumulativeHypsT
let tryOnAllClausesT = Tacticals.tryOnAllClausesT
let onAllMHypsT = Tacticals.onAllMHypsT
let onAllMCumulativeHypsT = Tacticals.onAllMCumulativeHypsT
let onAllAssumT = Tacticals.onAllAssumT
let onAllMAssumT = Tacticals.onAllMAssumT
let tryOnAllMHypsT = Tacticals.tryOnAllMHypsT
let tryOnAllMCumulativeHypsT = Tacticals.tryOnAllMCumulativeHypsT
let tryOnAllMClausesT = Tacticals.tryOnAllMClausesT
let onSomeAssumT = Tacticals.onSomeAssumT
let onSomeHypT = Tacticals.onSomeHypT
let onAnyHypT = Tacticals.onAnyHypT

doc <:doc<
   @modsection{Labels}

   Each node in a proof tree has a @emph{label}.  The labels have no logical
   meaning, but they are frequently used to provide an informal description
   of the kind of subgoal.  A label can be any string, but there are four
   commonly used labels: ``@tt[main]'' identifies the main steps of a proof,
   ``@tt[assertion]'' identifies nodes that are assumptions that have to be
   proved, ``@tt[wf]'' identifies nodes that require well-formedness reasoning, and
   ``@tt[equality]'' identifies nodes that require equality reasoning.

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
   The infix tacticals @tt[thenMT], @tt[thenET], and @tt[thenWT] are like the
   @tt[thenT] tactical, except that they apply their second argument
   only to the goals labeled ``@tt[main]'', ``@tt[equality]'', or ``@tt[wf]'' respectively.
   @tt[thenAT] applies its second argument only to the goal @emph{not}
   labeled ``@tt[main]''.}

   @item{@tactic[whileProgressMT], @tactic[untilFailMT], @tactic[repeatMT], @tactic[repeatMForT];
   These tactics repeat the argument tactic  only
   on the subgoals labeled ``@tt[main]''.}
   @end[description]

   @docoff
>>
let addHiddenLabelT = Tacticals.addHiddenLabelT
let removeHiddenLabelT = Tacticals.removeHiddenLabelT
let keepingLabelT = Tacticals.keepingLabelT
let ifLabT = Tacticals.ifLabT
let ifWT = Tacticals.ifWT
let ifAT = Tacticals.ifAT
let ifMT = Tacticals.ifMT
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

infix thenMT
infix thenMLT
infix thenAT
infix thenALT
infix thenWT
infix thenET
infix thenPT

let thinMatchT thin_many eq assum =
   funT (fun p ->
   let goal = Sequent.goal p in
   let index = Match_seq.match_hyps
      eq
      (explode_sequent goal)
      (explode_sequent assum)
   in
   let rec tac i j =
      if i = 0 then if j > 1 then thin_many 1 j else Tacticals.idT else
         match index.(pred i) with
            Some _ ->
               let tac = tac (pred i) 1 in
                  if j > 1 then thin_many (succ i) j thenT tac else tac
          | None ->
               tac (pred i) (succ j)
   in
      tac (Sequent.hyp_count p) 1)

let nameHypT i v =
   funT (fun p ->
   let v = Lm_symbol.add v in
   let i = Sequent.get_pos_hyp_num p i - 1 in
   let eseq = Sequent.explode_sequent_arg p in
      match SeqHyp.get eseq.sequent_hyps i with
         Hypothesis (v',hyp) ->
            if Lm_symbol.eq v' v then
                Tacticals.idT
            else
               let vt = mk_var_term v in
               let s1 t = subst1 t v' vt in
               let map i' hyp' =
                  if i'<i then hyp'
                  else if i=i' then Hypothesis(v,hyp)
                  else begin match hyp' with
                     Hypothesis (vv,hyp') -> Hypothesis (vv, subst1 hyp' v' vt)
                   | Context (vv, conts, ts) -> Context (vv, conts, List.map s1 ts)
                  end
               in
               let eseq = { eseq with sequent_hyps = SeqHyp.mapi map eseq.sequent_hyps;
                                      sequent_concl = subst1 eseq.sequent_concl v' vt } in
               let goal = mk_sequent_term eseq in
               let a = Sequent.num_assums p + 1 in
                  tryT (cutT goal thenLT [removeHiddenLabelT; nthAssumT a])
       | Context _ ->
            raise(RefineError("nameHypT", StringError "is a context")))

let rec nameHypsT is vs =
   match is, vs with
      [i], [v] -> nameHypT i v
    | i::is, v::vs -> nameHypT i v thenT nameHypsT is vs
    | _ -> raise (Invalid_argument("nameHypsT"))

(*
 * Merging tactics in int -> tactic tables
 *)
(* unused
let rec first_with_argT i = function
   [] -> raise (Invalid_argument "Dtactic.first_with_argT")
 | [tac] -> tac i
 | tac :: tacs -> tac i orelseT first_with_argT i tacs
*)

(*
 * -*-
 * Local Variables:
 * Caml-master: "editor.run"
 * End:
 * -*-
 *)
