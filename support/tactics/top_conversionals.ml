doc <:doc< 
   @begin[spelling]
   addrC applyAllC conv convs higherC idC orelseC rw rwa rwh thenC th
   @end[spelling]
  
   @begin[doc]
   @module[Top_conversionals]
  
   Conversionals are analog of tactics  (Section~@refmodule[Top_tacticals])
   for rewriting.  Conversionals are used extensively in the @Nuprl
   type theory (Section @refmodule[Itt_theory]) to express and
   apply computational equivalences.  The @tt{Top_conversionals}
   module defines the basic conversionals provided by the @MetaPRL
   prover.
  
   Each @bf{rewrite} definition in a module defines a conversion.
   For example, the definition of beta reduction in the @Nuprl type
   theory (Section @refmodule[Itt_rfun]), is defined as follows:
  
   @begin[center]
   @bf{rewrite} unfold_beta : $(@lambda x. b[x])@space a @longleftrightarrow b[a]$
   @end[center]
  
   This declaration defines a conversion called @tt[unfold_beta] that can
   be applied with the function @tt[rwh], which searches for the outermost
   valid applications of the rewrite.  Here is an example proof step:
  
   $$
   @rulebox{rwh; @tt[unfold_beta]@space 0;
     <<sequent(nil){ <H> >- <:doc< ((@lambda v. v + 1)@space 2) = 3 @in @int>>}>>;
     <<sequent(nil){ <H> >- <:doc< 2 + 1 = 3 @in @int>> }>> }
   $$
  
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
  
   Author: Jason Hickey
   @email{jyh@cs.caltech.edu}
  
   @end[license]
>>

doc <:doc< 
   @begin[doc]
   @parents
   @end[doc]
>>
extends Mptop
doc <:doc< @docoff >>

open Mp_debug
open Printf

open Refiner.Refiner.TermType
open Refiner.Refiner.Term
open Refiner.Refiner.TermSubst
open Refiner.Refiner.RefineError

open Mp_resource
open Simple_print
open Term_match_table

open Tactic_type.Rewrite
open Tactic_type.Conversionals
open Tactic_type.Sequent

(*
 * Debug statement.
 *)
let _ =
   show_loading "Loading Top_conversionals%t"

let debug_conv =
   create_debug (**)
      { debug_name = "conv";
        debug_description = "display conversion operation";
        debug_value = false
      }

let debug_reduce =
   create_debug (**)
      { debug_name = "reduce";
        debug_description = "display reductions";
        debug_value = false
      }

doc <:doc< 
   @begin[doc]
   @modsection{Conversion application}
  
   @begin[description]
   @item{@conv[rw];
   Conversions are not tactics: they have a different type @tt[conv]
   and they are applied differently.  The basic method for applying
   a conversion is to use @tt[rw], which converts a conversion to
   a tactic applied to a specific clause in a sequent (these functions
   are defined only for a sequent calculus).  The (@tt[rw] @it[conv] $i$)
   @emph{tactic} applies the conversion @it[conv] to clause $i$ in
   the current goal sequent.}
  
   @item{@conv[rwc];
   Conversions may be applied also to assumptions.
   The (@tt[rwc] @it[conv] $a$ $c$) @emph{tactic} applies the
   conversion @it[conv] to the $c$-th clause in the $a$-th assumption.}
  
   @item{@conv[rwAll] @conv[rwcAll] @conv[rwAllAll];
   The (@tt[rwAll] @it[conv]) @emph{tactic} applies the
   conversion @it[conv] to the whole goal sequent.
  
   The (@tt[rwcAll] @it[conv] $a$) @emph{tactic} applies the
   conversion @it[conv] to the whole $a$-th assumption.
  
   The (@tt[rwAllAll] @it[conv]) @emph{tactic} applies the
   conversion @it[conv] to all assumptions and to the goal sequent.}
   @end[description]
  
   @docoff
   @end[doc]
>>
let rw = Tactic_type.Conversionals.rw
let rwc = Tactic_type.Conversionals.rwc
let rwAll = Tactic_type.Conversionals.rwAll
let rwcAll = Tactic_type.Conversionals.rwcAll
let rwAllAll = Tactic_type.Conversionals.rwAllAll

let rwh = Tactic_type.Conversionals.rwh
let rwch = Tactic_type.Conversionals.rwch
let rwhAll = Tactic_type.Conversionals.rwhAll
let rwchAll = Tactic_type.Conversionals.rwchAll
let rwhAllAll = Tactic_type.Conversionals.rwhAllAll

let rwa = Tactic_type.Conversionals.rwa
let rwca = Tactic_type.Conversionals.rwca
let rwaAll = Tactic_type.Conversionals.rwaAll
let rwcaAll = Tactic_type.Conversionals.rwcaAll
let rwaAllAll = Tactic_type.Conversionals.rwaAllAll


doc <:doc< 
   @begin[doc]
   @modsection{Primitive conversions}
  
   @begin[description]
   @item{@conv[idC], @conv[failC];
   The @tt[idC] conversion is the identity conversion: no rewriting
   is performed.  The @tt[failC] conversion always fails.}
   @end[description]
  
   @docoff
   @end[doc]
>>
let idC = Tactic_type.Conversionals.idC
let failC = Tactic_type.Conversionals.failC
let failWithC = Tactic_type.Conversionals.failWithC

doc <:doc< 
   @begin[doc]
   @modsection{Conversionals}
  
   @begin[description]
   @item{@conv[thenC], @conv[orelseC];
   Conversionals can be combined in the same manner as tactics.
   The (@tt{$c_1$ thenC $c_2$}) conversion first applies conversion
   $c_1$, and then applies $c_2$ to the result term.  The (@tt{$c_1$ orelseC $c_2$})
   conversion first applies $c_1$@; if $c_1$ fails (because the conversion does not
   match the term being rewritten, or because of a call to @tt[failC]), $c_2$ is
   applied instead.}
  
   @item{@conv[tryC], @conv[firstC];
   There are several variations on @tt[orelseC].  The (@tt[tryC] $c$) conversion
   is equivalent to ($c$ orelseC idC).  The @tt[firstC] conversion takes a list of
   conversions to try in order until the first one succeeds.  The conversion (@tt[firstC]
   $[c_1; @cdots; c_n]$) is equivalent to @tt{$c_1$ orelseC $@cdots$ orelseC $c_n$}.}
  
   @item{@conv[untilFailC];
   The (@tt[untilFailC] $c$) conversion applies conversion $c$ repeatedly
   until it fails. It catches all exception and never fails itself}
  
   @item{@conv[whileProgressC];
   The (@tt[whileProgressC] $c$) conversion applies conversion $c$ repeatedly
   while it makes a progress. If $c$ fails then  @tt[whileProgressC] also fails.}
  
   @item{@conv[repeatC];
   The (@tt[repeatC] $c$) conversion applies conversion $c$ repeatedly
   until it fails, or until it fails to make progress.}
   @end[description]
  
   @docoff
   @end[doc]
>>
let prefix_thenC = Tactic_type.Conversionals.prefix_thenC
let prefix_orelseC = Tactic_type.Conversionals.prefix_orelseC
let tryC = Tactic_type.Conversionals.tryC
let firstC = Tactic_type.Conversionals.firstC
let untilFailC = Tactic_type.Conversionals.untilFailC
let whileProgressC = Tactic_type.Conversionals.whileProgressC
let repeatC = Tactic_type.Conversionals.repeatC
let repeatForC = Tactic_type.Conversionals.repeatForC

doc <:doc< 
   @begin[doc]
   @modsection{Addressing and search}
  
   Generally, the terms to be rewritten do not occur at the outermost
   level of a clause.  The following conversionals recursively search
   through the subterms of a clause for applicable rewrites.
  
   @begin[description]
   @item{@conv[someSubC];
   The most general of these is the (@tt[someSubC]  $c$) conversion,
   which tries applying conversion $c$ to all of the immediate subterms of
   the clause.  It succeeds if $c$ succeeds on any of the subterms@; it
   fails otherwise.  The conversion @tt[allSubC] requires success on
   @emph{all} of the immediate subterms.}
  
   @item{@conv[addrC];
   Subterms can also be addressed explicitly with the (@tt{addrC @it[addr] $c$})
   conversion, although the use is discouraged.  The address is an integer list
   that describes the @emph{path} leading to the term to be rewritten.  For
   example, the address $[ ]$ is the identity address, $[0]$ is its leftmost
   subterm, $[0; 1]$ is the second subterm of the first subterm, @i[etc].  Addresses
   are fragile, and correct addresses are difficult to discover.  For this
   reason, the @tt[addrC] conversion is almost never used.}
  
   @item{@conv[higherC];
   The (@tt[higherC] $c$) conversion searches for the outermost
   occurrences of subterms in the clause where conversion $c$
   applies.  It's definition uses @tt[someSubC].
  
   @begin[center]
   @code{let rec higherC c = c orelseC (someSubC (higherC c))}
   @end[center]}
  
   @item{@conv[lowerC], @conv[sweepDnC];
   The @tt[lowerC] conversional searches for the @emph{innermost}
   rewrite occurrences.  The (@tt[sweepDnC] $c$) conversion applies
   $c$ from the outermost to the innermost subterms.
  
   @begin[center]
   @code{let rec sweepDnC c = (tryC c) andalsoC (someSubC (sweepDnC c))}
   @end[center]}
  
   @item{@conv[sweepUpC];
   The @tt[sweepUpC] conversion works from the innermost to outermost subterms.
   Note that these conversions never fail@; however they may fail to
   make progress if the conversion $c$ never succeeds.}
  
   @item{@conv[applyAllC];
   The @tt[applyAllC] conversion takes a list of conversions
   and applies them to all subterms possible from outermost to
   innermost (it applies at most one conversion from the list at most once
   to each subterm).
   @begin[center]
   @code{let applyAllC convs = sweepUpC (firstC convs)}
   @end[center]}
  
   @item{@conv[rwh], @conv[rwch], @conv[rwhAll], @conv[rwchAll], @conv[rwhAllAll];
   For convenience, the @tt[rwh],  @tt[rwch], @tt[rwhAll], @tt[rwchAll],
   @tt[rwhAllAll] functions automatically
   apply the @tt[higherC] conversion. For example, the tactic (@tt{rwh $conv$ $i$})
   is equivalent to (@tt{rw (higherC $conv$) $i$}).}
  
   @item{@conv[rwa], @conv[rwca], @conv[rwaAll], @conv[rwcaAll], @conv[rwaAllAll];
   The @tt[rwa],  @tt[rwca], @tt[rwaAll], @tt[rwcaAll],
   @tt[rwaAllAll] functions takes a list of conversions and
   apply the @tt[applyAllC] conversion. For example, the tactic (@tt{rwa $convs$ $i$})
   is equivalent to (@tt{rw (applyAllC $convs$) $i$}).}
  
   @end[description]
  
   @docoff
   @end[doc]
>>
let someSubC = Tactic_type.Conversionals.someSubC
let allSubC = Tactic_type.Conversionals.allSubC
let higherC = Tactic_type.Conversionals.higherC
let lowerC = Tactic_type.Conversionals.lowerC
let sweepUpC = Tactic_type.Conversionals.sweepUpC
let sweepDnC = Tactic_type.Conversionals.sweepDnC
let applyAllC = Tactic_type.Conversionals.applyAllC

doc <:doc< 
   @begin[doc]
   @modsection{Conversion reversal}
  
   Computational rewrites define a congruence, and all of the
   equivalence relations hold, including reversing the application
   of the rewrite.  However, reversed rewrites are usually incompletely
   specified.
  
   @begin[description]
   @item{@conv[foldC], @conv[cutC];
   The (@tt[foldC] $t$ $c$) takes a term $t$ and a conversion that
   rewrites the term in the @emph{forward} direction, and generates
   reversed conversion.  For example, here is a reverse application of
   the beta rewrite.
  
   $$
   @rulebox{rwh; (@tt[foldC]@space (@lambda v. v + 1)@space 2@space @tt[unfold_beta])@space 0;
     <<sequent(nil){ <H> >- <:doc<2 + 1 = 3 @in @int>>}>>;
     <<sequent(nil){ <H> >- <:doc< ((@lambda v. v + 1)@space 2) = 3 @in @int>> }>>}
   $$
  
   @noindent
   The @tt[cutC] conversion is used to replace a term and generate a
   rewrite obligation.
  
   $$
   @rulebox{rw; (@tt[addrC]@space{} [1]@space (@tt[cutC]@space 3))@space 0;
     <<sequent(nil){ <H> >- <:doc< 3 = 3 @in @int>> }>> @cr
     <<sequent(nil){ <H> >- <:doc< ((@lambda v. v + 1)@space 2) @longleftrightarrow 3>>}>>;
     <<sequent(nil){ <H> >- <:doc< ((@lambda v. v + 1)@space 2) = 3 @in @int>>}>>}
   $$}
   @end[description]
  
   @docoff
   @end[doc]
>>
let addrC = Tactic_type.Conversionals.addrC
let foldC = Tactic_type.Conversionals.foldC
let makeFoldC = Tactic_type.Conversionals.makeFoldC
let cutC = Tactic_type.Conversionals.cutC

(************************************************************************
 * REDUCTION RESOURCE                                                   *
 ************************************************************************)

doc <:doc< 
   @begin[doc]
   @resources
  
   @bf{The @Comment!resource[reduce_resource]}
  
   The @tt{reduce} resource provides a generic method for
   defining @emph{evaluation}.  The @conv[reduceC] conversion
   can be used to apply this evaluator.
  
   For example, the @Nuprl type theory describes several
   generic reductions:
   @begin[description]
   @item{beta; $(@lambda v. b[v])@space a @longleftrightarrow b[a]$}
   @item{pair; $(@bf{match}@space (a, b)@space @bf{with}@space u, v @rightarrow c[u, v]) @longleftrightarrow c[a, b]$}
   @item{union; $(@bf{match}@space @i[inl](a)@space @bf{with}@space
                  @i[inl](u) @rightarrow b[u]
                  | @i[inr](v) @rightarrow c[v]) @longleftrightarrow b[a]$}
   @end[description]
  
   Each of the modules for functions (Section @refmodule[Itt_rfun]),
   tuples (Section @refmodule[Itt_dprod]), and union (Section @refmodule[Itt_union]),
   defines an addition to the @hrefresource[reduce_resource]: the @hrefmodule[Itt_rfun] adds
   the @hrefrewrite[reduce_beta] rewrite with redex $(@lambda v. b[v])@space a$@; the
   @hrefmodule[Itt_dprod] adds the @hrefrewrite[reduceSpread] rewrite with redex
   $(@bf{match}@space (a, b)@space @bf{with}@space u, v @rightarrow c[u, v])$@; and the
   @hrefmodule[Itt_union] adds the @hrefrewrite[reduceDecideInl] rewrite with
   redex $(@bf{match}@space @i[inl](a)@space @bf{with}@space
                  @i[inl](u) @rightarrow b[u]
                  | @i[inr](v) @rightarrow c[v])$
  
   In modules that @tt{extends} these three theories, the @tt[reduceC]
   conversion will recursively search for applications of these three
   rewrites in an attempt to fully reduce the term.
  
   The implementation of the @tt{reduce_resource} and the @tt[reduceC]
   conversion rely on tables to store the shape of redices, together with the
   conversions for the reduction.
  
   @docoff
   @end[doc]
>>
let identity x = x

let extract_data tbl =
   let rw e =
      let t = env_term e in
      let conv =
         try
            (* Find and apply the right tactic *)
            if !debug_reduce then
               eprintf "Conversionals: lookup %a%t" debug_print t eflush;
            snd (Term_match_table.lookup tbl t)
         with
            Not_found ->
               raise (RefineError ("Conversionals.extract_data", StringTermError ("no reduction for", t)))
      in
         if !debug_reduce then
            eprintf "Conversionals: applying %a%t" debug_print t eflush;
         conv
   in
      funC rw

let process_reduce_resource_rw_annotation = redex_and_conv_of_rw_annotation "reduce"

(*
 * Resource.
 *)
let resource reduce =
   table_resource_info identity extract_data

let reduceTopC_env e =
   get_resource_arg (env_arg e) get_reduce_resource

let reduceTopC = funC reduceTopC_env

let reduceC =
   repeatC (higherC reduceTopC)

(*
 * Debugging.
 *)
let create_iform =
   Tactic_type.Conversionals.create_iform

let apply_rewrite =
   Tactic_type.Conversionals.apply_rewrite

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
