doc <:doc<
   @module[Top_conversionals]

   @emph{Conversions} and @emph{conversionals} are analogs of tactics and tacticals
   (Section~@refmodule[Top_tacticals])
   for rewriting.  Conversions are used extensively in Computational Type Theory
   (Section @refmodule[Itt_theory]) to express and
   apply computational equivalences.  The @tt{Top_conversionals}
   module defines the basic conversionals provided by the @MetaPRL
   prover.

   Each @bf{rewrite} definition in a module defines a conversion.
   For example, the definition of beta reduction in the Type
   Theory (Section @refmodule[Itt_dfun]), is defined as follows:

   @begin[center]
   @bf{rewrite} unfold_beta : $(@lambda x. b[x])@space a @longleftrightarrow b[a]$
   @end[center]

   This declaration defines a conversion called @tt[unfold_beta] that can
   be applied with the function @tt[rwh], which searches for the outermost
   valid applications of the rewrite.  Here is an example proof step:

   $$
   @rulebox{rwh; @tt[unfold_beta]@space 0;
     <<sequent [dummy_arg] { <H> >- <:doc< ((@lambda v. v + 1)@space 2) = 3 @in @int>>}>>;
     <<sequent [dummy_arg] { <H> >- <:doc< 2 + 1 = 3 @in @int>> }>> }
   $$

   @docoff
   ----------------------------------------------------------------

   @begin[license]

   This file is part of MetaPRL, a modular, higher order
   logical framework that provides a logical programming
   environment for OCaml and other languages.

   See the file doc/htmlman/default.html or visit http://metaprl.org/
   for more information.

   Copyright (C) 1998-2006 MetaPRL Group, Cornell University and
   California Institute of Technology

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
   Modified by: Aleksey Nogin @email{nogin@cs.caltech.edu}

   @end[license]
>>

doc <:doc<
   @parents
>>
extends Perv
extends Mptop
doc docoff

open Lm_debug
open Lm_printf

open Rewrite_sig
open Refiner.Refiner
open Refiner.Refiner.TermType
open Refiner.Refiner.Term
open Refiner.Refiner.TermAddr
open Refiner.Refiner.TermMan
open Refiner.Refiner.RefineError
open Refiner.Refiner.Rewrite

open Term_match_table

open Tactic_type.Tacticals
open Tactic_type.Tactic
open Tactic_type.Conversionals
open Tactic_type.Sequent

open Options_boot
open Top_options

(*
 * Debugging.
 *)
(*
let debug_conv =
   create_debug (**)
      { debug_name = "conv";
        debug_description = "display conversion operation";
        debug_value = false
      }
*)

let debug_reduce =
   create_debug (**)
      { debug_name = "reduce";
        debug_description = "display reductions";
        debug_value = false
      }

doc <:doc<
   @modsection{Conversion application}

   @begin[description]
   @item{@conv[rw];
   Conversions are not tactics: they have a different type @tt[conv]
   and they are applied differently.  The basic method for applying
   a conversion is to use @tt[rw], which converts a conversion to
   a tactic applied to a specific clause in a sequent (these functions
   are defined only for a sequent calculus).  The (@tt[rw] @it[conv] $i$)
   tactic applies the conversion @it[conv] to clause $i$ in
   the current goal sequent.}

   @item{@conv[rwc];
   Conversions may be applied also to assumptions.
   The (@tt[rwc] @it[conv] $a$ $c$) tactic applies the
   conversion @it[conv] to the $c$-th clause in the $a$-th assumption.}

   @item{@conv[rwAll] @conv[rwcAll] @conv[rwAllAll];
   The (@tt[rwAll] @it[conv]) tactic applies the
   conversion @it[conv] to the whole goal sequent.

   The (@tt[rwcAll] @it[conv] $a$) tactic applies the
   conversion @it[conv] to the whole $a$-th assumption.

   The (@tt[rwAllAll] @it[conv]) tactic applies the
   conversion @it[conv] to all assumptions and to the goal sequent.}
   @end[description]

   @docoff
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
   @modsection{Primitive conversions}

   @begin[description]
   @item{@conv[idC], @conv[failC];
   The @tt[idC] conversion is the identity conversion: no rewriting
   is performed.  The @tt[failC] conversion always fails.}
   @end[description]

   @docoff
>>
let idC = Tactic_type.Conversionals.idC
let failC = Tactic_type.Conversionals.failC
let failWithC = Tactic_type.Conversionals.failWithC
let forceC = Tactic_type.Conversionals.forceC

doc <:doc<
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
   is equivalent to (@tt{$c$ orelseC idC}).  The @tt[firstC] conversion takes a list of
   conversions to try in order until the first one succeeds.  The conversion (@tt[firstC]
   $[c_1; @cdots; c_n]$) is equivalent to @tt{$c_1$ orelseC $@cdots$ orelseC $c_n$}.}

   @item{@conv[progressC];
   The (@tt[progressTC] $@i[conv]$) conversion applies its argument and fails
   if either $@i[conv]$ fails, or $@i[conv]$ convert the term to the alpha-equal term.}

   @item{@conv[untilFailC];
   The (@tt[untilFailC] $c$) conversion applies conversion $c$ repeatedly
   until it fails. It catches all exception and never fails itself.}

   @item{@conv[repeatC];
   The (@tt[repeatC] $c$) conversion applies conversion $c$ repeatedly
   until it fails, or until it fails to make progress.}

   @item{@conv[ifEqualC];
   The (@tt[ifEqualC] $t$ $c1$ $c2$) conversion applies conversion $c1$ if the term its applied to is alpha equal to $t$ and
   $c2$ otherwise.}

   @item{@conv[replaceUsingC];
   The (@tt[replaceUsingC] $t$ $c$) conversion applies conversion $c$ to the term $t$ and fails on any other term.}

   @end[description]

   @docoff
>>
let prefix_thenC = Tactic_type.Conversionals.prefix_thenC
let prefix_orelseC = Tactic_type.Conversionals.prefix_orelseC
let tryC = Tactic_type.Conversionals.tryC
let firstC = Tactic_type.Conversionals.firstC
let untilFailC = Tactic_type.Conversionals.untilFailC
let repeatC = Tactic_type.Conversionals.repeatC
let repeatForC = Tactic_type.Conversionals.repeatForC
let ifEqualC = Tactic_type.Conversionals.ifEqualC
let progressC = Tactic_type.Conversionals.progressC
let replaceUsingC = Tactic_type.Conversionals.replaceUsingC
let allSubThenC = Tactic_type.Conversionals.allSubThenC
let prefix_thenTC = Tactic_type.Conversionals.prefix_thenTC

infix thenC
infix orelseC
infix thenTC

doc <:doc<
   @modsection{Addressing and search}

   Generally, the terms to be rewritten do not occur at the outermost
   level of a clause.  The following conversionals recursively search
   through the subterms of a clause for applicable rewrites.

   @begin[description]
   @item{@conv[someSubC], @conv[allSubC];
   The most general of these is the (@tt[someSubC]  $c$) conversion,
   which tries applying conversion $c$ to all of the immediate subterms of
   the clause.  It succeeds if $c$ succeeds on any of the subterms@; it
   fails otherwise.  The conversion @tt[allSubC] requires success on
   @emph{all} of the immediate subterms.}

   @item{@conv[allSubThenC];
   @tt[allSubThenC] $c1$ $c2$ tries to apply $c1$ to every immediate subterm.
   If it succeed in at least one case then applies $c2$, otherwise fails .}

   @item{@conv[addrC];
   Subterms can also be addressed explicitly with the (@tt{addrC @it[addr] $c$})
   conversion.  The address is an integer list
   that describes the @emph{path} leading to the term to be rewritten.  For
   example, the address $[ ]$ is the identity address, $[1]$ is its leftmost
   subterm, $[1; 2]$ is the second subterm of the first subterm, @i[etc].
   However addresses are somewhat fragile, and correct addresses can be difficult
   to discover. For this reason, the use of @tt[addrC] is discouraged.}

   @item{@conv[higherC];
   The (@tt[higherC] $c$) conversion searches for the outermost
   occurrences of subterms in the clause where conversion $c$
   applies.  Its definition uses @tt[someSubC].

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

   @item{@conv[findThenC];
   The @tt[findThenC] conversion find the outermost term that matches a predicate
   and applies a conversion at that point.}

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
   @tt[rwaAllAll] functions take a list of conversions and
   apply the @tt[applyAllC] conversion. For example, the tactic (@tt{rwa $convs$ $i$})
   is equivalent to (@tt{rw (applyAllC $convs$) $i$}).}

   @end[description]

   @docoff
>>
let someSubC = Tactic_type.Conversionals.someSubC
let allSubC = Tactic_type.Conversionals.allSubC
let higherC = Tactic_type.Conversionals.higherC
let lowerC = Tactic_type.Conversionals.lowerC
let sweepUpC = Tactic_type.Conversionals.sweepUpC
let sweepDnC = Tactic_type.Conversionals.sweepDnC
let applyAllC = Tactic_type.Conversionals.applyAllC
let findThenC = Tactic_type.Conversionals.findThenC

doc <:doc<
   @modsection{Conversion reversal}

   Computational rewrites define a congruence, and all equivalence relations
   in the congruence closure hold, including reversing the application of
   the rewrite.  However, reversed rewrites are often incompletely specified.

   @begin[description]
   @item{@conv[foldC], @conv[cutC];
   The (@tt[foldC] $t$ $c$) takes a term $t$ and a conversion that
   rewrites the term in the @emph{forward} direction, and generates
   reversed conversion.  For example, here is a reverse application of
   the beta rewrite.

   $$
   @rulebox{rwh; (@tt[foldC]@space (@lambda v. v + 1)@space 2@space @tt[unfold_beta])@space 0;
     <<sequent [dummy_arg] { <H> >- <:doc<2 + 1 = 3 @in @int>>}>>;
     <<sequent [dummy_arg] { <H> >- <:doc< ((@lambda v. v + 1)@space 2) = 3 @in @int>> }>>}
   $$

   @noindent
   The @tt[cutC] conversion is used to replace a term and generate a
   rewrite obligation.

   $$
   @rulebox{rw; (@tt[addrC]@space{} [1]@space (@tt[cutC]@space 3))@space 0;
     <<sequent [dummy_arg] { <H> >- <:doc< 3 = 3 @in @int>> }>> @cr
     <<sequent [dummy_arg] { <H> >- <:doc< ((@lambda v. v + 1)@space 2) @longleftrightarrow 3>>}>>;
     <<sequent [dummy_arg] { <H> >- <:doc< ((@lambda v. v + 1)@space 2) = 3 @in @int>>}>>}
   $$}
   @end[description]

   @docoff
>>
let addrC = Tactic_type.Conversionals.addrC
let foldC = Tactic_type.Conversionals.foldC
let makeFoldC = Tactic_type.Conversionals.makeFoldC
let cutC = Tactic_type.Conversionals.cutC

(************************************************************************
 * REDUCTION RESOURCE                                                   *
 ************************************************************************)

doc <:doc<
   @resources

   @bf{The @Comment!resource[reduce] resource}

   The @tt{reduce} resource provides a generic method for
   defining @emph{evaluation}.  The @conv[reduceTopC] conversion
   can be used to apply this evaluator.
   The @conv[reduceC] conversion repeatedly applies @tt[reduceTopC] to any subterm.
   The @tactic[reduceT] tactic applies @tt[reduceC] to the goal sequent.

   For example, the @Nuprl type theory describes several
   generic reductions:
   @begin[description]
   @item{beta; $(@lambda v. b[v])@space a @longleftrightarrow b[a]$}
   @item{pair; $(@bf{match}@space (a, b)@space @bf{with}@space u, v @rightarrow c[u, v]) @longleftrightarrow c[a, b]$}
   @item{union; $(@bf{match}@space @i[inl](a)@space @bf{with}@space
                  @i[inl](u) @rightarrow b[u]
                  | @i[inr](v) @rightarrow c[v]) @longleftrightarrow b[a]$}
   @end[description]

   Each of the modules for functions (Section @refmodule[Itt_dfun]),
   tuples (Section @refmodule[Itt_dprod]), and union (Section @refmodule[Itt_union]),
   defines an addition to the @hrefresource[reduce] resource: the @hrefmodule[Itt_dfun] adds
   the @hrefrewrite[reduce_beta] rewrite with redex $(@lambda v. b[v])@space a$@; the
   @hrefmodule[Itt_dprod] adds the @hrefrewrite[reduceSpread] rewrite with redex
   $(@bf{match}@space (a, b)@space @bf{with}@space u, v @rightarrow c[u, v])$@; and the
   @hrefmodule[Itt_union] adds the @hrefrewrite[reduceDecideInl] rewrite with
   redex $(@bf{match}@space @i[inl](a)@space @bf{with}@space
                  @i[inl](u) @rightarrow b[u]
                  | @i[inr](v) @rightarrow c[v])$.

   In modules that @tt{extends} these three theories, the @tt[reduceC]
   conversion will recursively search for applications of these three
   rewrites in an attempt to fully reduce the term.

   The implementation of the @tt[reduce] resource and the @tt[reduceC]
   conversion relies on tables to store the shape of redices, together with the
   conversions for the reduction.

   @docoff
>>
type reduce_conv = conv * (option_table -> conv)
type reduce_info = rule_labels * conv
type reduce_entry = term * reduce_info

(* unused
let opnames_of_terms options =
   List.fold_left (fun options t -> OpnameSet.add options (opname_of_term t)) OpnameSet.empty options
*)

let wrap_reduce ?labels conv =
   rule_labels_of_opt_terms labels, conv

let wrap_reduce_crw ?labels conv =
   let labels =
      match labels with
         None -> Perv.crw_labels
       | Some labels -> select_crw :: labels
   in
      rule_labels_of_opt_terms (Some labels), conv

let extract_data =
   let rec mapsnd recrw = function
      [] -> recrw
    | (_, h) :: tl -> h orelseC (mapsnd recrw tl)
   in
   let select_option options (opts, _) =
      rule_labels_are_allowed options opts
   in
   let rw tbl =
      funC (fun e -> (**)
         let t = env_term e in
         let p = env_arg e in
         let options = get_options p in
            (* Find and apply the right tactic *)
            if !debug_reduce then
               eprintf "Conversionals: lookup %a%t" debug_print t eflush;
            match Term_match_table.lookup_bucket tbl (select_option options) t with
               Some convs ->
                  if !debug_reduce then
                     eprintf "Conversionals: applying %a%t" debug_print t eflush;
                  firstC (List.map snd convs)
             | None ->
                  raise (RefineError ("Conversionals.extract_data", StringTermError ("no reduction for", t))))
   in
   let hrw tbl options =
      let rec hrw t =
         let recrw = allSubC (termC hrw) in
            (* Find and apply the right tactic *)
            if !debug_reduce then
               eprintf "Conversionals: lookup %a%t" debug_print t eflush;
            match Term_match_table.lookup_bucket tbl (select_option options) t with
               Some convs ->
                  if !debug_reduce then
                     eprintf "Conversionals: applying %a%t" debug_print t eflush;
                  mapsnd recrw convs
             | None ->
                  recrw
      in termC hrw
   in
      (fun tbl -> rw tbl, hrw tbl)

(*
 * Resource.
 *)
let resource (reduce_entry, reduce_conv) reduce =
   table_resource_info extract_data

let reduceTopC_env e =
   fst (get_resource_arg (env_arg e) get_reduce_resource)

let reduceTopC = funC reduceTopC_env

let reduceC =
   funC (fun e ->
      let p = env_arg e in
         repeatC (snd (get_resource_arg p get_reduce_resource) (get_options p)))

let reduceT = funT (fun p ->
   let reduceHigherC = snd (get_resource_arg p get_reduce_resource) (get_options p) in
      rwAll (repeatC reduceHigherC))

let reduceHypsT = funT (fun p ->
   let reduceHigherC = snd (get_resource_arg p get_reduce_resource) (get_options p) in
      onAllMHypsT (rw (repeatC reduceHigherC)))

let simpleReduceTopC = withOptionInfoC Perv.select_crw OptionExclude reduceTopC
let simpleReduceC = withOptionInfoC Perv.select_crw OptionExclude reduceC
let simpleReduceT = withOptionInfoT Perv.select_crw OptionExclude reduceT

let rec wrap_addrs conv = function
   [] -> conv
 | addr :: addrs -> addrLiteralC addr reduceC thenC wrap_addrs conv addrs

let cound_vars tbl t =
   if is_so_var_term t then
      let v, _, _ = dest_so_var t in
         Lm_hashtbl_util.update tbl v succ 0

let find_conds tbl t _ =
   is_so_var_term t &&
   let v, _, _ = dest_so_var t in
      match Hashtbl.find_opt tbl v with
         Some i -> i > 1
       | _ -> false

let process_reduce_resource_rw_annotation ?labels name redex contractum assums addrs args loc rw =
   let conv = rewrite_of_pre_rewrite rw empty_rw_args [] in

   (*
    * If the contractum is potentially an instance of the redex,
    * add progressC to make sure we don't stop too early.
    *)
   let conv =
      let instanceof =
         try
            let redex = compile_redex Strict addrs redex in
               test_redex_applicability redex empty_rw_args contractum [];
               true
         with
            RefineError _
          | Not_found ->
               false
      in
         if instanceof then begin
            if !debug_reduce then
               eprintf "%s: contractum is an instance of the redex@." name;
            progressC conv
         end
         else
            conv
   in
      match addrs, args with
         { spec_ints = [||]; spec_addrs = [||] }, [] ->
            (*
             * Before executing conv, run recursive reduceC on all the subterms
             * that will be copied more than once to reduce the possibility
             * for an exponential blow-up.
             *)
            let vars = Hashtbl.create 19 in
            let () = List.iter (TermOp.iter_down (cound_vars vars)) (contractum :: assums) in
            let addrs = find_subterm redex (find_conds vars) in
            let labels = rule_labels_of_opt_terms labels in
               [redex, (labels, wrap_addrs conv addrs)]
       | _ ->
            raise (Invalid_argument ((Simple_print.string_of_loc loc) ^ ": reduce resource annotation:
rewrite " ^ name ^": rewrites that take arguments are not supported"))

(*
 * Debugging.
 *)
let apply_rewrite p t =
   get_resource_arg p Tactic_type.Conversionals.apply_rewrite t

(*
 * -*-
 * Local Variables:
 * End:
 * -*-
 *)
