(*
 * Tries to match hypothesys lists in a pair of sequents
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
 * Copyright (C) 1998 Alexey Nogin, Cornell University
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
 * Author: Alexey Nogin <nogin@cs.cornell.edu>
 *
 *)

open Lm_debug

open Term_sig
open Refiner.Refiner
open RefineError
open Term
open TermType
open TermSubst

let _ = show_loading "Loading Term_subst_ds%t"

let cant_match_hyp = RefineError ("Match_seq.match_hyp", StringError "sequents do not match")
let fail_match _ = raise cant_match_hyp

let aev v1 v2 t1 t2 = alpha_equal_vars t1 v1 t2 v2

let try_match_hyps relaxed big small =
   let big_hyp = big.sequent_hyps in
   let small_hyp = small.sequent_hyps in
   let small_length = SeqHyp.length small_hyp in
   let big_length = SeqHyp.length big_hyp in
   let may_skip = big_length - small_length in
   if (not relaxed) && may_skip < 0 then raise cant_match_hyp;
   let result = Array.create big_length None in
   let all_hyps = if relaxed then
         let rec aux small_skip =
            small_skip = small_length || match SeqHyp.get small_hyp small_skip with
               Hypothesis _ -> aux (succ small_skip)
             | Context _ -> false
         in aux
      else fail_match in
   let rec aux big_skip small_skip big_vars small_vars =
      if small_skip = small_length then
         relaxed || alpha_equal_vars big.sequent_concl big_vars small.sequent_concl small_vars
      else if big_skip = big_length then relaxed && all_hyps small_skip
      else if (not relaxed) && (big_skip - small_skip) > may_skip then false
      else
         match SeqHyp.get big_hyp big_skip, SeqHyp.get small_hyp small_skip with
            Context (v1, _, terms1), Context (v2, _, terms2) when
               (v1 = v2) && (List.for_all2 (aev big_vars small_vars) terms1 terms2) ->
               result.(big_skip) <- Some small_skip;
               aux (succ big_skip) (succ small_skip) big_vars small_vars
          | Hypothesis (v1, t1), Hypothesis (v2, t2) ->
               if alpha_equal_vars t1 big_vars t2 small_vars then
                  if aux (succ big_skip) (succ small_skip) (v1::big_vars) (v2::small_vars) then begin
                     result.(big_skip) <- Some small_skip;
                     true
                  end else
                     aux (succ big_skip) small_skip big_vars small_vars
               else
                  aux (succ big_skip) small_skip big_vars small_vars
          | Hypothesis _, Context _ ->
               aux (succ big_skip) small_skip big_vars small_vars
          | _ ->
            false
   in
      if aux 0 0 [] [] then result else raise cant_match_hyp

let match_hyps = try_match_hyps false

let match_some_hyps big small =
   let index = try_match_hyps true big small in
   let rec aux i =
      let i = pred i in
      if i<0 then 0 else match index.(i) with
         Some ind -> ind
       | None -> aux i
   in succ (aux (Array.length index))
