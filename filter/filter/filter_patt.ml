(*x
 * Camlp4 implementation of term patterns.
 * For now, we only allow patterns that are
 * one level deep: the patterns can not be
 * nested.  Eventually, we may want to update
 * this to be more general.
 *
 * The usage to match against a term t is this:
 *
 *     match explode_term t with
 *        << lambda{v. 'e} >> ->
 *            (*
 *             * v and e are binding occurrences.
 *             * v : string, e : term
 *             *)
 *            ...
 *
 * Note that 'e is not a second-order pattern,
 * although such patterns will be accepted.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2003 Mojave Group, Caltech
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
 * Author: Jason Hickey @email{jyh@cs.caltech.edu}
 * Modified By: Aleksey Nogin @email{nogin@cs.caltech.edu}
 * @end[license]
 *)
open Lm_symbol
open Lm_debug

open Opname
open Refiner.Refiner.TermType
open Refiner.Refiner.Term
open Refiner.Refiner.TermMan
open Refiner.Refiner.TermMeta
open Refiner.Refiner.RefineError

open Filter_type
open Filter_util

(*
 * Turn a term into a pattern expression.
 * The bterms should all be vars.
 *)
let build_term_nonvar_patt loc t =

   let { term_op = op; term_terms = bterms } = dest_term t in
   let { op_name = op; op_params = params } = dest_op op in

   let ops = Opname.dest_opname op in
   let ops = List.fold_right (fun x l -> <:patt< [$str:String.escaped x$ :: $l$] >>) ops <:patt< [] >> in

   (* Parameter patterns *)
   let params =
      List.map (fun param ->
            match dest_param param with
               Number n ->
                  if not (Lm_num.is_integer_num n) then
                     raise (Invalid_argument "build_term_patt: number is not an integer");
                  let i = string_of_int (Lm_num.int_of_num n) in
                     <:patt< Refiner.Refiner.TermType.MatchNumber ( _, Some $int:i$ ) >>

             | String s ->
                  <:patt< Refiner.Refiner.TermType.MatchString $str: String.escaped s$ >>

             | Token s ->
                  <:patt< Refiner.Refiner.TermType.MatchToken $str: String.escaped s$ >>

             | MNumber v ->
                  <:patt< Refiner.Refiner.TermType.MatchNumber ( $lid: (string_of_symbol v)$, _ ) >>

             | MString v ->
                  <:patt< Refiner.Refiner.TermType.MatchString $lid: (string_of_symbol v)$ >>

             | MToken v ->
                  <:patt< Refiner.Refiner.TermType.MatchToken $lid: (string_of_symbol v)$ >>

             | Var v ->
                  <:patt< Refiner.Refiner.TermType.MatchVar $lid: (string_of_symbol v)$ >>

             | MLevel l ->
                  (match dest_level l with
                      { le_const = 0; le_vars = [v] } ->
                         (match dest_level_var v with
                             { le_var = v; le_offset = 0 } ->
                                 <:patt< Refiner.Refiner.TermType.MatchLevel $lid: (string_of_symbol v)$ >>
                           | _ ->
                              raise (Invalid_argument "term_patt: complex level expressions not supported"))
                    | _ ->
                         raise (Invalid_argument "term_patt: complex level expressions not supported"))

             | ObId _ ->
                  raise (Invalid_argument "term_patt: object-ids not supported")
             | ParamList _ ->
                  raise (Invalid_argument "term_patt: parameter lists not supported")) params
   in
   let params = List.fold_right (fun x l -> <:patt< [$x$ :: $l$] >>) params <:patt< [] >> in

   (* Bterm patterns *)
   let bterms =
      List.map (fun bterm ->
         let { bvars = bvars; bterm = t } = dest_bterm bterm in
         let _ =
            if not (is_so_var_term t) then
               raise (Invalid_argument "term_patt: subterms must be variables")
         in
         let v, _ = dest_so_var t in
         let bvars_rhs = List.fold_right (fun v l -> <:patt< [$lid: string_of_symbol v$ :: $l$] >>) bvars <:patt< [] >> in
            <:patt< { Refiner.Refiner.TermType.bvars = $bvars_rhs$;
                      Refiner.Refiner.TermType.bterm = $lid: string_of_symbol v$
                    } >>) bterms
   in
   let bterms = List.fold_right (fun x l -> <:patt< [$x$ :: $l$] >>) bterms <:patt< [] >> in

      (* Full pattern match is a triple *)
      <:patt< ( $ops$ , $params$ , $bterms$ ) >>

let build_term_patt t =
   (* Fake the location for now *)
   let loc = 0, 0 in

   if is_var_term t then
      <:patt< $lid: string_of_symbol (dest_var t)$ >>
   else
      build_term_nonvar_patt loc t

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
