(*
 * Camlp4 implementation of term patterns.
 * For now, we only allow patterns that are
 * one level deep: the patterns can not be
 * nested.  Eventually, we may want to update
 * this to be more general.
 *
 * The usage to match against a term t is this:
 *
 *     match explode t with
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
 * Author: Jason Hickey
 * @email{jyh@cs.caltech.edu}
 * @end[license]
 *)
open Mp_debug

open Opname
open Refiner.Refiner.TermType
open Refiner.Refiner.Term
open Refiner.Refiner.TermMan
open Refiner.Refiner.TermMeta
open Refiner.Refiner.RefineError

open Filter_type
open Filter_util

(*
 * This is the term destructor.
 * It takes a term, and produces a triple:
 *    1. the opname
 *    2. the parameters of type param'
 *    3. a list of bterms of type bterm'
 *
 * Variables are a special case: we use the "standard"
 * form with opname ["variable"] and a var parameter.
 *)
let explode_term t =
   if is_so_var_term t then
      let s, terms = dest_so_var t in
      let bterms = List.map (fun t -> { bvars = []; bterm = t }) terms in
         ["variable"], [MatchVar s], bterms
   else
      let { term_op = op; term_terms = bterms } = dest_term t in
      let { op_name = op; op_params = params } = dest_op op in
      let op = dest_opname op in
      let params = List.map dest_match_param params in
      let bterms = List.map dest_bterm bterms in
         op, params, bterms

(*
 * Turn a term into a pattern expression.
 * The bterms should all be vars.
 *)
let build_term_patt t =
   (* Fake the location for now *)
   let loc = 0, 0 in

   let { term_op = op; term_terms = bterms } = dest_term t in
   let { op_name = op; op_params = params } = dest_op op in

   let ops = Opname.dest_opname op in
   let ops = List.fold_right (fun x l -> <:patt< [$str:x$ :: $l$] >>) ops <:patt< [] >> in

   (* Parameter patterns *)
   let params =
      List.map (fun param ->
            match dest_param param with
               Number n ->
                  if not (Mp_num.is_integer_num n) then
                     raise (Invalid_argument "build_term_patt: number is not an integer");
                  let i = string_of_int (Mp_num.int_of_num n) in
                     <:patt< $uid:"Refiner"$ . $uid:"Refiner"$ . $uid:"TermType"$ . $uid: "MatchNumber"$ ( _, $uid: "Some"$ $int:i$ ) >>

             | String s ->
                  <:patt< $uid:"Refiner"$ . $uid:"Refiner"$ . $uid:"TermType"$ . $uid: "MatchString"$ $str: s$ >>

             | Token s ->
                  <:patt< $uid:"Refiner"$ . $uid:"Refiner"$ . $uid:"TermType"$ . $uid: "MatchToken"$ $str: s$ >>

             | Var s ->
                  <:patt< $uid:"Refiner"$ . $uid:"Refiner"$ . $uid:"TermType"$ . $uid: "MatchVar"$ $str: s$ >>

             | MNumber v ->
                  <:patt< $uid:"Refiner"$ . $uid:"Refiner"$ . $uid:"TermType"$ . $uid: "MatchNumber"$ ( $lid: v$, _ ) >>

             | MString v ->
                  <:patt< $uid:"Refiner"$ . $uid:"Refiner"$ . $uid:"TermType"$ . $uid: "MatchString"$ $lid: v$ >>

             | MToken v ->
                  <:patt< $uid:"Refiner"$ . $uid:"Refiner"$ . $uid:"TermType"$ . $uid: "MatchToken"$ $lid: v$ >>

             | MVar v ->
                  <:patt< $uid:"Refiner"$ . $uid:"Refiner"$ . $uid:"TermType"$ . $uid: "MatchVar"$ $lid: v$ >>

             | MLevel l ->
                  (match dest_level l with
                      { le_const = 0; le_vars = [v] } ->
                         (match dest_level_var v with
                             { le_var = v; le_offset = 0 } ->
                                 <:patt< $uid:"Refiner"$ . $uid:"Refiner"$ . $uid:"TermType"$ . $uid: "MatchLevel"$ $lid: v$ >>
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
         let bvars_rhs = List.fold_right (fun v l -> <:patt< [$lid:v$ :: $l$] >>) bvars <:patt< [] >> in
            <:patt< { $uid:"Refiner"$ . $uid:"Refiner"$ . $uid:"TermType"$ . $lid: "bvars"$ = $bvars_rhs$;
                      $uid:"Refiner"$ . $uid:"Refiner"$ . $uid:"TermType"$ . $lid: "bterm"$ = $lid: v$
                    } >>) bterms
   in
   let bterms = List.fold_right (fun x l -> <:patt< [$x$ :: $l$] >>) bterms <:patt< [] >> in

      (* Full pattern match is a triple *)
      <:patt< ( $ops$ , $params$ , $bterms$ ) >>

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
