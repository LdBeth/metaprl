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
 *)
let explode_term t =
   let { term_op = op; term_terms = bterms } = dest_term t in
   let { op_name = op; op_params = params } = dest_op op in
   let params = List.map dest_param params in
   let bterms = List.map dest_bterm bterms in
      op, params, bterms

(*
 * Turn a term into a pattern expression.
 * The bterms should all be vars.
 *)
let opname_var = "_$opname"

let build_term_patt t =
   let { term_op = op; term_terms = bterms } = dest_term t in
   let { op_name = op; op_params = params } = dest_op op in

   (* Add opname *)
   let op = add_binding (BindOpname op) in

   (* Name generator *)
   let gensym i =
      "_$v" ^ string_of_int i
   in

   (* Sorry, no loc information... *)
   let loc = 0, 0 in

   (* Parameter patterns *)
   let _, whens, params =
      List.fold_left (fun (i, whens, params) param ->
            match dest_param param with
               Number n ->
                  let e = add_binding (BindNum n) in
                  let v = gensym i in
                  let when_exp = <:expr< $uid:"Mp_num"$ . $lid: "eq_num"$ $lid: v$ $e$ >> in
                  let patt = <:patt< $uid: "Term_simple_sig"$ . $uid: "Number"$ $lid: v$ >> in
                     succ i, when_exp :: whens, patt :: params

             | String s ->
                  let patt = <:patt< $uid: "Term_simple_sig"$ . $uid: "String"$ $str: s$ >> in
                     i, whens, patt :: params

             | Token s ->
                  let patt = <:patt< $uid: "Term_simple_sig"$ . $uid: "Token"$ $str: s$ >> in
                     i, whens, patt :: params

             | Var s ->
                  let patt = <:patt< $uid: "Term_simple_sig"$ . $uid: "Var"$ $str: s$ >> in
                     i, whens, patt :: params

             | MNumber v ->
                  let patt = <:patt< $uid: "Term_simple_sig"$ . $uid: "Number"$ $lid: v$ >> in
                     i, whens, patt :: params

             | MString v ->
                  let patt = <:patt< $uid: "Term_simple_sig"$ . $uid: "String"$ $lid: v$ >> in
                     i, whens, patt :: params

             | MToken v ->
                  let patt = <:patt< $uid: "Term_simple_sig"$ . $uid: "Token"$ $lid: v$ >> in
                     i, whens, patt :: params

             | MVar v ->
                  let patt = <:patt< $uid: "Term_simple_sig"$ . $uid: "Var"$ $lid: v$ >> in
                     i, whens, patt :: params

             | MLevel l ->
                  (match dest_level l with
                      { le_const = 0; le_vars = [v] } ->
                         (match dest_level_var v with
                             { le_var = v; le_offset = 0 } ->
                                 let patt = <:patt< $uid: "Term_simple_sig"$ . $uid: "Level"$ $lid: v$ >> in
                                    i, whens, patt :: params
                           | _ ->
                              raise (Invalid_argument "term_patt: complex level expressions not supported"))
                    | _ ->
                         raise (Invalid_argument "term_patt: complex level expressions not supported"))

             | ObId _ ->
                  raise (Invalid_argument "term_patt: object-ids not supported")
             | ParamList _ ->
                  raise (Invalid_argument "term_patt: parameter lists not supported")) (0, [], []) params
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
            <:patt< { $uid: "Term_simple_sig"$ . $lid: "bvars"$ = $bvars_rhs$;
                      $uid: "Term_simple_sig"$ . $lid: "bterm"$ = $lid: v$
                    } >>) bterms
   in
   let bterms = List.fold_right (fun x l -> <:patt< [$x$ :: $l$] >>) bterms <:patt< [] >> in

   (* Conjoin when clauses *)
   let whens =
      List.fold_right (fun w l -> <:expr< $w$ && $l$ >>)
         whens <:expr< $uid: "Opname"$ . $lid: "eq"$ $lid: opname_var$ $op$ >>
   in

   (* Full pattern match is a triple *)
   <:patt< ( $lid: opname_var$ , $params$ , $bterms$ ) >>

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
