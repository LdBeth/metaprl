(*
 * Rewrite functions.
 * ----------------------------------------------------------------
 *
 * Copyright (C) 2002 Adam Granicz, Caltech
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
 * Author: Adam Granicz
 * Email: granicz@cs.caltech.edu
 *)

open Mp_resource

open Tactic_type.Conversionals

open Opname
open Term_sig
open Refiner.Refiner.Term
open Refiner.Refiner.TermMan
open Refiner.Refiner.Rewrite
open Lm_num

open Phobos_type
open Phobos_util
open Phobos_exn

(* ATN: removed with connection of error recovery
let my_dest_loc loc =
   let s, i1, i2, i3, i4 = Location.dest_loc loc in
      Symbol.to_string s, i1, i2, i3, i4
*)
(* Empty term *)
let empty_term_opname = "__EPSILON__"
let empty_term_operator = mk_op (make_opname [empty_term_opname]) []
let empty_term = mk_any_term empty_term_operator []

(*
 * Numbered variable term.
 *)
let unique_var_number = ref 0
let unique_var_term (id, _) =
   incr(unique_var_number);
   mk_so_var_term (Lm_symbol.make id !unique_var_number) [] []

(* Position terms *)
let pos_opname = "__pos__"
let pos_wildcard1 = "__filename__"
let pos_wildcard2 = "__lpos1__"
let pos_wildcard3 = "__lpos2__"
let pos_wildcard4 = "__rpos1__"
let pos_wildcard5 = "__rpos2__"
let pos_param_number = ref 0

(* Make position operator with a unique meta-variable parameters. *)
let pos_operator () =
   let param1 = make_param (MString (Lm_symbol.make pos_wildcard1 !pos_param_number)) in
   let param2 = make_param (MNumber (Lm_symbol.make pos_wildcard2 !pos_param_number)) in
   let param3 = make_param (MNumber (Lm_symbol.make pos_wildcard3 !pos_param_number)) in
   let param4 = make_param (MNumber (Lm_symbol.make pos_wildcard4 !pos_param_number)) in
   let param5 = make_param (MNumber (Lm_symbol.make pos_wildcard5 !pos_param_number)) in
      incr pos_param_number;
      mk_op (make_opname [pos_opname]) [param1; param2; param3; param4; param5]

let pos_term () =
   mk_term (pos_operator ()) []

let pos_term_of (filename, lpos1, lpos2, rpos1, rpos2) =
   let param1 = make_param (String filename) in
   let param2 = make_param (Number (num_of_int lpos1)) in
   let param3 = make_param (Number (num_of_int lpos2)) in
   let param4 = make_param (Number (num_of_int rpos1)) in
   let param5 = make_param (Number (num_of_int rpos2)) in
   let operator = mk_op (make_opname [pos_opname]) [param1; param2; param3; param4; param5] in
      mk_any_term operator []

(*
 * Token terms
 *)
let token_opname = "__token__"
let token_wildcard = "__value__"
let token_param_number = ref 0

(* Make token operator with a unique string meta-variable parameter *)
let token_operator () =
   let name = token_wildcard ^ (string_of_int !token_param_number) in
      incr token_param_number;
      mk_op (make_opname [token_opname]) [make_param (MString (Lm_symbol.add name))]

(* Make token operator with given string parameter *)
let token_operator_of s =
   mk_op (make_opname [token_opname]) [make_param (String s)]

let term_of_token pos = function
   Terminal s ->
      let pos = pos_term_of pos in
         mk_term (token_operator_of s) [mk_bterm [] pos]
 | _ ->
   raise (Invalid_argument "term_of_token: not a token")

let term_of_token_string pos s =
   let pos = pos_term_of pos in
      mk_any_term (token_operator_of s) [pos]

(* Create a token term with a unique string meta-variable parameter. *)
(* We will also supply a position meta-subterm. *)
let token_term () =
       mk_any_term (token_operator ()) [pos_term ()]

(*
 * The product term
 *)
let prod_operator () = mk_op (make_opname ["__prod__"]) []
let prod_term terms =
   mk_any_term (prod_operator ()) terms

(* Compile a pattern {terms} -> term. *)
let compile_pattern from_terms contractum =
   try
      (* Lose position information *)
      let from_terms = List.map fst from_terms in
      let contractum = fst contractum in
      let redex = prod_term from_terms in
         term_rewrite Rewrite_sig.Relaxed empty_args_spec [redex] [contractum]
   with
      _ ->
         raise (RewriteException (snd contractum, "unable to rewrite this pattern"))

(* Compile a list of {terms} -> term patterns. *)
let compile_pattern_list lst =
   List.map (fun (terms, term) ->
      compile_pattern terms term) lst

(* Compile all lexer {terms}->term rewrites to c_redex->c_contractum. *)
let compile_lexer_rewrites lex_rewrites =
   PSymbolMTable.fold_all (fun new_lex_rewrites key rewrites ->
      let c_rewrites = compile_pattern_list rewrites in
         lex_rewrite_add_list new_lex_rewrites key c_rewrites) lex_rewrite_empty lex_rewrites

(* Compile all parser {terms}->term rewrites to c_redex->c_contractum. *)
let compile_parser_rewrites parser_rewrites =
   ProductionIdMTable.fold_all (fun new_parser_rewrites key rewrites ->
      let c_rewrites = compile_pattern_list rewrites in
         rewrite_add_list new_parser_rewrites key c_rewrites) rewrite_empty parser_rewrites

exception Got_it
exception Failed_rewrite

(* Apply the rewrite {terms} -> term. *)
let apply_rewrite rw terms =
   let product_term = prod_term terms in
      match apply_rewrite rw empty_args product_term [] with
         [result] ->
            result
(* ATN: removed error recovery
            begin try
               let s, pos =
                  try
                     Phobos_fc_ast_term.build_error result
                  with
                     exn ->
                        raise Got_it
               in
                  print_string "\nbuilt error term\n";
                  raise (RewriteException (my_dest_loc pos, s))
            with
               Got_it ->
                  result
            end
*)
        | _ ->
            raise(Invalid_argument("apply_rewrite"))

(*
 * Conversionals.
 *)
let _ = recompute_top ()
let apply_rw_top =
   Tactic_type.Conversionals.apply_rewrite (Mp_resource.find top_bookmark)

let iforms_conv iforms =
   let patterns =
      List.map (fun ((redex, _), (contractum, _)) ->
         create_iform "phobos" false redex contractum) iforms
   in
      match patterns with
         [] ->
            idC
       | _ ->
            repeatC (higherC (applyAllC patterns))

(* Rewrite {terms} according to the first matching rule in rules. *)
let apply_first_rewrite pos rules terms conversion =
   let result = ref (mk_term (token_operator_of "dummy_string_term") []) in
   try
      if List.length rules = 0 then
         raise (RewriteException (pos, "No rewrite rule found"));
      List.iter (fun rw ->
         try
            let res_term = apply_rewrite rw terms in
               result := res_term;
               raise Got_it
         with
            Got_it ->
               raise Got_it
          | RewriteException _ as exn ->
               raise exn
          | _ -> (**)
               ()) rules;
      raise Failed_rewrite
   with
      Got_it ->
         apply_rw_top conversion !result
    | Failed_rewrite ->
         raise (RewriteException (pos, "No matching rewrite rule found"))

let apply_post_rewrites term iform_bunch =
   let term =
      List.fold_left (fun term iforms ->
         apply_rw_top (iforms_conv iforms) term) term iform_bunch
   in
      term
