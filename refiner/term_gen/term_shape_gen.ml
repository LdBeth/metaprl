(*
 * Terms are reduced to these templates for indexing
 * purposes.  Each template just contains information
 * about the opname, the order and types of params,
 * and the arties of the subterms.
 *
 * ----------------------------------------------------------------
 *
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/htmlman/default.html or visit http://metaprl.org/
 * for more information.
 *
 * Copyright (C) 1998 Jason Hickey, Cornell University
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
 * Author: Aleksey Nogin <nogin@cs.cornell.edu>
 * Modified By: Jason Hickey <jyh@cs.cornell.edu>
 *)
open Lm_printf

open Opname
open Term_sig
open Term_base_sig
open Term_man_sig

module TermShape (**)
   (TermType : TermSig)
   (Term : TermBaseSig with module TermTypes = TermType)
   (TermMan : TermManSig with module ManTypes = TermType) =
struct
   open Term
   open TermMan

   type term = TermType.term
   type param = TermType.param

   let opname_of_shape { shape_opname = opname; _ } =
      opname

   (*
    * Shapes for special terms.
    * XXX HACK/TODO: shape type should probably be a choice type
    *)
   let sequent_shape =
      { shape_opname = sequent_opname;
        shape_params = [];
        shape_arities = [0; 1];
      }

   let var_shape =
      { shape_opname = var_opname;
        shape_params = [ShapeVar];
        shape_arities = []
      }

   let so_var_shape =
      let zero _ = 0 in
         fun terms -> { var_shape with shape_arities = List.map zero terms }

   (*
    * Fold together meta-parameters and parameters.
    *)
   let param_type p =
      match dest_param p with
         Number _ | MNumber _ -> ShapeNumber
       | String _ | MString _ -> ShapeString
       | Token _ | MToken _ -> ShapeToken
       | Shape _ | MShape _ -> ShapeShape
       | Operator _ | MOperator _ -> ShapeOperator
       | Var _ -> ShapeVar
       | MLevel _ -> ShapeLevel
       | Quote -> ShapeQuote
       | ObId _ | ParamList _ ->
            raise (Invalid_argument "Term.shape_of_term")

   let bterm_type bt =
      List.length (dest_bterm bt).bvars

   let shape_of_term trm =
      if is_var_term trm then var_shape else
      if is_so_var_term trm then let _,_,ts = dest_so_var trm in so_var_shape ts else
      if is_sequent_term trm then sequent_shape else
      let t = dest_term trm in
      let op = dest_op t.term_op in
         { shape_opname = op.op_name;
           shape_params = List.map param_type op.op_params;
           shape_arities = List.map bterm_type t.term_terms
         }

   let opparam_of_term t =
      if is_var_term t || is_so_var_term t || is_sequent_term t then
         raise (Invalid_argument "Term_shape_gen.opparam_of_term: variables and sequents are not allowed")
      else
         let t = dest_term t in
         let op = dest_op t.term_op in
            { opparam_name = op.op_name;
              opparam_params = op.op_params;
              opparam_arities = List.map bterm_type t.term_terms
            }

   let shape_of_opparam op =
      { shape_opname = op.opparam_name;
        shape_params = List.map param_type op.opparam_params;
        shape_arities = op.opparam_arities
      }

   (*
    * Remove quotations.
    *)
   let unquote_shape =
      let rec strip_quotes params =
         match params with
            ShapeQuote :: params ->
               strip_quotes params
          | _ ->
               params
      in
         (fun shape ->
               let params = shape.shape_params in
               let params' = strip_quotes params in
                  if params' == params then
                     shape
                  else
                     { shape with shape_params = params' })

   let eq { shape_opname = name1;
            shape_params = params1;
            shape_arities = arities1
       }
       { shape_opname = name2;
         shape_params = params2;
         shape_arities = arities2
       } =
      (Opname.eq name1 name2)
      & (params1 = params2)
      & (arities1 = arities2)

   let shape_eq = eq

   let canonical_param ps =
      make_param (**)
         (match ps with
            ShapeNumber   -> MNumber (Lm_symbol.add "n")
          | ShapeString   -> MString (Lm_symbol.add "s")
          | ShapeToken    -> MToken  (Lm_symbol.add "t")
          | ShapeLevel    -> MLevel (mk_level 0 [mk_level_var (Lm_symbol.add "l") 0])
          | ShapeShape    -> MShape (Lm_symbol.add "sh")
          | ShapeOperator -> MOperator (Lm_symbol.add "op")
          | ShapeVar      -> Var (Lm_symbol.add "v")
          | ShapeQuote    -> Quote)

   let canonical_subterm =
      let x = Lm_symbol.add "x" in
      let t = mk_so_var_term (Lm_symbol.add "t") [] [] in
      let rec mk = function
         0 -> []
       | i -> x :: (mk (i - 1))
      in
         fun i -> mk_bterm (mk i) t

   let canonical_term_of_shape sh =
      mk_term (**)
         (mk_op sh.shape_opname (List.map canonical_param sh.shape_params))
         (List.map canonical_subterm sh.shape_arities)

   let canonical_term_of_opparam op =
      mk_term (mk_op op.opparam_name op.opparam_params) (List.map canonical_subterm op.opparam_arities)

   let print_param buf param =
      let s =
         match param with
            ShapeNumber   -> "N"
          | ShapeString   -> "S"
          | ShapeToken    -> "T"
          | ShapeLevel    -> "L"
          | ShapeVar      -> "V"
          | ShapeQuote    -> "Q"
          | ShapeShape    -> "Sh"
          | ShapeOperator -> "Op"
      in
         Buffer.add_string buf s

   let rec print_arity buf = function
      [i] ->
         Buffer.add_string buf (string_of_int i)
    | i :: t ->
         Buffer.add_string buf (string_of_int i);
         Buffer.add_char buf ';';
         print_arity buf t
    | [] ->
         ()

   let string_of_shape { shape_opname = name; shape_params = params; shape_arities = arities } =
      let buf = Buffer.create 32 in
         Buffer.add_string buf (string_of_opname name);
         Buffer.add_char buf '[';
         List.iter (print_param buf) params;
         Buffer.add_string buf "]{";
         print_arity buf arities;
         Buffer.add_string buf "}";
         Buffer.contents buf

   let string_of_opparam op =
      string_of_shape (shape_of_opparam op)

   (*
    * Try to be a bit more brief about the printout.
    *)
   let short_string_of_shape { shape_opname = name; shape_params = params; shape_arities = arities } =
      let buf = Buffer.create 32 in
         Buffer.add_string buf (fst (dst_opname name));
         (match params with
             [] ->
                ()
           | _ ->
                Buffer.add_char buf '[';
                List.iter (print_param buf) params;
                Buffer.add_string buf "]");
         (match arities with
             [] ->
                ()
           | _ ->
                Buffer.add_string buf "{";
                print_arity buf arities;
                Buffer.add_string buf "}");
         Buffer.contents buf

   let print_shape out shape =
      output_string out (string_of_shape shape)

   let pp_print_shape out shape =
      pp_print_string out (string_of_shape shape)

   (************************************************************************
    * Sets and tables.
    *)
   module ShapeCompare =
   struct
      type t = shape

      let compare shape1 shape2 =
         let { shape_opname = opname1;
               shape_params = params1;
               shape_arities = arities1
             } = shape1
         in
         let { shape_opname = opname2;
               shape_params = params2;
               shape_arities = arities2
             } = shape2
         in
         let cmp = Opname.compare opname1 opname2 in
            if cmp = 0 then
               Stdlib.compare (params1, arities1) (params2, arities2)
            else
               cmp
   end

   let shape_compare = ShapeCompare.compare

   module ShapeSet = Lm_set.LmMake (ShapeCompare);;
   module ShapeTable = Lm_map.LmMake (ShapeCompare);;
   module ShapeMTable = Lm_map.LmMakeList (ShapeCompare);;
end
