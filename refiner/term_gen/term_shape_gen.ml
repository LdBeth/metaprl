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
 * See the file doc/index.html for information on Nuprl,
 * OCaml, and more information about this system.
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
 * Author: Jason Hickey
 * jyh@cs.cornell.edu
 *)

open Printf

open Opname
open Term_simple_sig
open Term_base_sig

module TermShape (**)
   (TermType : TermSimpleSig)
   (Term : TermBaseSig
    with type term = TermType.term
    with type term' = TermType.term'
    with type bound_term = TermType.bound_term
    with type bound_term' = TermType.bound_term') =
struct
   open TermType
   open Term

   type term = Term.term

   type shape =
      { shape_opname : opname;
        shape_params : shape_param list;
        shape_arities : int list
      }

   and shape_param =
      ShapeNumber
    | ShapeString
    | ShapeToken
    | ShapeLevel
    | ShapeVar

   (*
    * Fold together meta-parameters and parameters.
    *)
   let param_type = function
      Number _ -> ShapeNumber
    | String _ -> ShapeString
    | Token _ -> ShapeToken
    | Var _ -> ShapeVar
    | MNumber _ -> ShapeNumber
    | MString _ -> ShapeString
    | MToken _ -> ShapeToken
    | MLevel _ -> ShapeLevel
    | MVar _ -> ShapeVar
    | _ ->
         raise (Invalid_argument "Term.shape_of_term")

   let bterm_type bt =
      List.length (dest_bterm bt).bvars

   let shape_of_term trm =
      let t = dest_term trm in
      let op = t.term_op in
         { shape_opname = op.op_name;
           shape_params = List.map param_type op.op_params;
           shape_arities = List.map bterm_type t.term_terms
         }

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

   let print_shape out { shape_opname = name; shape_params = params; shape_arities = arities } =
      let print_param param =
         let s =
            match param with
               ShapeNumber ->
                  "N"
             | ShapeString ->
                  "S"
             | ShapeToken  ->
                  "T"
             | ShapeLevel  ->
                  "L"
             | ShapeVar    ->
                  "V"
         in
            output_string out s
      in
      let rec print_arity out = function
         [i] ->
            fprintf out "%d" i
       | i::t ->
            fprintf out "%d;%a" i print_arity t
       | [] ->
            ()
      in
         output_string out (string_of_opname name);
         output_string out "[";
         List.iter print_param params;
         output_string out "]{";
         print_arity out arities;
         output_string out "}"

end
