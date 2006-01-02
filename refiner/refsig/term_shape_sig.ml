(*
 * Compute the "shape" of the term that can be used for reductions.
 * Terms are reduced to these templates for indexing
 * purposes.
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
 * Author: Jason Hickey <jyh@cs.cornell.edu>
 * Modified by: Aleksey Nogin <nogin@cs.cornell.edu>
 *)
open Lm_printf

open Opname
open Term_sig

module type TermShapeSig =
sig
   type term
   type param

   (*
    * shape_of_term will be == to sequent_shape on any sequent and == to
    * var_shape on any FO variable
    *)
   val shape_of_term : term -> shape
   val eq : shape -> shape -> bool
   val shape_eq : shape -> shape -> bool
   val param_type : param -> shape_param

   val unquote_shape : shape -> shape
   val opname_of_shape : shape -> opname

   val sequent_shape : shape (* Any sequent *)
   val var_shape : shape (* First-order variable *)

   val print_shape : out_channel -> shape -> unit
   val pp_print_shape : formatter -> shape -> unit
   val string_of_shape : shape -> string
   val short_string_of_shape : shape -> string

   val shape_compare : shape -> shape -> int

   val canonical_term_of_shape : shape -> term

   val opparam_of_term : term -> param op_param
   val shape_of_opparam : param op_param -> shape
   val string_of_opparam : param op_param -> string
   val canonical_term_of_opparam : param op_param -> term

   module ShapeSet : Lm_set_sig.LmSet with type elt = shape;;
   module ShapeTable : Lm_map_sig.LmMap with type key = shape;;
   module ShapeMTable : Lm_map_sig.LmMapList with type key = shape;;
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
