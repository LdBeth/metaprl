(*
 * Some types shared by Filter_reflection and Filter_type.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2006 Mojave Group, Caltech
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
open Lm_symbol

open Opname
open Term_sig
open Term_shape_sig
open Refiner.Refiner.TermType
open Filter_shape

(*
 * The type of rule parameters.
 * %%MAGICBEGIN%%
 *)
type 'term poly_param =
   IntParam of var
 | AddrParam of var
 | TermParam of 'term
(* %%MAGICEND%% *)

type term_param = term poly_param

(*
 * For expanding quotations.
 *)
type parse_state =
   { parse_quotation : string -> string -> term;
     parse_opname    : op_kind -> string list -> shape_param list -> int list -> Opname.opname;
     parse_shape     : shape -> shape_class;
     parse_param     : term -> param
   }

(*
 * -*-
 * Local Variables:
 * Fill-column: 100
 * End:
 * -*-
 * vim:ts=3:et:tw=100
 *)
