(*
 * A term class is like a very simple type system.
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
 * Copyright (C) 2005 Mojave Group, Caltech
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
 * Author: Jason Hickey <jyh@cs.caltech.edu>
 *)
open Lm_printf

open Opname

(*
 * The type system.
 *)
type 'term poly_ty_param =
   TyNumber
 | TyString
 | TyToken of 'term
 | TyShape
 | TyLevel
 | TyVar
 | TyQuote

type 'term poly_ty_bterm =
   { ty_bvars : 'term list;
     ty_bterm : 'term
   }

type ('term1, 'term2) poly_ty_term =
   { ty_term   : 'term1;
     ty_opname : opname;
     ty_params : 'term2 poly_ty_param list;
     ty_bterms : 'term2 poly_ty_bterm list;
     ty_type   : 'term2
   }

module type TermTySig =
sig
   type term

   type ty_param = term poly_ty_param
   type ty_bterm = term poly_ty_bterm
   type ty_term  = (term, term) poly_ty_term

   (*
    * Get the term corresponding to the type.
    *)
   val term_of_ty : ty_term -> term

   (*
    * Printers.
    *)
   val string_of_ty_param : ty_param -> string

   (*
    * Test for alpha-equality.
    *)
   val eq : ty_term -> ty_term -> bool

   (*
    * Test for alpha-equality of just the class part,
    * not the term.
    *)
   val eq_ty : ty_term -> ty_term -> bool
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
