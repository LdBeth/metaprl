(*
 * Term utilities.
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
 *
 *)
open Lm_symbol

(************************************************************************
 * META-TERMS                                                           *
 ************************************************************************)

module type TermMetaSig =
sig
   (*
    * Have to import the type of terms.
    *)
   type term
   type meta_term

   (*
    * Some operations on meta_term.
    *)
   val binding_vars : meta_term -> var list
   val context_vars : meta_term -> var list
   val meta_alpha_equal : meta_term -> meta_term -> bool
   val unfold_mlabeled : string -> meta_term -> term
   val unzip_mimplies : meta_term -> term list * term
   val zip_mimplies : term list -> term -> meta_term
   val unzip_mfunction : meta_term -> (string list * term option * term) list * term
   val zip_mfunction : (term option * term) list -> term -> meta_term
   val strip_mfunction : meta_term -> meta_term

   (*
    * During parsing and display, the default contexts are "encoded"
    * as a singleton list containing just the variable itself
    *)
   val term_of_parsed_term : term -> term
   val term_of_parsed_term_with_vars : term -> term
   val display_term_of_term : term -> term
   val mterm_of_parsed_mterm : meta_term -> meta_term
   val mterms_of_parsed_mterms : meta_term -> term list -> meta_term * term list * (term -> term)
   (* finds all SO variables in a term and uses them *)
   val context_subst_of_terms : term list -> var -> int -> var list option
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
