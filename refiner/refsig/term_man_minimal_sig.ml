(*
 * Minimal term module:
 *
 * Operations on "manifest" terms. The only "manifest" terms that
 * are needed in the "minimal" module are sequents.
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
 * Copyright (C) 1998 Alexey Nogin, Cornell University
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
 * Author: Alexey Nogin <nogin@cs.cornell.edu>
 *)

open Lm_symbol

module type TermManMinimalSig =
sig
   module ManTypes : Term_sig.TermSig
   open ManTypes
   (*
    * Sequents.
    * This should be visible only to sequents, but oh well.
    *)
   val is_sequent_term : term -> bool
   val mk_sequent_term : esequent -> term
   val explode_sequent : term -> esequent

   val is_so_var_term : term -> bool
   val dest_so_var : term -> var * var list * term list
   val mk_so_var_term : var -> var list -> term list -> term
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
