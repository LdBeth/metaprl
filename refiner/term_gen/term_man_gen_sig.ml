(*
 * Manifest terms - extra signature for term_gen
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
 * Copyright (C) 2003, Aleksey Nogin, Caltech
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
 * Author: Aleksey Nogin <nogin@cs.caltech.edu>
 *)
open Lm_symbol
open Opname
open Term_man_sig

module type TermManGenSig =
sig
   include TermManSig
   open ManTypes

   val hyp_opname : opname
   val concl_opname : opname

   val match_hyp : string -> term -> bound_term list -> term
   val match_hyp_all : string -> term -> bound_term list -> term * var * term
   val match_context : operator -> string -> term -> bound_term list -> term
   val body_of_sequent : term -> term
   val dest_sequent_outer_term : term -> term * term
   val mk_sequent_outer_term : term -> term -> term
end
