(*
 * Convert between camlp4 terms and MetaPRL terms.
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

open MLast

open Opname
open Refiner_sig
open Filter_type

(*
 * Location is a pair of bignums.
 *)
type loc = Lm_num.num * Lm_num.num

(*
 * This is a parsing error when terms are converted to MLast.
 * FormatError (reason, term that failed)
 *)
exception FormatError of string * Refiner.Refiner.TermType.term

(*
 * The conversion is to an arbitrary term type.
 *)
module FilterOCaml (ToTerm : RefinerSig) :
sig
   open ToTerm.TermType

   (*
    * Parsing of terms.
    *)
   val expr_of_term : term -> expr
   val patt_of_term : term -> patt * term
   val type_of_term : term -> ctyp
   val sig_item_of_term : term -> sig_item
   val str_item_of_term : term -> str_item
   val module_type_of_term : term -> module_type
   val module_expr_of_term : term -> module_expr

   (*
    * These versions are never supposed to fail.
    *)
   val str_item_of_term_nofail : term -> str_item

   (*
    * MLast to term.
    *)
   val term_of_expr : string list -> expr -> term
   val term_of_patt : string list -> patt -> (string list -> term) -> term
   val term_of_type : ctyp -> term
   val term_of_sig_item : sig_item -> term
   val term_of_str_item : string list -> str_item -> term
   val term_of_module_type : module_type -> term
   val term_of_module_expr : string list -> module_expr -> term

   (*
    * Specific values useful for writing
    * term interpreters.
    *)
   val some_op : opname
   val none_op : opname
   val true_op : opname
   val false_op : opname

   (*
    * Common destructors.
    *)
   val dest_loc : term -> int * int
   val dest_loc_string : term -> (int * int) * string
   val dest_loc_int : term -> (int * int) * string
   val dest_opt : (term -> 'a) -> term -> 'a option
   val dest_string : term -> string

   (*
    * Special functions for resources
    *)
   val resource_sig_of_term : term -> ctyp resource_sig
   val term_of_resource_sig : opname -> ctyp resource_sig -> term
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
