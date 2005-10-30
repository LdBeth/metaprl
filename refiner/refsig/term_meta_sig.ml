(*
 * Term utilities.
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
 * Copyright (C) 1998-2004 MetaPRL Group
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
 * Modified By: Aleksey Nogin <nogin@cs.caltech.edu>
 *
 *)
open Lm_symbol

(* Encodes "this context does not introduce a binding" at parse-time *)
let hash_sym = Lm_symbol.make "#" 0

(************************************************************************
 * META-TERMS                                                           *
 ************************************************************************)

module type TermMetaSig =
sig
   (*
    * Have to import the type of terms.
    *)
   module MetaTypes : Term_sig.TermSig
   open MetaTypes

   (* Whether to allow binding in a sequents, based on its args *)
   type allow_seq_bindings = term -> bool

   (*
    * Some operations on meta_term.
    *)
   val free_vars_mterm : meta_term -> SymbolSet.t
   val context_vars : meta_term -> SymbolSet.t * SymbolSet.t
   val context_vars_info : (bool * int * int) SymbolTable.t -> meta_term -> (bool * int * int) SymbolTable.t
   val so_vars_info :  (int * int) SymbolTable.t -> meta_term -> (int * int) SymbolTable.t

   val meta_alpha_equal : meta_term -> meta_term -> bool
   val unfold_mlabeled : string -> meta_term -> term
   val unzip_mimplies : meta_term -> term list * term
   val zip_mimplies : term list -> term -> meta_term
   val unzip_mfunction : meta_term -> (string list * term option * term) list * term
   val zip_mfunction : (term option * term) list -> term -> meta_term
   val strip_mfunction : meta_term -> meta_term
   val unzip_mrewrite : meta_term -> term list * term * term

   (* Mappings *)
   val iter_mterm : (term -> unit) -> meta_term -> unit
   val map_mterm : (term -> term) -> meta_term -> meta_term

   (*
    * During parsing and display, the default contexts are "encoded"
    * as a singleton list containing just the variable itself
    *)
   val term_of_parsed_term : allow_seq_bindings -> term -> term
   val term_of_parsed_term_with_vars : allow_seq_bindings -> term -> term
   val mterms_of_parsed_mterms : allow_seq_bindings -> meta_term -> term list -> meta_term * term list * (term -> term)
   val rewrite_of_parsed_rewrite : allow_seq_bindings -> term -> term -> term * term
   val mrewrite_of_parsed_mrewrite : allow_seq_bindings -> term list -> term -> term list * term

   (* Shortener *)
   val display_term_of_term : term -> term

   (* finds all SO variables in a term and uses them *)
   val context_subst_of_terms : term list -> var -> int -> var list option

   (*
    * A free FO variable (denoted !v in input syntax) is temporarily encoded,
    * in order for the term_of_parsed_term functions to do the right thing
    *)
   val encode_free_var : var -> term
   val is_encoded_free_var : term -> bool
   val decode_free_var : term -> var

   (*
    * Conversion to terms.
    *)
   val is_meta_term : term -> bool
   val meta_term_of_term : term -> meta_term
   val term_of_meta_term : meta_term -> term
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
