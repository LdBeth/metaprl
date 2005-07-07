(*
 * Conversion between extracts and terms.
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
 * Copyright (C) 1999 Jason Hickey, Cornell University
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

open Refiner_sig

open Refiner.Refiner.Refine

open Mp_resource
open Tactic_boot.TacticInternalType

module ProofTerm (ToTerm : RefinerSig) :
sig
   (*
    * Convert to a term.
    *)
   val to_term :
      (string -> MLast.expr) ->         (* Parser *)
      (MLast.expr -> tactic) ->         (* Evaluator *)
      tactic_arg ->                     (* Goal arg *)
      extract ->                        (* Proof *)
      ToTerm.TermType.term

   (*
    * Convert from a term.
    *)
   val of_term :
      raw_attributes ->            (* Default attributes *)
      sentinal ->                  (* Proof checker *)
      global_resource ->           (* Resource bookmark *)
      (string -> MLast.expr) ->    (* Parser *)
      (MLast.expr -> tactic) ->    (* Evaluator *)
      ToTerm.TermType.term ->      (* Argument term *)
      extract

   val convert : term -> ToTerm.TermType.term
   val revert : ToTerm.TermType.term -> term

   (*
    * Some basic operations.
    *)
   val status_of_term : ToTerm.TermType.term -> lazy_status
   val node_count_of_term : ToTerm.TermType.term -> int * int
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "nl"
 * End:
 * -*-
 *)
