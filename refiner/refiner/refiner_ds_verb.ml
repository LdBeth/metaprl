(*
 * This is the standard refiner.
 *
 * ----------------------------------------------------------------
 *
 * This file is part of Nuprl-Light, a modular, higher order
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

open Term_meta_sig

module Refiner =
struct
   module TermType = Term_ds.TermType
   module AddressType = Term_addr_ds_verb.AddressType 
   module RefineError = Refine_error.MakeRefineError (TermType) (AddressType)
   module Term = Term_base_ds_verb.Term (RefineError)
   module TermOp = Term_op_ds_verb.TermOp (Term) (RefineError)
   module TermSubst = Term_subst_ds_verb.TermSubst (Term) (RefineError)
   module TermAddr = Term_addr_ds_verb.TermAddr (Term) (TermOp) (RefineError)
   module TermMan = Term_man_ds_verb.TermMan (Term) (TermOp) (TermAddr) (TermSubst) (RefineError)
   module TermShape = Term_shape_gen_verb.TermShape (TermType) (Term)
   module TermEval = Term_eval_ds_verb.TermEval (Term) (RefineError)
   module TermMeta = Term_meta_gen_verb.TermMeta (TermType) (Term) (TermSubst) (RefineError)
   module Rewrite = Rewrite_verb.Rewrite (TermType) (Term) (TermMan) (TermAddr) (TermSubst) (RefineError)
   module Refine = Refine_verb.Refine (TermType) (Term) (TermMan) (TermSubst) (TermAddr) (TermMeta) (Rewrite) (RefineError)
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
