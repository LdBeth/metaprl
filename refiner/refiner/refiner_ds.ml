(*
 * This is the standard refiner.
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
open Termmod_sig

module Refiner =
struct
   module TermType = Term_ds.TermType
   module ErrParams =
   struct
      type address = Term_addr_sig.addr_item list
      type ty_param = TermType.term Term_ty_sig.poly_ty_param
      type ty_term  = (TermType.term, TermType.term) Term_ty_sig.poly_ty_term
   end
   module RefineError = Refine_error.MakeRefineError (TermType) (ErrParams)
   module Term = Term_base_ds.Term (RefineError)
   module TermOp = Term_op_ds.TermOp (Term) (RefineError)
   module TermSubst = Term_subst_ds.TermSubst (Term) (RefineError)
   module TermMan = Term_man_ds.TermMan (Term) (TermOp) (TermSubst) (RefineError)
   module TermAddr = Term_addr_ds.TermAddr (Term) (TermSubst) (TermOp) (TermMan) (RefineError)
   module TermShape = Term_shape_gen.TermShape (TermType) (Term) (TermMan)
   module TermTy = Term_ty_gen.TermTy (TermType) (Term) (TermMan) (TermSubst)
   module TermMeta = Term_meta_gen.TermMeta (TermType) (Term) (TermSubst) (TermOp) (TermMan) (RefineError)
   module Rewrite = Rewrite.Rewrite (TermType) (Term) (TermOp) (TermMan) (TermAddr) (TermSubst) (TermShape) (RefineError)
   module Refine = Refine.Refine (TermType) (Term) (TermMan) (TermSubst) (TermAddr) (TermMeta) (TermShape) (Rewrite) (RefineError)
   module TermMod = struct
      module TermType = TermType
      module Term = Term
      module TermSubst = TermSubst
      module TermMan = TermMan
      module TermMeta = TermMeta (* XXX HACK: TermMan is here only for ASCII IO format versions <= 1.0.7 support *)
      module TermShape = TermShape
      module Refine = Refine
   end
   module TermHash = Term_hash.TermHash (TermMod)
   module TermNorm = Term_norm.TermNorm (TermMod) (TermHash)

   module TermHeaderConstr (FromTerm : TermModuleSig) =
   struct
      module THC = Term_header_constr.TermHeaderConstr (FromTerm) (TermMod) (TermHash);;

      let make_term_header = THC.make_term_header
      let make_meta_term_header = THC.make_meta_term_header
      let make_msequent_header = THC.make_msequent_header
   end
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
