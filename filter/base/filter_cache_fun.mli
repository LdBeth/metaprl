(*
 * We add a layer to filterSummry, to allow inlined modules
 * and cached info about opnames, axioms, and precedences.
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
 * Author: Jason Hickey <jyh@cs.cornell.edu>
 * Modified by: Aleksey Nogin <nogin@cs.cornell.edu>
 *)

open Filter_type
open Filter_summary_type

(*
 * SummaryCache constructor.
 *
 * The filter cache assumes that the type we store in the
 * library is summary_type.
 *)
module MakeFilterCache
   (SigMarshal : MarshalSig
      with type ctyp = MLast.ctyp
      with type resource = MLast.ctyp resource_sig)
   (StrMarshal : MarshalSig
      with type ctyp = SigMarshal.ctyp
      with type select = SigMarshal.select
      with type cooked = SigMarshal.cooked)
   (Base : SummaryBaseSig
           with type select = SigMarshal.select
           with type cooked = SigMarshal.cooked) :
   SummaryCacheSig
   with type sig_proof  = SigMarshal.proof
   with type sig_ctyp   = SigMarshal.ctyp
   with type sig_expr   = SigMarshal.expr
   with type sig_item   = SigMarshal.item
   with type str_proof  = StrMarshal.proof
   with type str_ctyp   = StrMarshal.ctyp
   with type str_expr   = StrMarshal.expr
   with type str_item   = StrMarshal.item
   with type str_resource = StrMarshal.resource
   with type select     = Base.select
   with type arg        = Base.arg

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
