(*
 * We add a layer to filterSummry, to allow inlined modules
 * and cached info about opnames, axioms, and precedences.
 *
 *)

open Refiner.Refiner.Term

open Filter_summary_type
open Filter_summary

(*
 * SummaryCache constructor.
 *
 * The filter cache assumes that the type we store in the
 * library is summary_type.
 *)
module MakeFilterCache
   (SigMarshal : MarshalSig)
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
   with type select     = Base.select

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
