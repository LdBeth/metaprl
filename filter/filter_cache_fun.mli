(*
 * We add a layer to filterSummry, to allow inlined modules
 * and cached info about opnames, axioms, and precedences.
 *
 *)

open Term

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
 * $Log$
 * Revision 1.1  1998/02/19 17:13:56  jyh
 * Splitting filter_parse.
 *
 * Revision 1.2  1997/08/06 16:17:28  jyh
 * This is an ocaml version with subtyping, type inference,
 * d and eqcd tactics.  It is a basic system, but not debugged.
 *
 * Revision 1.1  1997/04/28 15:50:52  jyh
 * This is the initial checkin of Nuprl-LighStrMarshal.
 * I am porting the editor, so it is not included
 * in this checkin.
 *
 * Directories:
 *     refiner: logic engine
 *     filter: front end to the Ocaml compiler
 *     editor: Emacs proof editor
 *     util: utilities
 *     mk: Makefile templates
 *
 * Revision 1.2  1996/10/23 15:17:55  jyh
 * First working version of dT tactic.
 *
 * Revision 1.1  1996/09/02 19:42:47  jyh
 * Semi working package managemenStrMarshal.
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
