(************************************************************************
 * REFINER ERRORS                                                       *
 ************************************************************************)

open Term_sig
open Term_addr_sig
open Term_meta_sig
open Refine_errors_sig

module RefineErrors
   (Term : TermSig)
   (TermAddr : TermAddrSig
    with type term = Term.term)
   (TermMeta : TermMetaSig
    with type term = Term.term)
: RefineErrorsSig 
with type term = Term.term
with type bound_term = Term.bound_term
with type param = Term.param
with type address = TermAddr.address
with type level_exp = Term.level_exp
with type meta_term = TermMeta.meta_term
