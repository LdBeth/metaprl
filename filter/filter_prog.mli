(*
 * Conversion form filter_summary to program text.
 *)

open Filter_cache_type

module MakeFilterProg (Cache : FilterCacheSig)
: ExtractSig
  with type proof_type = Cache.proof_type

(*
 * $Log$
 * Revision 1.4  1998/04/13 17:08:36  jyh
 * Adding interactive proofs.
 *
 * Revision 1.3  1998/04/09 18:25:52  jyh
 * Working compiler once again.
 *
 * Revision 1.2  1998/02/23 14:46:16  jyh
 * First implementation of binary file compilation.
 *
 * Revision 1.1  1998/02/21 20:57:49  jyh
 * Two phase parse/extract.
 *
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
