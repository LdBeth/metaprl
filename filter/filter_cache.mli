(*
 * We add a layer to filterSummry, to allow inlined modules
 * and cached info about opnames, axioms, and precedences.
 *
 *)

open Term

open Filter_cache_type

module MakeFilterCache (Proof : ProofSig)
: FilterCacheSig
  with type proof = Proof.proof
      
(*
 * $Log$
 * Revision 1.5  1998/04/13 17:08:29  jyh
 * Adding interactive proofs.
 *
 * Revision 1.4  1998/02/19 21:08:20  jyh
 * Adjusted proof type to be primitive or derived.
 *
 * Revision 1.3  1998/02/19 17:13:55  jyh
 * Splitting filter_parse.
 *
 * Revision 1.2  1997/08/06 16:17:28  jyh
 * This is an ocaml version with subtyping, type inference,
 * d and eqcd tactics.  It is a basic system, but not debugged.
 *
 * Revision 1.1  1997/04/28 15:50:52  jyh
 * This is the initial checkin of Nuprl-Light.
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
 * Semi working package management.
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
