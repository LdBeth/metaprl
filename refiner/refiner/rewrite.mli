(*
 * This module specifies rewrite rules, which require second
 * order variables.  Each rule has a "redex" and a "contractum",
 * although rewrites can be performed in either direction.
 *
 *)

open Term_sig
open Term_man_sig
open Term_addr_sig
open Term_subst_sig
open Term_meta_sig
open Refine_errors_sig
open Rewrite_sig

module Rewrite (**)
   (Term : TermSig)
   (TermMan : TermManSig
    with type term = Term.term)
   (TermAddr : TermAddrSig
    with type term = Term.term)
   (TermSubst : TermSubstSig
    with type term = Term.term) 
   (RefineErrors : RefineErrorsSig
    with type term = Term.term
    with type param = Term.param
    with type level_exp = Term.level_exp
    with type bound_term = Term.bound_term
    with type address = TermAddr.address) :
   RewriteSig
   with type term = Term.term
   with type level_exp = Term.level_exp
   with type operator = Term.operator
   with type address = TermAddr.address

(*
 * $Log$
 * Revision 1.2  1998/07/01 04:36:58  nogin
 * Moved Refiner exceptions into a separate module RefineErrors
 *
 * Revision 1.1  1998/05/28 15:00:37  jyh
 * Partitioned refiner into subdirectories.
 *
 * Revision 1.4  1998/05/27 15:14:09  jyh
 * Functorized the refiner over the Term module.
 *
 * Revision 1.3  1998/04/29 14:48:21  jyh
 * Added ocaml_sos.
 *
 * Revision 1.2  1997/08/07 19:43:48  jyh
 * Updated and added Lori's term modifications.
 * Need to update all pattern matchings.
 *
 * Revision 1.1  1997/04/28 15:51:36  jyh
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
 * Revision 1.12  1996/03/25 20:50:51  jyh
 * Intermediate commit while modifying grammer.  Restricting
 * ML hooks to condition terms.
 *
 * Revision 1.11  1996/03/11 18:34:33  jyh
 * The filterModule module is untested, but it seems to work
 * correctly on most inputs, except for mlbegin ... mlend expressions.
 * That's the next task.
 *
 * Revision 1.10  1996/03/08 22:04:59  jyh
 * This rewriter contains the pattern analysis.
 *
 * Revision 1.9  1996/03/08 15:40:53  jyh
 * This version works for most constructs except for ML rewrites.
 * The next step will be to break apart the rewriter so that
 * redices and contracta can be compiled separately.
 *
 * Revision 1.8  1996/03/05 19:48:40  jyh
 * Preliminary version with logical framework.
 *
 * Revision 1.7  1996/02/18 23:32:33  jyh
 * Changin Format module to more Nuprl-like format.
 *
 * Revision 1.6  1996/02/13 21:32:36  jyh
 * This is an intermediate checkin while matching is being added to the rewriter.
 *
 * Revision 1.5  1996/02/07 23:41:22  jyh
 * First working version in CamlSpecialLight.
 *
 * Revision 1.4  1996/02/07 20:25:01  jyh
 * Partial checkin whil I change filenames to lowercase.
 *
 * Revision 1.3  1996/02/05 18:15:02  jyh
 * Merge context rewrites onto the main branch.
 *
 * Revision 1.2.4.1  1996/02/05 06:09:55  jyh
 * This version has the rewriter with contexts, and Rule application
 * in Sequent.ml, but it is not fully debugged.
 *
 * Revision 1.2  1996/01/26 20:15:03  jyh
 * This version has a complete rewriter using the simple term structure.
 * Next implement sequents and refinement.
 *
 * Revision 1.1  1995/12/06 16:42:57  jyh
 * This is an ML version of a term rewriting system.
 * This checkin is partial, and provides a rewriter on
 * regular terms.
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner.run"
 * End:
 * -*-
 *)

