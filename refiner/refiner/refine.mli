(*
 * The refiner works on proof trees, which are trees of sequents.
 * A basic refinement takes a sequent (a "goal") and produces a
 * list of sequents (the "subgoals"), and an extract term.  The type
 * "tactic" packages refinements, and elements of tactics are
 * always "correct" in the sense that they can be reduced to steps
 * of primitive inferences.
 *
 * The refiner also tracks rewrites, and just as for tactics,
 * elements of type Rewrite are always "correct".
 *
 *)

open Term_sig
open Term_man_sig
open Term_subst_sig
open Term_addr_sig
open Term_meta_sig
open Rewrite_sig
open Refine_sig

module Refine (**)
   (Term : TermSig)
   (TermMan : TermManSig
    with type term = Term.term)
   (TermSubst : TermSubstSig
    with type term = Term.term)
   (TermAddr : TermAddrSig
    with type term = Term.term)
   (TermMeta : TermMetaSig
    with type term = Term.term)
   (Rewrite : RewriteSig
    with type term = Term.term
    with type address = TermAddr.address)
: RefineSig
with type term = Term.term
with type address = TermAddr.address
with type meta_term = TermMeta.meta_term
with type rewrite_error = Rewrite.rewrite_error

(*
 * $Log$
 * Revision 1.1  1998/05/28 15:00:25  jyh
 * Partitioned refiner into subdirectories.
 *
 * Revision 1.2  1998/05/27 15:13:53  jyh
 * Functorized the refiner over the Term module.
 *
 * Revision 1.1  1997/04/28 15:51:32  jyh
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
 * Revision 1.19  1996/11/13 22:58:09  jyh
 * Initial version of forward/backward chaining cache.
 *
 * Revision 1.18  1996/10/23 15:17:57  jyh
 * First working version of dT tactic.
 *
 * Revision 1.17  1996/09/25 22:52:00  jyh
 * Initial "tactical" commit.
 *
 * Revision 1.16  1996/05/21 02:14:05  jyh
 * This is a semi-working version before Wisconsin vacation.
 *
 * Revision 1.15  1996/03/25 20:50:45  jyh
 * Intermediate commit while modifying grammer.  Restricting
 * ML hooks to condition terms.
 *
 * Revision 1.14  1996/03/11 18:34:25  jyh
 * The filterModule module is untested, but it seems to work
 * correctly on most inputs, except for mlbegin ... mlend expressions.
 * That's the next task.
 *
 * Revision 1.13  1996/03/08 15:40:49  jyh
 * This version works for most constructs except for ML rewrites.
 * The next step will be to break apart the rewriter so that
 * redices and contracta can be compiled separately.
 *
 * Revision 1.12  1996/03/05 19:48:36  jyh
 * Preliminary version with logical framework.
 *
 * Revision 1.11  1996/02/19 18:46:58  jyh
 * Updating format.prl
 *
 * Revision 1.10  1996/02/18 23:32:32  jyh
 * Changin Format module to more Nuprl-like format.
 *
 * Revision 1.9  1996/02/14 03:51:51  jyh
 * This is a version common to Caml-Light and Caml-Special-Light.
 *
 * Revision 1.8  1996/02/13 21:32:31  jyh
 * This is an intermediate checkin while matching is being added to the rewriter.
 *
 * Revision 1.7  1996/02/10 20:19:56  jyh
 * Initial checkin of filter (prlcomp).
 *
 * Revision 1.6  1996/02/08 16:02:32  jyh
 * Adding type Theory.
 *
 * Revision 1.5  1996/02/07 23:41:17  jyh
 * First working version in CamlSpecialLight.
 *
 * Revision 1.4  1996/02/07 20:24:57  jyh
 * Partial checkin whil I change filenames to lowercase.
 *
 * Revision 1.3  1996/02/07 17:34:09  jyh
 * This is Version 0 of the refiner in Caml-Light.  At this point,
 * Caml-Light becomes a branch, and main development will be
 * in Caml-Special-Light.
 *
 * Revision 1.2  1996/02/05 18:14:56  jyh
 * Merge context rewrites onto the main branch.
 *
 * Revision 1.1.4.1  1996/02/05 06:09:53  jyh
 * This version has the rewriter with contexts, and Rule application
 * in Sequent.ml, but it is not fully debugged.
 *
 * Revision 1.1  1996/01/31 20:02:40  jyh
 * Generalizing rewriter to work on Sequents.
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner.run"
 * End:
 * -*-
 *)