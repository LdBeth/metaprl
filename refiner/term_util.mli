(*
 * Term utilities.
 *
 *)

open Term

(************************************************************************
 * META-TERMS                                                           *
 ************************************************************************)

(*
 * The terms in the meta-logical framework include
 * a meta-implication and met-iff.
 *)
type meta_term =
   MetaTheorem of term
 | MetaImplies of meta_term * meta_term
 | MetaFunction of term * meta_term * meta_term
 | MetaIff of meta_term * meta_term

exception MetaTermMatch of meta_term

(*
 * Some operations on meta_term.
 *)
val normalize_mterm : meta_term -> meta_term

val binding_vars : meta_term -> string list
val context_vars : meta_term -> string list
val meta_alpha_equal : meta_term -> meta_term -> bool
val unzip_mimplies : meta_term -> term list
val zip_mimplies : term list -> meta_term
val unzip_mfunction : meta_term -> (term option * term) list * term
val zip_mfunction : (term option * term) list -> term -> meta_term
val strip_mfunction : meta_term -> meta_term

(************************************************************************
 * OTHER UTILITIES                                                      *
 ************************************************************************)

(*
 * generalization: see if the first term generalizes the second term.
 * Return the alpha conversion if so, otherwise fail with
 * Invalid_argument "generalization"
 *
 * generalizes: boolean equivalent of the proceeding
 *)
val generalizes : term -> term -> bool
val generalization : (string * string) list -> term -> term -> (string * string) list

(*
 * Construct a redex out of some vars, params, and other terms.
 *)
val construct_redex : string array -> term list -> term list -> term

(*
 * $Log$
 * Revision 1.3  1998/04/09 15:26:48  jyh
 * Added strip_mfunction.
 *
 * Revision 1.2  1998/02/21 20:58:26  jyh
 * Two phase parse/extract.
 *
 * Revision 1.1  1997/04/28 15:51:49  jyh
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
 * Revision 1.3  1996/09/02 19:43:31  jyh
 * Semi working package management.
 *
 * Revision 1.2  1996/03/25 20:51:07  jyh
 * Intermediate commit while modifying grammer.  Restricting
 * ML hooks to condition terms.
 *
 * Revision 1.1  1996/03/11 18:34:48  jyh
 * The filterModule module is untested, but it seems to work
 * correctly on most inputs, except for mlbegin ... mlend expressions.
 * That's the next task.
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
