(*
 * This file includes definitions for operators,
 * terms with delayed substitution, and regular terms.
 *
 * Note: many functions in this module (most dest_* and
 * is_* functions, shape, etc) have side-effect:
 * if the term is a Subst, they push substitutions one step down
 * and _replace_ the referenced term with the resulting Term term_single.
 * alpha_equal* functions may push down and eliminate all the Substs, not
 * only the top ones.
 *)

open Term_ds_sig

module TermType : TermDsTypeSig
