(*
 * Pretty printer for terms.
 *
 *)

open Opname
open Term
open Term_util
open Rformat

(************************************************************************
 * PRINTERS                                                             *
 ************************************************************************)

val format_simple_level_exp : buffer -> level_exp -> unit
val print_simple_level_exp_fp : out_channel -> level_exp -> unit
val print_simple_level_exp : level_exp -> unit
val prerr_simple_level_exp : level_exp -> unit
val string_of_level_exp : level_exp -> string

val string_of_opname : opname -> string

val format_simple_param : buffer -> param -> unit
val print_simple_param_fp : out_channel -> param -> unit
val print_simple_param : param -> unit
val prerr_simple_param : param -> unit
val string_of_param : param -> string

val format_simple_term : buffer -> term -> unit
val print_simple_term_fp : out_channel -> term -> unit
val print_simple_term : term -> unit
val prerr_simple_term : term -> unit
val string_of_term : term -> string

val format_simple_bterm : buffer -> bound_term -> unit
val print_simple_bterm_fp : out_channel -> bound_term -> unit
val print_simple_bterm : bound_term -> unit
val prerr_simple_bterm : bound_term -> unit
val string_of_bterm : bound_term -> string

val format_simple_mterm : buffer -> meta_term -> unit
val print_simple_mterm_fp : out_channel -> meta_term -> unit
val print_simple_mterm : meta_term -> unit
val prerr_simple_mterm : meta_term -> unit
val string_of_mterm : meta_term -> string

val print_simple_address_fp : out_channel -> address -> unit
val print_simple_address : address -> unit
val prerr_simple_address : address -> unit

(*
 * $Log$
 * Revision 1.1  1997/04/28 15:51:39  jyh
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
 * Revision 1.12  1996/05/21 02:14:19  jyh
 * This is a semi-working version before Wisconsin vacation.
 *
 * Revision 1.11  1996/04/07 18:24:55  jyh
 * This is an intermediate commit while adjusting the dforms.
 * We intend that dform printers just return a list of terms.
 *
 * Revision 1.10  1996/03/25 20:51:00  jyh
 * Intermediate commit while modifying grammer.  Restricting
 * ML hooks to condition terms.
 *
 * Revision 1.9  1996/03/08 15:40:58  jyh
 * This version works for most constructs except for ML rewrites.
 * The next step will be to break apart the rewriter so that
 * redices and contracta can be compiled separately.
 *
 * Revision 1.8  1996/02/25 15:16:23  jyh
 * This is a partial checkin as filterModule is being developed.
 * After the meta-logical framework is developed, sequent.* will go away.
 *
 * Revision 1.7  1996/02/19 18:47:11  jyh
 * Updating format.prl
 *
 * Revision 1.6  1996/02/18 23:32:38  jyh
 * Changin Format module to more Nuprl-like format.
 *
 * Revision 1.5  1996/02/13 21:33:11  jyh
 * This is an intermediate checkin while matching is being added to the rewriter.
 *
 * Revision 1.4  1996/02/07 23:41:51  jyh
 * First working version in CamlSpecialLight.
 *
 * Revision 1.3  1996/02/07 20:25:13  jyh
 * Partial checkin whil I change filenames to lowercase.
 *
 * Revision 1.2  1996/01/26 20:15:15  jyh
 * This version has a complete rewriter using the simple term structure.
 * Next implement sequents and refinement.
 *
 * Revision 1.1  1995/12/06 16:43:21  jyh
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

