(*
 * A grammar for terms as quotatations.
 *)

open Opname
open Refiner.Refiner.Term
open Refiner.Refiner.TermMeta
open Filter_type

(*
 * Productions to extend.
 *)
module type TermGrammarSig =
sig
   val mk_opname : MLast.loc -> string list -> opname
   val term_eoi : term Grammar.Entry.e
   val term : term Grammar.Entry.e
   val quote_term : quote_term Grammar.Entry.e
   val mterm : meta_term Grammar.Entry.e
   val singleterm : aterm Grammar.Entry.e
   val bound_term : aterm Grammar.Entry.e
   val xdform : term Grammar.Entry.e
end

module MakeTermGrammar (TermGrammar : TermGrammarSig) : TermGrammarSig

(*
 * $Log$
 * Revision 1.3  1998/05/27 15:13:14  jyh
 * Functorized the refiner over the Term module.
 *
 * Revision 1.2  1997/08/06 16:17:41  jyh
 * This is an ocaml version with subtyping, type inference,
 * d and eqcd tactics.  It is a basic system, but not debugged.
 *
 * Revision 1.1  1997/04/28 15:51:06  jyh
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
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
