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
 * Debugging.
 *)
val debug_grammar : bool ref

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
