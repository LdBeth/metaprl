(*
 * Term quotation tester.
 *)

open Printf
open Debug

open Ml_string
open Term_grammar

(*
 * Show the file loading.
 *)
let _ =
   if !debug_load then
      eprintf "Loading Term_quote%t" eflush


(************************************************************************
 * GRAMMAR                                                              *
 ************************************************************************)

(*
 * Grammar.
 *)
module TermGrammarBefore : TermGrammarSig =
struct
   let rec mk_opname = function
      op::t ->
         Opname.mk_opname op (mk_opname t)
    | [] ->
         Opname.nil_opname

   (*
    * Term grammar.
    *)
   let gram = Grammar.create Plexer.make
   let term_eoi = Grammar.Entry.create gram "term"
   let term = Grammar.Entry.create gram "term"
   let quote_term = Grammar.Entry.create gram "quote_term"
   let mterm = Grammar.Entry.create gram "mterm"
end

module TermGrammar = MakeTermGrammar (TermGrammarBefore)

(************************************************************************
 * QUOTATION                                                            *
 ************************************************************************)

(*
 * String -> string translator.
 *)
let term_exp s =
   let cs = Gstream.of_string s in
   let t = Grammar.Entry.parse TermGrammar.term_eoi cs in
   let file = StringFile.create () in
      StringPrint.print_term file t;
      StringFile.get file

let _ = Quotation.add "term" term_exp
let _ = Quotation.default := "term"

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
