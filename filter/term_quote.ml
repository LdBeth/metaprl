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
 * $Log$
 * Revision 1.4  1998/06/01 13:53:26  jyh
 * Proving twice one is two.
 *
 * Revision 1.3  1998/04/24 19:38:44  jyh
 * Updated debugging.
 *
 * Revision 1.2  1998/04/24 02:42:23  jyh
 * Added more extensive debugging capabilities.
 *
 * Revision 1.1  1997/04/28 15:51:07  jyh
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
