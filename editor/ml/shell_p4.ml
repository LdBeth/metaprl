(*
 * Define the additional grammar for the shell.
 *)

open Printf
open Debug

open Toploop

open Refiner.Refiner.Term

open Filter_ast
open Term_grammar

(* Force Dynlink to load *)
let _ = Dynlink.init

(************************************************************************
 * PRINTERS                                                             *
 ************************************************************************)

(*
 * This is the current display form database.
 *)
let df = ref None
    
let set_df df' =
   df := df'

let get_df found notfound =
   match !df with
      Some df ->
         found df
    | None ->
         notfound

(*
 * Printers.
 *)
let string_of_term t =
   (get_df Dform.string_of_term Simple_print.string_of_term) t

(*
 * Use format library for term printing.
 *)
let print_term t =
   Format.print_string (string_of_term t)

(************************************************************************
 * QUOTATIONS                                                           *
 ************************************************************************)

(*
 * This is a hack!
 * We have to get around the type system somehow.
 * The FilterCache modules have different type for
 * signatures and implementations, but their mk_opname
 * functions will have the same type.  However, at the
 * time that the TermGrammar is defined, we don't know if this
 * is a signature or implementation.  So instead we leave a
 * reference to the mk_opname.  The refence is set by the get_proc
 * method, below, which knows if this is an implementation or
 * interface.
 *)
let mk_opname_null _ =
   raise (Failure "Shell_p4.mk_opname: no current package")

let mk_opname_ref = ref mk_opname_null

let set_mk_opname = function
   Some f ->
      mk_opname_ref := f
 | None ->
      mk_opname_ref := mk_opname_null

(*
 * Base term grammar.
 *)
module TermGrammarBefore : TermGrammarSig =
struct
   let mk_opname loc l =
      try !mk_opname_ref l with
         exn ->
            Stdpp.raise_with_loc loc exn
   
   (*
    * Term grammar.
    *)
   let gram = Pcaml.gram
   let term_eoi = Grammar.Entry.create gram "term"
   let term = Grammar.Entry.create gram "term"
   let quote_term = Grammar.Entry.create gram "quote_term"
   let mterm = Grammar.Entry.create gram "mterm"
   let singleterm = Grammar.Entry.create gram "singleterm"
   let bound_term = Grammar.Entry.create gram "bound_term"
   let xdform = Grammar.Entry.create gram "xdform"
end

(*
 * Extended term grammar.
 *)
module TermGrammar = MakeTermGrammar (TermGrammarBefore)

(*
 * String -> string translator.
 *)
let term_exp s =
   let cs = Stream.of_string s in
   let t = Grammar.Entry.parse TermGrammar.term_eoi cs in
      build_ml_term (0, 0) t

let term_patt s =
   raise (Failure "Shell_p4.term_patt: not implemented yet")

let _ = Quotation.add "term" (Quotation.ExAst (term_exp, term_patt))
let _ = Quotation.default := "term"

(*
 * $Log$
 * Revision 1.3  1998/05/28 13:46:00  jyh
 * Updated the editor to use new Refiner structure.
 * ITT needs dform names.
 *
 * Revision 1.2  1998/04/28 18:29:56  jyh
 * ls() works, adding display.
 *
 * Revision 1.1  1998/04/23 20:04:14  jyh
 * Initial rebuilt editor.
 *
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
