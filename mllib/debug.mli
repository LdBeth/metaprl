(*
 * Debugging tools.
 *
 *)

(*
 * Debug during load.
 *)
val debug_load : bool ref

(*
 * Debug display forms.
 *)
val debug_dform : bool ref

(*
 * Debug the rewriter.
 *)
val debug_rewrite : bool ref

(*
 * Debug the refiner.
 *)
val debug_refiner : bool ref

(*
 * Debug the printer.
 *)
val debug_simple_print : bool ref

(*
 * Debug Files
 *)
val debug_file_base : bool ref

(*
 * Debug the term grammar.
 *)
val debug_grammar : bool ref

(*
 * Resource and inheritance debugging.
 *)
val debug_resource : bool ref

(*
 * Library debugging.
 *)
val debug_library_base : bool ref

(*
 * Summary debugging.
 *)
val debug_summary : bool ref

(*
 * Conversion to program code.
 *)
val debug_filter_prog : bool ref

(*
 * Parser.
 *)
val debug_filter_parse : bool ref

(*
 * Debug FilterCache.
 *)
val debug_filter_cache : bool ref

(*
 * Print a list of strings.
 *)
val print_strings : string -> out_channel -> string list -> unit

(*
 * Flush output.
 *)
val eflush : out_channel -> unit

(*
 * $Log$
 * Revision 1.4  1998/04/28 18:30:25  jyh
 * ls() works, adding display.
 *
 * Revision 1.3  1998/04/24 02:42:27  jyh
 * Added more extensive debugging capabilities.
 *
 * Revision 1.2  1998/02/23 14:46:32  jyh
 * First implementation of binary file compilation.
 *
 * Revision 1.1  1997/08/06 16:17:52  jyh
 * This is an ocaml version with subtyping, type inference,
 * d and eqcd tactics.  It is a basic system, but not debugged.
 *
 * Revision 1.1  1997/04/28 15:51:16  jyh
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
 * Revision 1.4  1996/03/08 15:40:37  jyh
 * This version works for most constructs except for ML rewrites.
 * The next step will be to break apart the rewriter so that
 * redices and contracta can be compiled separately.
 *
 * Revision 1.3  1996/02/18 23:32:25  jyh
 * Changin Format module to more Nuprl-like format.
 *
 * Revision 1.2  1996/02/14 03:51:48  jyh
 * This is a version common to Caml-Light and Caml-Special-Light.
 *
 * Revision 1.1  1996/02/13 21:32:05  jyh
 * This is an intermediate checkin while matching is being added to the rewriter.
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner.run"
 * End:
 * -*-
 *)
