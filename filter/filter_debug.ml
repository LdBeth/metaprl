(*
 * Debug flags for the compiler.
 *)

(*
 * Debug the term grammar.
 *)
let debug_grammar = false

(*
 * Resource and inheritance debugging.
 *)
let debug_resource = false

(*
 * Library debugging.
 *)
let debug_library_base = false

(*
 * Summary debugging.
 *)
let debug_summary = false

(*
 * Conversion to program code.
 *)
let debug_filter_prog = false

(*
 * Parser.
 *)
let debug_filter_parse = false

(*
 * Debug FilterCache.
 *)
let debug_filter_cache = false

(*
 * $Log$
 * Revision 1.5  1998/04/08 20:47:22  jyh
 * Errnoneous label.
 *
 * Revision 1.4  1998/04/08 14:57:08  jyh
 * ImpDag is in mllib.
 *
 * Revision 1.3  1998/02/23 14:46:05  jyh
 * First implementation of binary file compilation.
 *
 * Revision 1.2  1998/02/12 23:38:05  jyh
 * Added support for saving intermediate files to the library.
 *
 * Revision 1.1  1997/04/28 15:50:52  jyh
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
 * Caml-master: "prlcomp.run"
 * End:
 * -*-
 *)
