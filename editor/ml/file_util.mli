(*
 * Some utilities for working fwith filenames.
 *
 *)

val parse_path : string list -> string -> string list
val build_path : string list -> string
val path_dir : string -> string
val path_file : string -> string

(*
 * $Log$
 * Revision 1.1  1997/08/06 16:17:12  jyh
 * This is an ocaml version with subtyping, type inference,
 * d and eqcd tactics.  It is a basic system, but not debugged.
 *
 * Revision 1.1  1996/09/02 19:32:59  jyh
 * Semi-working package management.
 *
 * -*-
 * Local Variables:
 * Caml-master: "editor.run"
 * End:
 * -*-
 *)
