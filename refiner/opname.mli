(*
 * Opnames are basically just lists of strings, although
 * they may be hash-consed.
 *)

type opname

(* Constructors *)
val nil_opname : opname
val mk_opname : string -> opname -> opname
val make_opname : string list -> opname

val normalize_opname : opname -> opname

(* Destructors *)
val dest_opname : opname -> string list
val flat_opname : opname -> string

(*
 *
 * $Log$
 * Revision 1.2  1998/05/07 16:02:51  jyh
 * Adding interactive proofs.
 *
 * Revision 1.1  1997/04/28 15:51:28  jyh
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
 * Revision 1.2  1996/09/02 19:43:22  jyh
 * Semi working package management.
 *
 * Revision 1.1  1996/04/07 18:27:09  jyh
 * Intermediate checking while updating dform commands.
 *
 * -*-
 * Local Variables:
 * Caml-master: "manager"
 * End:
 * -*-
 *)
