(*
 * Opnames are basically just lists of strings, although
 * they may be hash-consed.
 *
 * $Log$
 * Revision 1.1  1997/08/06 16:17:43  jyh
 * This is an ocaml version with subtyping, type inference,
 * d and eqcd tactics.  It is a basic system, but not debugged.
 *
 * Revision 1.2  1996/09/02 19:43:22  jyh
 * Semi working package management.
 *
 * Revision 1.1  1996/04/07 18:27:09  jyh
 * Intermediate checking while updating dform commands.
 *
 *)

type opname;;

(* Constructors *)
val nil_opname : opname;;
val mk_opname : string -> opname -> opname;;
val make_opname : string list -> opname;;

val normalize_opname : opname -> opname;;

(* Destructors *)
val dest_opname : opname -> string list;;

(*
 * -*-
 * Local Variables:
 * Caml-master: "manager"
 * End:
 * -*-
 *)
