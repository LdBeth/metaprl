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
val string_of_opname : opname -> string

(*
 * Debugging.
 *)
val debug_opname : bool ref

(*
 * -*-
 * Local Variables:
 * Caml-master: "manager"
 * End:
 * -*-
 *)
