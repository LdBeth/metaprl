(*
 * Opnames are basically just lists of strings, although
 * they may be hash-consed.
 *)

type atom
type opname

(* Constructors *)
val nil_opname : opname
val mk_opname : string -> opname -> opname
val make_opname : string list -> opname

(* Atoms are always equal iff they are pointer equal *)
val intern : opname -> atom

(* Opnames should always be compared with this equality *)
val eq : opname -> opname -> bool

(* Opnames can be normalized when they are unmarshaled *)
val normalize_opname : opname -> opname

(* Destructors *)
val dest_opname : opname -> string list
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
