(*
 * Utilities on filenames.
 *)

val split : string -> string list

(*
 * Components of the path.
 *)
val tail : string -> string
val root : string -> string

(*
 * $Log$
 * Revision 1.1  1998/02/24 05:33:16  jyh
 * Added filename utilities.
 *
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
