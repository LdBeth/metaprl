(*
 * Commands for editing a rewrite.
 *)

include Shell_type
include Package_info
include Package_df

(*
 * Make an editable rewrite.
 *)
val create : Package.package -> Filter_prog.t -> string -> edit_object

(*
 * $Log$
 * Revision 1.1  1998/04/17 20:48:18  jyh
 * Updating refiner for extraction.
 *
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
