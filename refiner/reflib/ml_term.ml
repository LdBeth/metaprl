(*
 * Use Obj marshaler to convert between strings and terms.
 *)

open Refiner.Refiner.Term
open Refiner.Refiner.TermMeta

(*
 * Convert to a string.
 *)
let string_of_term (t : term) =
   Marshal.to_string t []

(*
 * Convert from a string.
 *)
let term_of_string s =
   (Marshal.from_string s 0 : term)

(*
 * Convert to a string.
 *)
let string_of_mterm (t : meta_term) =
   Marshal.to_string t []

(*
 * Convert from a string.
 *)
let mterm_of_string s =
   (Marshal.from_string s 0 : meta_term)


(*
 * $Log$
 * Revision 1.1  1998/06/17 13:18:31  jyh
 * Added ml_term.
 *
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
