(*
 * Use Obj marshaler to convert between strings and terms.
 *)

open Refiner.Refiner.Term
open Refiner.Refiner.TermMeta
open Term_copy

(*
 * Convert to a string.
 *)
let string_of_term t =
   Marshal.to_string (denormalize_term_single t) []

(*
 * Convert from a string.
 *)
let term_of_string s =
   let t = (Marshal.from_string s 0 : Refiner_std.Refiner.TermType.term) in
      normalize_term_single t

(*
 * Convert to a string.
 *)
let string_of_mterm t =
   Marshal.to_string (denormalize_meta_term_single t) []

(*
 * Convert from a string.
 *)
let mterm_of_string s =
   let t = (Marshal.from_string s 0 : Refiner_std.Refiner.TermType.meta_term) in
      normalize_meta_term_single t

(*
 * $Log$
 * Revision 1.4  1998/07/03 22:05:44  jyh
 * IO terms are now in term_std format.
 *
 * Revision 1.3  1998/07/02 22:24:57  jyh
 * Created term_copy module to copy and normalize terms.
 *
 * Revision 1.2  1998/06/17 15:46:00  jyh
 * Optimizing compiler.
 *
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
