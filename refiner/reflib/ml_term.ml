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
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
