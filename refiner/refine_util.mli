(*
 * Some utilities on refiner objects.
 *)

open Term
open Refine_sig

(*
 * Alpha equality on sequent objects.
 * Tactic argument is ignored
 *)
val msequent_alpha_equal : msequent -> msequent -> bool
val tactic_arg_alpha_equal : 'a tactic_arg -> 'b tactic_arg -> bool

(*
 * Utils.
 *)
val msequent_of_tactic_arg : 'a tactic_arg -> msequent
val split_sequent_list : msequent list -> term list * term list list

(*
 * $Log$
 * Revision 1.1  1998/04/22 14:14:32  jyh
 * Utilities for the refiner.
 *
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
