(*
 * Define the additional grammar for the shell.
 *)

open Refiner.Refiner.TermType

open Tacticals
open Shell_p4_type

(*
 * Toploop implementation.
 *)
module ShellP4 : ShellP4Sig

(*
 * The toploop uses this function to return
 * the tactic that was compiled during eval_tactic.
 *)
val install_tactic : tactic -> unit

(*
 * Print a term.
 *)
val print_term : term -> unit

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
