(*
 * Define the additional grammar for the shell.
 *)

open Refiner.Refiner.Term
open Dform
open Opname

(*
 * Set the current display form base.
 *)
val set_df : dform_base option -> unit

(*
 * This is the opname function used when terms are built.
 *)
val set_mk_opname : (string list -> opname) option -> unit

(*
 * Printers.
 *)
val print_term : term -> unit

(*
 * Get the tactic for the last refinement.
 *)
val get_tactic : unit -> string * MLast.expr

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
