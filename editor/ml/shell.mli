(*
 * General purpose toploop.
 *)

include Tacticals
include Package_info
include Package_df
include Shell_rewrite
include Shell_rule

open Refiner.Refiner.Term

open Tacticals

open Shell_p4_type

module Shell (ShellP4 : ShellP4Sig) :
sig
   (*
    * Some initialization function.
    *)
   val main : unit -> unit

   (*
    * Navigation and display.
    *)
   val cd : string -> string
   val pwd : unit -> string
   val set_window_width : int -> unit

   (*
    * Module commands.
    *)
   val load : string -> unit
   val create_pkg : string -> unit
   val set_writeable : unit -> unit
   val save : unit -> unit
   val save_all : unit -> unit

   (*
    * The possible objects in a package.
    *)
   val create_rw : string -> unit
   val create_axiom : string -> unit
   val create_thm : string -> unit
   val create_tptp : string -> unit
   val create_opname : string -> unit
   val create_condition : string -> unit
   val create_parent : string -> unit
   val create_dform : string -> unit
   val create_prec : string -> unit
   val create_prec_rel : string -> string -> string -> unit
   val create_resource : string -> unit
   val create_infix : string -> unit
   val create_ml : string -> unit

   (*
    * View, close, check object.
    * An object is not installed until it is checked.
    *)
   val view : string -> unit
   val ls : unit -> unit

   (*
    * Editing commands.
    *)
   val set_goal : term -> unit
   val set_redex : term -> unit
   val set_contractum : term -> unit
   val set_assumptions : term list -> unit
   val set_params : term Filter_summary.param list -> unit
   val check : unit -> unit
   val expand : unit -> unit

   (*
    * Proof editing.
    *)
   val root : unit -> unit
   val up : int -> unit
   val down : int -> unit
   val goal : unit -> Tactic_type.tactic_arg
   val refine : tactic -> unit
   val undo : unit -> unit
   val fold : unit -> unit
   val fold_all : unit -> unit
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "editor.top"
 * End:
 * -*-
 *)
