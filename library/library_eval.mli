
open Refiner.Refiner.Term

(*
 *	uses NUPRLLIB_PORT & NUPRLLIB_HOST environment variables.
 *	
 *	lib mnemonic should be "NuprlLight";
 *	
 *	Then just use following functions to connect to lib and loop:
 *	
 *	rhook : (term (*goal*) -> term (*tactic*) -> term list(*subgoals*) )
 *	
 *	Any catchable errors thrown by the rhook will be coerced to a term and returned
 *	to caller. The coercion will result in readable errors being returned to the
 *	proof editro if the library Basic.error function is used.
 *	
 *)	


val library_open_eval	: string (* lib mnemonic *)
				-> (term -> term -> term list)
				-> unit

(*
 *	library must be open.
 *	multiple calls to loop_eval ok as long as no intevening close.
 *	doesn't return until lib sends quit term.
 *)

val library_loop_eval	: unit -> unit

val library_close	: unit -> unit



(*
 *	Following combines above three steps into one call.
 *	library closed at loop quit.
 *)

val library_open_and_loop_eval	: string -> (term -> term -> term list) -> unit


(*	a trivial refiner hook for testing : *)
val faux_refine : term -> term -> term list





(*	
 *	<ref_req>	: !nuprl5_implementation!{!refine:t}(<term{goal}>; <term{tactic}>; <obids>)
 *	
 *	<obids>		: !nuprl5_implementation!{!oid:t,<obid>:o list}()
 *	
 *	The obid list are those which would trivially cause dependency loops if referenced.
 *	In this application it seems save to ignore those.
 *	
 *	
 *	Any catchable errors thrown by the ehook will be coerced to a term and returned
 *	to caller. The coercion will result in readable errors if the library Basic.error
 *	function is used.
 *)

