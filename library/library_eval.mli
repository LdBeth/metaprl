
open Term

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


val library_open_eval	: string (* lib mnemonic *)
				-> (term (*goal*) -> term (*tactic*) -> term list(*subgoals*) )
				-> unit

val library_close	: unit -> unit


(*
 *	library must be open.
 *	multiple calls to loop_eval ok as long as no intevening close.
 *	doesn't return until lib sends quit term.
 *)

val library_loop_eval	: unit -> unit



(*
 *	library closed at loop quit.
 *)

val library_open_and_loop_eval	: string -> (term -> term -> term list) -> unit


val refine_req_p	: term -> bool
val refine_args		: term -> term * term