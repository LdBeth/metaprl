
open Term

type connection
type environment
type orb

val orb_open	: string (* mnemonic *)  -> orb

val connect	: orb	-> string (* remote hostname *) -> int (* remote socket *)
			-> int (* local socket *)
			-> connection

val disconnect	: orb -> connection -> unit

val orb_close	: orb -> unit


val open_library_environment	: connection			
				-> string			(* "" means new lib env, otherwise -> restore *)
				-> (bound_term list -> unit) 	(* broadcast hook *)
				-> (term -> term) 		(* local eval *)
				-> environment

val close_library_environment	: environment -> unit

(*
val open_library_environment	: connection		
				-> string			(* "" means new lib env, otherwise -> restore *)
				-> (term -> unit) 		(* broadcast hook *)
				-> (term -> term) option	(* local eval *)
				-> (environment -> term (* stamp *) -> unit)	(* start hook *)
				-> environment
*)

val eval_string		: environment -> bound_term (* tid *) -> string -> unit
val eval		: environment -> bound_term (* tid *) -> term -> unit
val eval_args		: environment -> bound_term (* tid *) -> term -> term list -> unit
 
val eval_string_to_term		: environment -> bound_term (* tid *) -> string -> term
val eval_to_term		: environment -> bound_term (* tid *) -> term -> term
val eval_args_to_term		: environment -> bound_term (* tid *) -> term -> term list -> term

val eval_with_callback		: environment
					-> bound_term (* tid *)
					-> (term -> unit) -> term -> term list
					-> unit

val eval_to_term_with_callback	: environment
					-> bound_term (* tid *)
					-> (term -> unit) -> term -> term list
					-> term

(*
val request_loop	: environment -> unit
   (* only viable if the local eval hook supplied *)
*)
