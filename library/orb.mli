
open Term
open Basic
open Definition

type connection
type environment
type orb

val orb_open	: string (* mnemonic *)  -> orb

val connect	: orb	-> string (* remote hostname *) -> int (* remote socket *)
			-> int (* local socket *)
			-> connection

val disconnect	: orb -> connection -> unit

val orb_close	: orb -> unit

val resource			: environment -> string -> termtable

val open_library_environment	: connection			
				-> string			(* "" means new lib env, otherwise -> restore *)
				-> (term -> term) 		(* local eval *)
				-> environment

val close_library_environment	: environment -> string


val join_library_environment	: connection			
				-> string list			
				-> (term -> term) 		(* local eval *)
				-> environment

val leave_library_environment	: environment -> unit

val restore_library_environment	: connection			
				-> string
				-> (term -> term) 		(* local eval *)
				-> environment



val eval_string		: environment -> term (* tid *) -> string -> unit
val eval		: environment -> term (* tid *) -> term -> unit
val eval_args		: environment -> term (* tid *) -> term -> term list -> unit
 
val eval_string_to_term		: environment -> term (* tid *) -> string -> term
val eval_to_term		: environment -> term (* tid *) -> term -> term
val eval_args_to_term		: environment -> term (* tid *) -> term -> term list -> term


(* all these callback variants seem overkill.
   single callback with checkpoint option  should be sufficient
   as other reqs can be done within call back.
 *)

(* twould be cleaner if tid were term *)
val eval_callback	: bool -> environment
				-> term (* tid *)
				-> (term -> unit)
				-> unit


(* 
   intention is to not allow g to mask exception from f.
   g may handle failure of f but if f and g both fail then the exception is from f.

   this is important in callbacks to the library, as if f is local eval then failure
   in f needs to be passed back through the library. The original failure can not be
   marshalled/unmarshalled thus it must be cached locally. with fail protect is an
   abstract method of accomplishing this.
 *)
 val with_fail_protect	: (('b -> unit) -> unit) (* g *) -> ('b -> 'a) (* f *) -> 'a


(*
val eval_with_callback		: environment
					-> bound_term (* tid *)
					-> (term -> unit) -> term
					-> unit

val eval_to_term_with_callback		: environment
					-> bound_term (* tid *)
					-> (term -> unit) -> term
					-> term

val eval_args_with_callback		: environment
					-> bound_term (* tid *)
					-> (term -> unit) -> term -> term list
					-> unit

val eval_args_to_term_with_callback	: environment
					-> bound_term (* tid *)
					-> (term -> unit) -> term -> term list
					-> term
*)

(*
val request_loop	: environment -> unit
   (* only viable if the local eval hook supplied *)
*)

val orb_req_loop 	: environment -> unit
