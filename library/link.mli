
open Refiner.Refiner.Term


type link

val connect_with_callback	: string (* remote hostname *) -> int (* remote socket *)
				-> int (* local socket *) -> link (*not complete*)
val connect_callback 		: link (*not complete*) -> link (*complete*)
				
val disconnect		: link -> unit

val send		: link -> term -> unit 

(*blocks on read*)
val recv		: link -> term

(*returns None if nothing on input channel*)
 val recv_nohang		: link -> term option


(*nuprl5 terms*)

val iconnect_term	: int -> string -> term

val idisconnect_term	: bool (* error-p *) -> term


(*testing purposes*)
val cautious_in		: Unix.file_descr ref
val cautious_out	: Unix.file_descr ref
val cautious_socket	: Unix.file_descr ref

