
 open Term
 open Object_id

 type library
 and transaction

 type connection


 (*
  *	Library
  *)

 val connect	: string (* remote hostname *) -> int (* remote socket *)
			-> int (* local socket *)
			-> connection

 val disconnect	: connection -> unit

 val lib_open	: connection -> library

 val save	: library -> (transaction -> unit) -> string
 val oid_export	: transaction -> object_id -> string
   (* only works in scope of checkpoint *)		

 val restore	: connection
			-> string -> (transaction -> unit) -> library

 val oid_import	: transaction -> string -> object_id
      (* only works in scope of restore *)	

 val lib_close	: library -> unit


 (*
  *	Transaction
  *)
 val with_transaction		: library -> (transaction -> 'a) -> 'a
 val with_local_transaction	: library -> (transaction -> 'a) -> 'a


 val create		: transaction
				-> string (* object type *)
				-> object_id (* dir *)
				-> object_id
 val create_top		: transaction
				-> string (* object type *)
				-> object_id

 val delete		: transaction -> object_id -> unit

 val parent		: transaction -> object_id -> object_id
 val children		: transaction -> object_id -> object_id list

 val put		: transaction -> object_id -> term -> unit
 val get		: transaction -> object_id -> term

 val put_property 	: transaction -> object_id -> string -> term -> unit
 val get_property 	: transaction -> object_id -> string -> term

 (* this is not meant as total solution but just as an example of the kinds of 
    things which can easily be done.
  *)
 (* mnemonic could be string list insead of term *)
 find			: transaction -> term   (* mnemonic *) -> object_id
 find_child		: transaction -> string (* mnemonic *) -> object_id (* parent *) -> object_id
