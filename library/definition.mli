
 open Basic
 open Refiner.Refiner.Term

 type term_entry
 type termtable

 val make_termtable	: unit -> termtable

 val apply_broadcast	: termtable -> 
				term (* data *) -> 
				term (* description *) -> 
				stamp (* transaction *) ->
				stamp option (* auto-commit *) ->
				unit

 val termtable_lookup	: termtable -> stamp -> object_id -> term_entry 

 val termtable_unit_map	: termtable -> stamp -> (object_id -> term_entry -> unit) -> unit
 val termtable_map	: termtable -> stamp -> (object_id -> term_entry -> 'a option) -> 'a list



 (* more specific funcs for termtable variants *)
 val roots		: termtable -> stamp -> (string * object_id) list
 val directory_p	: termtable -> stamp -> object_id -> bool
 val directory_children	: termtable -> stamp -> object_id -> (string * object_id) list

 (* would be nice if termtable were functorized so
    that term_entry need not be defined apriori. *)



