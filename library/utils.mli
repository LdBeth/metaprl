
  (* reference with option *)
type 'a oref

exception OrefNone

val null_oref	: unit -> 'a oref
val oref	: 'a -> 'a oref
val oref_set	: 'a oref -> 'a -> 'a
val oref_nullify	: 'a oref -> unit

val oref_p	: 'a oref -> bool
val oref_val	: 'a oref -> 'a		(* fails if None *)
val oref_option	: 'a oref -> 'a option


open List

(* some useful list hacking funcs *)

val assoc_if	: ('a -> bool) -> 'a list -> 'a option

(* removes first occurence *)
val remove_if	: ('a -> bool) -> 'a list
			 -> ('a option (* value removed, if any *) 
			      * 'a list)
val remove_from_end_if	: ('a -> bool) -> 'a list
			 -> ('a option (* value removed, if any *) 
			      * 'a list)
val remove_if'	: ('a -> bool) -> 'a list -> 'a list
val remove	: 'a -> 'a list -> 'a list

(* removes all occurences *)
val filter	: ('a -> bool) -> 'a list -> 'a list




