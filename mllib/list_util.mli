(*
 * Additional operations on lists.
 *)

(* Filter items out of a list *)
val filter : ('a -> bool) -> 'a list -> 'a list

(* Lexicographic comparison of two lists *)
val compare_lists : ('a -> 'b -> int) -> 'a list -> 'b list -> int

(* Elements must by physically equal *)
val compare_eq : 'a list -> 'a list -> bool

(*
 * These functions are just liek the List functions
 * but they raise Failure, not Invalid_argument.
 *)
val nth : 'a list -> int -> 'a
val allp : ('a -> bool) -> 'a list -> bool
val existsp : ('a -> bool) -> 'a list -> bool
val for_all2 : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
val exists2 : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
val map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
val fold_left2 : ('a -> 'b -> 'c -> 'a) -> 'a -> 'b list -> 'c list -> 'a
val iter2 : ('a -> 'b -> unit) -> 'a list -> 'b list -> unit

(* Remove items marked by a vector of bools *)
val remove_elements : bool list -> 'a list -> 'a list
val remove_suffix : 'a list -> 'a list -> 'a list

(* Interated forms *)
val nth_tl : int -> 'a list -> 'a list

(* Functional replacement *)
val replacef_nth : int -> ('a -> 'a) -> 'a list -> 'a list
val replacef_arg_nth : int -> ('a -> 'a * 'b) -> 'a list -> 'a list * 'b
val replace_nth : int -> 'a -> 'a list -> 'a list
val replaceq : 'a -> 'a -> 'a list -> 'a list
val replace_first : ('a -> bool) -> 'a -> 'a list -> 'a list
val replace_all : ('a -> bool) -> 'a -> 'a list -> 'a list

val remove_nth : int -> 'a list -> 'a list
val insert_nth : int -> 'a -> 'a list -> 'a list
val removeq : 'a -> 'a list -> 'a list

(* Find the index of the element that satisfies a predicate *)
val find : ('a -> bool) -> 'a list -> 'a
val find_item : ('a -> bool) -> 'a list -> int
val find_index : 'a -> 'a list -> int
val find_indexq : 'a -> 'a list -> int

(* Set-like operations *)
val intersect : 'a list -> 'a list -> 'a list
val intersectq : 'a list -> 'a list -> 'a list
val subtract : 'a list -> 'a list -> 'a list
val subtractq : 'a list -> 'a list -> 'a list
val union : 'a list -> 'a list -> 'a list
val unionq : 'a list -> 'a list -> 'a list

(*
 * Reverse iteration
 *)
val rev_iter : ('a -> 'b) -> 'a list -> unit
val rev_iter2 : ('a -> 'b -> unit) -> 'a list -> 'b list -> unit
val flat_map : ('a -> 'b list) -> 'a list -> 'b list
val fail_map : ('a -> 'b) -> 'a list -> 'b list
val some_map : ('a -> 'b option) -> 'a list -> 'b list
val fold_left : ('a -> 'b -> 'a * 'c) -> 'a -> 'b list -> 'a * 'c list

(*
 * Association lists.
 *)
val zip_list : ('a * 'b) list -> 'a list -> 'b list -> ('a * 'b) list
val zip : 'a list -> 'b list -> ('a * 'b) list
val assoc_index : ('a * 'b) list -> 'a -> int
val assoc_replace : ('a * 'b) list -> 'a -> 'b -> ('a * 'b) list
val add_assoc : 'a * 'b -> ('a * 'b) list -> ('a * 'b) list
val assoc_in_range : ('b -> 'c -> bool) -> 'b -> ('a * 'c) list -> bool

(*
 * List splitting.
 *)
val split_list : int -> 'a list -> 'a list * 'a list
val split_last : 'a list -> 'a list * 'a
val last : 'a list -> 'a
val fst_split : ('a * 'b) list -> 'a list

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
