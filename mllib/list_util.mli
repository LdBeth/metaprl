(*
 *
 *)

(* Filter items out of a list *)
val filter : ('a -> bool) -> 'a list -> 'a list

(* Lexicographic comparison of two lists *)
val compare_lists : ('a -> 'b -> int) -> 'a list -> 'b list -> int

(* Remove items marked by a vector of bools *)
val remove_elements : 'a list -> bool list -> 'a list
val remove_suffix : 'a list -> 'a list -> 'a list

(* Interated forms *)
val nth_tl : 'a list -> int -> 'a list

(* Functional replacement *)
val replacef_nth : 'a list -> int -> ('a -> 'a) -> 'a list
val replace_nth : 'a list -> int -> 'a -> 'a list
val remove_nth : 'a list -> int -> 'a list
val insert_nth : 'a list -> int -> 'a -> 'a list
val removeq : 'a list -> 'a -> 'a list

(* find the index of the element that satisfies a predicate *)
val find : 'a list -> ('a -> bool) -> 'a
val find_item : 'a list -> ('a -> bool) -> int
val find_index : 'a list -> 'a -> int
val find_indexq : 'a list -> 'a -> int

(* subtract an elenet from a list *)
val intersect : 'a list -> 'a list -> 'a list
val intersectq : 'a list -> 'a list -> 'a list
val subtract : 'a list -> 'a list -> 'a list
val subtractq : 'a list -> 'a list -> 'a list
val union : 'a list -> 'a list -> 'a list
val unionq : 'a list -> 'a list -> 'a list

(* Reverse iteration *)
val rev_iter : ('a -> 'b) -> 'a list -> unit
val flat_map : ('a -> 'b list) -> 'a list -> 'b list

(*
 * Association lists.
 *)
val zip_list : ('a * 'b) list -> 'a list -> 'b list -> ('a * 'b) list
val zip : 'a list -> 'b list -> ('a * 'b) list
val assoc_index : ('a * 'b) list -> 'a -> int
val assoc_replace : ('a * 'b) list -> 'a -> 'b -> ('a * 'b) list
val add_assoc : 'a * 'b -> ('a * 'b) list -> ('a * 'b) list

(*
 * List splitting.
 *)
val split_list : int -> 'a list -> 'a list * 'a list
val split_last : 'a list -> 'a list * 'a
val last : 'a list -> 'a

(*
 * $Log$
 * Revision 1.1  1997/08/06 16:18:00  jyh
 * This is an ocaml version with subtyping, type inference,
 * d and eqcd tactics.  It is a basic system, but not debugged.
 *
 * Revision 1.1  1997/04/28 15:51:22  jyh
 * This is the initial checkin of Nuprl-Light.
 * I am porting the editor, so it is not included
 * in this checkin.
 *
 * Directories:
 *     refiner: logic engine
 *     filter: front end to the Ocaml compiler
 *     editor: Emacs proof editor
 *     util: utilities
 *     mk: Makefile templates
 *
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
