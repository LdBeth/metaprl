(*
 * Additional operations on lists.
 *)

(* Filter items out of a list *)
val filter : ('a -> bool) -> 'a list -> 'a list

(* Lexicographic comparison of two lists *)
val compare_lists : ('a -> 'b -> int) -> 'a list -> 'b list -> int

(*
 * These function raise Failure, not Invalid_argument.
 *)
val allp : ('a -> bool) -> 'a list -> bool
val existsp : ('a -> bool) -> 'a list -> bool
val for_all2 : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
val exists2 : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
val map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list

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
val remove_nth : int -> 'a list -> 'a list
val insert_nth : int -> 'a -> 'a list -> 'a list
val removeq : 'a -> 'a list -> 'a list

(* Find the index of the element that satisfies a predicate *)
val find : ('a -> bool) -> 'a list -> 'a
val find_item : ('a -> bool) -> 'a list -> int
val find_index : 'a -> 'a list -> int
val find_indexq : 'a -> 'a list -> int

(* Subtract an elenet from a list *)
val intersect : 'a list -> 'a list -> 'a list
val intersectq : 'a list -> 'a list -> 'a list
val subtract : 'a list -> 'a list -> 'a list
val subtractq : 'a list -> 'a list -> 'a list
val union : 'a list -> 'a list -> 'a list
val unionq : 'a list -> 'a list -> 'a list

(* Reverse iteration *)
val rev_iter : ('a -> 'b) -> 'a list -> unit
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

(*
 * List splitting.
 *)
val split_list : int -> 'a list -> 'a list * 'a list
val split_last : 'a list -> 'a list * 'a
val last : 'a list -> 'a

(*
 * $Log$
 * Revision 1.8  1998/06/03 22:19:22  jyh
 * Nonpolymorphic refiner.
 *
 * Revision 1.7  1998/04/28 18:30:31  jyh
 * ls() works, adding display.
 *
 * Revision 1.6  1998/04/23 20:04:36  jyh
 * Initial rebuilt editor.
 *
 * Revision 1.5  1998/04/21 19:53:54  jyh
 * Upgraded refiner for program extraction.
 *
 * Revision 1.4  1998/04/17 20:48:36  jyh
 * Updating refiner for extraction.
 *
 * Revision 1.3  1998/02/21 20:58:14  jyh
 * Two phase parse/extract.
 *
 * Revision 1.2  1998/02/12 23:35:22  jyh
 * Generalized file base to allow the library.
 *
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
