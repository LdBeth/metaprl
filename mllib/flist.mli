(*
 * Nonempty list with quick access to first and last,
 * and append operations.
 *)

type 'a t

type 'a tree =
   Empty
 | Leaf of 'a
 | Append of 'a tree * 'a tree

val create : 'a -> 'a t
val first : 'a t -> 'a
val last : 'a t -> 'a
val singleton : 'a t -> bool
val append : 'a t -> 'a t -> 'a t

(*
 * This operation appends the two lists,
 * without the last elemnt from the first list,
 * without the first element from the second list,
 * and the second argument in their place.
 *)
val append_skip : 'a t -> 'a -> 'a t -> 'a t

(*
 * Get the tree.
 * This is O(1).
 *)
val tree_of_list : 'a t -> 'a tree

(*
 * $Log$
 * Revision 1.1  1998/06/23 22:12:13  jyh
 * Improved rewriter speed with conversion tree and flist.
 *
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
