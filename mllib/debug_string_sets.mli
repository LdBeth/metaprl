(*
 * Combine two sets to check that they have the same
 * behavior.
 *)
module Make
   (Set1: Splay_set.S with type elt = string)
   (Set2: Splay_set.S with type elt = string)
      : Splay_set.S with type elt = string

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
