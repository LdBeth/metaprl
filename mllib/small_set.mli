(*
 * Overlay lists as small sets.
 *)

module Make (Set : Splay_set.S)
: Splay_set.S
  with type elt = Set.elt

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
