(*
 * Overlay lists as small sets.
 *)

module Make (Set : Nl_set.S)
: Nl_set.S
  with type elt = Set.elt

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
