(*
 * A splay set is an implementation of
 * a set over an ordered type.
 *)
module type S =
sig
   type elt
   type t

   val empty : t
   val is_empty : t -> bool
   val mem : t -> elt -> bool
   val add : elt -> t -> t
   val make : elt -> t
   val remove : elt -> t -> t
   val union : t -> t -> t
   val elements : t -> elt list
   val iter : (elt -> unit) -> t -> unit
   val cardinal : t -> int
   val mem_filt : t -> elt list -> elt list
   val fst_mem_filt : t -> (elt * 'a) list -> (elt * 'a) list
end

(*
 * Build the set from an ordered type.
 *)
module Make (Ord: Set.OrderedType) : S with type elt = Ord.t

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
