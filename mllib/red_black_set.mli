(*
 * Implementation of sets based on red-black trees.
 *)

module type OrderedType =
sig
   type t

   val print : t -> unit
   val compare : t -> t -> int
end

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
   val not_mem_filt : t -> elt list -> elt list
   val intersectp : t -> t -> bool
   val of_list : elt list -> t

   val print : t -> unit
end

module MakeDebug (Ord : OrderedType)
: S with type elt = Ord.t

module Make (Ord : Set.OrderedType)
: Splay_set.S with type elt = Ord.t

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
