module type S = 
sig
   type elt
   type t 

   val empty : t
   val is_empty : t -> bool
   val mem : elt -> t -> bool
   val add : elt -> t -> t
   val make : elt -> t
   val remove : elt -> t -> t
   val union : t -> t -> t
   val elements : t -> elt list
end

module Make (Ord: Set.OrderedType) : S with type elt = Ord.t

