module type InfiniteROArraySig =
  sig
    type 'a descriptor
    val describe : 'a descriptor -> int
    val subscribe : int -> 'a descriptor
    type 'a t
    exception EmptySlot
    val create : int -> 'a t
    val store : 'a t -> 'a option -> 'a descriptor
    val get : 'a t -> 'a descriptor -> 'a option
  end

module InfiniteROArray : InfiniteROArraySig
