module type SerialMap =
  sig
    type key
    and 'a tt
    val empty : 'a tt
    val is_empty : 'a tt -> bool
    val cardinal : 'a tt -> int
    val add : 'a tt -> key -> 'a -> 'a tt
    val find : 'a tt -> key -> 'a
    val remove : 'a tt -> key -> 'a tt
    val mem : 'a tt -> key -> bool
    val iter : (key -> 'a -> unit) -> 'a tt -> unit
    val fold : ('a -> key -> 'b -> 'a) -> 'a -> 'b tt -> 'a
    val keys : 'a tt -> key list
    val data : 'a tt -> 'a list
  end
module SerialMapMake :
  functor (Base : Map.OrderedType) ->
    sig
      type key = Base.t
      and 'a tt
      val empty : 'a tt
      val is_empty : 'a tt -> bool
      val cardinal : 'a tt -> int
      val add : 'a tt -> key -> 'a -> 'a tt
      val find : 'a tt -> key -> 'a
      val remove : 'a tt -> key -> 'a tt
      val mem : 'a tt -> key -> bool
      val iter : (key -> 'a -> unit) -> 'a tt -> unit
      val fold : ('a -> key -> 'b -> 'a) -> 'a -> 'b tt -> 'a
      val keys : 'a tt -> key list
      val data : 'a tt -> 'a list
    end
