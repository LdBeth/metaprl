module ArrayMemo :
  functor(Hash : Simplehash_sig.SimpleHashSig) ->
  functor(IAr : Infinite_ro_array_sig.InfiniteROArraySig) ->
sig
   type ('header, 'image) skeleton = 'header * 'image IAr.descriptor list
   and ('param, 'header, 'domain, 'image) t
   val create :
          ('param -> 'domain -> 'header * 'domain list) ->
          ('header -> 'header -> bool) ->
          ('param -> 'header -> 'image list -> 'image) -> ('param, 'header, 'domain, 'image) t
   val index : ('param, 'header, 'domain, 'image) t -> 'param -> 'domain               -> 'image IAr.descriptor
   val get   : ('param, 'header, 'domain, 'image) t -> 'param -> 'image IAr.descriptor -> 'image
   val apply : ('param, 'header, 'domain, 'image) t -> 'param -> 'domain               -> 'image
end

