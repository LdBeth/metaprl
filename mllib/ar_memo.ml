open List
open Infinite_ro_array_sig
open Simplehashtbl

module ArrayMemo =
  functor(Hash : Simplehash_sig.SimpleHashSig) ->
  functor(IAr : Infinite_ro_array_sig.InfiniteROArraySig) ->
struct

   (************************************************************************
    * TYPES                                                                *
    ************************************************************************)

   type ('header, 'image) skeleton = 'header * 'image IAr.descriptor list

   type ('param, 'header, 'domain, 'image) t =
      {
        make_header : 'param -> 'domain -> ( 'header * ( 'domain list ) );
        compare_header : 'header -> 'header -> bool; 
        make_result : 'param -> 'header -> 'image list -> 'image;
        index_table : ( ('header, 'image) skeleton, 'image IAr.descriptor ) Hash.t;
        image_array : 'image IAr.t;
      }

   let list_mem_eq = List_util.compare_eq

   let skeleton_compare cmp_hd (hd_a, indices_a) (hd_b, indices_b) =
      (cmp_hd hd_a hd_b) && (list_mem_eq indices_a indices_b)

   let create mk_hd cmp_hd mk_rslt =
      {
        make_header = mk_hd;
        compare_header = cmp_hd;

        make_result = mk_rslt;
        index_table = Hash.create 17 ( skeleton_compare cmp_hd );
        image_array = IAr.create 17;
      }

   let ar_get ar d =
      match IAr.get ar d with
         Some a -> a
       | None -> raise IAr.EmptySlot

   let convert info param header indices =
      info.make_result param header (map (ar_get info.image_array) indices)

   let rec index info param arg =
      let (header, subterms) = info.make_header param arg in
      let subindices = ( map (index info param) subterms ) in
      let bucket = (header, subindices) in
      let hash = Hashtbl.hash bucket in
      let table = info.index_table in
         match Hash.seek table hash bucket with
            Hash.None -> 
               let arg_index = IAr.store info.image_array ( Some ( convert info param header subindices ) ) in
                  Hash.insert table hash bucket arg_index;
                  arg_index
          | Hash.Some arg_index ->
               arg_index 

   let get info param index =
      ar_get info.image_array index

   let apply info param arg =
      let arg_index = index info param arg in
         ar_get info.image_array arg_index   

end

