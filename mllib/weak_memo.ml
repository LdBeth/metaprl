(* This file implements memoize function based on weak array of results
 *
 * -----------------------------------------------------------------
 * This file is part of Nuprl-Light, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/index.html for information on Nuprl,
 * OCaml, and more information about this system.
 *
 * Copyright (C) 1998 Yegor Bryukhov, Moscow State University
 * 
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 * 
 * Author: Yegor Bryukhov
 *)

module WeakMemo =
  functor(Hash : Simplehash_sig.SimpleHashSig) ->
  functor(IAr : Infinite_weak_array_sig.InfiniteWeakArraySig) ->
struct

   (************************************************************************
    * TYPES                                                                *
    ************************************************************************)

   type 'a descriptor = 'a IAr.descriptor
(*   type 'a weak_descriptor = 'a IAr.weak_descriptor *)

   type ('param, 'header, 'weak_header, 'image) t =
      {
        header_weaking : 'header -> 'weak_header;
        compare_header : 'weak_header -> 'weak_header -> bool; 
        make_result : 'param -> 'header -> 'image;
        index_table : ( 'weak_header, 'image IAr.weak_descriptor ) Hash.t;
        image_array : 'image IAr.t;
      }

   let gc tbl ar =
      let new_holes = ref [] in
      let list_length = ref 0 in
      let agent (header, wd) =
         match IAr.weak_get ar wd with
            Some item ->
               true
          | None ->
               new_holes:= wd::!new_holes;
               incr list_length;
               false
         in
         Hash.gc Hashtbl.hash agent tbl;
         (!list_length, !new_holes)

   let create hash_size iar_size hd_weaking cmp_hd mk_rslt =
      let tbl = Hash.create hash_size cmp_hd in
      {
        header_weaking = hd_weaking;
        compare_header = cmp_hd;
        make_result = mk_rslt;
        index_table = tbl;
        image_array = IAr.create iar_size (gc tbl);
      }

   let ar_get ar d =
      match IAr.get ar d with
         Some a -> a
       | None -> raise Not_found

   let rec lookup info param header =
      let weak_header = info.header_weaking header in
      let hash = Hashtbl.hash weak_header in
      let table = info.index_table in
         match Hash.seek table hash weak_header with
            None -> 
               let img_index = IAr.store info.image_array ( info.make_result param header ) in
                  Hash.insert table hash weak_header ( IAr.weaking img_index );
                  img_index
          | Some weak_index ->
               try IAr.subscribe info.image_array weak_index with
                  Not_found ->
                     IAr.set info.image_array weak_index ( info.make_result param header )

   let rec unsafe_lookup info param header =
      let weak_header = info.header_weaking header in
      let hash = Hashtbl.hash weak_header in
      let table = info.index_table in
         match Hash.seek table hash weak_header with
            None -> 
               raise Not_found
          | Some weak_index ->
               IAr.subscribe info.image_array weak_index 

   let retrieve info param index =
      IAr.get info.image_array index

   let unsafe_retrieve info param weak_index =
      match IAr.weak_get info.image_array weak_index with
      	 Some item -> item
       | None -> raise Not_found

end

module TheWeakMemo = WeakMemo(Simplehashtbl.Simplehashtbl)(Infinite_weak_array.InfiniteWeakArray)

(*
 * -*-
 * Local Variables:
 * Caml-master: "term_hash, term_header_constr"
 * End:
 * -*-
 *)
