(* This file implements memoize function based on weak array of results
 *
 * -----------------------------------------------------------------
 * This file is part of MetaPRL, a modular, higher order
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
  functor(Hash : Hash_with_gc_sig.HashWithGCSig) ->
struct

   (************************************************************************
    * TYPES                                                                *
    ************************************************************************)

   type 'a weak_descriptor = int

   type 'a descriptor = { 
                           descriptor: 'a weak_descriptor;
                           anchor: 'a 
                        }

   let wd_hash i = i
   let weaking d = d.descriptor

   exception Cell_is_full
   exception Inconsistency

   let counter = ref 0
   let is_gc = ref true

   type ('param, 'header, 'weak_header, 'image) t =
      {
        header_weaking : 'header -> 'weak_header;
        compare_header : 'weak_header -> 'weak_header -> bool; 
        make_result : 'param -> 'header -> 'image;
        index_table : ( 'weak_header, 'image weak_descriptor ) Hash.t;
        mutable image_array : 'image Weak.t;
        mutable count : int;

        mutable gc_on : bool;

        id: int;

      }

   let gc_tst id ar (wh, wd) = 
      !is_gc &&
      match Weak.get ar wd with
         Some _ -> false
       | None -> true

   let create halfsize crit_lev hd_weaking cmp_hd mk_rslt =
     incr(counter);
     let iar = Weak.create (2*halfsize) in 
      {
        header_weaking = hd_weaking;
        compare_header = cmp_hd;
        make_result = mk_rslt;
        image_array = iar;
        index_table = Hash.create halfsize crit_lev Hashtbl.hash cmp_hd;
        count = 0;
        gc_on = false;

        id = !counter;
      }

   let guard_get ar wd = 
if (Weak.length ar) <= wd then invalid_arg "WeakMemo.guard_get: out of range";
   match Weak.get ar wd with
      Some item -> item
    | None -> raise Not_found

   let subscribe info i = 
      { descriptor = i; anchor = guard_get info.image_array i }

   let expand_weak_array wa id new_size =
      let old_ar_length = Weak.length wa in
      if new_size <= old_ar_length then
         invalid_arg "Can't expand weak array to less size"
      else
         let new_ar = Weak.create new_size in
                  (Weak.blit wa 0 new_ar 0 old_ar_length);
            new_ar

   let set info wd item = 
      match Weak.get info.image_array wd with
         Some _ -> invalid_arg "WeakMemo.set: entry is not empty"
       | None -> Weak.set info.image_array wd (Some item)
            
   let rec lookup info param header =
      let weak_header = info.header_weaking header in
      let table = info.index_table in
      let hash = Hash.hash table weak_header in
         match Hash.seek table hash weak_header with
            Some weak_index -> 
               begin match Weak.get info.image_array weak_index with
                  Some item ->
                     { descriptor = weak_index; anchor = item }
                | None ->
                     let item = info.make_result param header in
                     set info weak_index item;
                     { descriptor = weak_index; anchor = item }
               end
          | None ->
               let result = info.make_result param header in
                  if info.gc_on then begin
                     if ((Weak.length info.image_array) <> info.count) then raise Inconsistency;
                     match Hash.gc_iter (gc_tst info.id info.image_array) info.index_table with
                        Some (whd, wd) ->
                           Hash.insert info.index_table hash weak_header wd;
                           set info wd result;
                           subscribe info wd
                      | None ->
                           let length = Weak.length info.image_array in
                           info.gc_on <- false;
                           Hash.insert info.index_table hash weak_header length;
                           info.image_array <- expand_weak_array info.image_array info.id ( 2*(length +1) - 1 );
                           info.count <- succ length;
                           set info length result;
                           subscribe info length
                  end else begin
                     let count = info.count in
                     let count' = succ(count) in
                        begin
                           Hash.insert info.index_table hash weak_header count;
                           set info count result;
                           info.count <- count';
                           if count' = (Weak.length info.image_array) then
                              begin
                                 Hash.gc_start table;
                                 info.gc_on <- true
                              end;
                           subscribe info count
                        end
                  end

   let rec unsafe_lookup info param header =
      let weak_header = info.header_weaking header in
      let hash = Hash.hash info.index_table weak_header in
      let table = info.index_table in
         match Hash.seek table hash weak_header with
            None -> 
               raise Not_found
          | Some weak_index ->
               subscribe info weak_index

   let retrieve info param index =
if (Weak.length info.image_array) <= index.descriptor then invalid_arg "WeakMemo.retrieve: out of range";
      match Weak.get info.image_array index.descriptor with
         Some item -> if index.anchor == item then item
                         else raise Inconsistency
       | None -> raise Inconsistency

   let unsafe_retrieve info param weak_index =
      snd (guard_get info.image_array weak_index)

   let gc_on info = is_gc := true
   let gc_off info = is_gc := false 

end

module TheWeakMemo = WeakMemo(Hash_with_gc.HashWithGC)

(*
 * -*-
 * Local Variables:
 * Caml-master: "term_hash, term_header_constr"
 * End:
 * -*-
 *)
