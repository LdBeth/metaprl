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
open Printf

(*
 * For Debug: Weak's double uses two copies of Weak and compares their behaviour.
 *)
module DoubledWeak =
struct

   exception Inconsistency of string

   type 'a t =
      {
         first : 'a Weak.t;
         second : 'a Weak.t;
         }

   let create i = {first=Weak.create i; second=Weak.create i}

   let length ar = Weak.length ar.first

   let set ar i opt =
      begin
         Weak.set ar.first i opt;
         Weak.set ar.second i opt
      end

   let get ar i =
      let opt1 = Weak.get ar.first i in
      let opt2 = Weak.get ar.second i in
      match opt1 with
         Some v1 ->
            (match opt2 with
               Some v2 ->
                  if v1==v2 then
                     opt1
                  else
                     raise (Inconsistency "Different values in different copies of array")
             | None -> opt2)
       | None -> opt1

   let get_copy ar i =
      let opt1 = Weak.get_copy ar.first i in
      let opt2 = Weak.get_copy ar.second i in
      match opt1 with
         Some v1 ->
            (match opt2 with
               Some v2 ->
                  if v1=v2 then
                     opt1
                  else
                     raise (Inconsistency "Different values in different copies of array")
             | None -> opt2)
       | None -> opt1

   let check ar i =
      (Weak.check ar.first i) &&
      (Weak.check ar.second i)

   let fill ar i j v =
      begin
         Weak.fill ar.first i j v;
         Weak.fill ar.second i j v
      end

   let blit ar1 i1 ar2 i2 n =
      begin
         Weak.blit ar1.first i1 ar2.first i2  n;
         Weak.blit ar1.second i1 ar2.second i2 n;
      end

end

module WeakMemo (Hash : Hash_with_gc_sig.HashWithGCSig) =
struct
   (************************************************************************
    * TYPES                                                                *
    ************************************************************************)

   type 'a weak_descriptor = int

IFDEF VERBOSE_EXN THEN
   type 'a descriptor =
      { descriptor : 'a weak_descriptor;
        desc_name : string;
        anchor : 'a
      }
ELSE
   type 'a descriptor =
      { descriptor : 'a weak_descriptor;
        anchor : 'a
      }
ENDIF

   exception Cell_is_full
   exception Inconsistency of string

   module Weak = DoubledWeak

   type ('param, 'arg, 'header, 'weak_header, 'image) t =
      { make_header : 'param -> 'arg -> 'header;
        header_weaken : 'param -> 'header -> 'weak_header;
        compare_header : 'weak_header -> 'weak_header -> bool;
        make_result : 'param -> 'header -> 'image;
        index_table : ('weak_header, 'image weak_descriptor) Hash.t;
        mutable image_array : 'image Weak.t;

        (*
         * All entries image_array.(count) or after are guaranteed to be free.
         *)
        mutable count : int;

        (*
         * Keep track of GC status.
         *)
        mutable gc_on : bool;

        (*
         * For debugging, this is the total number of tables
         * that have been created before this table.
         *)
        name: string;
        id: int
      }

   (************************************************************************
    * IMPLEMENTATION                                                       *
    ************************************************************************)

   (*
    * HAsh code for the descriptor.
    *)
   let wd_hash i = i

   (*
    * Release the anchor for possible GC.
    *)
   let weaken d = d.descriptor

   (*
    * Total number of weak tables.
    *)
   let counter = ref 0

   (*
    * Is GC active?
    *)
   let is_gc = ref true

   (*
    * Descriptors remain static and anchored,
    * so this comparison is always valid.
    * NOTE: if desciptor becomes mutable, fix this.
    *)
   let compare d1 d2 =
      d1.descriptor - d2.descriptor

   (*
    * Has the value been collected by GC?
    *)
   let gc_tst weak_id image_array (_, weak_descriptor) =
      match Weak.get image_array weak_descriptor with
          Some _ -> false
        | None -> true

   (*
    * Create a new table.
    *)
   let create halfsize critical_level name make_header header_weaken compare_header make_result =
      incr counter;
      let image_array = Weak.create (2 * halfsize) in
      let id = !counter in
         { make_header = make_header;
           header_weaken = header_weaken;
           compare_header = compare_header;
           make_result = make_result;
           image_array = image_array;
           index_table = Hash.create halfsize critical_level Hashtbl.hash compare_header;
           count = 0;
           gc_on = false;
           name = name;
           id = id
         }

   let create_default name weaken compare make =
      let fail _ _ =
         raise (Invalid_argument "Weak_memo.create_default: applications are not allowed")
      in
         create 17 17 name fail weaken compare make

   (*
    * Get the value from the table.
    *)
   let guard_get ar wd =
      match Weak.get ar wd with
         Some item -> item
       | None -> raise Not_found

   (*
    * Get the value back from the table.
    *)
   let subscribe info i =
IFDEF VERBOSE_EXN THEN
      { descriptor = i;
        desc_name = info.name;
        anchor = guard_get info.image_array i
      }
ELSE
      { descriptor = i;
        anchor = guard_get info.image_array i
      }
ENDIF

   (*
    * Grow the weak array so we can add more entries.
    *)
   let expand_weak_array wa id new_size =
      let old_ar_length = Weak.length wa in
         if new_size <= old_ar_length then
            invalid_arg "Can't expand weak array to less size"
         else
            let new_ar = Weak.create new_size in
               (Weak.blit wa 0 new_ar 0 old_ar_length);
               new_ar

   (*
    * Store a new value in the weak array.
    * For debugging, check that the entry does not exist.
    *)
   let set info wd item =
      match Weak.get info.image_array wd with
         Some _ -> invalid_arg "WeakMemo.set: entry is not empty"
       | None -> Weak.set info.image_array wd (Some item)

   (*
    * Find a value in the table.
    *)
   let lookup info param header =
      let weak_header = info.header_weaken param header in
      let table = info.index_table in
      let hash = Hash.hash table weak_header in
         match Hash.seek table hash weak_header with
            Some weak_index ->
               begin
                  (* The result was previously stored in the table *)
                  match Weak.get info.image_array weak_index with
                     Some item ->
                        (* It has not been collected, so return the value *)
IFDEF VERBOSE_EXN THEN
                        { descriptor = weak_index;
                          desc_name = info.name;
                          anchor = item
                        }
ELSE
                        { descriptor = weak_index;
                          anchor = item
                        }
ENDIF
                   | None ->
                        (* It has been collected, so create a new value *)
                        let item = info.make_result param header in
                           set info weak_index item;
                           subscribe info weak_index
               end
          | None ->
               (* This is a new call to the function *)
               let result = info.make_result param header in
                  if info.gc_on then
                     begin
                        (* GC is only triggered when the weak table gets full *)
                        if ((Weak.length info.image_array) <> info.count) then
                           raise (Inconsistency "weak table length does not match expected value");

                        (* Search for a free location in the hash table *)
                        match Hash.gc_iter (gc_tst info.id info.image_array) info.index_table with
                           Some (_, weak_index) ->
                              (* Found a free entry in the hash table *)
                              Hash.insert info.index_table hash weak_header weak_index;
                              set info weak_index result;
                              subscribe info weak_index
                         | None ->
                              (*
                               * The weak array is totally full.
                               * Double the array size, and store the entry
                               * at the first free entry that was created.
                               *)
                              let length = Weak.length info.image_array in
                                 info.gc_on <- false;
                                 Hash.insert info.index_table hash weak_header length;
                                 info.image_array <- expand_weak_array info.image_array info.id ((length * 2) + 1);
                                 info.count <- succ length;
                                 set info length result;
                                 subscribe info length
                     end
                  else
                     (* Store the entry in the next free position in the array *)
                     let count = info.count in
                     let count' = succ count in
                        Hash.insert info.index_table hash weak_header count;
                        set info count result;
                        info.count <- count';
                        if count' = (Weak.length info.image_array) then
                           begin
                              (* Table looks full, so trigger GC analysis on the next allocation *)
                              Hash.gc_start table;
                              info.gc_on <- true
                           end;
                        subscribe info count

   (*
    * Lookup a value that was previously stored in the table.
    * Raises Invalid_argument if the value is in the process of
    * being collected by GC.  Raises Not_found if the value
    * was never stored, or if we have completely cleaned up
    * after the value was collected.
    *)
   let rec unsafe_lookup info param header =
      let weak_header = info.header_weaken param header in
      let hash = Hash.hash info.index_table weak_header in
      let table = info.index_table in
         match Hash.seek table hash weak_header with
            None ->
               raise Not_found
          | Some weak_index ->
               subscribe info weak_index

   (*
    * Get the value associated with a descriptor.
    * For debugging, check that the value saved in the table
    * still exists and matches the value passed in the index.
    *)
   let retrieve info param index =
      if Weak.length info.image_array <= index.descriptor then
         invalid_arg "WeakMemo.retrieve: out of range";
      (IFDEF VERBOSE_EXN THEN (**)
          if index.desc_name <> info.name then
             invalid_arg (sprintf "WeakMemo.retrieve: try to retrieve from wrong table: %s from %s" index.desc_name info.name)
       ENDIF);
      match Weak.get info.image_array index.descriptor with
         Some item ->
            if index.anchor == item then
               item
            else
               raise (Inconsistency "descriptor does not match item")
       | None ->
            raise (Inconsistency "item was lost")

   let retrieve_hack d =
      d.anchor

   (*
    * Check that the descriptor is valid in this table.
    *)
   let retrieve_check info index =
      match Weak.get info.image_array index.descriptor with
         Some item ->
            item == index.anchor
       | None ->
            false

   (*
    * Compose the lookup and result functions.
    *)
   let apply info param arg =
      retrieve info param (lookup info param (info.make_header param arg))

   let gc_on info = is_gc := true
   let gc_off info = is_gc := false
end

module MakeMemo = WeakMemo

module TheWeakMemo = WeakMemo (Hash_with_gc.HashWithGC)

module TheMemo = TheWeakMemo

(*
 * -*-
 * Local Variables:
 * Caml-master: "term_hash, term_header_constr"
 * End:
 * -*-
 *)

