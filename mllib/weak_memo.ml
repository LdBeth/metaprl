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
 * Modified By: Aleksey Nogin <nogin@cs.caltech.edu>
 *)

(*
 * BUG WEAK: 8/22/2003 JYH
 * Something is definitely wrong, probably in Term_hash, which
 * fails if we use real Weak arrays.  This probably means that
 * somewhere, someone has a weak_descriptor, but the actual
 * descriptor is dead.  We'll have to investigate Term_hash
 * to see what is going on.  In the meantime, we can use real
 * arrays.  This is probably very inefficient.
 *)
module Weak2 =
struct
   type 'a t = 'a option array

   let create len =
      Array.create len None

   let set = Array.set

   let get = Array.get

   let blit = Array.blit

   let length = Array.length
end

open Printf
open Lm_debug
let debug_weak_memo =
   create_debug (**)
      { debug_name = "debug_weak_memo";
        debug_description = "Print out size of table when it got expanded";
        debug_value = false
      }

let debug_weak_memo2 =
   create_debug (**)
      { debug_name = "debug_weak_memo2";
        debug_description = "Print ids of tables that are GCed";
        debug_value = false
      }

open Weak_memo_sig

(*
 * Data used for garbage collection
 *)
type gc_info = Weak_memo_sig.gc_info

(*
 * Empty instance of gc_info
 *)
let empty_gci = {counters = []; gcollectors = []}

(*
 * Total number of weak tables.
 *)
let counter = ref 0

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
         * true - collect everything at once at store in vacant
         * false - use "old" by-request scan
         *)
        burst_gc : bool;

        (*
         * Keep track of GC status, only when burst_gc=false
         *)
        mutable gc_on : bool;

        (*
         * List of vacant slots in image_array, used only when burst_gc=true
         *)
        mutable vacant : 'image weak_descriptor list;

	   (*
   	 * List of garbage collectors of all tables.
	    * We need it to perform total (burst) GCion
   	 * because there may be recursive references between different tables
	    * and we want to clean recursive structures completely.
   	 *)
	     mutable gcinfo : gc_info;


        (*
         * For debugging, this is the total number of tables
         * that have been created before this table.
         *)
        name : string;
        id : int
      }

   (************************************************************************
    * IMPLEMENTATION                                                       *
    ************************************************************************)

   (*
    * Hash code for the descriptor.
    *)
   let wd_hash i =
      i

   (*
    * Release the anchor for possible GC.
    *)
   let weaken d =
      d.descriptor

   (*
    * Don't understand/remember what's the point in global is_gc
    *)
(*   (*
    * Is GC active?
    *)
   let is_gc = ref true
*)

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
   Here we prohibit reuse of GCed slots. 
   This may resolve our issue with weak_memo.
*)
(*
   let gc_tst weak_id image_array (_, weak_descriptor) = false
*)

   (*
    * Run GCion on table and put collected slots in vacant)
    *)
   let collect info () =
      let hash_table = info.index_table in
      let old_size = (Hash.size hash_table) in
      let found = Hash.gc_all (gc_tst info.id info.image_array) hash_table in
      let strip (a,b) = b in
         info.vacant <- List.map strip found;
	     	if !debug_weak_memo2 then
   	     	eprintf " id=%i found=%i/%i %t" info.id (List.length info.vacant) old_size eflush

	let get_counter info _ = info.count

	let total_count gci =
		List.fold_left (fun s c -> s + (c ())) 0 gci.counters

   (*
    * Create a new table.
    *)
   let create halfsize critical_level name make_header header_weaken compare_header make_result gci =
      incr counter;
(*     	if !debug_weak_memo2 then
  	     	eprintf "Creating %i%t" !counter eflush;*)
      let array_length = (2 * halfsize) in
      let image_array = Weak.create array_length in
      let id = !counter in
      let info : ('param, 'arg, 'header, 'weak_header, 'image) t =
         { make_header = make_header;
           header_weaken = header_weaken;
           compare_header = compare_header;
           make_result = make_result;
           image_array = image_array;
           index_table = Hash.create halfsize critical_level Hashtbl.hash compare_header;
           count = 0;
           burst_gc=true;
           vacant = [];
           gcinfo=gci;
           gc_on = false;
           name = name;
           id = id
         } in
         begin
            info.gcinfo <- {counters=(get_counter info)::gci.counters; gcollectors=(collect info)::gci.gcollectors};
            info
         end

   let create_default name weaken compare make gci =
      let fail _ _ =
         raise (Invalid_argument "Weak_memo.create_default: applications are not allowed")
      in
         create 17 17 name fail weaken compare make gci

	let union eq l1 l2 =
		List.rev_append l1 (List.filter (fun i -> not (List.exists (eq i) l1)) l2)

	let add_gci info gci =
		let gci0=info.gcinfo in
		info.gcinfo <- {counters=union (==) gci0.counters gci.counters; 
		                gcollectors=union (==) gci0.gcollectors gci.gcollectors}
		
	let get_gci info = info.gcinfo

   let make_descriptor info i item =
IFDEF VERBOSE_EXN THEN
      { descriptor = i;
        desc_name = info.name;
        anchor = item
      }
ELSE
      { descriptor = i;
        anchor = item
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
               Weak.blit wa 0 new_ar 0 old_ar_length;
               new_ar

   (*
    * Store a new value in the weak array.
    * For debugging, check that the entry does not exist.
    *)
   let set info wd item =
      match Weak.get info.image_array wd with
         Some _ -> invalid_arg "WeakMemo.set: entry is not empty"
       | None ->
            Weak.set info.image_array wd (Some item);
            make_descriptor info wd item

   let run f = f ()

   let expand info weak_header hash result =
		let length = Weak.length info.image_array in
		let new_length = ((length * 2) + 1) in
     	if !debug_weak_memo then
        	eprintf "\nexpanding %s from %i to %i%t" info.name length new_length eflush;
      Hash.insert info.index_table hash weak_header length;
      info.image_array <- expand_weak_array info.image_array info.id new_length;
      info.count <- succ length;
      set info length result

   let threshold = 2

   let do_burst_gc info weak_header hash result =
		let count=info.count in
      if count=Weak.length info.image_array then
      	match info.vacant with
         	index::tl ->
         		begin
	            	info.vacant <- tl;
   	            Hash.insert info.index_table hash weak_header index;
      	         set info index result
      	      end
          | [] ->
          		let gci=info.gcinfo in
          		let total_count=total_count gci in
            	if count * (List.length gci.gcollectors) > threshold * total_count then
            		begin
					     	if !debug_weak_memo2 then
					        	eprintf "\nGC %s.%i start count=%i/%i%t" info.name info.id count total_count eflush;
		            	List.iter run gci.gcollectors;
					     	if !debug_weak_memo2 then
					        	eprintf "GC %s.%i end%t" info.name info.id eflush;
   		            match info.vacant with
      		         	index::tl ->
      		         		begin
	         		         	info.vacant <- tl;
   	         		         Hash.insert info.index_table hash weak_header index;
      	         		      set info index result
      	         		   end
                 		 | [] ->
                  			expand info weak_header hash result
                  end
               else
               	expand info weak_header hash result
   	else
      	let count' = succ count in
      		begin
	         	info.count <- count';
   	         Hash.insert info.index_table hash weak_header count;
      	      set info count result
      	   end

	let do_incremental_gc info weak_header hash result =
	   if info.gc_on then
	      begin
   	      (* GC is only triggered when the weak table gets full *)
               if Weak.length info.image_array <> info.count then
	               raise (Inconsistency "weak table length does not match expected value");
            (* Search for a free location in the hash table *)
               match Hash.gc_iter (gc_tst info.id info.image_array) info.index_table with
   	            Some (_, weak_index) ->
      	            (* Found a free entry in the hash table *)
                     Hash.insert info.index_table hash weak_header weak_index;
                     set info weak_index result
                | None ->
         	         (*
                      * The weak array is totally full.
                      * Double the array size, and store the entry
                      * at the first free entry that was created.
                      *)
                     let length = Weak.length info.image_array in
            	         if !debug_weak_memo then
               	         eprintf "\nexpanding %s from %i to %i%t" info.name length ((length * 2) + 1) eflush
                        else
                           ();
                  	   info.gc_on <- false;
                        Hash.insert info.index_table hash weak_header length;
                        info.image_array <- expand_weak_array info.image_array info.id ((length * 2) + 1);
                        info.count <- succ length;
                        set info length result
			end
      else
         (* Store the entry in the next free position in the array *)
         let count = info.count in
         let count' = succ count in
         info.count <- count';
         if count' = Weak.length info.image_array then
	         begin
   	         (* Table looks full, so trigger GC analysis on the next allocation *)
               Hash.gc_start info.index_table;
               info.gc_on <- true
            end;
         Hash.insert info.index_table hash weak_header count;
         set info count result

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
                        make_descriptor info weak_index item
                   | None ->
                        (* It has been collected, so create a new value *)
                        let item = info.make_result param header in
                           set info weak_index item
               end
          | None ->
               (* This is a new call to the function *)
               let result = info.make_result param header in
                  if info.burst_gc then
                  	do_burst_gc info weak_header hash result
                  else
                  	do_incremental_gc info weak_header hash result

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
          | Some weak_index -> begin
               match Weak.get info.image_array weak_index with
                  Some item -> make_descriptor info weak_index item
                | None -> raise Not_found
            end

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
             invalid_arg (Printf.sprintf "WeakMemo.retrieve: try to retrieve from wrong table: %s from %s" index.desc_name info.name)
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

   (*
    * Don't understand/remember what's the point in global is_gc
   let gc_on info = is_gc := true
   let gc_off info = is_gc := false
   *)
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

