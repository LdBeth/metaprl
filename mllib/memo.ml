(*
 * Generic memoize function.
 *)

open Debug
open Printf

let debug_memo =
   create_debug (**)
      { debug_name = "memo";
        debug_description = "Display memo operations";
        debug_value = false
      }

(************************************************************************
 * TYPES                                                                *
 ************************************************************************)

(*
 * Memo table.
 * The function is the function we are caching.
 * The table is the cached values of the function.
 * The count is used for rehashing.
 *
 * The nth key corresponds to the nth value.
 *)
type ('a, 'b, 'c, 'd) t =
   { memo_f : 'a -> 'b -> 'c;
     memo_g : 'a -> 'c -> 'd;
     memo_compare : 'c -> 'c -> bool;

     mutable memo_f_table : ('b * 'd) list array;
     mutable memo_f_count : int;

     mutable memo_g_table : ('c * 'd) list array;
     mutable memo_g_count : int
   }

(************************************************************************
 * IMPLEMENTATION                                                       *
 ************************************************************************)

(*
 * Empty tables.
 *)
let create f g compare =
   { memo_f = f;
     memo_g = g;
     memo_compare = compare;

     memo_f_table = Array.create 17 [];
     memo_f_count = 0;

     memo_g_table = Array.create 17 [];
     memo_g_count = 0
   }

(*
 * Rehash the table.
 *)
let rehash table =
   if !debug_memo then
      eprintf "Memo.rehash: %d%t" (Array.length table) eflush;
   let entries = ref [] in
   let len = Array.length table in
   let _ =
      for i = 0 to len - 1 do
         entries := table.(i) :: !entries
      done
   in
   let len = (len + 1) * 2 - 1 in
   let table = Array.create len [] in
   let insert bucket =
      let insert ((x, _) as entry) =
         let index = Hashtbl.hash x mod len in
            table.(index) <- entry :: table.(index)
      in
         List.iter insert bucket
   in
      List.iter insert !entries;
      table

(*
 * Asssocive search by pointer equality.
 *)
let rec assocq x = function
   (x', b) :: tl ->
      if x' == x then
         Some b
      else
         assocq x tl
 | [] ->
      None

(*
 * Associative search.
 *)
let rec assoc compare x = function
   (x', b) :: tl ->
      if compare x' x then
         Some b
      else
         assoc compare x tl
 | [] ->
      None

(*
 * Insert a f-value into its table.
 * Rehash if necessary.
 *)
let insert_f info hash x b =
   let { memo_f_count = count; memo_f_table = table } = info in
   let len = Array.length table in
   let index = hash mod len in
      info.memo_f_count <- count + 1;
      table.(index) <- (x, b) :: table.(index);
      if count > len * 2 then
         info.memo_f_table <- rehash table

(*
 * Insert a g-value into its table.
 * Rehash if necessary.
 *)
let insert_g info hash x b =
   let { memo_g_count = count; memo_g_table = table } = info in
   let len = Array.length table in
   let index = hash mod len in
      info.memo_g_count <- count + 1;
      table.(index) <- (x, b) :: table.(index);
      if count > len * 2 then
         info.memo_g_table <- rehash table

(*
 * Lookup.
 *)
let apply info arg x =
   let table = info.memo_f_table in
   let len = Array.length table in
   let hash_x = Hashtbl.hash x in
   let index = hash_x mod len in
   let bucket = table.(index) in
      match assocq x bucket with
         Some b ->
            (*
             * We have seen this exact application before.
             *)
            if !debug_memo then
               eprintf "F: success%t" eflush;
            b
       | None ->
            let y = info.memo_f arg x in
            let table = info.memo_g_table in
            let len = Array.length table in
            let hash_y = Hashtbl.hash y in
            let index = hash_y mod len in
            let bucket = table.(index) in
               match assoc info.memo_compare y bucket with
                  Some b ->
                     (*
                      * This happens when g finds an approximate match,
                      * but the exact pointer equality has not been seen.
                      * Remember this in f_table.
                      *)
                     if !debug_memo then
                        eprintf "G: success%t" eflush;
                     insert_f info hash_x x b;
                     b
                | None ->
                     (*
                      * We have never seen the value before.
                      * Remember it in both f_table and g_table.
                      *)
                     let b = info.memo_g arg y in
                        if !debug_memo then
                           eprintf "G: failed%t" eflush;
                        insert_f info hash_x x b;
                        insert_g info hash_y y b;
                        b

(*
 * $Log$
 * Revision 1.2  1998/07/03 22:05:35  jyh
 * IO terms are now in term_std format.
 *
 * Revision 1.1  1998/07/02 22:24:54  jyh
 * Created term_copy module to copy and normalize terms.
 *
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
