(*
 * Generic memoize function.
 *)

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
type ('a, 'b) t =
   { memo_fun : 'a -> 'b;
     memo_compare : 'a -> 'a -> bool;
     mutable memo_table : ('a * 'b) list array;
     mutable memo_count : int
   }

(************************************************************************
 * IMPLEMENTATION                                                       *
 ************************************************************************)

(*
 * Empty tables.
 *)
let create f compare =
   { memo_fun = f;
     memo_compare = compare;
     memo_table = Array.create 17 [];
     memo_count = 0
   }

(*
 * Rehash the table.
 *)
let rehash info =
   let { memo_table = table } = info in
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
      info.memo_table <- table

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
 * Lookup.
 *)
let apply info x =
   let { memo_compare = compare; memo_table = table } = info in
   let len = Array.length table in
   let index = (Hashtbl.hash x) mod len in
   let bucket = table.(index) in
      match assoc compare x bucket with
         Some b ->
            b
       | None ->
            let { memo_fun = f; memo_count = count } = info in
            let b = f x in
               info.memo_count <- count + 1;
               table.(index) <- (x, b) :: bucket;
               if count > len * 2 then
                  rehash info;
               b

(*
 * $Log$
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
