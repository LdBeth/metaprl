(*
 * Overlay list of elements as small sets over another
 * set implementation.
 *)

module Make (Ord : Set.OrderedType) =
struct
   (************************************************************************
    * TYPES                                                                *
    ************************************************************************)

   (*
    * Type of elements in the set.
    *)
   type elt = Ord.t

   (*
    * The set is a hashtable,
    * but most operations are delayed.
    *)
   type table =
    | Empty
    | Element of elt
    | Union of table * table
    | Add of table * elt
    | Remove of table * elt
    | Hash of (elt, elt) Hashtbl.t * int

   type t =
      { mutable table : table }

   (*
    * Exception to abort intersections early.
    *)
   exception Found

   (************************************************************************
    * IMPLEMENTATION                                                       *
    ************************************************************************)

   (*
    * Get the delayed hashtable.
    *)
   let rec compile table = function
      Empty ->
         0
    | Element x ->
         begin
            try Hashtbl.find table x; 0 with
               Not_found ->
                  Hashtbl.add table x x;
                  1
         end
    | Union (s1, s2) ->
         compile table s1 + compile table s2
    | Add (s1, x) ->
         begin
            let count = compile table s1 in
               try
                  Hashtbl.find table x;
                  count
               with
                  Not_found ->
                     Hashtbl.add table x x;
                     succ count
         end
    | Remove (s1, x) ->
         begin
            let count = compile table s1 in
               try
                  Hashtbl.find table x;
                  Hashtbl.remove table x;
                  pred count
               with
                  Not_found ->
                     count
         end
    | Hash (hash, count) ->
         let count = ref 0 in
         let add x y =
            try Hashtbl.find table x; () with
               Not_found ->
                  Hashtbl.add table x y;
                  incr count
         in
            Hashtbl.iter add hash;
            !count

   let flush s1 =
      match s1.table with
         Hash (table, count) ->
            table, count
       | prog ->
            let table = Hashtbl.create 19 in
            let count = compile table prog in
               s1.table <- Hash (table, count);
               table, count

   (*
    * Create the set from a list.
    *)
   let of_list elements =
      let table = Hashtbl.create 19 in
         List.iter (fun x -> Hashtbl.add table x x) elements;
         { table = Hash (table, List.length elements) }

   (*
    * Get the elements in the set.
    * They are not sorted.
    *)
   let elements s1 =
      let table, _ = flush s1 in
      let elements = ref [] in
         Hashtbl.iter (fun x _ -> elements := x :: !elements) table;
         !elements

   (*
    * Add an element.
    *)
   let add x s1 =
      { table = Add (s1.table, x) }

   (*
    * Membership in the set.
    *)
   let mem s1 x =
      let table, _ = flush s1 in
         try Hashtbl.find table x; true with
            Not_found ->
               false

   (*
    * Remove an element.
    *)
   let remove x { table = table } =
      { table = Remove (table, x) }

   (*
    * Set operations.
    *)
   let empty =
      { table = Empty }

   let is_empty s1 =
      let _, count = flush s1 in
         count = 0

   let make x =
      { table = Element x }

   let union s1 s2 =
      { table = Union (s1.table, s2.table) }

   let iter f s1 =
      Hashtbl.iter (fun x _ -> f x) (fst (flush s1))

   let cardinal s1 =
      snd (flush s1)

   (*
    * Intersection.
    *)
   let intersect_aux table1 table2 =
      let check x _ =
         try
            Hashtbl.find table1 x;
            raise Found
         with
            Not_found ->
               ()
      in
         try Hashtbl.iter check table2; false with
            Found ->
               true

   let intersectp set1 set2 =
      let table1, count1 = flush set1 in
      let table2, count2 = flush set2 in
         if count1 < count2 then
            intersect_aux table2 table1
         else
            intersect_aux table1 table2

   (*
    * Filter out the elements that are in the intersection.
    *)
   let rec mem_filt s = function
      [] ->
         []
    | (h :: t) as l ->
         if mem s h then
            let rem = mem_filt s t in
               if rem == t then
                  l
               else
                  h :: rem
         else
            mem_filt s t

   let rec not_mem_filt s = function
      [] ->
         []
    | (h :: t) as l ->
         if mem s h then
            not_mem_filt s t
         else
            let rem = not_mem_filt s t in
               if rem == t then
                  l
               else
                  h :: rem

   let rec fst_mem_filt s = function
      [] ->
         []
    | (((v, _) as h) :: t) as l ->
         if mem s v then
            let rem = fst_mem_filt s t in
               if rem == t then
                  l
               else
                  h :: rem
         else
            fst_mem_filt s t

end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
