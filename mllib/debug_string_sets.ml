open Lm_set_sig
open Lm_printf

module Make
   (Set1: MinimalSetSig with type elt = string)
   (Set2: MinimalSetSig with type elt = string) =

struct

   type t = Set1.t * Set2.t
   type elt = string

   let print_sets s1 s2 =
      eprintf "MakeDebugSet.Set1 :\n[%a] (%d)\nMakeDebugSet.Set2 :\n[%a] (%d)\n"
              print_string_list (Set1.elements s1)
              (Set1.cardinal s1)
              print_string_list (Set2.elements s2)
              (Set2.cardinal s2)

   let error_sets s1 s2 =
      print_sets s1 s2;
      eprintf "%t" flush;
      raise (Invalid_argument "DebugSet")

   let empty = Set1.empty, Set2.empty

   let is_empty (s1,s2) =
      match Set1.is_empty s1, Set2.is_empty s2 with
         (true,true) -> true
       | (false,false) -> false
       | _ ->
            eprintf "is_empty_set mismatch:\n"; error_sets s1 s2

   let mem (s1,s2) v =
      match Set1.mem s1 v, Set2.mem s2 v with
         (true,true) -> true
       | (false,false) -> false
       | m1,m2 ->
            eprintf "mem mismatch:\nElement: %s membership: %b %b\n" v m1 m2; error_sets s1 s2

   let singleton v =
      let s1 = Set1.singleton v in
      let s2 = Set2.singleton v in
      match Set1.cardinal s1, Set2.cardinal s2 with
         (1,1) -> (s1,s2)
       | _ ->
            eprintf "(make %s) cardinality is wrong:\n" v; error_sets s1 s2

   let remove (s1,s2) v =
      let s1' = Set1.remove s1 v in
      let s2' = Set2.remove s2 v in
      if Set1.cardinal s1' = Set2.cardinal s2' then (s1',s2') else
      begin
         eprintf "(remove %s) cardinality is wrong:\nWas:\n" v;
         print_sets s1 s2;
         eprintf "Now:\n";
         error_sets s1' s2'
      end

   let add (s1,s2) v =
      let s1' = Set1.add s1 v in
      let s2' = Set2.add s2 v in
      if Set1.cardinal s1' = Set2.cardinal s2' then (s1',s2') else
      begin
         eprintf "(add %s) cardinality is wrong:\nWas:\n" v;
         print_sets s1 s2;
         eprintf "Now:\n";
         error_sets s1' s2'
      end

   let union (s1_1, s2_1) (s1_2, s2_2) =
      let s1' = Set1.union s1_1 s1_2 in
      let s2' = Set2.union s2_1 s2_2 in
      if Set1.cardinal s1' = Set2.cardinal s2' then (s1',s2') else
      begin
         eprintf "Union cardinality is wrong:\nFirst arg:\n";
         print_sets s1_1 s2_1;
         eprintf "Second arg:\n";
         print_sets s1_2 s2_2;
         eprintf "Result:\n";
         error_sets s1' s2'
      end

   let iter f (s1,_) = Set1.iter f s1

   let cardinal (s1,s2) =
      let c1 = Set1.cardinal s1 in
      let c2 = Set2.cardinal s2 in
      if c1 = c2 then c1 else
      begin
         eprintf "Cardinality is wrong:\n";
         error_sets s1 s2
      end

   let rec mem_filt s = function
      [] -> []
    | (h::t) as l ->
         if mem s h
         then let rem = mem_filt s t in
                 if rem == t then l else h::rem
         else mem_filt s t

   let rec not_mem_filt s = function
      [] -> []
    | (h::t) as l ->
         if mem s h then
            not_mem_filt s t
         else
            let rem = not_mem_filt s t in
               if rem == t then
                  l
               else
                  h::rem

   let rec fst_mem_filt s = function
      [] -> []
    | (((v,_) as h)::t) as l ->
         if mem s v
         then let rem = fst_mem_filt s t in
                 if rem == t then l else h::rem
         else fst_mem_filt s t

   let intersectp (s1_1, s2_1) (s1_2, s2_2) =
      match Set1.intersectp s1_1 s1_2, Set2.intersectp s2_1 s2_2 with
         (true,true) -> true
       | (false,false) -> false
       | _ ->
            eprintf "Intersectp mismatch:\nFirst arg:\n";
            print_sets s1_1 s2_1;
            eprintf "Second arg:\n";
            error_sets s1_2 s2_2

   let elements (s1,_) = Set1.elements s1

   let to_list = elements

   let of_list l =
      let s1=Set1.of_list l and s2=Set2.of_list l in
      if Set1.cardinal s1 = Set2.cardinal s2 then s1,s2 else begin
         eprintf "of_list: cardinality is wrong\n";
         error_sets s1 s2
      end
end
