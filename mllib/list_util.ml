(*
 * Extra operations on lists.
 *)

open Printf
open Debug
(*
 * Show the file loading.
 *)
let _ =
   if !debug_load then
      eprintf "Loading List_util%t" eflush

(*
 * Get the nth item.
 *)
let rec nth l i =
   if i <= 0 then
      raise (Failure "nth")
   else
      match l with
         _::t ->
            nth t (i - 1)
       | [] ->
            raise (Failure "nth")

(* Filter items out of a list *)
let rec filter f = function
   [] -> []
 | h::t ->
      if f h then
         h::(filter f t)
      else
         filter f t

(*
 * Compare two lists of things.
 *)
let rec compare_lists cmp l1 l2 =
   match (l1,l2) with
      h1::t1, h2::t2 ->
         let i = cmp h1 h2 in
            if i = 0 then
               compare_lists cmp t1 t2
            else
               i
    | [], [] -> 0
    | [], _ -> -1
    | _ -> 1

let rec compare_eq l1 l2 =
   match l1, l2 with
      h1::t1, h2::t2 ->
         h1 == h2 & compare_eq t1 t2
    | [], [] ->
         true
    | _ ->
         false

(*
 * Test two lists.
 *)
let rec for_all2 f l1 l2 =
   match (l1,l2) with
      h1::t1, h2::t2 -> for_all2 f t1 t2 & f h1 h2
    | [], [] -> true
    | _ -> false

(*
 * Exists a pair in the two lists.
 *)
let rec exists2 f l1 l2 = match (l1,l2) with
   h1::t1, h2::t2 ->
      f h1 h2 or exists2 f t1 t2
 | _ -> false

(*
 * Map a function over two lists.
 *)
let rec map2 f l1 l2 = match (l1,l2) with
   h1::t1, h2::t2 ->
      let h = f h1 h2 in
         h :: map2 f t1 t2
 | [], [] -> []
 | _ -> raise (Failure "map2")

(*
 * Remove marked elements.
 *)
let rec remove_elements l1 l2 =
   match l1, l2 with
      flag::ft, h::t ->
         if flag then
            remove_elements ft t
         else
            h :: remove_elements ft t
    | _, l ->
         l

let rec removeq x = function
   h::t ->
      if h == x then
         t
      else
         h :: removeq x t
 | [] ->
      raise (Failure "removeq")

(*
 * Iterated tail.
 *)
let rec nth_tl i l =
   if i = 0 then
      l
   else
      match l with
         h::t ->
            nth_tl (i - 1) l
       | [] ->
            raise (Failure "nth_tl")

(*
 * Functional replacement.
 *)
let rec replacef_nth i f = function
   h::t ->
      if i = 0 then
         f h :: t
      else
         h :: replacef_nth (i - 1) f t
 | [] ->
      raise (Failure "replacef_nth")

let rec replacef_arg_nth i f = function
   h::t ->
      if i = 0 then
         let h, arg = f h in
            h :: t, arg
      else
         let t, arg = replacef_arg_nth (i - 1) f t in
            h :: t, arg
 | [] ->
      raise (Failure "replacef_arg_nth")

(*
 * Functional replacement.
 *)
let rec replace_nth i x = function
   h::t ->
      if i = 0 then
         x :: t
      else
         h :: replace_nth (i - 1) x t
 | [] ->
      raise (Failure "replace_nth")

let rec replace_first f x = function
   h::t ->
      if f h then
         x :: t
      else
         h :: replace_first f x t
 | [] ->
      raise Not_found

let rec replace_all f x = function
   h::t ->
      (if f h then x else h) :: (replace_all f x t)
 | [] ->
      []

(*
 * Functional replacement.
 *)
let rec replaceq x1 x2 = function
   h::t ->
      if h == x1 then
         x2 :: replaceq x1 x2 t
      else
         h :: replaceq x1 x2 t
 | [] ->
      []

(*
 * Remove an element.
 *)
let rec remove_nth i l =
   match l with
      h::t ->
         if i = 0 then
            t
         else
            h :: remove_nth (i - 1) t
    | [] ->
         raise (Failure "remove_nth")

(*
 * Insert an element into a position.
 *)
let rec insert_nth i x l =
   if i = 0 then
      x :: l
   else
      match l with
         h::t ->
            h :: insert_nth (i - 1) x t
       | [] ->
            raise (Failure "insert_nth")

(*
 * Find the elemnt.
 *)
let rec find f = function
   h::t ->
      if f h then
         h
      else
         find f t
 | [] ->
      raise Not_found

let rec find_item_aux f i = function
   h::t ->
      if f h then
         i
      else
         find_item_aux f (i + 1) t
 | [] ->
      raise Not_found

let find_item f l = find_item_aux f 0 l

let rec find_index_aux v i = function
   h::t ->
      if h = v then
         i
      else
         find_index_aux v (i + 1) t
 | [] ->
      raise Not_found

let find_index v l = find_index_aux v 0 l

let rec find_indexq_aux v i = function
   h::t ->
      if h == v then
         i
      else
         find_indexq_aux v (i + 1) t
 | [] ->
      raise Not_found

let find_indexq v l = find_indexq_aux v 0 l

(*
 * Intersect two lists.
 * Quadratic algorithm.
 *)
let rec intersect l = function
   h::t ->
      if List.mem h l then
         h :: intersect l t
      else
         intersect l t
 | [] -> []

let rec intersectq l = function
   h::t ->
      if List.memq h l then
         h :: intersectq l t
      else
         intersectq l t
 | [] -> []

(*
 * Subtract an element from a list.
 * Quadratic algorithm.
 *)
let rec subtract l1 l2 = match l1 with
   h::t ->
      if List.mem h l2 then
         t
      else
         h::(subtract t l2)
 | [] -> []

(*
 * Subtract an element from a list.
 * Quadratic algorithm.
 *)
let rec subtractq l1 l2 = match l1 with
   h::t ->
      if List.memq h l2 then
         t
      else
         h::(subtractq t l2)
 | [] -> []

(*
 * Union of lists by structural equality.
 *)
let rec union l = function
   h::t ->
      if List.mem h l then
         union l t
      else
         h::(union l t)
 | [] ->
      l

(*
 * Union of lists by physical equality.
 *)
let rec unionq l = function
   h::t ->
      if List.memq h l then
         unionq l t
      else
         h::(unionq l t)
 | [] ->
      l

(*
 * Zip two lists.
 *)
let rec zip_list l l1 l2 = match (l1,l2) with
   (h1::t1), (h2::t2) ->
      (h1, h2)::(zip_list l t1 t2)
 | [], [] ->
      l
 | _ -> raise (Failure "zip")

let zip a b = zip_list [] a b

(*
 * Find index of association.
 *)
let rec assoc_index_aux a i = function
   (a', _)::t ->
      if a' = a then
         i
      else
         assoc_index_aux a (i + 1) t
 | [] -> raise Not_found

let assoc_index l a = assoc_index_aux a 0 l

(*
 * Replace an association, but preserve order.
 *)
let rec assoc_replace l a b = match l with
   (a', b')::t ->
      if a' = a then
         (a, b)::t
      else
         (a', b')::(assoc_replace t a b)
 | [] -> raise Not_found

(*
 * Add the association if it doesn't already exist.
 *)
let add_assoc (v1, v2) l =
   try let v2' = List.assoc v1 l in
          if v2 = v2' then
             l
          else
             raise (Failure "add_assoc")
   with
      Not_found -> (v1, v2)::l

(*
 * See if a value is in range.
 *)
let rec assoc_in_range eq y = function
   (_, y')::tl ->
      if eq y y' then
         true
      else
         assoc_in_range eq y tl
 | [] ->
      false

(*
 * Split a list.
 *)
let rec split_list i l = match (i,l) with
   0, _ ->
      [], l
 | _, h::t ->
      let l, l' = split_list (i - 1) t in
         h::l, l'
 | _, [] ->
      raise (Failure "split_list")

(*
 * Split off the last item.
 *)
let rec split_last = function
   [h] ->
      [], h
 | h::t ->
      let l, x = split_last t in
         h::l, x
 | [] ->
      raise (Failure "split_last")

(*
 * Split off the last item.
 *)
let rec last = function
   [h] ->
      h
 | h::t ->
      last t
 | [] ->
      raise (Failure "last")

(*
 * Produce a list of first elements out of the list of pairs
 *)
let rec fst_split = function
   [] -> []
 | (a,b)::tl -> a::(fst_split tl)

(*
 * Remove the specified suffix from the list.
 *)

let rec remove_suffix_aux suffix = function
   (0, l') ->
      if l' = suffix then
         []
      else
         raise (Failure "remove_suffix")
 | (i, _::t) ->
      remove_suffix_aux suffix (i - 1, t)
 | _ ->
      (* This will never happen *)
      raise (Failure "remove_suffix")

let remove_suffix l suffix =
   let i = (List.length l) - (List.length suffix) in
      if i >= 0 then
         remove_suffix_aux suffix (i, l)
      else
         raise (Failure "remove_suffix")

(*
 * Reverse do_list.
 *)
let rec rev_iter f = function
   h::t ->
      rev_iter f t;
      f h;
      ()
 | [] ->
      ()

(*
 * Flat map.
 *)
let rec flat_map f = function
   h::t ->
      let h = f h in
         if h = [] then
            flat_map f t
         else
            h @ flat_map f t
 | [] ->
      []

(*
 * Map, and discard errors.
 *)
let rec fail_map f = function
   h::t ->
      begin
         try
            let h = f h in
               h :: fail_map f t
         with
            Failure _ ->
               fail_map f t
      end
 | [] ->
      []

(*
 * Map, and discard None.
 *)
let rec some_map f = function
   h::t ->
      begin
         match f h with
            Some h ->
               h :: some_map f t
          | None ->
               some_map f t
      end
 | [] ->
      []

(*
 * Cross between map and fold_left.
 *)
let rec fold_left_aux f x l = function
   h :: t ->
      let x', h' = f x h in
         fold_left_aux f x' (h' :: l) t
 | [] ->
      x, List.rev l

let fold_left f x l = fold_left_aux f x [] l

(*
 * Inherited.
 *)
let allp = List.for_all
let existsp = List.exists

let rec iter2 f al bl =
   match (al, bl) with
      h1::t1, h2::t2 ->
         f h1 h2;
         iter2 f t1 t2
    | [], [] ->
         ()
    | _ ->
         raise (Failure "iter2")

let rec rev_iter2 f a b =
   match (a,b) with
      ([], []) -> ()
    | (ha::ta, hb::tb) -> rev_iter2 f ta tb; f ha hb
    | _ -> raise (Failure "List_util.rev_iter2")

(*
 * Fold left over two lists.
 *)
let rec fold_left2 f x al bl =
   match (al, bl) with
      (h1::t1, h2::t2) ->
         fold_left2 f (f x h1 h2) t1 t2
    | [], [] ->
         x
    | _ ->
         raise (Failure "fold_left2")

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
