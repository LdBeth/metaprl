(*
 * List with easy acces to first and last elements.
 *)

(************************************************************************
 * TYPES                                                                *
 ************************************************************************)

(*
 * List with quick access to first, and last elements.
 *)
type 'a t =
   { flist_head : 'a option;
     flist_middle : 'a tree;
     flist_tail : 'a option
   }

and 'a tree =
   Empty
 | Leaf of 'a
 | Append of 'a tree * 'a tree

(************************************************************************
 * IMPLEMENTATION                                                       *
 ************************************************************************)

(*
 * Append two trees.
 *)
let append_tree tree1 tree2 =
   if tree1 = Empty then
      tree2
   else if tree2 = Empty then
      tree1
   else
      Append (tree1, tree2)

(*
 * Create a singleton list.
 *)
let create x =
   { flist_head = Some x; flist_middle = Empty; flist_tail = None }

(*
 * See if contains only one element.
 *)
let singleton = function
   { flist_head = None }
 | { flist_tail = None } ->
      true
 | _ ->
      false

(*
 * Only have to shift on empty lists.
 *)
let first = function
      { flist_head = Some x } ->
         x
    | { flist_head = None; flist_middle = Empty; flist_tail = Some x } ->
         x
    | _ ->
         raise (Failure "Rewrite_type.first")

let last = function
     { flist_tail = Some x } ->
         x
     | { flist_head = Some x; flist_middle = Empty; flist_tail = None } ->
         x
     | _ ->
         raise (Failure "Rewrite_type.shift_rightlast")

(*
 * Shift the last element into the middle.
 *)
let shift_left = function
   { flist_head = head; flist_tail = None } ->
      head, Empty
 | { flist_head = None; flist_middle = Empty; flist_tail = Some tail } ->
      Some tail, Empty
 | { flist_head = head; flist_middle = middle; flist_tail = Some tail } ->
      head, append_tree middle (Leaf tail)

(*
 * Shift the first element into the middle.
 *)
let shift_right = function
   { flist_head = None; flist_tail = tail } ->
      Empty, tail
 | { flist_head = Some head; flist_middle = Empty; flist_tail = None } ->
      Empty, Some head
 | { flist_head = Some head; flist_middle = middle; flist_tail = tail } ->
      append_tree (Leaf head) middle, tail

(*
 * Append two lists.
 *)
let append list1 list2 =
   let left, middle1 = shift_left list1 in
   let middle2, right = shift_right list2 in
   let middle = append_tree middle1 middle2 in
      { flist_head = left; flist_middle = middle; flist_tail = right }

(*
 * Append the lists,
 * but skip the last element from the first list,
 * and the first element from the second list,
 * and put x in their place.
 *)
let append_skip list1 x list2 =
   if singleton list1 then
      if singleton list2 then
         create x
      else
         let { flist_middle = middle; flist_tail = tail } = list2 in
            { flist_head = Some x; flist_middle = middle; flist_tail = tail }
   else if singleton list2 then
      let { flist_head = head; flist_middle = middle } = list1 in
         { flist_head = head; flist_middle = middle; flist_tail = Some x }
   else
      let { flist_head = head1; flist_middle = middle1 } = list1 in
      let { flist_middle = middle2; flist_tail = tail2 } = list2 in
      let middle = append_tree middle1 (Leaf x) in
      let middle = append_tree middle middle2 in
         { flist_head = head1; flist_middle = middle; flist_tail = tail2 }

(*
 * Get the tree.
 *)
let tree_of_list { flist_head = head; flist_middle = middle; flist_tail = tail } =
   let middle =
      match head with
         None ->
            middle
       | Some head ->
            append_tree (Leaf head) middle
   in
      match tail with
         None ->
            middle
       | Some tail ->
            append_tree middle (Leaf tail)

(*
 * $Log$
 * Revision 1.1  1998/06/23 22:12:12  jyh
 * Improved rewriter speed with conversion tree and flist.
 *
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
