(*
 * Build the set from an ordered type.
 *)
module Make (Ord : Set.OrderedType) =
struct
   (************************************************************************
    * TYPES                                                                *
    ************************************************************************)

   type elt = Ord.t

   (*
    * Table is a binary tree.
    * Each node has four fields:
    *    1. a key
    *    2. a left child
    *    3. a right child
    *    4. the total number of elements in the tree
    *)
   type tree =
      Leaf
    | Node of elt * tree * tree * int

   (*
    * The tree is mutable
    * so that we can rearrange the tree in place.
    * However, we all splay operations are functional,
    * and we assume that the rearranged tree can be
    * assigned atomically to this field.
    *
    * The timer is used to count splay operations.
    * When the timer goes off, we assume that the
    * tree is balanced, and membership does not
    * splay the tree any more.
    *)
   type t =
      { mutable splay_tree : tree }

   (*
    * Directions are used to define
    * paths in the tree.
    *)
   type direction =
      Left of tree
    | Right of tree

   (*
    * Result of a splay operation.
    *)
   type splay_result =
      SplayFound of tree
    | SplayNotFound of tree

   (************************************************************************
    * IMPLEMENTATION                                                       *
    ************************************************************************)

   (*
    * Size of a table.
    *)
   let cardinality = function
      Node (_, _, _, size) ->
         size
    | Leaf ->
         0

   let cardinal s1 =
      cardinality s1.splay_tree

   (*
    * Add two nodes.
    *)
   let new_node key left right =
      Node (key, left, right, cardinality left + cardinality right + 1)

   (*
    * This function performs the action of moving an entry
    * to the root.  The argument is the path to the entry.
    *)
   let rec lift key left right = function
      [] ->
         new_node key left right
    | [Left (Node (key', _, right', _))] ->
            new_node key left (new_node key' right right')
    | [Right (Node (key', left', _, _))] ->
            new_node key (new_node key' left' left) right
    | Left (Node (key_left, _, left_right, _)) :: Left (Node (key', _, right', _)) :: ancestors ->
            lift key left (new_node key_left right (new_node key' left_right right')) ancestors
    | Right (Node (key_right, right_left, _, _)) :: Right (Node (key', left', _, _)) :: ancestors ->
            lift key (new_node key_right (new_node key' left' right_left) left) right ancestors
    | Left (Node (key_right, _, right_right, _)) :: Right (Node (key', left', _, _)) :: ancestors ->
            lift key (new_node key' left' left) (new_node key_right right right_right) ancestors
    | Right (Node (key_left, left_left, _, _)) :: Left (Node (key', _, right', _)) :: ancestors ->
            lift key (new_node key_left left_left left) (new_node key' right right') ancestors
    | _ -> raise (Invalid_argument "lift")

   (*
    * Find an entry in the tree.
    * Returns true iff the entry is found.
    * Transforms the tree so that either the
    * entry becomes the root, or an adjacent entry
    * becomes the root if the entry is not found.
    *)
   let rec splay key0 path = function
      Node (key, left, right, _) as node ->
         let comp = Ord.compare key0 key in
            if comp = 0 then
               SplayFound (lift key left right path)
            else if comp < 0 then
               if left = Leaf then
                  SplayNotFound (lift key left right path)
               else
                  splay key0 (Left node :: path) left
            else if right = Leaf then
               SplayNotFound (lift key left right path)
            else
               splay key0 (Right node :: path) right

    | Leaf ->
         SplayNotFound Leaf

   (*
    * Search without reorganizing the tree.
    *)
   let rec search key0 = function
      Node (key, left, right, _) as node ->
         let comp = Ord.compare key0 key in
            if comp = 0 then
               true
            else if comp < 0 then
               search key0 left
            else
               search key0 right

    | Leaf ->
         false

   (*
    * Move the rightmost node to the root.
    *)
   let rec lift_right = function
      Node (key, left, Leaf, _) ->
         key, left
    | Node (key, left', Node (key', left, Leaf, _), _) ->
            key', new_node key left' left
    | Node (key, left', Node (key_right, right_left, right, _), _) ->
         let key', left = lift_right right in
            key', new_node key_right (new_node key left' right_left) left
    | Leaf ->
         raise (Invalid_argument "lift_right")

   let rec splay_right = function
      Leaf ->
         Leaf
    | node ->
         let key, left = lift_right node in
            new_node key left Leaf

   (*
    * An empty tree is just a leaf.
    *)
   let empty =
      { splay_tree = Leaf }

   let is_empty s1 =
      s1.splay_tree = Leaf

   let make key =
      { splay_tree = Node (key, Leaf, Leaf, 1) }

   (*
    * check if a key is listed in the table.
    *)
   let mem t key =
      match splay key [] t.splay_tree with
         SplayFound tree ->
            t.splay_tree <- tree;
            true
       | SplayNotFound tree ->
            t.splay_tree <- tree;
            false

   (*
    * Add an entry to the table.
    * If the entry already exists,
    * the new value is added to the data.
    *)
   let add_aux tree key =
      match splay key [] tree with
         SplayFound tree ->
            tree
       | SplayNotFound tree ->
            begin
               match tree with
                  Node (key', left, right, size) ->
                     if Ord.compare key key' < 0 then
                        new_node key left (new_node key' Leaf right)
                     else
                        new_node key (new_node key' left Leaf) right
                | Leaf ->
                     (* Tree is empty, so make a new root *)
                     new_node key Leaf Leaf
            end

   let add key t =
      { splay_tree = add_aux t.splay_tree key }

   (*
    * Remove the first entry from the hashtable.
    * If the value list becomes empty, remove the
    * entire entry from the tree.
    *)
   let remove key t =
      match splay key [] t.splay_tree with
         SplayFound tree ->
            begin
               match tree with
                  Node (_, Leaf, right, _) ->
                     { splay_tree = right }
                | Node (_, left, Leaf, _) ->
                     { splay_tree = left }
                | Node (_, left, right, _) ->
                     begin
                        match splay_right left with
                           Node (key, left_left, Leaf, _) ->
                              { splay_tree = new_node key left_left right }
                         | _ ->
                              raise (Failure "remove")
                     end
                | Leaf ->
                     raise (Failure "remove")
            end
       | SplayNotFound tree ->
            t.splay_tree <- tree;
            t

   (*
    * Merge two hashtables.
    * Data fields get concatenated.
    *)
   let rec union_aux s1 s2 =
      match s1, s2 with
         Leaf, _ ->
            s2
       | _, Leaf ->
            s1
       | Node (key1, left1, right1, size1),
         Node (key2, left2, right2, size2) ->
            if size1 >= size2 then
               if size2 = 1 then
                  add_aux s1 key2
               else
                  match splay key1 [] s2 with
                     SplayFound (Node (key2, left2, right2, _)) ->
                        let left3 = union_aux left1 left2 in
                        let right3 = union_aux right1 right2 in
                           new_node key1 left3 right3
                   | SplayNotFound (Node (key2, left2, right2, _)) ->
                        if compare key1 key2 < 0 then
                           let left3 = union_aux left1 left2 in
                           let right3 = union_aux right1 (new_node key2 Leaf right2) in
                              new_node key1 left3 right3
                        else
                           let left3 = union_aux left1 (new_node key2 left2 Leaf) in
                           let right3 = union_aux right1 right2 in
                              new_node key1 left3 right3
                   | _ ->
                        raise (Failure "union")
            else if size1 = 1 then
               add_aux s2 key1
            else
               match splay key2 [] s1 with
                  SplayFound (Node (key1, left1, right1, _)) ->
                     let left3 = union_aux left1 left2 in
                     let right3 = union_aux right1 right2 in
                        new_node key2 left3 right3
                | SplayNotFound (Node (key1, left1, right1, _)) ->
                     if compare key2 key1 < 0 then
                        let left3 = union_aux left1 left2 in
                        let right3 = union_aux (new_node key1 Leaf right1) right2 in
                           new_node key2 left3 right3
                     else
                        let left3 = union_aux (new_node key1 left1 Leaf) left2 in
                        let right3 = union_aux right1 right2 in
                           new_node key2 left3 right3
                | _ ->
                     raise (Failure "union")

   let union s1 s2 =
      { splay_tree = union_aux s1.splay_tree s2.splay_tree }

   (*
    * See if two sets intersect.
    *)
   let rec intersects s1 s2 =
      match s1, s2 with
         Leaf, _
       | _, Leaf ->
            false
       | Node (key1, left1, right1, size1),
         Node (key2, left2, right2, size2) ->
            if size1 >= size2 then
               if size2 = 1 then
                  search key2 s1
               else
                  match splay key1 [] s2 with
                     SplayFound tree ->
                        true
                   | SplayNotFound (Node (key2, left2, right2, _) as tree) ->
                        if compare key1 key2 < 0 then
                           (intersects left1 left2)
                           || (intersects right1 (new_node key2 Leaf right2))
                        else
                           (intersects right1 right2)
                           || (intersects left1 (new_node key2 left2 Leaf))
                   | _ ->
                        raise (Failure "intersects")
            else if size1 = 1 then
               search key1 s2
            else
               match splay key1 [] s1 with
                  SplayFound _ ->
                     true
                | SplayNotFound (Node (key1, left1, right1, _)) ->
                     if compare key2 key1 < 0 then
                        (intersects left1 left2)
                        || (intersects (new_node key1 Leaf right1) right2)
                     else
                        (intersects right1 right2)
                        || (intersects (new_node key1 left1 Leaf) left2)
                | _ ->
                     raise (Failure "union")

   let intersectp s1 s2 =
      intersects s1.splay_tree s2.splay_tree

   (*
    * Iterate a function over the hashtable.
    *)
   let rec iter_aux f = function
      Node (key, left, right, _) ->
         iter_aux f left;
         f key;
         iter_aux f right
    | Leaf ->
         ()

   let iter f t =
      iter_aux f t.splay_tree

   (*
    * Convert the set to a list.
    *)
   let rec elements_aux l = function
      Node (key, left, right, _) ->
         elements_aux (key :: elements_aux l right) left
    | Leaf ->
         l

   let elements t =
      elements_aux [] t.splay_tree

   (*
    * Intersection.
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
