(*
 * A splay table is like a functional hash table.
 * This code is derived from the Splay_set code.
 *)
module type SplayTableSig =
sig
   type elt
   type set
   type 'a t

   val create : set -> 'a t
   val add : 'a t -> elt -> 'a -> 'a t
   val union : ('a list -> 'a list -> 'a list) -> 'a t -> 'a t -> 'a t
   val mem : 'a t -> elt -> bool
   val find : 'a t -> elt -> 'a
   val find_all : 'a t -> elt -> 'a list
   val remove : 'a t -> elt -> 'a t
   val iter : (elt -> 'a -> unit) -> 'a t -> unit
   val map : (elt -> 'a -> 'b) -> 'a t -> 'b t
end

(*
 * Ordering module takes a comparison set.
 *)
module type OrdSig =
sig
   type t
   type set

   val union : set -> set -> set
   val compare : set -> t -> t -> int
end

(*
 * Build the set from an ordered type.
 *)
module MakeSplayTable (Ord : OrdSig) =
struct
   (************************************************************************
    * TYPES                                                                *
    ************************************************************************)

   type elt = Ord.t
   type set = Ord.set

   (*
    * Table is a binary tree.
    * Each node has five fields:
    *    1. a key
    *    2. a list of values associated with the key
    *    3. a left child
    *    4. a right child
    *    5. the total number of keys in the tree
    *)
   type 'a tree =
      Leaf
    | Node of elt * 'a list * 'a tree * 'a tree * int

   (*
    * We keep an argument for the comparison,
    * and the aplay tree.  The tree is mutable
    * so that we can rearrange the tree in place.
    * However, we all splay operations are functional,
    * and we assume that the rearranged tree can be
    * assigned atomically to this field.
    *)
   type 'a t =
      { mutable splay_tree : 'a tree;
        splay_arg : set
      }

   (*
    * Directions are used to define
    * paths in the tree.
    *)
   type 'a direction =
      Left of 'a tree
    | Right of 'a tree

   (*
    * Result of a splay operation.
    *)
   type 'a splay_result =
      SplayFound of 'a tree
    | SplayNotFound of 'a tree

   (************************************************************************
    * IMPLEMENTATION                                                       *
    ************************************************************************)

   (*
    * Size of a table.
    *)
   let cardinality = function
      Node (_, _, _, _, size) ->
         size
    | Leaf ->
         0

   (*
    * Add two nodes.
    *)
   let new_node key data left right =
      Node (key, data, left, right, cardinality left + cardinality right + 1)

   (*
    * Rotate kthe entries so that the child becomes the root.
    * Return the new left and right children.
    *)
   let rotate_left left right = function
      Node (key, data, _, right', _) ->
         left, new_node key data right right'
    | _ ->
         raise (Invalid_argument "rotate_left")

   let rotate_right left right = function
      Node (key, data, left', _, _) ->
         new_node key data left left', right
    | _ ->
         raise (Invalid_argument "rotate_right")

   let rotate_left_left left right = function
      Node (key,
            data,
            Node (key_left, data_left, _, left_right, _),
            right',
            _) ->
         left, new_node key_left data_left right (new_node key data left_right right')
    | _ ->
         raise (Invalid_argument "rotate_left_left")

   let rotate_right_right left right = function
      Node (key,
            data,
            left',
            Node (key_right, data_right, right_left, _, _),
            _) ->
         new_node key_right data_right (new_node key data left' right_left) left, right
    | _ ->
         raise (Invalid_argument "rotate_right_right")

   let rotate_left_right left right = function
      Node (key,
            data,
            Node (key_left, data_left, left_left, _, _),
            right',
            _) ->
         new_node key_left data_left left_left left, new_node key data right right'
    | _ ->
         raise (Invalid_argument "rotate_left_right")

   let rotate_right_left left right = function
      Node (key,
            data,
            left',
            Node (key_right, data_right, _, right_right, _),
            _) ->
         new_node key data left' left, new_node key_right data_right right right_right
    | _ ->
         raise (Invalid_argument "rotate_right_left")

   (*
    * This function performs the action of moving an entry
    * to the root.  The argument is the path to the entry.
    *)
   let rec lift key data left right = function
      [] ->
         new_node key data left right
    | [Left parent] ->
         let left, right = rotate_left left right parent in
            new_node key data left right
    | [Right parent] ->
         let left, right = rotate_right left right parent in
            new_node key data left right
    | Left parent :: Left grandparent :: ancestors ->
         let left, right = rotate_left_left left right grandparent in
            lift key data left right ancestors
    | Right parent :: Right grandparent :: ancestors ->
         let left, right = rotate_right_right left right grandparent in
            lift key data left right ancestors
    | Left parent :: Right grandparent :: ancestors ->
         let left, right = rotate_right_left left right grandparent in
            lift key data left right ancestors
    | Right parent :: Left grandparent :: ancestors ->
         let left, right = rotate_left_right left right grandparent in
            lift key data left right ancestors

   (*
    * Find an entry in the tree.
    * Returns true iff the entry is found.
    * Transforms the tree so that either the
    * entry becomes the root, or an adjacent entry
    * becomes the root if the entry is not found.
    *)
   let rec splay arg key0 path = function
      Node (key, data, left, right, _) as node ->
         let comp = Ord.compare arg key0 key in
            if comp = 0 then
               SplayFound (lift key data left right path)
            else if comp < 0 then
               (* node is down the left branch *)
               if left = Leaf then
                  SplayNotFound (lift key data left right path)
               else
                  splay arg key0 (Left node :: path) left
            else if right = Leaf then
               SplayNotFound (lift key data left right path)
            else
               splay arg key0 (Right node :: path) right

    | Leaf ->
         SplayNotFound Leaf

   (*
    * Move the rioghtmost node to the root.
    *)
   let rec lift_right = function
      Node (key, data, left, Leaf, _) as parent ->
         key, data, left, Leaf
    | Node (_, _, _, Node (key, data, left, Leaf, _), _) as parent ->
         let left, right = rotate_right left Leaf parent in
            key, data, left, right
    | Node (_, _, _, Node (_, _, left, right, _), _) as grandparent ->
         let key, data, left, right = lift_right right in
         let left, right = rotate_right_right left right grandparent in
            key, data, left, right
    | Leaf ->
         raise (Invalid_argument "lift_right")

   let rec splay_right = function
      Leaf ->
         Leaf
    | node ->
         let key, data, left, right = lift_right node in
            new_node key data left right

   (*
    * An empty tree is just a leaf.
    *)
   let empty = Leaf

   let create arg =
      { splay_tree = empty;
        splay_arg = arg
      }

   (*
    * Check if a key is listed in the table.
    *)
   let mem t key =
      let { splay_tree = tree; splay_arg = arg } = t in
         match splay arg key [] tree with
            SplayFound tree ->
               t.splay_tree <- tree;
               true
          | SplayNotFound tree ->
               t.splay_tree <- tree;
               false

   let find t key =
      let { splay_tree = tree; splay_arg = arg } = t in
         match splay arg key [] tree with
            SplayFound tree ->
               begin
                  t.splay_tree <- tree;
                  match tree with
                     Node (_, data :: _, _, _, _) ->
                        data
                   | _ ->
                        raise (Failure "find")
               end
          | SplayNotFound tree ->
               t.splay_tree <- tree;
               raise Not_found

   let find_all t key =
      let { splay_tree = tree; splay_arg = arg } = t in
         match splay arg key [] tree with
            SplayFound tree ->
               begin
                  t.splay_tree <- tree;
                  match tree with
                     Node (_, data, _, _, _) ->
                        data
                   | _ ->
                        raise (Failure "find_all")
               end
          | SplayNotFound tree ->
               t.splay_tree <- tree;
               raise Not_found


   (*
    * Add an entry to the table.
    * If the entry already exists,
    * the new value is added to the data.
    *)
   let add_list arg tree key data =
      match splay arg key [] tree with
         SplayFound tree ->
            begin
               match tree with
                  Node (key, data', left, right, size) ->
                     Node (key, data @ data', left, right, size)
                | Leaf ->
                     raise (Failure "add_list")
            end
       | SplayNotFound tree ->
            begin
               match tree with
                  Node (key', data', left, right, size) ->
                     if Ord.compare arg key key' < 0 then
                        (* Root should become right child *)
                        new_node key data left (new_node key' data' empty right)
                     else
                        (* Root should become left child *)
                        new_node key data (new_node key' data left empty) right
                | Leaf ->
                        (* Tree is empty, so make a new root *)
                     new_node key data empty empty
            end

   let add t key data =
      let { splay_tree = tree; splay_arg = arg } = t in
         { splay_tree = add_list arg tree key [data]; splay_arg = arg }

   (*
    * Remove the first entry from the hashtable.
    * If the value list becomes empty, remove the
    * entire entry from the tree.
    *)
   let remove t key =
      let { splay_tree = tree; splay_arg = arg } = t in
      let tree =
         match splay arg key [] tree with
            SplayFound tree ->
               begin
                  match tree with
                     Node (key, _ :: data, left, right, size) ->
                        Node (key, data, left, right, size)
                   | Node (_, [], Leaf, right, _) ->
                        right
                   | Node (_, [], left, Leaf, _) ->
                        left
                   | Node (_, [], left, right, _) ->
                        begin
                           match splay_right left with
                              Node (key, data, left_left, Leaf, _) ->
                                 new_node key data left_left right
                            | _ ->
                                 raise (Failure "remove")
                        end
                   | Leaf ->
                        raise (Failure "remove")
               end
          | SplayNotFound tree ->
               tree
      in
         { splay_tree = tree; splay_arg = arg }

   (*
    * Merge two hashtables.
    * Data fields get concatenated.
    *)
   let rec union_aux arg append s1 s2 =
      match s1, s2 with
         Leaf, _ ->
            s2
       | _, Leaf ->
            s1
       | Node (key1, data1, left1, right1, size1),
         Node (key2, data2, left2, right2, size2) ->
            if size1 >= size2 then
               if size2 = 1 then
                  add_list arg s1 key2 data2
               else
                  match splay arg key1 [] s2 with
                     SplayFound (Node (key2, data2, left2, right2, _)) ->
                        let left3 = union_aux arg append left1 left2 in
                        let right3 = union_aux arg append right1 right2 in
                           new_node key1 (append data1 data2) left3 right3
                   | SplayNotFound (Node (key2, data2, left2, right2, _)) ->
                        if compare key1 key2 < 0 then
                           let left3 = union_aux arg append left1 left2 in
                           let right3 = union_aux arg append right1 (new_node key2 data2 empty right2) in
                              new_node key1 data1 left3 right3
                        else
                           let left3 = union_aux arg append left1 (new_node key2 data2 left2 empty) in
                           let right3 = union_aux arg append right1 right2 in
                              new_node key1 data1 left3 right3
                   | _ ->
                        raise (Failure "union")
            else if size1 = 1 then
               add_list arg s2 key1 data1
            else
               match splay arg key2 [] s1 with
                  SplayFound (Node (key1, data1, left1, right1, _)) ->
                     let left3 = union_aux arg append left1 left2 in
                     let right3 = union_aux arg append right1 right2 in
                        new_node key2 (append data1 data2) left3 right3
                | SplayNotFound (Node (key1, data1, left1, right1, _)) ->
                     if compare key2 key1 < 0 then
                        let left3 = union_aux arg append left1 left2 in
                        let right3 = union_aux arg append (new_node key1 data1 empty right1) right2 in
                           new_node key2 data2 left3 right3
                     else
                        let left3 = union_aux arg append (new_node key1 data1 left1 empty) left2 in
                        let right3 = union_aux arg append right1 right2 in
                           new_node key2 data2 left3 right3
                | _ ->
                     raise (Failure "union")

   let union append s1 s2 =
      let { splay_tree = tree1; splay_arg = arg1 } = s1 in
      let { splay_tree = tree2; splay_arg = arg2 } = s2 in
      let arg = Ord.union arg1 arg2 in
         { splay_tree = union_aux arg append tree1 tree2;
           splay_arg = arg
         }

   (*
    * Iterate a function over the hashtable.
    *)
   let rec iter_aux f = function
      Node (key, data, left, right, _) ->
         List.iter (f key) data;
         iter_aux f left;
         iter_aux f right
    | Leaf ->
         ()

   let iter f { splay_tree = t } =
      iter_aux f t

   (*
    * Map a function over the table.
    *)
   let rec map_aux f = function
      Node (key, data, left, right, size) ->
         Node (key, List.map (f key) data, map_aux f left, map_aux f right, size)
    | Leaf ->
         Leaf

   let map f { splay_tree = tree; splay_arg = arg } =
      { splay_tree = map_aux f tree; splay_arg = arg }
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
