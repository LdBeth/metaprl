(*
 * A splay table is like a functional hash table.
 * This code is derived from the Splay_set code.
 *)
module type SplayTableSig =
sig
   type elt
   type set
   type t
   type data

   val create : set -> t
   val add : t -> elt -> data -> t
   val union : t -> t -> t
   val mem : t -> elt -> bool
   val find : t -> elt -> data
   val find_all : t -> elt -> data list
   val remove : t -> elt -> t
   val iter : (elt -> data -> unit) -> t -> unit
   val map : (elt -> data -> data) -> t -> t
end

(*
 * Ordering module takes a comparison set.
 *)
module type TableBaseSig =
sig
   type elt
   type set
   type data

   val union : set -> set -> set
   val compare : set -> elt -> elt -> int
   val append : data list -> data list -> data list
end

(*
 * Build the set from an ordered type.
 *)
module MakeSplayTable (Base : TableBaseSig) =
struct
   (************************************************************************
    * TYPES                                                                *
    ************************************************************************)

   type elt = Base.elt
   type set = Base.set
   type data = Base.data

   (*
    * Table is a binary tree.
    * Each node has five fields:
    *    1. a key
    *    2. a list of values associated with the key
    *    3. a left child
    *    4. a right child
    *    5. the total number of keys in the tree
    *)
   type tree =
      Leaf
    | Node of elt * data list * tree * tree * int

   (*
    * We keep an argument for the comparison,
    * and the aplay tree.  The tree is mutable
    * so that we can rearrange the tree in place.
    * However, we all splay operations are functional,
    * and we assume that the rearranged tree can be
    * assigned atomically to this field.
    *)
   type t =
      { mutable splay_tree : tree;
        splay_arg : set
      }

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
      Node (_, _, _, _, size) ->
         size
    | Leaf ->
         0

   (*
    * Add two nodes.
    *)
   let new_node key data left right =
      if data == [] then raise (Invalid_argument "Splay_table.new_node : empty data") else
      Node (key, data, left, right, cardinality left + cardinality right + 1)

   (*
    * This function performs the action of moving an entry
    * to the root.  The argument is the path to the entry.
    *)
   let rec lift key data left right = function
      [] ->
         new_node key data left right
    | [Left (Node (key', data', _, right', _))] ->
         new_node key data left (new_node key' data' right right')
    | [Right (Node (key', data', left', _, _))] ->
         new_node key data (new_node key' data' left' left) right
    | Left (Node (key_left, data_left, _, left_right, _)) :: Left (Node (key', data', _, right', _)) :: ancestors ->
         lift key data left (new_node key_left data_left right (new_node key' data' left_right right')) ancestors
    | Right (Node (key_right, data_right, right_left, _, _)) :: Right (Node (key', data', left', _, _)) :: ancestors ->
         lift key data (new_node key_right data_right (new_node key' data' left' right_left) left) right ancestors
    | Left (Node (key_right, data_right, _, right_right, _)) :: Right (Node (key', data', left', _, _)) :: ancestors ->
         lift key data (new_node key' data' left' left) (new_node key_right data_right right right_right) ancestors
    | Right (Node (key_left, data_left, left_left, _, _)) :: Left (Node (key', data', _, right', _)) :: ancestors ->
         lift key data (new_node key_left data_left left_left left) (new_node key' data' right right') ancestors
    | _ ->
         raise (Invalid_argument "lift")

   (*
    * Find an entry in the tree.
    * Returns true iff the entry is found.
    * Transforms the tree so that either the
    * entry becomes the root, or an adjacent entry
    * becomes the root if the entry is not found.
    *)
   let rec splay arg key0 path = function
      Node (key, data, left, right, _) as node ->
         let comp = Base.compare arg key0 key in
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
      Node (key, data, left, Leaf, _) ->
         key, data, left
    | Node (key', data', left', Node (key, data, left, Leaf, _), _) ->
         key, data, new_node key' data' left' left
    | Node (key', data', left', Node (key_right, data_right, right_left, right, _), _) ->
         let key, data, left = lift_right right in
            key, data, new_node key_right data_right (new_node key' data' left' right_left) left
    | Leaf ->
         raise (Invalid_argument "lift_right")

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
      match splay t.splay_arg key [] t.splay_tree with
         SplayFound tree ->
            t.splay_tree <- tree;
            true
       | SplayNotFound tree ->
            t.splay_tree <- tree;
            false

   let find t key =
      match splay t.splay_arg key [] t.splay_tree with
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
      match splay t.splay_arg key [] t.splay_tree with
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
                     Node (key, Base.append data data', left, right, size)
                | Leaf ->
                     raise (Failure "add_list")
            end
       | SplayNotFound tree ->
            begin
               match tree with
                  Node (key', data', left, right, size) ->
                     if Base.compare arg key key' < 0 then
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
      let arg = t.splay_arg in
      { splay_tree = add_list arg t.splay_tree key [data]; splay_arg = arg }

   (*
    * Remove the first entry from the hashtable.
    * If the value list becomes empty, remove the
    * entire entry from the tree.
    *)
   let remove t key =
      let arg = t.splay_arg in
      match splay arg key [] t.splay_tree with
         SplayFound tree ->
            begin
               match tree with
                  Node (_, [_], Leaf, right, _) ->
                     { splay_tree = right; splay_arg = arg }
                | Node (_, [_], left, Leaf, _) ->
                     { splay_tree = left; splay_arg = arg }
                | Node (_, [_], left, right, _) ->
                     let key, data, left_left = lift_right left in
                     { splay_tree = new_node key data left_left right; splay_arg = arg }
                | Node (key, _ :: data, left, right, size) ->
                     { splay_tree = Node (key, data, left, right, size); splay_arg = arg }
                | _ ->
                     raise (Failure "remove")
            end
       | SplayNotFound tree ->
            t.splay_tree <- tree;
            t

   (*
    * Merge two hashtables.
    * Data fields get concatenated.
    *)
   let rec union_aux arg s1 s2 =
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
                        let left3 = union_aux arg left1 left2 in
                        let right3 = union_aux arg right1 right2 in
                           new_node key1 (Base.append data1 data2) left3 right3
                   | SplayNotFound (Node (key2, data2, left2, right2, _)) ->
                        if compare key1 key2 < 0 then
                           let left3 = union_aux arg left1 left2 in
                           let right3 = union_aux arg right1 (new_node key2 data2 empty right2) in
                              new_node key1 data1 left3 right3
                        else
                           let left3 = union_aux arg left1 (new_node key2 data2 left2 empty) in
                           let right3 = union_aux arg right1 right2 in
                              new_node key1 data1 left3 right3
                   | _ ->
                        raise (Failure "union")
            else if size1 = 1 then
               add_list arg s2 key1 data1
            else
               match splay arg key2 [] s1 with
                  SplayFound (Node (key1, data1, left1, right1, _)) ->
                     let left3 = union_aux arg left1 left2 in
                     let right3 = union_aux arg right1 right2 in
                        new_node key2 (Base.append data1 data2) left3 right3
                | SplayNotFound (Node (key1, data1, left1, right1, _)) ->
                     if compare key2 key1 < 0 then
                        let left3 = union_aux arg left1 left2 in
                        let right3 = union_aux arg (new_node key1 data1 empty right1) right2 in
                           new_node key2 data2 left3 right3
                     else
                        let left3 = union_aux arg (new_node key1 data1 left1 empty) left2 in
                        let right3 = union_aux arg right1 right2 in
                           new_node key2 data2 left3 right3
                | _ ->
                     raise (Failure "union")

   let union s1 s2 =
      let { splay_tree = tree1; splay_arg = arg1 } = s1 in
      let { splay_tree = tree2; splay_arg = arg2 } = s2 in
      let arg = Base.union arg1 arg2 in
         { splay_tree = union_aux arg tree1 tree2;
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
