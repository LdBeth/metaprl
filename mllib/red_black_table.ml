(*
 * Build a set using a red-black tree.
 * Every node in the tree is colored either black or red.
 * A red-black tree has the following invariants:
 *    1. Every leaf is colored black
 *    2. All children of every red node are black.
 *    3. Every path from the root to a leaf has the
 *       same number of black nodes as every other path.
 *    4. The root is always black.
 *
 * We get some corollaries:
 *    1. The longest path from the root to a leaf is
 *       at most twice as long as the shortest path.
 *    2. Both children of a red node are either leaves,
 *       or they are both not.
 *
 * This code is meant to be fast, so all the cases have
 * been expanded, and the insert and delete functions are
 * long (12 cases for insert, 18 for delete in lift_black).
 *
 * ----------------------------------------------------------------
 *
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/index.html for information on Nuprl,
 * OCaml, and more information about this system.
 *
 * Copyright (C) 1998 Jason Hickey, Cornell University
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
 * Author: Jason Hickey
 * jyh@cs.cornell.edu
 *)
open Format

open Set_sig

(************************************************************************
 * TYPES                                                                *
 ************************************************************************)

(*
 * Table is a binary tree.
 * Color is kept in the label to save space.
 *)
type ('elt, 'data) tree =
   Leaf
 | Red of 'elt * 'data list * ('elt, 'data) tree * ('elt, 'data) tree * int
 | Black of 'elt * 'data list * ('elt, 'data) tree * ('elt, 'data) tree * int

(*
 * Pass around the comparison function.
 * The tree is always balanced, so we don't need
 * extra mutable fields.
 *)
type ('arg, 'elt, 'data) table = 'arg * ('elt, 'data) tree
type ('arg, 'elt, 'data) t = ('arg, 'elt, 'data) table

(*
 * Path into the tree.
 *)
type ('elt, 'data) path =
   Left of ('elt, 'data) tree
 | Right of ('elt, 'data) tree
 | Delete of ('elt, 'data) tree

(*
 * Make the set.
 *)
let create
    (ord_print : 'arg -> 'elt -> 'data list -> unit)
    (ord_union : 'arg -> 'arg -> 'arg)
    (ord_compare : 'arg -> 'elt -> 'elt -> int)
    (ord_append : 'data list -> 'data list -> 'data list) =
   (*
    * Size of a table.
    *)
   let cardinality = function
      Red (_, _, _, _, size) ->
         size
    | Black (_, _, _, _, size) ->
         size
    | Leaf ->
         0
   in

   let cardinal (_, tree) =
      cardinality tree
   in

   (*
    * Add two nodes.
    *)
   let new_black key data left right =
      Black (key, data, left, right, cardinality left + cardinality right + 1)
   in

   let new_red key data left right =
      Red (key, data, left, right, cardinality left + cardinality right + 1)
   in

   (************************************************************************
    * DEBUGGING                                                            *
    ************************************************************************)

   (*
    * Check the size of the set.
    *)
   let rec check_size = function
      Black (_, _, left, right, size) ->
         if size <> check_size left + check_size right then
            raise (Invalid_argument "check_size");
         size

    | Red (_, _, left, right, size) ->
         if size <> check_size left + check_size right then
            raise (Invalid_argument "check_size");
         size

    | Leaf ->
         1
   in

   (*
    * Check the red-invariant.
    *)
   let rec check_red = function
      Red (_, _, left, right, _) ->
         begin
            match left, right with
               Red _, _
             | _, Red _ ->
                  raise (Failure "Red_black_tableed_black_set.check_red")

             | _ ->
                  check_red left;
                  check_red right
         end
    | Black (_, _, left, right, _) ->
         check_red left;
         check_red right

    | Leaf ->
         ()
   in

   (*
    * Check the black invariant.
    *)
   let rec black_depth i = function
      Black (_, _, left, _, _) ->
         black_depth (succ i) left
    | Red (_, _, left, _, _) ->
         black_depth i left
    | Leaf ->
         i
   in

   let rec check_black_aux i j = function
      Black (_, _, left, right, _) ->
         check_black_aux i (succ j) left;
         check_black_aux i (succ j) right
    | Red (_, _, left, right, _) ->
         check_black_aux i j left;
         check_black_aux i j right
    | Leaf ->
         if j <> i then
            raise (Failure "Red_black_table.check_black")
   in

   let check_black tree =
      check_black_aux (black_depth 0 tree) 0 tree
   in

   (*
    * Check that all the nodes are sorted.
    *)
   let rec check_sort_lt arg key = function
      Black (key', _, left, right, _) ->
         if ord_compare arg key' key >= 0 then
            raise (Failure "Red_black_table.check_sort");
         check_sort_lt arg key' left;
         check_sort_gt_lt arg key' key right

    | Red (key', _, left, right, _) ->
         if ord_compare arg key' key >= 0 then
            raise (Failure "Red_black_table.check_sort");
         check_sort_lt arg key' left;
         check_sort_gt_lt arg key' key right

    | Leaf ->
         ()

   and check_sort_gt arg key = function
      Black (key', _, left, right, _) ->
         if ord_compare arg key' key <= 0 then
            raise (Failure "Red_black_table.check_sort");
         check_sort_gt_lt arg key key' left;
         check_sort_gt arg key right

    | Red (key', _, left, right, _) ->
         if ord_compare arg key' key <= 0 then
            raise (Failure "Red_black_table.check_sort");
         check_sort_gt_lt arg key key' left;
         check_sort_gt arg key right

    | Leaf ->
         ()

   and check_sort_gt_lt arg key key' = function
      Black (key'', _, left, right, _) ->
         if ord_compare arg key'' key <= 0 || ord_compare arg key'' key' >= 0 then
            raise (Failure "Red_black_table.check_sort");
         check_sort_gt_lt arg key key'' left;
         check_sort_gt_lt arg key'' key' right

    | Red (key'', _, left, right, _) ->
         if ord_compare arg key'' key <= 0 || ord_compare arg key'' key' >= 0 then
            raise (Failure "Red_black_table.check_sort");
         check_sort_gt_lt arg key key'' left;
         check_sort_gt_lt arg key'' key' right

    | Leaf ->
         ()
   in

   let check_sort arg = function
      Black (key, _, left, right, _) ->
         check_sort_lt arg key left;
         check_sort_gt arg key right
    | Red _ ->
         raise (Failure "Red_black_table.check_sort: root is red")
    | Leaf ->
         ()
   in

   (*
    * Perform all the checks.
    *)
   let check ((arg, tree) as s) =
      let _ =
         check_red tree;
         check_black tree;
         check_sort arg tree;
         check_size tree
      in
         s
   in

   (*
    * Print the tree.
    *)
   let rec print_aux arg tree =
      print_space ();
      match tree with
         Black (key, data, left, right, size) ->
            print_string "(";
            open_hvbox 0;
            print_string "Black";
            print_space ();
            ord_print arg key data;
            print_string ":";
            print_int size;
            print_aux arg left;
            print_aux arg right;
            print_string ")";
            close_box ()

       | Red (key, data, left, right, size) ->
            print_string "(";
            open_hvbox 0;
            print_string "Red";
            print_space ();
            ord_print arg key data;
            print_string ":";
            print_int size;
            print_aux arg left;
            print_aux arg right;
            print_string ")";
            close_box ()

       | Leaf ->
            print_string "Leaf"
   in

   let print (arg, tree) =
      print_aux arg tree
   in

   let print_path_entry arg = function
      Left tree ->
         print_space ();
         print_string "Left";
         open_hvbox 0;
         print_aux arg tree;
         close_box ()
    | Right tree ->
         print_space ();
         print_string "Right";
         open_hvbox 0;
         print_aux arg tree;
         close_box ()

    | Delete tree ->
         print_space ();
         print_string "Delete";
         open_hvbox 1;
         print_aux arg tree;
         close_box ()
   in

   let print_path arg path =
      open_vbox 0;
      List.iter (print_path_entry arg) path;
      print_newline ()
   in

   (************************************************************************
    * INSERTION                                                            *
    ************************************************************************)

   (*
    * Insert an entry into the tree.
    *)
   let rec insert (arg : 'arg) (key : 'elt) (dataf : 'data list -> 'data list) = function
      Black (key0, data0, left0, right0, size0) ->
         begin
            let comp = ord_compare arg key key0 in
               if comp = 0 then
                  Black (key0, dataf data0, left0, right0, size0)

               else if comp < 0 then
                  match left0 with
                     Black _
                   | Leaf ->
                        (*
                         * Ok even if child becomes red.
                         *)
                        new_black key0 data0 (insert arg key dataf left0) right0

                   | Red (key1, data1, left1, right1, size1) ->
                        let comp = ord_compare arg key key1 in
                           if comp = 0 then
                              Black (key0, data0,
                                     Red (key1, dataf data1, left1, right1, size1),
                                     right0,
                                     size0)
                           else if comp < 0 then
                              match insert arg key dataf left1, right0 with
                                 Red _ as node, Red (key2, data2, left2, right2, size2) ->
                                    (*
                                     * Recoloring:
                                     *
                                     *     key0:b             key0:r
                                     *    /     \             /    \
                                     *  key1:r key2:r      key1:b  key2:b
                                     *   /    \             /   \
                                     * key2:r right1     key2:r right1
                                     *)
                                    new_red key0 data0 (**)
                                       (new_black key1 data1 node right1)
                                       (Black (key2, data2, left2, right2, size2))

                               | Red _ as node, _ ->
                                    (*
                                     * Rotation:
                                     *
                                     *      key0:b             key1:b
                                     *     /     \             /    \
                                     *   key1:r key2:b      key3:r  key0:b
                                     *   /    \                    /    \
                                     * key3:r right1             right1 key2:r
                                     *)
                                    new_black key1 data1 node (new_red key0 data0 right1 right0)

                               | node, _ ->
                                    (*
                                     * Inline:
                                     *
                                     *        key0:b         key0:b
                                     *        /    \         /    \
                                     *     key1:r key2    key1:r  key2
                                     *     /   \          /    \
                                     *  key3:b right1  key3:b right1
                                     *)
                                    new_black key0 data0 (**)
                                       (new_red key1 data1 node right1)
                                       right0

                           else
                              match insert arg key dataf right1, right0 with
                                 Red _ as node, Red (key2, data2, left2, right2, size2) ->
                                    (*
                                     * Recoloring:
                                     *
                                     *       key0:b              key0:r
                                     *       /    \              /   \
                                     *    key1:r key2:r       key1:b key2:b
                                     *    /   \               /   \
                                     *  left1 node:r        left1 node:r
                                     *)
                                    new_red key0 data0 (**)
                                       (new_black key1 data1 left1 node)
                                       (Black (key2, data2, left2, right2, size2))

                               | Red (key3, data3, left3, right3, size3), _ ->
                                    (*
                                     * Rotation:
                                     *
                                     *       key0:b              key3:b
                                     *       /    \             /      \
                                     *    key1:r  right0     key1:r    key0:r
                                     *    /   \              /    \    /    \
                                     *  left1 key3:r     left1 left3 right3 right0
                                     *        /   \
                                     *      left3 right3
                                     *)
                                    new_black key3 data3 (**)
                                       (new_red key1 data1 left1 left3)
                                       (new_red key0 data0 right3 right0)

                               | node3, _ ->
                                    (*
                                     * Inline:
                                     *
                                     *      key0:b
                                     *      /    \
                                     *   key1:r  right0
                                     *   /     \
                                     * left1  node3:b
                                     *)
                                    new_black key0 data0 (**)
                                       (new_red key1 data1 left1 node3)
                                       right0

               else
                  (* comp > 0 *)
                  match right0 with
                     Black _
                   | Leaf ->
                        (*
                         * Node can be replaced even if it becomes red.
                         *)
                        new_black key0 data0 left0 (insert arg key dataf right0)

                   | Red (key2, data2, left2, right2, size2) ->
                        let comp = ord_compare arg key key2 in
                           if comp = 0 then
                              Black (key0, data0, left0,
                                     Red (key2, dataf data2, left2, right2, size2),
                                     size0)

                           else if comp < 0 then
                              match left0, insert arg key dataf left2 with
                                 Red (key1, data1, left1, right1, size1), (Red _ as node) ->
                                    (*
                                     * Recoloring:
                                     *
                                     *       key0:b              key0:r
                                     *       /    \              /   \
                                     *    key1:r key2:r       key1:b key2:b
                                     *           /   \               /   \
                                     *        node:r right2       node:r right2
                                     *)
                                    new_red key0 data0 (**)
                                       (Black (key1, data1, left1, right1, size1))
                                       (new_black key2 data2 node right2)

                               | _, Red (key3, data3, left3, right3, size3) ->
                                    (*
                                     * Rotate:
                                     *
                                     *       key0:b                  key3:b
                                     *       /    \                  /     \
                                     *    key1:b  key2:r          key0:r   key2:r
                                     *            /    \          /   \    /     \
                                     *         key3:r  right2 left0 left3 right3 right2
                                     *         /   \
                                     *      left3 right3
                                     *)
                                    new_black key3 data3 (**)
                                       (new_red key0 data0 left0 left3)
                                       (new_red key2 data2 right3 right2)

                               | _, node3 ->
                                    (*
                                     * Inline:
                                     *
                                     *      key0:b
                                     *      /    \
                                     *   left0  key2:r
                                     *          /   \
                                     *      key3:b right2
                                     *)
                                    new_black key0 data0 (**)
                                       left0
                                       (new_red key2 data2 node3 right2)

                           else
                              match left0, insert arg key dataf right2 with
                                 Red (key1, data1, left1, right1, size1), (Red _ as node) ->
                                    (*
                                     * Recoloring:
                                     *
                                     *     key0:b                  key0:r
                                     *     /    \                  /   \
                                     *   key1:r key2:r          key1:b key2:b
                                     *          /    \                 /    \
                                     *        left2  node:r          left2 node:r
                                     *)
                                    new_red key0 data0 (**)
                                       (Black (key1, data1, left1, right1, size1))
                                       (new_black key2 data2 left2 node)

                               | _, (Red _ as node) ->
                                    (*
                                     * Rotation:
                                     *
                                     *      key0:b                 key2:b
                                     *      /    \                 /    \
                                     *   left0:b key2:r         key0:r node:r
                                     *           /   \          /   \
                                     *         left2 node:r left0:b left2
                                     *)
                                    new_black key2 data2 (**)
                                       (new_red key0 data0 left0 left2)
                                       node

                               | _, node3 ->
                                    (*
                                     * Inline:
                                     *
                                     *     key0:b
                                     *     /   \
                                     * left0:b key2:r
                                     *         /    \
                                     *       left2 node3:b
                                     *)
                                    new_black key0 data0 (**)
                                       left0
                                       (new_red key2 data2 left2 node3)
         end
    | Leaf ->
         (* Leaf is colored red *)
         Red (key, dataf [], Leaf, Leaf, 1)

    | (Red _) as tree ->
         (* Red nodes will not come up *)
         raise (Invalid_argument "Red_black_table.insert")
   in

   (*
    * Add an element to the set.
    *)
   let add_aux (arg : 'arg) (key : 'elt) (dataf : 'data list -> 'data list) (tree : ('elt, 'data) tree) =
      let tree =
         match tree with
            Leaf ->
               Black (key, dataf [], Leaf, Leaf, 1)
          | node ->
               match insert arg key dataf node with
                  Red (key, data, left, right, size) ->
                     Black (key, data, left, right, size)
                | tree ->
                     tree
      in
         (tree : ('elt, 'data) tree)
   in

   let add ((arg, tree) : ('arg, 'elt, 'data) t) (key : 'elt) (data : 'data) =
      let append tl =
         data :: tl
      in
         arg, add_aux arg key append tree
   in

   let add_list_aux (arg : 'arg) (key : 'elt) (data : 'data list) (tree : ('elt, 'data) tree) =
      let append = function
         [] ->
            data
       | tl ->
            ord_append data tl
      in
         add_aux arg key append tree
   in

   let add_list (key : 'elt) (data : 'data list) ((arg, tree) : ('arg, 'elt, 'data) t) =
      add_list_aux arg key data tree
   in

   (************************************************************************
    * FIND ENTRIES                                                         *
    ************************************************************************)

   (*
    * Return the data for the entry.
    *)
   let rec find_aux arg key = function
      Black (key0, data0, left0, right0, _) ->
         let comp = ord_compare arg key key0 in
            if comp = 0 then
               data0
            else if comp < 0 then
               find_aux arg key left0
            else
               find_aux arg key right0
    | Red (key0, data0, left0, right0, _) ->
         let comp = ord_compare arg key key0 in
            if comp = 0 then
               data0
            else if comp < 0 then
               find_aux arg key left0
            else
               find_aux arg key right0
    | Leaf ->
         raise Not_found
   in

   let find_all (arg, tree) key =
      find_aux arg key tree
   in

   let find s key =
      match find_all s key with
         data :: _ ->
            data
       | [] ->
            raise Not_found
   in

   (************************************************************************
    * REMOVAL                                                              *
    ************************************************************************)

   (*
    * Construct a path during the removal.
    *)
   let rec delete arg key filter path node =
      match node with
         Black (key0, data0, left0, right0, size0) ->
            let comp = ord_compare arg key key0 in
               if comp = 0 then
                  match filter data0 with
                     [] ->
                        begin
                           match left0, right0 with
                              Leaf, Leaf ->
                                 lift_black key0 path Leaf
                            | Red (key1, data1, left1, right1, size1), Leaf ->
                                 lift key path (Black (key1, data1, left1, right1, size1))
                            | _ ->
                                 delete_min (Delete node :: path) right0
                        end
                   | data0 ->
                        restore path (Black (key0, data0, left0, right0, size0))
               else if comp < 0 then
                  delete arg key filter (Left node :: path) left0
               else
                  delete arg key filter (Right node :: path) right0
       | Red (key0, data0, left0, right0, size0) ->
            let comp = ord_compare arg key key0 in
               if comp = 0 then
                  match filter data0 with
                     [] ->
                        begin
                           match right0 with
                              Leaf ->
                                 lift key path Leaf
                            | _ ->
                                 delete_min (Delete node :: path) right0
                        end
                   | data0 ->
                        restore path (Red (key0, data0, left0, right0, size0))
               else if comp < 0 then
                  delete arg key filter (Left node :: path) left0
               else
                  delete arg key filter (Right node :: path) right0
       | Leaf ->
            raise Not_found

   and restore path node =
      match path with
         Left (Black (key0, data0, _, right0, size0)) :: path ->
            restore path (Black (key0, data0, node, right0, size0))
       | Left (Red (key0, data0, _, right0, size0)) :: path ->
            restore path (Red (key0, data0, node, right0, size0))
       | Right (Black (key0, data0, left0, _, size0)) :: path ->
            restore path (Black (key0, data0, left0, node, size0))
       | Right (Red (key0, data0, left0, _, size0)) :: path ->
            restore path (Red (key0, data0, left0, node, size0))
       | [] ->
            node
       | Left Leaf :: _
       | Right Leaf :: _
       | Delete _ :: _ ->
            raise (Invalid_argument "restore")

   and delete_min path node =
      match node with
         Black (key0, _, Leaf, Leaf, _) ->
            lift_black key0 path Leaf
       | Black (key0, _, Leaf, Red (key2, data2, left2, right2, size2), _) ->
            lift key0 path (Black (key2, data2, left2, right2, size2))
       | Red (key0, _, Leaf, Leaf, _) ->
            lift key0 path Leaf
       | Black (_, _, left0, _, _) ->
            delete_min (Left node :: path) left0
       | Red (_, _, left0, _, _) ->
            delete_min (Left node :: path) left0
       | Leaf ->
            raise Not_found

   (*
    * Copy the tree with no need to propagate black.
    *)
   and lift key path node =
      match path, node with
         Left (Black (key0, data0, _, right0, size0)) :: path, left ->
            lift key path (Black (key0, data0, left, right0, pred size0))
       | Left (Red (key0, data0, _, right0, size0)) :: path, left ->
            lift key path (Red (key0, data0, left, right0, pred size0))
       | Right (Black (key0, data0, left0, _, size0)) :: path, right ->
            lift key path (Black (key0, data0, left0, right, pred size0))
       | Right (Red (key0, data0, left0, _, size0)) :: path, right ->
            lift key path (Red (key0, data0, left0, right, pred size0))
       | Delete (Black (key0, data0, left0, _, size0)) :: path, right ->
            lift key path (Black (key, data0, left0, right, pred size0))
       | Delete (Red (key0, data0, left0, _, size0)) :: path, right ->
            lift key path (Red (key, data0, left0, right, pred size0))
       | [], node ->
            node
       | Left Leaf :: _, _
       | Right Leaf :: _, _
       | Delete Leaf :: _, _ ->
            raise (Invalid_argument "lift")

   (*
    * Propagate the extra black up the tree.
    *)
   and lift_black key path node =
      match path, node with
         Left (Black (key0, data0, _, right0, size0)) :: path, left ->
            begin
               match right0 with
                  Black (key2, data2, left2, right2, size2) ->
                     begin
                        match left2, right2 with
                           _, Red (key3, data3, left3, right3, size3) ->
                              (*
                               *    key0:b                 key2:b
                               *   /     \                 /    \
                               * left:bb key2:b          key0:b right2:b
                               *         /   \           /    \
                               *      left2  right2:r  left:b left2
                               *)
                              lift key path (**)
                                 (Black (key2,
                                         data2,
                                         new_black key0 data0 left left2,
                                         Black (key3, data3, left3, right3, size3),
                                         pred size0))

                         | Red (key3, data3, left3, right3, size3), _ ->
                              (*
                               *      key0:b                    key3:b
                               *      /    \                  /       \
                               *   left:bb key2:b          key0:b     key2:b
                               *           /    \          /    \     /     \
                               *        key3:r right2:b left:b left3 right3 right2:b
                               *        /   \
                               *     left3 right3
                               *)
                              lift key path (**)
                                 (Black (key3,
                                         data3,
                                         new_black key0 data0 left left3,
                                         new_black key2 data2 right3 right2,
                                         pred size0))

                         | _ ->
                              (*
                               *     key0:b                 key0:bb
                               *     /    \                 /    \
                               * left:bb  key2:b       left:b    key2:r
                               *          /    \                 /    \
                               *       left2:b right2:b       left2:b right2:b
                               *)
                              lift_black key path (**)
                                 (Black (key0,
                                         data0,
                                         left,
                                         Red (key2, data2, left2, right2, size2),
                                         pred size0))
                     end

                | Red (key2, data2, left2, right2, size2) ->
                     begin
                        match left2 with
                           Black (key3, data3, Red (key4, data4, left4, right4, _), d, _) ->
                              (*
                               *     key0:b                   key2:b
                               *     /    \                   /    \
                               *  left:bb key2:r           key4:b right2:b
                               *          /    \           /     \
                               *        key3:b right2:b  key0:r   key3:r
                               *        /   \           /   \    /     \
                               *     key4:r  d     left:b left4 right4 d
                               *     /   \
                               *   left4 right4
                               *)
                              lift key path (**)
                                 (Black (key2,
                                         data2,
                                         new_black key4 data4 (**)
                                            (new_red key0 data0 left left4)
                                            (new_red key3 data3 right4 d),
                                         right2,
                                         pred size0))

                         | Black (key3, data3, c, Red (key4, data4, left4, right4, size4), _) ->
                              (*
                               *     key0:b                   key2:b
                               *     /    \                   /    \
                               * left:bb  key2:r            key3:r right2
                               *          /    \            /    \
                               *       key3:b  right2     key0:b key4:b
                               *       /    \             /    \
                               *      c    key4:r       left:b  c
                               *)
                              lift key path (**)
                                 (Black (key2,
                                         data2,
                                         new_red key3 data3 (**)
                                            (new_black key0 data0 left c)
                                            (Black (key4, data4, left4, right4, size4)),
                                            right2,
                                            pred size0))

                         | Black (key3, data3, c, d, _) ->
                              (*
                               *     key0:b               key2:b
                               *     /    \               /     \
                               * left:bb key2:r         key0:b  right2:b
                               *         /   \          /     \
                               *      key3:b right2:b left:b  key3:r
                               *      /   \                   /   \
                               *     c:b  d:b                c:b   d:b
                               *)
                              lift key path (**)
                                 (Black (key2,
                                         data2,
                                         new_black key0 data0 left (new_red key3 data3 c d),
                                         right2,
                                         pred size0))

                         | Red _
                         | Leaf ->
                              raise (Invalid_argument "lift_black1")
                     end

                | Leaf ->
                     raise (Invalid_argument "lift_black2")
            end

       | Right (Black (key0, data0, left0, _, size0)) :: path, right ->
            begin
               match left0 with
                  Black (key1, data1, left1, right1, size1) ->
                     begin
                        match left1, right1 with
                           Red (key3, data3, left3, right3, size3), _ ->
                              (*
                               *        key0:b              key1:b
                               *        /    \              /    \
                               *      key1:b right:bb   left1:b key0:b
                               *      /    \                    /    \
                               *  left1:r right1            right1   right:b
                               *)
                              lift key path (**)
                                 (Black (key1,
                                         data1,
                                         Black (key3, data3, left3, right3, size3),
                                         new_black key0 data0 right1 right,
                                         pred size0))

                         | _, Red (key3, data3, left3, right3, size3) ->
                              (*
                               *      key0:b                    key3:b
                               *      /     \                 /        \
                               *    key1:b  right:bb        key1:b     key0:b
                               *    /    \                 /    \      /    \
                               * left1:b key3:r         left1:b left3 right3 right
                               *         /    \
                               *       left3 right3
                               *)
                              lift key path (**)
                                 (Black (key3,
                                         data3,
                                         new_black key1 data1 left1 left3,
                                         new_black key0 data0 right3 right,
                                         pred size0))

                         | _ ->
                              (*
                               *        key0:b                 key0:bb
                               *        /    \                 /    \
                               *    key1:b  right:bb      key1:r    right:bb
                               *    /    \                /    \
                               * left1:b right1:b     left1:b right1:b
                               *)
                              lift_black key path (**)
                                 (Black (key0,
                                         data0,
                                         Red (key1, data1, left1, right1, size1),
                                         right,
                                         pred size0))

                     end

                | Red (key1, data1, left1, right1, size1) ->
                     begin
                        match right1 with
                           Black (key3, data3, d, Red (key4, data4, left4, right4, _), _) ->
                              (*
                               *        key0:b                key1:b
                               *        /     \               /    \
                               *    key1:r   right:bb    left1:b  key4:b
                               *    /    \                        /    \
                               * left1:b key3:b              key3:r    key0:r
                               *         /   \               /   \     /    \
                               *        d    key4:r         d  left4 right4 right:b
                               *             /   \
                               *          left4 right4
                               *)
                              lift key path (**)
                                 (Black (key1,
                                         data1,
                                         left1,
                                         new_black key4 data4 (**)
                                            (new_red key3 data3 d left4)
                                            (new_red key0 data0 right4 right),
                                            pred size0))

                         | Black (key3, data3, Red (key4, data4, left4, right4, size4), c, _) ->
                              (*
                               *     key0:b                 key1:b
                               *     /    \                 /    \
                               *  key1:r  right:bb       left1  key3:r
                               *  /    \                        /    \
                               * left1 key3:b                 key4:b key0:b
                               *       /   \                         /   \
                               *    key4:r c                        c   right:b
                               *)
                              lift key path (**)
                                 (Black (key1,
                                         data1,
                                         left1,
                                         new_red key3 data3 (**)
                                            (Black (key4, data4, left4, right4, size4))
                                            (new_black key0 data0 c right),
                                            pred size0))

                         | Black (key3, data3, c, d, size3) ->
                              (*
                               *      key0:b               key1:b
                               *      /    \               /    \
                               *   key1:r  right:bb     left1  key0:b
                               *   /   \                       /    \
                               * left1 key3:b               key3:r right:b
                               *       /   \                /    \
                               *      c:b  d:b            c:b    d:b
                               *)
                              lift key path (**)
                                 (Black (key1,
                                         data1,
                                         left1,
                                         new_black key0 data0 (Red (key3, data3, c, d, size3)) right,
                                         pred size0))


                         | Red _
                         | Leaf ->
                              raise (Invalid_argument "lift_black3")
                     end

                | Leaf ->
                     raise (Invalid_argument "lift_black4")
            end

       | Left (Red (key0, data0, _, right0, size0)) :: path, left ->
            begin
               match right0 with
                  Black (key2, data2, left2, right2, size2) ->
                     begin
                        match left2, right2 with
                           _, Red (key3, data3, left3, right3, size3) ->
                              (*
                               *     key0:r                   key2:r
                               *     /    \                   /    \
                               *  left:bb key2:b           key0:b right2:b
                               *          /    \           /    \
                               *       left2:b right2:r left:b left2:b
                               *)
                              lift key path (**)
                                 (Red (key2,
                                       data2,
                                       new_black key0 data0 left left2,
                                       Black (key3, data3, left3, right3, size3),
                                       pred size0))

                         | Red (key3, data3, left3, right3, size3), _ ->
                              (*
                               *     key0:r                   key3:b
                               *     /    \                  /       \
                               * left:bb  key2:b          key0:r     key2:r
                               *          /    \         /   \       /    \
                               *        key3:r right2 left:b left3 right3 right2
                               *        /   \
                               *     left3 right3
                               *)
                              lift key path (**)
                                 (Black (key3,
                                         data3,
                                         new_red key0 data0 left left3,
                                         new_red key2 data2 right3 right2,
                                         pred size0))

                         | _ ->
                              (*
                               *     key0:r                  key0:b
                               *    /     \                  /    \
                               * left:bb  key2:b          left:b key2:r
                               *          /    \                 /   \
                               *     left2:b  right2:b      left2:b right2:b
                               *)
                              lift key path (**)
                                 (Black (key0,
                                         data0,
                                         left,
                                         Red (key2, data2, left2, right2, size2),
                                         pred size0))
                     end
                | Red _
                | Leaf ->
                     raise (Invalid_argument "lift_black5")
            end

       | Right (Red (key0, data0, left0, _, size0)) :: path, right ->
            begin
               match left0 with
                  Black (key1, data1, left1, right1, size1) ->
                     begin
                        match left1, right1 with
                           Red (key3, data3, left3, right3, size3), _ ->
                              (*
                               *       key0:r                key1:r
                               *       /    \                /    \
                               *    key1:b  right:bb      left1:b key0:b
                               *   /     \                        /    \
                               * left1:r right1                right1 right:b
                               *)
                              lift key path (**)
                                 (Red (key1,
                                       data1,
                                       Black (key3, data3, left3, right3, size3),
                                       new_black key0 data0 right1 right,
                                       pred size0))

                         | _, Red (key3, data3, left3, right3, size3) ->
                              (*
                               *       key0:r                 key3:b
                               *       /    \                /       \
                               *     key1:b right:bb      key1:r    key0:r
                               *     /    \               /    \    /    \
                               *  left1  key3:r        left1 left3 right3 right:b
                               *         /    \
                               *       left3 right3
                               *)
                              lift key path (**)
                                 (Black (key3,
                                         data3,
                                         new_red key1 data1 left1 left3,
                                         new_red key0 data0 right3 right,
                                         pred size0))

                         | _ ->
                              (*
                               *        key0:r              key0:b
                               *        /    \              /    \
                               *     key1:b right:bb     key1:r right:b
                               *     /   \               /    \
                               * left1:b right1:b     left1:b right1:b
                               *)
                              lift key path (**)
                                 (Black (key0,
                                         data0,
                                         Red (key1, data1, left1, right1, size1),
                                         right,
                                         pred size0))

                     end
                | Red _
                | Leaf ->
                     raise (Invalid_argument "lift_black6")
            end

       | Delete (Black (_, data0, left0, right0, size0)) :: path, node ->
            lift_black key (Right (Black (key, data0, left0, right0, size0)) :: path) node

       | Delete (Red (_, data0, left0, right0, size0)) :: path, node ->
            lift_black key (Right (Red (key, data0, left0, right0, size0)) :: path) node

       | [], node ->
            node

       | Left Leaf :: _, _
       | Right Leaf :: _, _
       | Delete Leaf :: _, _ ->
            raise (Invalid_argument "lift_black7")
   in

   (*
    * Remove the item.
    *)
   let remove ((arg, tree) as s) key =
      try arg, delete arg key (fun _ -> []) [] tree with
         Not_found ->
            s
   in

   let filter (arg, tree) key filter =
      arg, delete arg key filter [] tree
   in

   (************************************************************************
    * UNION & INTERSECTION                                                 *
    ************************************************************************)

   (*
    * Get the elements of the list.
    *)
   let rec to_list_aux elements = function
      Black (key, data, left, right, _) ->
         to_list_aux ((key, data) :: to_list_aux elements right) left
    | Red (key, data, left, right, _) ->
         to_list_aux ((key, data) :: to_list_aux elements right) left
    | Leaf ->
         elements
   in

   let to_list (arg, tree) =
      to_list_aux [] tree
   in

   let elements = to_list
   in

   let rec reverse elements = function
      h :: t ->
         reverse (h :: elements) t
    | [] ->
         elements
   in

   let rec merge arg elements elements1 elements2 =
      match elements1, elements2 with
         ((key1, data1) as hd1) :: tl1, ((key2, data2) as hd2) :: tl2 ->
            let comp = ord_compare arg key1 key2 in
               if comp = 0 then
                  merge arg ((key1, ord_append data1 data2) :: elements) tl1 tl2
               else if comp < 0 then
                  merge arg (hd1 :: elements) tl1 elements2
               else
                  merge arg (hd2 :: elements) elements1 tl2
       | _, [] ->
            reverse elements1 elements
       | [], _ ->
            reverse elements2 elements
   in

   (*
    * Log of a number.
    *)
   let rec log2 i x =
      if 1 lsl i >= x then
         i
      else
         log2 (succ i) x
   in

   (*
    * Build a set from a list.
    *)
   let rec log2 i j =
      if 1 lsl i >= j then
         i
      else
         log2 (succ i) j
   in

   let rec of_array depth max_depth elements off len =
      if len = 1 then
         let key, data = elements.(off) in
            if depth = max_depth then
               Red (key, data, Leaf, Leaf, 1)
            else
               Black (key, data, Leaf, Leaf, 1)
      else if len = 2 then
         let key1, data1 = elements.(off) in
         let key0, data0 = elements.(succ off) in
            Black (key0, data0, Red (key1, data1, Leaf, Leaf, 1), Leaf, 2)
      else
         let len2 = len lsr 1 in
         let key0, data0 = elements.(off + len2) in
            Black (key0, data0,
                   of_array (succ depth) max_depth elements off len2,
                   of_array (succ depth) max_depth elements (off + len2 + 1) (len - len2 - 1),
                   len)
   in

   let of_list arg elements =
      let tree =
         match elements with
            [] ->
               Leaf
          | [key, data] ->
               Black (key, data, Leaf, Leaf, 1)
          | elements ->
               let elements = Array.of_list elements in
               let length = Array.length elements in
               let max_depth = pred (log2 1 (succ length)) in
                  of_array 0 max_depth elements 0 length
      in
         arg, tree
   in

   (*
    * Union flattens the two trees,
    * merges them, then creates a new tree.
    *)
   let rec union_aux (arg : 'arg) (s1 : ('elt, 'data) tree) (s2 : ('elt, 'data) tree) =
      match s2 with
         Black (key, data, left, right, _) ->
            union_aux arg (add_list_aux arg key data (union_aux arg s1 left)) right
       | Red (key, data, left, right, _) ->
            union_aux arg (add_list_aux arg key data (union_aux arg s1 left)) right
       | Leaf ->
            s1
   in

   let union (arg1, s1) (arg2, s2) =
      let size1 = cardinality s1 in
      let size2 = cardinality s2 in
      let arg = ord_union arg1 arg2 in
      let s =
         if size1 < size2 then
            union_aux arg s2 s1
         else
            union_aux arg s1 s2
      in
         arg, s
   in

   (*
    * Build a path into a tree.
    *)
   let rec initial_path path node =
      match node with
         Black (_, _, Leaf, _, _) ->
            Left node :: path
       | Red (_, _, Leaf, _, _) ->
            Left node :: path
       | Black (_, _, left, _, _) ->
            initial_path (Left node :: path) left
       | Red (_, _, left, _, _) ->
            initial_path (Left node :: path) left
       | Leaf ->
            raise (Invalid_argument "initial_path")
   in

   let key_of_path = function
      Left (Black (key, _, _, _, _)) :: _ ->
         key
    | Left (Red (key, _, _, _, _)) :: _ ->
         key
    | Right (Black (key, _, _, _, _)) :: _ ->
         key
    | Right (Red (key, _, _, _, _)) :: _ ->
         key
    | _ ->
         raise (Invalid_argument "key_of_path")
   in

   let rec next_path = function
      Left (Black (_, _, _, Leaf, _)) :: path ->
         next_path path
    | Left (Red (_, _, _, Leaf, _)) :: path ->
         next_path path
    | Left (Black (_, _, _, right, _)) :: path ->
         initial_path path right
    | Left (Red (_, _, _, right, _)) :: path ->
         initial_path path right
    | Right  _ :: path ->
         next_path path
    | [] ->
         raise Not_found
    | _ ->
         raise (Invalid_argument "next_path")
   in

   (*
    * See if two sets intersect.
    *)
   let rec intersect_aux arg path1 path2 =
      let key1 = key_of_path path1 in
      let key2 = key_of_path path2 in
      let comp = ord_compare arg key1 key2 in
         if comp = 0 then
            true
         else if comp < 0 then
            intersect_aux arg (next_path path1) path2
         else
            intersect_aux arg path1 (next_path path2)
   in

   let intersectp (arg, s1) (_, s2) =
      match s1, s2 with
         Leaf, _
       | _, Leaf ->
            false
       | _ ->
            let path1 = initial_path [] s1 in
            let path2 = initial_path [] s2 in
               try intersect_aux arg path1 path2 with
                  Not_found ->
                     false
   in

   (************************************************************************
    * IMPLEMENTATION                                                       *
    ************************************************************************)

   (*
    * Search without reorganizing the tree.
    *)
   let rec mem_aux arg tree key =
      match tree with
         Black (key', _, left, right, _) ->
            let comp = ord_compare arg key key' in
               if comp = 0 then
                  true
               else if comp < 0 then
                  mem_aux arg left key
               else
                  mem_aux arg right key

       | Red (key', _, left, right, _) ->
            let comp = ord_compare arg key key' in
               if comp = 0 then
                  true
               else if comp < 0 then
                  mem_aux arg left key
               else
                  mem_aux arg right key

       | Leaf ->
            false
   in

   let mem (arg, tree) key =
      mem_aux arg tree key
   in

   (*
    * An empty tree is just a leaf.
    *)
   let create arg = (arg, Leaf)
   in

   let is_empty = function
      _, Leaf ->
         true
    | _ ->
         false
   in

   let make arg key data =
      arg, Black (key, data, Leaf, Leaf, 1)
   in

   (*
    * Iterate a function over the hashtable.
    *)
   let rec iter_aux f = function
      Black (key, data, left, right, _) ->
         iter_aux f left;
         List.iter (f key) data;
         iter_aux f right
    | Red (key, data, left, right, _) ->
         iter_aux f left;
         List.iter (f key) data;
         iter_aux f right
    | Leaf ->
         ()
   in

   let iter f (_, tree) =
      iter_aux f tree
   in

   let rec map_aux f = function
      Black (key, data, left, right, size) ->
         let left = map_aux f left in
         let data = List.map (f key) data in
         let right = map_aux f right in
            Black (key, data, left, right, size)
      | Red (key, data, left, right, size) ->
         let left = map_aux f left in
         let data = List.map (f key) data in
         let right = map_aux f right in
            Red (key, data, left, right, size)
      | Leaf ->
           Leaf
   in

   let map f (arg, tree) =
      arg, map_aux f tree
   in

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
   in

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
   in

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
   in
      { create = create;
        is_empty = is_empty;
        mem = mem;
        add = add;
        find = find;
        find_all = find_all;
        make = make;
        remove = remove;
        union = union;
        elements = elements;
        iter = iter;
        map = map;
        cardinal = cardinal;
        mem_filt = mem_filt;
        not_mem_filt = not_mem_filt;
        intersectp = intersectp;
        of_list = of_list;
        print = print
      }

module Create =
struct
   type ('arg, 'elt, 'data) t = ('arg, 'elt, 'data) table

   let create = create
end

(*
 * Module version.
 *)
module MakeTable (Base : TableBaseSig) =
   Table_util.MakeTable (Create) (Base)

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
