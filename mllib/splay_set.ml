(*
 * A splay set is an implementation of
 * a set over an ordered type.
 *)
module type S =
sig
   type elt
   type t

   val empty : t
   val is_empty : t -> bool
   val mem : t -> elt -> bool
   val add : elt -> t -> t
   val make : elt -> t
   val remove : elt -> t -> t
   val union : t -> t -> t
   val elements : t -> elt list
   val iter : (elt -> unit) -> t -> unit
   val cardinal : t -> int
   val mem_filt : t -> elt list -> elt list
   val fst_mem_filt : t -> (elt * 'a) list -> (elt * 'a) list
   val intersectp : t -> t -> bool
end

(*
 * Build the set from an ordered type.
 *)
module Make (Ord: Set.OrderedType) =
struct
   type elt = Ord.t

   type tree =
      Leaf
    | Node of node * int
   and node = (elt * t * t) ref
   and t = tree 

   type direction = Left of node | Right of node

   let cardinal = function
      Leaf -> 0
    | Node (_,i) -> i 

   let new_node k l r =
      Node(ref(k,l,r),succ(cardinal(l)+cardinal(r)))

   let right_node k r =
      Node(ref(k,Leaf,r), succ(cardinal(r)))

   let left_node k l =
      Node(ref(k,l,Leaf), succ(cardinal(l)))

   let rotate_left = function
      { contents = key,Node({ contents = left_key,left_left,left_right },_),right } as node ->
         node := (left_key,left_left,new_node key left_right right)
    | _ -> raise (Invalid_argument "rotate_left")

   let rotate_right = function
      { contents = key,left,Node({ contents = right_key,right_left,right_right },_) } as node ->
         node := (right_key,new_node key left right_left,right_right)
    | _ -> raise (Invalid_argument "rotate_right")

   let rec lift = function
      [] -> ()
    | [Left parent] ->
         rotate_left parent
    | [Right parent] ->
         rotate_right parent
    | Left parent :: Left grandparent :: ancestors ->
         (
            rotate_left grandparent;
            rotate_left grandparent;  (* parent has moved into grandparent's position *)
            lift ancestors
         )
    | Right parent :: Right grandparent :: ancestors ->
         (
            rotate_right grandparent;
            rotate_right grandparent;  (* parent has moved into grandparent's position *)
            lift ancestors
         )
    | Left parent :: Right grandparent :: ancestors ->
         (
            rotate_left parent;
            rotate_right grandparent;
            lift ancestors
         )
    | Right parent :: Left grandparent :: ancestors ->
         (
            rotate_right parent;
            rotate_left grandparent;
            lift ancestors
         )

   let rec lift_right = function
      [] -> ()
    | [parent] ->
         rotate_right parent
    | parent :: grandparent :: ancestors ->
         (
            rotate_right grandparent;
            rotate_right grandparent;  (* parent has moved into grandparent's position *)
            lift_right ancestors
         )

   let rec splay key0 path = function
      Node ({ contents = (key, left, right) } as node, _) ->
         let comp = Ord.compare key0 key
         in
            if comp = 0 then
               (
                  lift path;
                  true
               )
            else if comp < 0 then
            (* left *)
               splay key0 (Left node :: path) left
            else
            (* right *)
               splay key0 (Right node :: path) right
    | Leaf ->
         (match path with
             [] -> false
           | _ :: path' ->
                (
                   lift path';
                   false
                ))

   let rec splay_right path = function
      Node ({ contents = (_, _, right) } as node,_) ->
         splay_right (node :: path) right
    | Leaf ->
         (match path with
             [] -> ()
           | _ :: path' ->
                lift_right path')

   let empty = Leaf
   let is_empty = function
      Leaf -> true
    | _ -> false

   let mem t key =
      splay key [] t

   let add key t =
      if splay key [] t then
         t
      else
         match t with
            Node ({contents = (key', left, right) }, _ ) ->
               let sl = cardinal left in
               let sr = cardinal right in
               if Ord.compare key key' < 0 then
               (* left *)
                  Node (ref (key, left, Node (ref (key', empty, right), succ sr)),2+sl+sr)
               else
                  Node (ref (key, Node (ref (key', left, empty), succ sl), right),2+sl+sr)
          | Leaf -> Node (ref (key, empty, empty),1)

   let make key = Node (ref (key, empty, empty),1)

   let remove key t =
      if splay key [] t then
         match t with
            Node ({ contents = (_, Leaf, right)}, _) -> right
          | Node ({ contents = (_, left, Leaf)}, _) -> left
          | Node ({ contents = (_, left, right)}, _) ->
               (splay_right [] left;
                match left with
                   Node ({contents = (left_key, left_left, Leaf) },_) ->
                      new_node left_key left_left right
                 | _ -> failwith "remove")
          | _ -> failwith "remove"
      else
         t

   let rec union s1 s2 =
      match (s1,s2) with
         (Leaf, _) -> s2
       | (_, Leaf) -> s1
       | (Node (n1, sz1), Node (n2, sz2)) ->
            if (sz1>=sz2) then
               if sz2=1 then
                  let (x2,_,_)=(!n2) in add x2 s1
               else
                  let (x,l,r)=(!n1) in
                     if (splay x [] s2)
                     then
                        let (_,l2,r2) = (!n2) in
                        let ll = union l l2 in
                        let rr = union r r2 in
                           new_node x ll rr
                     else
                        let (x2,l2,r2) = (!n2) in
                           if Ord.compare x x2 < 0 then
                              let ll = union l l2 in
                              let rr = union r (right_node x2 r2) in
                                 new_node x ll rr
                           else
                              let ll = union l (left_node x2 l2) in
                              let rr = union r r2 in
                                 new_node x ll rr
            else
            if sz1=1 then
               let (x1,_,_)=(!n1) in add x1 s2
            else
               let (x,l,r)=(!n2) in
                  if (splay x [] s1)
                  then
                     let (_,l1,r1) = (!n1) in
                     let ll = union l l1 in
                     let rr = union r r1 in
                        new_node x ll rr
                  else
                     let (x1,l1,r1) = (!n1) in
                        if Ord.compare x x1 < 0 then
                           let ll = union l l1 in
                           let rr = union r (right_node x1 r1) in
                              new_node x ll rr
                        else
                           let ll = union l (left_node x1 l1) in
                           let rr = union r r1 in
                              new_node x ll rr

   let rec elements_aux coll = function
      Leaf -> coll
    | Node ({contents=(x,l,r)},_) ->
         x::(elements_aux (elements_aux coll l) r)

   let elements = elements_aux []

   let rec iter f = function
      Leaf -> ()
    | Node ({contents = (x,l,r)},_) ->
         iter f l; f x; iter f r

   let rec mem_filt s = function
      [] -> []
    | (h::t) as l ->
         if mem s h
         then let rem = mem_filt s t in
                 if rem == t then l else h::rem
         else mem_filt s t

   let rec fst_mem_filt s = function
      [] -> []
    | (((v,_) as h)::t) as l ->
         if mem s v
         then let rem = fst_mem_filt s t in
                 if rem == t then l else h::rem
         else fst_mem_filt s t

   (*
    * Test if two sets intersect at all.
    *)
   let rec intersectp s1 s2 =
      match s1, s2 with
         (Leaf, _) -> false
       | (_, Leaf) ->
            false

       | (Node (n1, sz1), Node (n2, sz2)) ->
            if (sz1>=sz2) then
               if sz2=1 then
                  let (x2,_,_)=(!n2) in mem s1 x2
               else
                  let (x,l,r)=(!n1) in
                     if (splay x [] s2)
                     then
                        true
                     else
                        let (x2,l2,r2) = (!n2) in
                           if Ord.compare x x2 < 0 then
                              (intersectp l l2)
                              || (intersectp r (right_node x2 r2))
                           else
                              (intersectp r r2)
                              || (intersectp l (left_node x2 l2))
            else if sz1=1 then
               let (x1,_,_)=(!n1) in mem s2 x1
            else
               let (x,l,r)=(!n2) in
                  if (splay x [] s1)
                  then
                     true
                  else
                     let (x1,l1,r1) = (!n1) in
                        if Ord.compare x x1 < 0 then
                           (intersectp l l1)
                           || (intersectp r (right_node x1 r1))
                        else
                           (intersectp l (left_node x1 l1))
                           || (intersectp r r1)
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
