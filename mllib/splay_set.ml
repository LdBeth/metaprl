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
end

(*
 * Build the set from an ordered type.
 *)
module Make (Ord: Set.OrderedType) =
struct
   type elt = Ord.t

   type tree =
      LEAF
    | NODE of node 
   and node = (elt * t * t) ref
   and t = tree * int

   type direction = LEFT of node | RIGHT of node;;

   let cardinal (_,c) = c

   let rotate_left = function
      { contents = key,(NODE({ contents = left_key,left_left,((_,slr) as left_right) }),_),((_,sr) as right) } as node ->
         node := (left_key,left_left,(NODE(ref(key,left_right,right)),slr+sr+1))
    | _ -> raise (Invalid_argument "rotate_left")

   let rotate_right = function
      { contents = key,((_,sl) as left),(NODE({ contents = right_key,((_,srl) as right_left),right_right }),_) } as node ->
         node := (right_key,(NODE(ref(key,left,right_left)),1+sl+srl),right_right)
    | _ -> raise (Invalid_argument "rotate_right")

   let rec lift = function
      [] -> ()
    | [LEFT parent] ->
         rotate_left parent
    | [RIGHT parent] ->
         rotate_right parent
    | LEFT parent :: LEFT grandparent :: ancestors ->
         (
            rotate_left grandparent;
            rotate_left grandparent;  (* parent has moved into grandparent's position *)
            lift ancestors
         )
    | RIGHT parent :: RIGHT grandparent :: ancestors ->
         (
            rotate_right grandparent;
            rotate_right grandparent;  (* parent has moved into grandparent's position *)
            lift ancestors
         )
    | LEFT parent :: RIGHT grandparent :: ancestors ->
         (
            rotate_left parent;
            rotate_right grandparent;
            lift ancestors
         )
    | RIGHT parent :: LEFT grandparent :: ancestors ->
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
      ((NODE ({ contents = (key, left, right) } as node)),_) ->
         let comp = Ord.compare key0 key
         in
            if comp = 0 then
               (
                  lift path;
                  true
               )
            else if comp < 0 then
            (* left *)
               splay key0 (LEFT node :: path) left
            else
            (* right *)
               splay key0 (RIGHT node :: path) right
    | (LEAF,_) ->
         (match path with
             [] -> false
           | _ :: path' ->
                (
                   lift path';
                   false
                ))

   let rec splay_right path = function
      ((NODE ({ contents = (_, _, right) } as node)),_) ->
         splay_right (node :: path) right
    | (LEAF,_) ->
         (match path with
             [] -> ()
           | _ :: path' ->
                lift_right path')

   let empty = (LEAF,0)
   let is_empty = function
      (LEAF,_) -> true
    | _ -> false

   let mem t key =
      splay key [] t

   let add key t =
      if splay key [] t then
         t
      else
         match t with
            (NODE {contents = (key', ((_,sl) as left), ((_,sr) as right)) }, _ ) ->
               if Ord.compare key key' < 0 then
               (* left *)
                  (NODE (ref (key, left, (NODE (ref (key', empty, right)), succ sr))),2+sl+sr)
               else
                  (NODE (ref (key, (NODE (ref (key', left, empty)), succ sl), right)),2+sl+sr)
          | (LEAF,_) -> (NODE (ref (key, empty, empty)),1)

   let make key = (NODE (ref (key, empty, empty)),1)

   let remove key t =
      if splay key [] t then
         match t with
            (NODE { contents = (_, (LEAF,_), right)}, _) -> right
          | (NODE { contents = (_, left, (LEAF,_))}, _) -> left
          | (NODE { contents = (_, left, ((_,sr) as right))}, _) ->
               (splay_right [] left;
                match left with
                   (NODE {contents = (left_key, ((_,sll) as left_left), (LEAF,_)) },_) ->
                      (NODE (ref (left_key, left_left, right)), 1+sll+sr)
                 | _ -> failwith "remove")
          | _ -> failwith "remove"
      else
         t

   let rec union s1 s2 =
      match (s1,s2) with
         ((LEAF,_), _) -> s2
       | (_, (LEAF,_)) -> s1
       | ((NODE n1, sz1), (NODE n2, sz2)) ->
            if (sz1>=sz2) then
               if sz2=1 then
                  let (x2,_,_)=(!n2) in add x2 s1
               else
                  let (x,l,r)=(!n1) in
                     if (splay x [] s2)
                     then
                        let (_,l2,r2) = (!n2) in
                        let ((_,sll) as ll) = union l l2 in
                        let ((_,srr) as rr) = union r r2 in
                           (NODE ( ref (x, ll, rr)), 1+sll+srr)
                     else
                        let (x2,((_,sl2) as l2),((_,sr2) as r2)) = (!n2) in
                           if Ord.compare x x2 < 0 then
                              let ((_,sll) as ll) = union l l2 in
                              let ((_,srr) as rr) = union r (NODE(ref(x2,empty,r2)), succ sr2) in
                                 (NODE ( ref (x, ll, rr)), 1+sll+srr)
                           else
                              let ((_,sll) as ll) = union l (NODE(ref(x2,l2,empty)), succ sl2) in
                              let ((_,srr) as rr) = union r r2 in
                                 (NODE ( ref (x, ll, rr)), 1+sll+srr)
            else
            if sz1=1 then
               let (x1,_,_)=(!n1) in add x1 s2
            else
               let (x,l,r)=(!n2) in
                  if (splay x [] s1)
                  then
                     let (_,l1,r1) = (!n1) in
                     let ((_,sll) as ll) = union l l1 in
                     let ((_,srr) as rr) = union r r1 in
                        (NODE ( ref (x, ll, rr)), 1+sll+srr)
                  else
                     let (x1,((_,sl1) as l1),((_,sr1) as r1)) = (!n1) in
                        if Ord.compare x x1 < 0 then
                           let ((_,sll) as ll) = union l l1 in
                           let ((_,srr) as rr) = union r (NODE(ref(x1,empty,r1)), succ sr1) in
                              (NODE ( ref (x, ll, rr)), 1+sll+srr)
                        else
                           let ((_,sll) as ll) = union l (NODE(ref(x1,l1,empty)), succ sl1) in
                           let ((_,srr) as rr) = union r r1 in
                              (NODE ( ref (x, ll, rr)), 1+sll+srr)

   let rec elements_aux coll = function
      (LEAF,_) -> coll
    | (NODE {contents=(x,l,r)},_) ->
         x::(elements_aux (elements_aux coll l) r)

   let elements = elements_aux []

   let rec iter f = function
      (LEAF,_) -> ()
    | (NODE {contents = (x,l,r)},_) ->
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
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
