open Array

module InfiniteROArray =
struct

type 'a descriptor = int

let describe d = d
let subscribe i = i

type 'a t =
   {
     mutable count : int;
     mutable array: ('a option) array;
   }

exception EmptySlot

let create size = { count = 0; array = Array.create size None; }

let get_aux ar i =
   if Array.length ar <= i then
      None
   else
      Array.get ar i

let store ar item =
   let {array=array; count=count} = ar in
   if count=Array.length array then
     begin
      let new_ar = Array.init (2*count) (get_aux array) in
         ar.array <- new_ar
     end;
   Array.set ar.array count item;
   ar.count<-count+1;
   count

let get ar i = Array.get ar.array i

end


