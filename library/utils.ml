
type 'a oref = {mutable ocontents : 'a option}

let null_oref ()  = {ocontents = None}
let oref a = {ocontents = (Some  a)}
let oref_set a b = (a.ocontents <- Some b); b

exception OrefNone

let oref_p = function 
 { ocontents = None} -> false
 | { ocontents = Some a } -> true

let oref_option a = a.ocontents

let oref_val = function 
 { ocontents = None} -> raise OrefNone
 | { ocontents = Some a } -> a



open List


let assoc_if f l =
    let rec aux l = 
      match l with
       [] -> None
      | head :: tail -> if (f head) then (Some head) else (aux tail) in
    aux l


let remove_if f l =
   let item = null_oref () in
    let rec aux l = 
      match l with
       [] -> []
      | head :: tail -> if (f head)
			   then ((oref_set item head); tail) 
			   else head :: (aux tail) in
    ((oref_option item), (aux l))
    
let remove_if' f l =
    let rec aux l = 
      match l with
       [] -> []
      | head :: tail -> if (f head)
			   then tail
			   else head :: (aux tail) in
     (aux l)
    

let remove item l =
    let rec aux l = 
      match l with
       [] -> []
      | head :: tail -> if (item =  head)
			   then tail
			   else head :: (aux tail) in
     (aux l)

let filter f l =
    let rec aux l = 
      match l with
       [] -> []
      | head :: tail -> if (f head)
			   then aux tail
			   else head :: (aux tail) in
     (aux l)
    


