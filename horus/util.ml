(*
 * Basic utility functions.
 * There is no particular order.
 *
 * $Log$
 * Revision 1.2  1998/06/16 16:25:36  jyh
 * Added itt_test.
 *
 * Revision 1.1  1997/08/06 16:17:45  jyh
 * This is an ocaml version with subtyping, type inference,
 * d and eqcd tactics.  It is a basic system, but not debugged.
 *
 *)

open List;;
open Array;;

(************************************************************************
 * LISTS                                                                *
 ************************************************************************)

(* Filter items out of a list *)
let filter f l =
   let rec aux = function
      [] -> []
    | h::t ->
         if f h then
            h::(aux t)
         else
            aux t
   in
      aux l;;

(*
 * Compare two lists of things.
 *)
let compare_lists cmp l1 l2 =
   let rec aux = function
      h1::t1, h2::t2 ->
         let i = cmp h1 h2 in
            if i = 0 then
               aux (t1, t2)
            else
               i

    | [], [] -> 0
    | [], _ -> -1
    | _ -> 1
   in
      aux (l1, l2);;

(*
 * Remove marked elements.
 *)
let rec remove_elements l1 l2 =
   match l1, l2 with
      h::t, flag::ft ->
         if flag then
            remove_elements t ft
         else
            h::(remove_elements t ft)
    | l, _ -> l;;

let removeq l x =
   let rec aux = function
      h::t ->
         if h == x then
            t
         else
            h::(aux t)
    | [] ->
         raise (Invalid_argument "removeq")
   in
      aux l;;

(*
 * Iterated tail.
 *)
let rec nth_tl l i =
   if i = 0 then
      l
   else
      match l with
         h::t ->
            nth_tl t (i - 1)
       | [] ->
            raise (Invalid_argument "nth_tl");;

(*
 * Functional replacement.
 *)
let rec replacef_nth l i f =
   let rec aux i = function
      h::t ->
         if i = 0 then
            (f h)::t
         else
            h::(aux (i - 1) t)
    | [] ->
         raise (Invalid_argument "replacef_nth")
   in
      aux i l;;

(*
 * Functional replacement.
 *)
let rec replace_nth l i x =
   let rec aux i = function
      h::t ->
         if i = 0 then
            x::t
         else
            h::(aux (i - 1) t)
    | [] ->
         raise (Invalid_argument "replace_nth")
   in
      aux i l;;

(*
 * Remove an element.
 *)
let rec remove_nth l i =
   match l with
      h::t ->
         if i = 0 then
            t
         else
            h::(remove_nth t (i - 1))
    | [] ->
         raise (Invalid_argument "remove_nth");;

(*
 * Insert an element into a position.
 *)
let insert_nth l i x =
   let rec aux i l =
      if i = 0 then
         x::l
      else
         match l with
            h::t ->
               h::(aux (i - 1) t)
          | [] ->
               raise (Invalid_argument "insert_nth")
   in
      aux i l;;

(*
 * Find the elemnt.
 *)
let find l f =
   let rec aux = function
      h::t ->
         if f h then
            h
         else
            aux t
    | [] ->
         raise Not_found
   in
      aux l;;

let find_item l f =
   let rec aux i = function
      h::t ->
         if f h then
            i
         else
            aux (i + 1) t
    | [] -> raise Not_found
   in
      aux 0 l;;

let find_index l v =
   let rec aux i = function
      h::t ->
         if h = v then
            i
         else
            aux (i + 1) t
    | [] -> raise Not_found
   in
      aux 0 l;;

let find_indexq l v =
   let rec aux i = function
      h::t ->
         if h == v then
            i
         else
            aux (i + 1) t
    | [] -> raise Not_found
   in
      aux 0 l;;

(*
 * Subtract an element from a list.
 * Quadratic algorithm.
 *)
let subtract l1 l2 =
   let rec aux = function
      h::t ->
         if mem h l2 then
            t
         else
            h::(aux t)
    | [] -> []
   in
      aux l1;;

(*
 * Subtract an element from a list.
 * Quadratic algorithm.
 *)
let subtractq l1 l2 =
   let rec aux = function
      h::t ->
         if memq h l2 then
            t
         else
            h::(aux t)
    | [] -> []
   in
      aux l1;;

(*
 * Union of lists by physical equality.
 *)
let union l l' =
   let rec aux = function
      h::t ->
         if mem h l' then
            aux t
         else
            h::(aux t)
    | [] ->
         l'
   in
      aux l;;

(*
 * Union of lists by physical equality.
 *)
let unionq l l' =
   let rec aux = function
      h::t ->
         if memq h l' then
            aux t
         else
            h::(aux t)
    | [] ->
         l'
   in
      aux l;;

(*
 * Zip two lists.
 *)
let zip_list l l1 l2 =
   let rec aux = function
      (h1::t1), (h2::t2) ->
         (h1, h2)::(aux (t1, t2))
    | [], [] ->
         l
    | _ -> raise (Invalid_argument "zip")
   in
      aux (l1, l2);;

let zip a b = zip_list [] a b;;

(*
 * Find index of association.
 *)
let assoc_index l a =
   let rec aux i = function
      (a', _)::t ->
         if a' = a then
            i
         else
            aux (i + 1) t
    | [] -> raise Not_found
   in
      aux 0 l;;

(*
 * Replace an association, but preserve order.
 *)
let assoc_replace l a b =
   let rec aux = function
      (a', b')::t ->
         if a' = a then
            (a, b)::t
         else
            (a', b')::(aux t)
    | [] -> raise Not_found
   in
      aux l;;

(*
 * Add the association if it doesn't already exist.
 *)
let add_assoc (v1, v2) l =
   try let v2' = assoc v1 l in
          if v2 = v2' then
             l
          else
             raise (Invalid_argument "add_assoc")
   with
      Not_found -> (v1, v2)::l;;

(*
 * Split a list.
 *)
let split_list i l =
   let rec aux = function
      0, l -> [], l
    | i, h::t ->
         let l, l' = aux (i - 1, t) in
            h::l, l'
    | i, [] ->
         raise (Invalid_argument "split_list: list is too short")
   in
      aux (i, l);;

(*
 * Split off the last item.
 *)
let rec split_last = function
   [h] -> [], h
 | h::t ->
      let l, x = split_last t in
         h::l, x
 | [] ->
      raise (Invalid_argument "split_last: list is empty");;

(*
 * Split off the last item.
 *)
let rec last = function
   [h] -> h
 | h::t -> last t
 | [] -> raise (Invalid_argument "last: list is empty");;

(*
 * Remove the specified suffix from the list.
 *)
let remove_suffix l suffix =
   let rec aux = function
      (0, l') ->
         if l' = suffix then
            []
         else
            raise (Invalid_argument "remove_suffix")
    | (i, _::t) ->
         aux (i - 1, t)
    | _ ->
         (* This will never happen *)
         raise (Invalid_argument "remove_suffix")
   in
   let i = (List.length l) - (List.length suffix) in
      if i >= 0 then
         aux (i, l)
      else
         raise (Invalid_argument "remove_suffix");;

(*
 * Reverse do_list.
 *)
let rev_do_list f =
   let rec aux = function
      h::t ->
         aux t;
         f h;
         ()
    | [] ->
         ()
   in
      aux;;

(************************************************************************
 * REFS                                                                 *
 ************************************************************************)

(*
 * Stack operations.
 *)
let push a stack =
   stack := a::!stack;;

let pop stack =
   match !stack with
      h::t ->
         stack := t;
         h
    | [] ->
         raise (Invalid_argument "pop");;

(************************************************************************
 * ARRAYS                                                               *
 ************************************************************************)

(*
 * Membership in an array.
 *)
let array_mem i v =
   let l = Array.length v in
   let rec aux j =
      if j < l then
         if i = v.(j) then
            true
         else
            aux (j + 1)
      else
         false
   in
      aux 0;;

(*
 * Membership in an array.
 *)
let array_index i v =
   let l = Array.length v in
   let rec aux j =
      if j < l then
         if i = v.(j) then
            j
         else
            aux (j + 1)
      else
         raise Not_found
   in
      aux 0;;

(*
 * Membership in an array.
 *)
let array_exists f v =
   let l = Array.length v in
   let rec aux j =
      if j < l then
         if f v.(j) then
            true
         else
            aux (j + 1)
      else
         false
   in
      aux 0;;

let array_find_index v f =
   let l = Array.length v in
   let rec aux j =
      if j < l then
         if f v.(j) then
            j
         else
            aux (j + 1)
      else
         raise Not_found
   in
      aux 0;;

(************************************************************************
 * CHARACTERS                                                           *
 ************************************************************************)

(*
 * Map a character to lowercase.
 *)
let tolower = function
   'A' -> 'a'
 | 'B' -> 'b'
 | 'C' -> 'c'
 | 'D' -> 'd'
 | 'E' -> 'e'
 | 'F' -> 'f'
 | 'G' -> 'g'
 | 'H' -> 'h'
 | 'I' -> 'i'
 | 'J' -> 'j'
 | 'K' -> 'k'
 | 'L' -> 'l'
 | 'M' -> 'm'
 | 'N' -> 'n'
 | 'O' -> 'o'
 | 'P' -> 'p'
 | 'Q' -> 'q'
 | 'R' -> 'r'
 | 'S' -> 's'
 | 'T' -> 't'
 | 'U' -> 'u'
 | 'V' -> 'v'
 | 'W' -> 'w'
 | 'X' -> 'x'
 | 'Y' -> 'y'
 | 'Z' -> 'z'
 | c -> c;;

(*
 * Map a character to lowercase.
 *)
let toupper = function
   'a' -> 'A'
 | 'b' -> 'B'
 | 'c' -> 'C'
 | 'd' -> 'D'
 | 'e' -> 'E'
 | 'f' -> 'F'
 | 'g' -> 'G'
 | 'h' -> 'H'
 | 'i' -> 'I'
 | 'j' -> 'J'
 | 'k' -> 'K'
 | 'l' -> 'L'
 | 'm' -> 'M'
 | 'n' -> 'N'
 | 'o' -> 'O'
 | 'p' -> 'P'
 | 'q' -> 'Q'
 | 'r' -> 'R'
 | 's' -> 'S'
 | 't' -> 'T'
 | 'u' -> 'U'
 | 'v' -> 'V'
 | 'w' -> 'W'
 | 'x' -> 'X'
 | 'y' -> 'Y'
 | 'z' -> 'Z'
 | c -> c;;

(************************************************************************
 * STRINGS                                                              *
 ************************************************************************)

(*
 * Find a char in a string.
 *)
let strchr s c =
   let l = String.length s in
   let rec aux i =
      if i < l then
         if s.[i] = c then
            i
         else
            aux (i + 1)
      else
         raise (Invalid_argument "strchr")
   in
      aux 0;;

(*
 * Case conversion.
 *)
let uppercase s =
   let l = String.length s in
   let s' = String_util.create "Util.uppercase" l in
      s'.[0] <- toupper s.[0];
      String_util.blit "Util.uppercase" s 1 s' 1 (l - 1);
      s';;

let lowercase s =
   let l = String.length s in
   let s' = String_util.create "Util.lowercase" l in
      s'.[0] <- tolower s.[0];
      String_util.blit "Util.lowercase" s 1 s' 1 (l - 1);
      s';;

(************************************************************************
 * FILES                                                                *
 ************************************************************************)

(* Can't open and can't find a file *)
exception CantOpen of string;;
exception CantFind of string;;

(*
 * Open a file somewhere in the path.
 *)
let open_in_path path name =
   let rec aux = function
      [] -> raise (CantFind name)
    | dir::rest ->
         let fullname = Filename.concat dir name in
            try let ifile = open_in fullname in (ifile, fullname)
               with _ -> aux rest
   in
      aux path;;

(*
 * Safe file handler.
 *)
let with_input_file name f =
   let iport = open_in name in
   let a = try f iport with x -> close_in iport; raise x in
       close_in iport;
       a;;

let with_output_file name f =
   let oport = open_out name in
   let a = try f oport with x -> close_out oport; raise x in
      close_out oport;
      a;;


(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner.run"
 * End:
 * -*-
 *)

