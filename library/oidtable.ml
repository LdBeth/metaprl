
 open Basic
 open Term

 open Tentfunctor
 open Hashtbl

module type Oid =
 sig
  type t = object_id
  val eq : object_id -> object_id -> bool
 end

module Oid =
 struct
  type t = object_id
  let eq = (fun x y -> (x = y))
 end

module OidTent = Tent (Oid)

open OidTent

type 'a oidtable = (object_id, 'a tent) t

let print_object_id oid =  List.map Mbterm.print_param (dest_object_id oid)

let make_oidtable () = ((Hashtbl.create 997): 'a oidtable)

let delete ot stamp oid i =
  let tent = find ot oid  in
    tent_delete tent stamp i oid

let undo ot stamp oid i =
  let tent = find ot oid  in
    tent_undo tent stamp i; ()

let commit ot stamp oid i =
  (* print_string "commit "; print_object_id oid; print_int (hash oid); print_newline(); *)
  let tent = find ot oid  in
    tent_commit tent stamp i

let lookup ot stamp oid =
  let tent = find ot oid  in
    tent_lookup tent stamp


(*34567890123456789012345678901234567890123456789012345 *)

let insert ot st oid i v =
  (* print_string "Insert ";  print_object_id oid; print_int (hash oid); print_newline(); *)
  let tent = (try (find ot oid)
	      with Not_found -> let ntent = new_tent () in (add ot oid ntent); ntent) in
    (tent_insert tent st i oid v)

exception Oidtablemap

let oidtable_unit_map ot stamp f =
 iter (fun oid tent -> 
	try (f oid (try tent_lookup tent stamp with _ -> raise Oidtablemap))
	with Oidtablemap -> ())
      ot

let oidtable_map ot stamp f =
 let acc = ref [] in
   iter (fun oid tent -> 
	try (match (f oid (try tent_lookup tent stamp with _ -> raise Oidtablemap)) with
	      None -> ()
	    | Some x -> (acc :=  x :: !acc); ())
	with Oidtablemap -> ())
       ot;
 !acc

