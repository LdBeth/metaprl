(*
 * This file is part of Nuprl-Light, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/index.html for information on Nuprl,
 * OCaml, and more information about this system.
 *
 * Copyright (C) 1998 Lori Lorigo, Richard Eaton, Cornell University
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
 * Authors: Lori Lorigo, Richard Eaton
 *)

open Printf
open Nl_debug

let _ =
   if !debug_load then
      eprintf "Loading Oidtable%t" eflush


 open Basic
 open Refiner.Refiner.Term

 open Tentfunctor
 open Hashtbl

module type Oid =
 sig
  type t = object_id
  val equal	: object_id -> object_id -> bool
  val hash	: object_id -> int
 end

module Oid =
 struct
  type t = object_id
  let equal = (fun x y -> (oideq x y))
  let hash oid = hash (List.map parmhash oid)
 end

module OidTent = Tent (Oid)

open OidTent

module OidHashTable = Hashtbl.Make (Oid)

open OidHashTable

type 'a oidtable = 'a tent t

let print_object_id oid =  List.map Mbterm.print_param (dest_object_id oid)

let make_oidtable () = ((OidHashTable.create 997): 'a oidtable)

let delete ot stamp oid i =
  (* print_string "deleteing "; *)
  let tent = find ot oid  in
    (* print_string " found tent "; *)
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
	     (* (print_string "oid_table_map "); *)
	try (match (f oid (try tent_lookup tent stamp with _ -> raise Oidtablemap)) with
	      None -> ()
	    | Some x -> (acc :=  x :: !acc); ())
	with Oidtablemap -> (print_string "oidtablemap fail"); ())
       ot;
 !acc

