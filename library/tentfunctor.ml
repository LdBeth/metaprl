(*
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/htmlman/default.html or visit http://metaprl.org/
 * for more information.
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

(*34567890123456789012345678901234567890123456789012345 *)

open Lm_debug

let _ =
   show_loading "Loading TentFunctor%t"

module type OID_TYPE =
 sig
  type t
  val equal : t -> t -> bool
 end

module type TENTFUNCTOR =
 functor (Id: OID_TYPE) ->

 sig

(* unused
 open Basic

 type 'a tent

 val new_tent	: unit -> 'a tent

 val tent_lookup 	: 'a tent -> stamp -> 'a
*)

 (*
 val tent_contains_committed_oid_p	: tent -> stamp -> Id.t -> bool
 val tent_contains_oid_p		: tent -> stamp -> Id.t -> bool
 val tent_contains_pending_p		: tent -> stamp -> int -> bool
 val tent_contains_visible_p		: tent -> stamp -> bool
 *)

(* unused
 val tent_insert 	: 'a tent -> stamp -> int -> Id.t -> 'a -> unit
 val tent_delete 	: 'a tent -> stamp -> int -> Id.t -> unit
 val tent_undo		: 'a tent -> stamp -> int -> (Id.t * 'a option)
 val tent_commit	: 'a tent -> stamp -> int -> unit

 val tent_collect 	: 'a tent -> stamp list -> unit
*)

end

module TentFunctor =
 functor (OID : OID_TYPE) ->
  struct

  open List
  open Utils
  open Basic

  type oid = OID.t

  (*type 'a maybe_data = Data of 'a | Null of unit *)

  type 'a sent =
	{
	mutable stamp : stamp;
	seq : int;
	oid : oid;
	data : 'a option;
	mutable deletes : 'a sent list
	}

  let delete_sent_p s = match s.data with None -> true | _ -> false

  type 'a tent =
	{
	mutable committed : 'a sent list;
	mutable pending : 'a sent list
	}

  let new_tent () = {committed = []; pending = []}

  let tent_lookup_sent tent stamp =
   let f sent =
       (in_transaction_p sent.stamp stamp)
    in
    match (assoc_if f tent.pending) with
      None -> (let f sent =
		   (*
		    print_string " tent lookup sent committed";
		   Mbterm.print_term (stamp_to_term sent.stamp);
		   Mbterm.print_term (stamp_to_term stamp);
		   *)
		(transaction_less sent.stamp stamp) in

              match (assoc_if f tent.committed) with
		None -> error ["Tent"; "LookupSent"; "None"][][]
              | Some sent -> sent)
    | Some sent -> sent

  let tent_lookup tent stamp =
     (* print_string " tent lookup "; *)
     match (tent_lookup_sent tent stamp).data with
       None -> (* print_string " tent lookup none"; *)
		error ["Tent"; "Lookup"; "None"][][]
     | Some data -> data


  let tent_lookup_woid tent stamp oid =
   let f sent =
      (in_transaction_p sent.stamp stamp) && (OID.equal sent.oid oid)  in
    match (assoc_if f tent.pending) with
      None -> (let f sent =
		(transaction_less sent.stamp stamp) && (OID.equal sent.oid oid) in
              match (assoc_if f tent.committed) with
		None -> error ["Tent"; "LookupWoid"; "None"][][]
              | Some sent -> sent)
    | Some sent -> sent

  let tent_insert tent stamp seq oid data =
    tent.pending <- {stamp = stamp; seq = seq; oid = oid; data = Some data; deletes = []}
	            :: tent.pending;
    (*print_int seq; print_endline " Yo insert "; *)
    ()

  let tent_delete tent stamp seq oid =
   let sent = tent_lookup_woid tent stamp oid in
   let nsent = {stamp = stamp; seq = seq; oid = oid; data = None; deletes = []}
   in
    sent.deletes <- nsent :: sent.deletes;
    tent.pending <- nsent :: tent.pending;
    ()

  let tent_undo tent stamp seq =
   let f sent = (if (in_transaction_p sent.stamp stamp)
		   then if (inteq seq sent.seq)
			     then true
			     else error ["Tent"; "Undo"; "BadSeq"][][]
		   else false) in
    let (usent', pending) = (remove_if f tent.pending) in
     match usent' with
       None -> error ["Tent"; "Undo"; "Missing"][][]
     | Some usent ->

      begin

        tent.pending <- pending;

        (* if undoing delete must remove usent from deleted sent *)
        (if delete_sent_p usent
           then let sent = tent_lookup_woid tent stamp usent.oid in
                  sent.deletes <- remove usent sent.deletes);

        (usent.oid, usent.data)

      end


  let tent_commit tent stamp seq =

    (*
    print_endline "tent_commit "; print_int seq;
    print_stamp stamp; print_newline();
    *)

    (* need to look in reverse order *)
    let f sent =  (* print_stamp sent.stamp; print_newline();  *)
    		(if (in_transaction_p sent.stamp stamp)
		    then if (inteq seq sent.seq)
			     then true
			     else error ["Tent"; "Commit"; "BadSeq"] [][]
	  	     else false) in

    let (csent', pending) = (remove_from_end_if f tent.pending) in
     match csent' with
       None -> error ["Tent"; "Commit"; "Missing"][][]
     | Some csent ->

        begin
         tent.pending <- pending;
     	 csent.stamp <- stamp;
         tent.committed <- csent :: tent.committed;
	 (* print_endline "Yo commit"; *)
	 ()
        end

 let tent_collect tent stamps =
  let committed = tent.committed
  and dsents = ref []
  and removes = ref [] in
  let collect sent =
        (if (delete_sent_p sent)
           then (if (List.for_all (function s -> in_transaction_p sent.stamp s)
			    stamps)
		    then dsents := (sent :: !dsents))
	   else match sent.deletes with
		  s :: [] -> (if (mem s !dsents)
				 then removes := (s :: sent :: !removes))
		| _ -> ())
  in

   List.iter collect committed;
   (if not (nullp !removes)
      then tent.committed <- filter (function s -> List.mem s !removes) committed);
   ()

(*3456789 123456789 123456789 123456789 123456789 12345 *)

end

module type TENT =
 sig

 open Basic

 type oid
 type 'a tent

 val new_tent	: unit -> 'a tent

 val tent_lookup 	: 'a tent -> stamp -> 'a

 (*
 (* not sure we want these in sig. *)
 val tent_contains_committed_oid_p	: tent -> stamp -> oid -> bool
 val tent_contains_oid_p		: tent -> stamp -> oid -> bool
 val tent_contains_pending_p		: tent -> stamp -> int -> bool
 val tent_contains_visible_p		: tent -> stamp -> bool
 *)

 val tent_insert 	: 'a tent -> stamp -> int -> oid -> 'a -> unit
 val tent_delete 	: 'a tent -> stamp -> int -> oid -> unit
 val tent_undo		: 'a tent -> stamp -> int -> (oid * 'a option)
 val tent_commit	: 'a tent -> stamp -> int -> unit

 val tent_collect 	: 'a tent -> stamp list -> unit

 end




module Tent = (TentFunctor : functor (Oid : OID_TYPE) -> (TENT with type oid = Oid.t))
