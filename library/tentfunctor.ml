(*34567890123456789012345678901234567890123456789012345 *)

module type OID_TYPE =
 sig
  type t
  val eq: t -> t -> bool
 end


module type TENTFUNCTOR =
 functor (Id: OID_TYPE) ->

 sig 

 open Basic

 type 'a tent

 exception TentLookupWoidNone
 exception TentLookupNone
 exception TentUndoBadSeq
 exception TentUndoMissing
 exception TentCommitBadSeq
 exception TentCommitMissing

 val new_tent	: unit -> 'a tent

 val tent_lookup 	: 'a tent -> stamp -> 'a

 (*
 val tent_contains_committed_oid_p	: tent -> stamp -> Id.t -> bool
 val tent_contains_oid_p		: tent -> stamp -> Id.t -> bool
 val tent_contains_pending_p		: tent -> stamp -> int -> bool
 val tent_contains_visible_p		: tent -> stamp -> bool
 *)

 val tent_insert 	: 'a tent -> stamp -> int -> Id.t -> 'a -> unit
 val tent_delete 	: 'a tent -> stamp -> int -> Id.t -> unit
 val tent_undo		: 'a tent -> stamp -> int -> (Id.t * 'a option)
 val tent_commit	: 'a tent -> stamp -> int -> unit

 val tent_collect 	: 'a tent -> stamp list -> unit

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

  exception TentLookupWoidNone
  exception TentLookupNone
 
  let tent_lookup_sent tent stamp =
   let f sent = 
      (in_transaction_p sent.stamp stamp) in
    match (assoc_if f tent.pending) with
      None -> (let f sent = 
		(transaction_less sent.stamp stamp) in 
              match (assoc_if f tent.committed) with
		None -> raise TentLookupNone
              | Some sent -> sent)
    | Some sent -> sent

  let tent_lookup tent stamp =
     match (tent_lookup_sent tent stamp).data with
       None -> raise TentLookupNone
     | Some data -> data


  let tent_lookup_woid tent stamp oid =
   let f sent = 
      (in_transaction_p sent.stamp stamp) & (OID.eq sent.oid oid)  in
    match (assoc_if f tent.pending) with
      None -> (let f sent = 
		(transaction_less sent.stamp stamp) & (OID.eq sent.oid oid) in 
              match (assoc_if f tent.committed) with
		None -> raise TentLookupWoidNone
              | Some sent -> sent)
    | Some sent -> sent

  let tent_insert tent stamp seq oid data =
    tent.pending <- {stamp = stamp; seq = seq; oid = oid; data = Some data; deletes = []}
	            :: tent.pending;
    ()

  let tent_delete tent stamp seq oid =
   let sent = tent_lookup_woid tent stamp oid in 
   let nsent = {stamp = stamp; seq = seq; oid = oid; data = None; deletes = []}
   in
    sent.deletes <- nsent :: sent.deletes;
    tent.pending <- nsent :: tent.pending;
    ()

  exception TentUndoBadSeq
  exception TentUndoMissing
  exception TentCommitBadSeq
  exception TentCommitMissing

  let tent_undo tent stamp seq =
   let f sent = (if (in_transaction_p sent.stamp stamp)
		   then if (seq = sent.seq)
			     then true
			     else raise TentUndoBadSeq
		   else false) in
    let (usent', pending) = (remove_if f tent.pending) in
     match usent' with
       None -> raise TentUndoMissing
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
    (* print_string "tent_commit"; print_newline();
       print_stamp stamp; print_newline(); *)
    let f sent = (* print_stamp sent.stamp; print_newline(); *)
    		(if (in_transaction_p sent.stamp stamp)
		     then if (seq = sent.seq)
			     then true
			     else raise TentCommitBadSeq
	  	     else false) in

    let (csent', pending) = (remove_if f tent.pending) in
     match csent' with
       None -> raise TentCommitMissing
     | Some csent ->
  
        begin
         tent.pending <- pending;
     	 csent.stamp <- stamp;
         tent.committed <- csent :: tent.committed;
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
   (if not ([] = !removes)
      then tent.committed <- filter (function s -> List.mem s !removes) committed);
   ()

(*3456789 123456789 123456789 123456789 123456789 12345 *)

end

module type TENT =
 sig

 open Basic

 type oid
 type 'a tent

 exception TentLookupWoidNone
 exception TentLookupNone
 exception TentUndoBadSeq
 exception TentUndoMissing
 exception TentCommitBadSeq
 exception TentCommitMissing

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
