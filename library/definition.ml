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

open Lm_debug

open Basic
open Term_sig
open Refiner.Refiner.Term
open Refiner.Refiner.TermType
open Nuprl5
open List
open Db
open Opname

let _ =
   show_loading "Loading Definition%t"

let token s = Token (mk_opname s nil_opname)

(* unused
let idescription_parameter = make_param (token "!description")
let idescription_op sys = mk_nuprl5_op [idescription_parameter; sys]
let idescription_term sys version purposes =
  mk_term (idescription_op sys)
	[ mk_bterm [] (list_to_ilist_map inatural_term version)
	; mk_bterm [] (list_to_ilist_map itoken_term purposes)
	]
*)

let purposes_of_idescription_term t =
  match dest_term t with
    { term_op = op; term_terms = [version; purposes] }
    -> map_isexpr_to_list string_of_itoken_term (term_of_unbound_term purposes)
    | { term_op = op; term_terms = [version; purposes; rest] } -> map_isexpr_to_list string_of_itoken_term (term_of_unbound_term purposes)
    |_ -> error ["term"; "!description"; "not"] [] [t]
(*

let purposes_of_idescription_term t =
  match dest_term t with
    { term_op = op; term_terms = bts }
    -> map string_of_itoken_term (map term_of_unbound_term (tl bts))
    |_ -> error ["term"; "!description"; "not"] [] [t]
*)

type dependency = {data : stamp; objc : stamp; oid : object_id }

let idata_op = mk_nuprl5_op [make_param (token "!data")]
let term_of_idata_term t =
  match dest_term t with
    { term_op = op; term_terms = [subterm] } when opeq op idata_op
      -> term_of_unbound_term subterm
    |_ -> error ["term"; "!data"] [] [t]

(* unused
open Mbterm

let mydb_read s f =
  print_string "calling_dbread";
  let t = db_read s "SUBSTANCE" in
    print_term t;
    t
*)

class data (s : stamp) ft  =
 object (self)
  val stamp = s
  val ftype = ft
  val mutable term = None

  method get_stamp = stamp
  method read_term = let t = (term_of_idata_term (db_read stamp ftype)) in term <- Some t; t
  method get_term =
     match term with
       None -> self#read_term
     | Some t -> t
end

(* unused
exception InlineDataRead
*)

class inline_data (s : stamp) (inl : term) =
 object (self)
 inherit data s ""

 val inline = inl

  method read_term = term_of_idata_term inline
  method get_term =
     match term with
       None -> self#read_term
     | Some t -> t

end

let idata_persist_param = make_param (token "!data_persist")
let idata_persist_inline_param = make_param (token "!data_persist_inline")

let term_to_data t =
 match dest_term t with
   { term_op = op; term_terms = [istamp] }
      -> (match dest_op op with
	   { op_name = opname; op_params = [id; ftype] } when parmeq id idata_persist_param
		-> new data (term_to_stamp (term_of_unbound_term istamp))
			    (dest_token_param ftype)
		|_ -> error  ["term"; "data_persist"; "op"] [] [t]
	)
   | { term_op = op; term_terms = [istamp; inline] }
      -> (match dest_op op with
	   { op_name = opname; op_params = [id; ftype] } when parmeq id idata_persist_inline_param
		-> new inline_data (term_to_stamp (term_of_unbound_term istamp))
				   (term_of_unbound_term inline)
		|_ -> error  ["term"; "data_persist_inline"; "op"] [] [t]
	)
   |_ -> error ["term"; "data_persist"] [] [t]

type substance_type = TermSubstance | Substance

(* to avoid making substance fields mutable. substance import will be peformed
   at initialization.
 *)

let isubstance_op = mk_nuprl5_op [make_param (token "!substance")]

let term_of_isubstance_term t =
 match dest_term t with
   { term_op = op; term_terms = [sub; refs; props; super] } when opeq op isubstance_op
	-> term_of_unbound_term sub
   |_ -> error ["term"; "!substance"] [] [t]

class substance (t : term) =
 object
  val term = term_of_isubstance_term t

  method get_term = term
end

class term_substance (t : term) =
 object
  inherit substance t
end

(*
 *	due to the difficulties of initializing class instances I think using
 *	classes may not be best.
 *
 *	  - unroll from sub -> super, ie new a term, term would be !a with a subterm for super.
 *	      * class a term = val ata = fa term val atb = ga term end
 *	      * class b term = inherit a (bb term) val bta = fb term val btb = gb term end
 *	    not bad except initializer funcs fa and ga, not flexible enough, for example
 *	    one might want to produce some intermediate data structure from term and used that
 *	    to init ata and atb. Could make values mutable and do provide/import.
 *
 *	Winner: see bot haven't considered variants.
 *	  - unroll from super -> sub. impossible except via provide/import.
 *	      * Accept provide/import then have super class define a subterm method and
 *		things work pretty well?
 *	  Ugly but effective.
 *
 *  	Not better might be to use records where sub class contains a super class
 *
 *	a = { t :term }
 *  	b = { x : 'a; y : 'b, super :a}
 *
 *	make_b t =
 *	  let (super, bterm) = make_a t in
 *  	     let foo = f bterm in { x = g foo, y = h foo, super = super}
 *
 *	this works well for constructing but is messy when accesing super record values.
 *
 *	x : b
 *	term_of = b.super.term
 *
 *	however can not make a list of two derived records without defining variant class.
 *	this is getting very tedious.
 *	OTOH is it common to have a list of heterogeneous derived records?
 *
 *
 *	maybe functors would help here.
 *	Haven't considered variant types where subtype is parameterized supertype :
 *	  - ugly to acces subtype data as have to match however good polymorphism.
 *	or supertype is parameterized subtype (nfg: not polymorphic for supertype ie table funcs
 *	need supertype)
 *)

(* testing 1,2,3 *)
(*
class sub_substance t =
 object (self)
 inherit term_substance t as super

 (* super does not work in following. Thus derived val's will have to be mutable and provided
    via an provide/import methods after initialization, or supply non-method access function to get data
    during init
  *)

 val mutable derived = ivoid_term
 val private mutable provided = false

 method provide = derived <- super#get_term (*get_sub_term*); provided <- true; ()

 method myterm = if provided then derived else (self#provide; derived)
end
*)

(*
 *	definitions will be subclassed but need to be able to recover subclass
 *	from a superclass.
 *
 *	one method is to parameterize class so that substance class is available.
 *	and to use a variant to hold the superclasses instead of referring to the subclass.
 *
 *	Have a choice :
 *	  - parameterize by substance,
 *	      * slightly cooler (more typesafe?)
 *
 *	  - substance variant.
 *	      * less complicated.
 *
 *)

(* exception UnexpectedSubstanceType of substance_type *)

let substance_import def stype data =
  print_string "subimp ";
  match stype with
    Substance -> def#set_substance (new substance data#get_term); ()
  | TermSubstance -> def#set_substance (new term_substance data#get_term); ()

exception NoSubstance

class ['a] definition (d : dependency) (da : data) st =
 object (self)
  val dep = d
  val dat = da
  val mutable sub = None
  val substance_type = st

  method get_dependency = dep

  method set_substance (s : 'a) = sub <- Some s
  method substance_p = match sub with None -> false | Some s -> true
  method provide =
	  match sub with None -> (substance_import self substance_type dat); () | Some s -> ()
  method get_substance =
	  self#provide;
	  match sub with None -> raise NoSubstance | Some s -> s
end

class term_dyneval (t: term) =
 object(self)

  (*val mutable stamp = None*)
  val term = t
  (*val mutable flags = None

  method get_term = self#get_term*)

end

exception NoDyneval

class ['a] term_definition d da st =
 object (self)
  inherit ['a] definition d da st

  val mutable dyneval = Some (new term_dyneval ivoid_term)

  method set_dyneval s = dyneval <- Some s
  method dyn_p = match dyneval with None -> false | Some s -> true
  method get_dyneval = match dyneval with None -> raise NoDyneval | Some s -> s

  method set_substance (s : term_substance) = sub <- Some s
  method get_term = (self#get_substance)#get_term

end

let idag_child_param = make_param (token "!dag_child")
let idirectory_param = make_param (token "!directory")
(* unused
let idyneval_param = make_param (token "!dyneval")
*)

let idag_cons_op = mk_nuprl5_op [make_param (token "!dag_cons")]

let idirectory_term_p t =
 match dest_term t with
  { term_op = op; term_terms = [children]} when unbound_bterm_p children
     -> (match dest_op op with
	  { op_name = opname; op_params = id :: rest}
		when nuprl5_opname_p opname && parmeq id idirectory_param
		-> true
	  |_ -> false
	)
  |_ -> false

(* unused
let idyneval_term_p t =
 match dest_term t with
  { term_op = op; term_terms = bts}  -> (*lal make stronger test*)
  (match dest_op op with
      { op_name = opname; op_params =  id :: rest}
      when Opname.eq nuprl5_opname opname && parmeq id idyneval_param -> true
      | _ -> false
     )
  (* | _ -> false *)
*)

let children_of_idirectory_term t =
 match dest_term t with
  { term_op = op; term_terms = [children]} when unbound_bterm_p children
     -> map_isexpr_to_list_by_op
		idag_cons_op
		(fun c -> match dest_term c with
			    { term_op = op; term_terms = [] }
			       -> (match dest_op op with
				    { op_name = opname; op_params = [id; name; oid] }
					when parmeq id idag_child_param
				       -> (dest_token_param name, dest_obid_param oid)
				    |_ -> error ["term"; "!directory"; "child"; "op"] [] [t]
				   )
			    |_ -> error ["term"; "!directory"; "child"] [] [t])
		(term_of_unbound_term children)
  |_ -> error ["term"; "!directory"; "children"] [] [t]

(* assumes idirectory_p true *)
let iroot_directory_term_p t =
 match dest_term t with
  { term_op = op; term_terms = terms}
     -> (match dest_op op with
	  { op_name = opname; op_params = id :: rest}
		when not (nullp rest)
		-> (match dest_param (hd rest) with
		    Token name -> true
		   |_ -> false
		)
	  |_ -> false
	)

(* assumes iroot_directory_p true *)
let name_of_iroot_directory_term t =
 match dest_term t with
  { term_op = op; term_terms = terms}
     -> (match dest_op op with
	  { op_name = opname; op_params = [id; name] }
		-> dest_token_param name
	  |_ -> error ["term"; "!directory"; "root"; "op"] [] [t]
	)

exception NotRootDirectory

(* need to merge with term_def as term def must have term_defs since cannot cast to sub class.
   au contraire, it would seem that a variant of dir_def and term_def as term entry would be suitable.
 *)

class ['a] directory_definition d dat st =
 let datterm = dat#get_term in
 object
  inherit ['a] term_definition d dat st as _super

  (* instantiation of values could be lazy, but seems like inconsequential space savings *)
  (*
  val children = children_of_idirectory_term (super#get_substance)#get_term (*LAL fails, not a dir term*)
  val rootp = iroot_directory_term_p (super#get_substance)#get_term
  val root_name = let term = (super#get_substance)#get_term in
			(print_string " n "; if (iroot_directory_term_p term)
			   then name_of_iroot_directory_term term
			   else "")
  *)
  val children = children_of_idirectory_term (term_of_isubstance_term datterm)
  val rootp = iroot_directory_term_p (term_of_isubstance_term
				      datterm
				      )
  val root_name = let term = (term_of_isubstance_term datterm) in
			if (iroot_directory_term_p term) then name_of_iroot_directory_term term
			   else ""

  method get_children = children
  method rootp = rootp
  method get_root_name = if rootp then root_name else (print_string "nonroot"; raise NotRootDirectory)
end

type term_entry
	= DirectoryDefinition of term_substance directory_definition
	| TermDefinition of term_substance term_definition

let dag_description_p t =
  if ivoid_term_p t
     then false
     else let purposes = (purposes_of_idescription_term t) in
       ((mem "ObjectIdDAG" purposes) && not (mem "Derived" purposes))

let idefinition_op = mk_nuprl5_op [make_param (token "!definition")]

let idependency_param = make_param (token "!dependency")

let term_to_dependency t =
  match dest_term t with
    { term_op = op; term_terms = [objc; data] }
      -> (match dest_op op with
	  { op_name = opname; op_params = [id; oid] } when parmeq id idependency_param
							   && nuprl5_opname_p opname
		->	{ data = term_to_stamp (term_of_unbound_term data)
			; objc = term_to_stamp (term_of_unbound_term objc)
			; oid = dest_obid_param oid
			}
	    |_ -> error ["term"; "!dependency"; "op"] [] [t])
    |_ -> error ["term"; "!dependency"] [] [t]

(* would like this to take table as arg and table contains term -> def
   want polymorphism with definitions and tables
 *)
(* unused
let import_term_old idef idesc =
  print_string " import_term "; Mbterm.print_term idef;
  match dest_term idef with
    { term_op = op; term_terms = [idep; idata] } when opeq op idefinition_op
      -> (let dep = term_to_dependency (term_of_unbound_term idep) in
	  let data = term_to_data (term_of_unbound_term idata) in

	  print_string " import_term "; Mbterm.print_term (term_of_unbound_term idata);
	  if not (dag_description_p idesc) then print_string " uh oh";

	  if not (dag_description_p idesc)
	     then TermDefinition (new term_definition dep data TermSubstance)
	     else
		  DirectoryDefinition (new directory_definition dep data Substance) (*LAL fails, subst not a dir term*)
	)

    |_ -> error ["term"; "!definition"] [] [idef]
*)
(*
let dest_dyneval_term t =
  match dest_term t with
  { term_op = op; term_terms = [cond; exp; stamp; value]} (*lal make stronger test*)
     -> (match dest_op op with
	  { op_name = opname; op_params = id :: rest}
		when nuprl5_opname_p opname && parmeq id idyneval_param
		->
	  | _ -> fail
	)
  |_ -> fail
*)

let import_term idef idesc =
  (*print_string " import_term "; Mbterm.print_term idef;*)
  match dest_term idef with
    { term_op = op; term_terms = [idep; idata] } when opeq op idefinition_op
      -> (let dep = term_to_dependency (term_of_unbound_term idep) in
	  let data = term_to_data (term_of_unbound_term idata) in

	  (*print_string " import_term "; Mbterm.print_term (term_of_unbound_term idata);*)

	  if not (dag_description_p idesc)
	     then TermDefinition (new term_definition dep data TermSubstance)
	     else
		let datterm = data#get_term in
                let idir = term_of_isubstance_term datterm in
		if idirectory_term_p idir then DirectoryDefinition (new directory_definition dep data Substance) (*LAL fails, subst not a dir term*)
		else TermDefinition (new term_definition dep data TermSubstance)

		(*if (idyneval_term_p idir) then
		let def = new term_definition dep data TermSubstance in
		and [stamp, flags, term] = dest_dyneval_term idir in
		(def#set_dyneval (new term_dyneval idir); TermDefinition def)
		else error ["import"; "term"; "not"] [] [idir]*)

	)
    |_ -> error ["term"; "!definition"] [] [idef]

open Oidtable

type termtable = term_entry oidtable

let make_termtable () = ((make_oidtable ()): termtable)

(*

let termtable_insert tt idef idesc st oid seq =
  insert tt st oid seq (import_term idef idesc)

let termtable_insert tt idef idesc st oid seq =

let termtable_unit_map
*)

let definition_of_entry entry =
  match entry with
    DirectoryDefinition def -> (def : term_substance directory_definition :> term_substance definition)
  | TermDefinition def -> (def : term_substance term_definition :> term_substance definition)

let oid_of_term_entry e = ((definition_of_entry e)#get_dependency).oid

let idefinition_insert_param = make_param (token "!definition_insert")
let idefinition_delete_param = make_param (token "!definition_delete")
let icommit_param = make_param (token "!commit")
let iundo_param = make_param (token "!undo")

let apply_broadcast ttable ibcast idesc stamp commit_stamp =
  let auto_commit oid seq =
	(match commit_stamp with
		  None -> ()
		| Some s -> commit ttable s oid seq
		) in


  (*print_string "apply broadcast";
  print_newline();
  Mbterm.print_term ibcast;
  print_newline();*)

  match dest_term ibcast with
    { term_op = op; term_terms = terms} ->
	match dest_op op with
	  { op_name = opn; op_params = pid :: pseq :: rest } when nuprl5_opname_p opn
	    -> (if parmeq pid idefinition_insert_param
		    then
                    let entry = (import_term (term_of_unbound_term (hd terms)) idesc) in
		         let oid = (oid_of_term_entry entry)
			 and seq = (dest_int_param pseq) in
			  (insert ttable stamp oid seq entry;
			  auto_commit oid seq)
		else if parmeq pid idefinition_delete_param
		    then let oid = (dest_obid_param (hd rest))
			 and seq = (dest_int_param pseq) in
			  delete ttable stamp oid seq
			  ; auto_commit oid seq
		else if parmeq pid icommit_param
		    then commit ttable stamp
					(dest_obid_param (hd rest))
					(dest_int_param pseq)
		else if parmeq pid iundo_param
		    then undo ttable stamp
					(dest_obid_param (hd rest))
					(dest_int_param pseq)
		else error ["term"; "broadcast"; "opid"] [] [ibcast]
		)
	   |_ -> error ["term"; "broadcast"] [] [ibcast]

let termtable_lookup ttable stamp oid = lookup ttable stamp oid
let termtable_unit_map tt st f =  oidtable_unit_map tt st f
let termtable_map tt st f = oidtable_map tt st f

let roots tt stamp =
 termtable_map tt stamp
  (fun oid te ->
    match te with
      DirectoryDefinition def ->
	(if def#rootp
		then Some (def#get_root_name, oid)
		else None)
    | TermDefinition def -> None)

(* unused
let root_p tt stamp oid =
 let te = termtable_lookup tt stamp oid in
    match te with
      DirectoryDefinition def -> def#rootp
    | TermDefinition def -> false

let root_name tt stamp oid =
 let te = termtable_lookup tt stamp oid in
    match te with
      DirectoryDefinition def ->
	(if def#rootp
		then def#get_root_name
		else error ["Term Table"; "root name"; "root"; "not"] [oid] [])
    | TermDefinition def -> error ["Term Table"; "root name"; "directory"; "not"] [oid] []
*)

let directory_p tt stamp oid =
 try
 (let te = termtable_lookup tt stamp oid in
    match te with
      DirectoryDefinition def -> true
    | TermDefinition def -> false)
 with e -> false

let directory_children tt stamp oid =
 let te = termtable_lookup tt stamp oid in
    match te with
      DirectoryDefinition def -> def#get_children
    | TermDefinition def -> error ["Term Table"; "root children"; "directory"; "not"] [oid] []
