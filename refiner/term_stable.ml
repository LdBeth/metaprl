(*
 * This is a simplified version of termTable.
 *
 * $Log$
 * Revision 1.1  1997/04/28 15:51:45  jyh
 * This is the initial checkin of Nuprl-Light.
 * I am porting the editor, so it is not included
 * in this checkin.
 *
 * Directories:
 *     refiner: logic engine
 *     filter: front end to the Ocaml compiler
 *     editor: Emacs proof editor
 *     util: utilities
 *     mk: Makefile templates
 *
 * Revision 1.1  1996/11/13 22:58:57  jyh
 * Initial version of forward/backward chaining cache.
 *
 *)

open Opname
open Term
open Hashtbl

(************************************************************************
 * TYPES                                                                *
 ************************************************************************)

(*
 * Terms are reduced to these templates for indexing
 * purposes.  Each templet just contains information
 * about the opname, the order and types of params,
 * and the arties of the subterms.
 *)
type term_template =
   { template_opname : opname;
     template_params : paramTemplate array;
     template_arities : int array
   }

and paramTemplate =
   Number
 | String
 | Token
 | Level
 | Var

(*
 * A table is just a list of items.
 *)
type 'a term_stable = (term_template * 'a) list

(*
 * An extracted table is a hashtbl.
 *)
type 'a term_sextract = (term_template, 'a) Hashtbl.t

(************************************************************************
 * IMPLEMENTATION                                                       *
 ************************************************************************)

(*
 * Empty table.
 *)
let new_stable () = []

(*
 * Compute the template for a particular term.
 *)
let compute_template t =
   let { term_op = op; term_terms = bterms } = dest_term t in
   let { op_name = opname; op_params = params } = dest_op op in
   let compute_arity bterm =
      let { bvars = bvars } = dest_bterm bterm in
         List.length bvars
   in
   let compute_param param =
      match dest_param param with
         Term.Number _ -> Number
       | Term.String _ -> String
       | Term.Token _ -> Token
       | Term.Level _ -> Level
       | Term.Var _ -> Var
       | Term.MNumber _ -> Number
       | Term.MString _ -> String
       | Term.MToken _ -> Token
       | Term.MLevel _ -> Level
       | Term.MVar _ -> Var
       | Term.MSum _
       | Term.MDiff _
       | Term.MProduct _
       | Term.MQuotient _
       | Term.MRem _ -> Number
       | Term.MLessThan _
       | Term.MEqual _
       | Term.MNotEqual _ -> Token
   in
      { template_opname = opname;
        template_params =
           if params = [] then
              [||]
           else
              Array.of_list (List.map compute_param params);
        template_arities = Array.of_list (List.map compute_arity bterms)
      }

(*
 * Insert into the list.
 *)
let sinsert tbl t v =
   (compute_template t, v)::tbl

(*
 * Check if the first list is a suffix of the other.
 *)
let check_suffix list1 =
   let rec aux l =
      if list1 == l then
         true
      else
         match l with
            _::t ->
               aux t
          | [] ->
               false
   in
      aux

(*
 * Insert the first list into the second.
 *)
let rec insert_data data1 = function
   h::t ->
      begin
         match h with
            name, tac ->
               begin
                  try 
                     List.assq name data1;
                     insert_data data1 t
                  with
                     Not_found ->
                        insert_data (h :: data1) t
               end
      end
      
 | [] -> data1
            
(*
 * Join the data from two bases.
 * First check if one is a suffix of the other.
 * This will commonly be the case, and so we optimize it.
 *)
let join_stables data1 data2 =
   if check_suffix data1 data2 then
      data2
   else if check_suffix data2 data1 then
      data1
   else
      insert_data data2 data1

(*
 * Compute the hashtable from the info.
 *)
let sextract info =
   let tbl = Hashtbl.create (List.length info) in
   let aux (key, v) =
      Hashtbl.add tbl key v
   in
      List.iter aux info;
      tbl

(*
 * Lookup.
 *)
let slookup tbl t =
   Hashtbl.find tbl (compute_template t)

(*
 * -*-
 * Local Variables:
 * Caml-master: "editor.run"
 * End:
 * -*-
 *)
