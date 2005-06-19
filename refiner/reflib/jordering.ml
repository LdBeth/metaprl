open Lm_debug
open Lm_symbol
open Lm_printf
open Lm_string_set

open Term_sig
open Refiner.Refiner
open Term
open TermType
open TermOp
open TermSubst
open TermMan
open RefineError
open Opname
open Refiner.Refiner.TermType

open Jlogic_sig
open Jtypes

exception Not_unifiable
exception Failed

module JOrdering (JLogic : JLogicSig) =
struct

   module JTy = JTypes(JLogic)
   open JTy

(* make a term list out of a bterm list *)

   let rec collect_subterms tail = function
      [] -> tail
    | bt::r ->
         ((dest_bterm bt).bterm)::(collect_subterms tail r)

   let rec collect_delta_terms = function
      [] -> []
    | t::r ->
         let dt = dest_term t in
            if Opname.eq (dest_op dt.term_op).op_name jprover_op then
               (dest_string_param t)::(collect_delta_terms r)
            else
               collect_delta_terms (collect_subterms r dt.term_terms)

(* ***************** REDUCTION ORDERING -- both types **************************** *)

   exception Reflexive

   let rec transitive_irreflexive_closure addset const ordering =
      match ordering with
         [] ->
            []
       | ((pos,fset) as pos_fset)::r ->
            if (pos = const) or (StringSet.mem fset const) then
(* check reflexsivity during transitive closure wrt. addset ONLY!!! *)
               if StringSet.mem addset pos then
                  raise Reflexive
               else
                  (pos,(StringSet.union fset addset))::(transitive_irreflexive_closure addset const r)
            else
               pos_fset::(transitive_irreflexive_closure addset const r)

   let rec search_set var = function
      [] ->
         raise (Invalid_argument "Jprover: element in ordering missing")
    | (pos,fset)::r ->
         if pos = var then
            StringSet.add fset pos
         else
            search_set var r

   let add_sets var const ordering =
      let addset =  search_set var ordering in
      transitive_irreflexive_closure addset const ordering

(* ************* J ordering ********************************************** *)

   let rec add_arrowsJ v ordering = function
      [] -> ordering
    | f::r ->
         if ((String.get f 0)='c') then
            let new_ordering = add_sets v f ordering in
            add_arrowsJ v new_ordering r
         else
            add_arrowsJ v ordering r

   let rec add_substJ replace_vars replace_string ordering atom_rel =
      match replace_vars with
         [] -> ordering
       | v::r ->
            if (String.get v 1 = 'n') (* don't integrate new variables *)
                  or (List.exists (fun (x,_,_) -> (x.aname = v)) atom_rel) then   (* no reduction ordering at atoms *)
               (add_substJ r replace_string ordering atom_rel)
            else
               let next_ordering = add_arrowsJ v ordering replace_string in
               (add_substJ r replace_string next_ordering atom_rel)

   let build_orderingJ replace_vars replace_string ordering atom_rel =
      try
         add_substJ replace_vars replace_string ordering atom_rel
      with Reflexive ->        (* only possible in the FO case *)
         raise Not_unifiable    (*search for alternative string unifiers *)

   let rec build_orderingJ_list substJ ordering atom_rel =
      match substJ with
         [] -> ordering
       | (v,vlist)::r ->
            let next_ordering = build_orderingJ [v] vlist ordering atom_rel in
            build_orderingJ_list r next_ordering atom_rel

(* ************* J ordering  END ********************************************** *)

(* ************* quantifier ordering ********************************************** *)

   let rec add_arrowsQ v clist ordering =
      match clist with
         [] -> ordering
       | f::r ->
            let new_ordering = add_sets v f ordering in
            add_arrowsQ v r new_ordering

   let rec print_sigmaQ sigmaQ =
      match sigmaQ with
         [] ->
            print_endline "."
       | (v,term)::r ->
            begin
               open_box 0;
               print_endline " ";
               print_string (v^" = ");
               print_term stdout term;
               force_newline ();
               print_flush ();
               print_sigmaQ r
            end

   let rec print_term_list tlist =
      match tlist with
         [] -> print_string "."
       | t::r ->
            begin
               print_term stdout t;
               print_string "   ";
               print_term_list r
            end

   let rec add_sigmaQ new_elements ordering =
      match new_elements with
         [] -> ([],ordering)
       | (v,termlist)::r ->
            let dterms = collect_delta_terms termlist in
            begin
(*        open_box 0;
   print_endline " ";
   print_endline "sigmaQ: ";
   print_string (v^" = ");
   print_term_list termlist;
   force_newline ();
   print_stringlist dterms;
   force_newline ();
   print_flush ();
*)
               let new_ordering = add_arrowsQ v dterms ordering in
               let (rest_pairs,rest_ordering) = add_sigmaQ r new_ordering in
               ((v,dterms)::rest_pairs),rest_ordering
            end

   let build_orderingQ new_elements ordering =
(* new_elements is of type (string * term list) list, since one variable can receive more than *)
(* a single term due to substitution multiplication *)
      try
(*   print_endline "build orderingQ in"; *)
         add_sigmaQ new_elements ordering;
      with Reflexive ->
         raise Failed                (* new connection, please *)

(* ************* quantifier ordering  END ********************************************** *)

end
