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

type pos_kind = Dummy | Atom | Const | EigenVar | Var | NewVar | NewVarQ | GammaVar | Root
type position = pos_kind * int

let jsuffix = "_jprover"

let rec pos_to_string (kind,i) =
   let s = string_of_int i in
   match kind with
    | Root ->
         "w"
    | Atom ->
         "a"^s
    | Const ->
         "c"^s
    | EigenVar ->
         "Jprover_r"^s
    | NewVar ->
         "vnew"^s
    | Var ->
         "v"^s
    | Dummy ->
         ""
    | NewVarQ ->
         "vnewq"^s
    | GammaVar ->
         "v"^s^jsuffix

let rec string_to_pos s =
   let aux x =
      try int_of_string x with
         e ->
            raise (Invalid_argument ("Can't extract index from "^x))
   in
   if String.contains s '_' then
      let last = String.rindex s '_' in
      if String.rcontains_from s (pred last) '_' then
         raise (Invalid_argument ("Underscore occurs more than once: "^s))
      else
         if String.sub s last (String.length s - last) = jsuffix then
            let index = String.sub s 1 last in
            GammaVar, aux index
         else
            if String.sub s 0 9 = "Jprover_r" then
               EigenVar, aux (String.sub s 9 (String.length s - 9))
            else
               raise (Invalid_argument ("Unknown type of variable: "^s))
   else
      if s = "" then
         raise (Invalid_argument "Empty position string")
      else
         if (String.length s >= 4) && (String.sub s 0 4 = "vnew") then
            if String.get s 4 = 'q' then
               NewVarQ, aux (String.sub s 5 (String.length s - 5))
            else
               NewVar, aux (String.sub s 4 (String.length s - 4))
         else
            let sub = String.sub s 1 (String.length s - 1) in
            match String.get s 0 with
               'a' -> Atom, aux sub
             | 'v' -> Var, aux sub
             | 'c' -> Const, aux sub
             | 'w' -> Root, 0
             |  _  -> raise (Invalid_argument ("Unexpected code of position: "^s))

let gamma_to_simple p =
   match p with
      GammaVar, i ->
         Var, i
    | (Dummy | Atom | Const | EigenVar | Var | NewVar | NewVarQ | Root), _ ->
         let s = pos_to_string p in
         raise (Invalid_argument ("GammaVar is expected instead of: "^s))

module PosOrdering =
struct

   type t = position

   let rec compare (a,(i:int)) (b,j) =
      match a,b with
       | Dummy,Dummy -> Pervasives.compare i j
       | Dummy, _ -> -1
       | Atom,Dummy -> 1
       | Atom,Atom -> Pervasives.compare i j
       | Atom,_ -> -1
       | Const,(Dummy|Atom) -> 1
       | Const,Const -> Pervasives.compare i j
       | Const,_ -> -1
       | EigenVar,(Dummy|Atom|Const) -> 1
       | EigenVar, EigenVar -> Pervasives.compare i j
       | EigenVar, _ -> -1
       | Var,(Atom|Const|Dummy|EigenVar) -> 1
       | Var,Var -> Pervasives.compare i j
       | Var,_ -> -1
       | GammaVar,(Dummy|Atom|Const|EigenVar|Var) -> 1
       | GammaVar, GammaVar -> Pervasives.compare i j
       | GammaVar, _ -> -1
       | NewVar,(Atom|Const|Dummy|EigenVar|Var|GammaVar) -> 1
       | NewVar,NewVar -> Pervasives.compare i j
       | NewVar,_ -> -1
       | NewVarQ,(Atom|Const|Dummy|EigenVar|Var|GammaVar|NewVar) -> 1
       | NewVarQ,NewVarQ -> Pervasives.compare i j
       | NewVarQ,_ -> -1
       | Root, (Atom|Const|Dummy|EigenVar|NewVar|NewVarQ|Var|GammaVar) -> 1
       | Root,Root -> Pervasives.compare i j

end

module Set = Lm_set.LmMake(PosOrdering)

let list_pos_to_string = List.map pos_to_string
let list_string_to_pos = List.map string_to_pos
let set_pos_to_string set =
   StringSet.of_list (List.map pos_to_string (Set.to_list set))
let set_string_to_pos set =
   Set.of_list (List.map string_to_pos (StringSet.to_list set))

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
               let string_param = dest_string_param t in
               let pos = string_to_pos string_param in
               pos::(collect_delta_terms r)
            else
               collect_delta_terms (collect_subterms r dt.term_terms)

(* ***************** REDUCTION ORDERING -- both types **************************** *)

   exception Reflexive

   let rec transitive_irreflexive_closure addset const ordering =
      match ordering with
         [] ->
            []
       | ((pos,fset) as pos_fset)::r ->
            if (pos = const) or (Set.mem fset const) then
(* check reflexsivity during transitive closure wrt. addset ONLY!!! *)
               if Set.mem addset pos then
                  raise Reflexive
               else
                  (pos,(Set.union fset addset))::(transitive_irreflexive_closure addset const r)
            else
               pos_fset::(transitive_irreflexive_closure addset const r)

   let rec search_set var = function
      [] ->
         raise (Invalid_argument ("Jprover: element in ordering missing: "^(pos_to_string var)))
    | (pos,fset)::r ->
         if pos = var then
            Set.add fset pos
         else
            search_set var r

   let add_sets var const ordering =
      let addset =  search_set var ordering in
      transitive_irreflexive_closure addset const ordering

(* ************* J ordering ********************************************** *)

   let rec add_arrowsJ v ordering = function
      [] -> ordering
    | ((Const,i) as f)::r ->
         let new_ordering = add_sets v f ordering in
            add_arrowsJ v new_ordering r
    | _::r ->
         add_arrowsJ v ordering r

   let rec add_substJ replace_vars replace_string ordering atom_rel =
      match replace_vars with
         [] -> ordering
       | ((NewVar | NewVarQ),_)::r -> (* don't integrate new variables *)
            add_substJ r replace_string ordering atom_rel
       | v::r (* no reduction ordering at atoms *)
            when List.exists (fun (x,_,_) -> (x = v)) atom_rel ->
               add_substJ r replace_string ordering atom_rel
       | v::r ->
            let next_ordering = add_arrowsJ v ordering replace_string in
               add_substJ r replace_string next_ordering atom_rel

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
