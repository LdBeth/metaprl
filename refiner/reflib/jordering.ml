(*
 * JProver helper module.
 *
 * ----------------------------------------------------------------
 *
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/htmlman/default.html or visit http://metaprl.org/
 * for more information.
 *
 * Copyright (C) 2000-2005, MetaPRL Group
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
 * Author: Stephan Schmitt <schmitts@spmail.slu.edu>
 * Modified by: Aleksey Nogin <nogin@cs.cornell.edu>
 * Modified by: Yegor Bryukhov <ybryukhov@gc.cuny.edu>
 *
 *)
open Lm_debug
open Lm_symbol
open Lm_printf

open Refiner.Refiner
open Term

open Jlogic_sig
open Jtypes

let debug_s4prover =
   create_debug (**)
      { debug_name = "s4prover";
        debug_description = "Display S4-Jprover operations";
        debug_value = false
      }

let debug_jtunify =
   create_debug (**)
      { debug_name = "jtunify";
        debug_description = "Display J-Prover T-unification operations";
        debug_value = false
      }

exception Not_unifiable
exception Failed of int

let rec kind_to_string = function
 | Root ->
      "w"
 | Atom ->
      "a"
 | Const 0 ->
      "c"
 | Dummy ->
      "d"
 | EigenVar ->
      "e"
 | NewVar 0 ->
      "n"
 | Var 0 ->
      "v"
 | EmptyVar ->
      ""
 | NewVarQ 0 ->
      "q"
 | GammaPos k ->
      (kind_to_string k)^"_jprover"
 | Const i ->
 		"c"^(string_of_int i)^"_"
 | Var i ->
 		"v"^(string_of_int i)^"_"
 | NewVar i ->
 		"n"^(string_of_int i)^"_"
 | NewVarQ i ->
      "q"^(string_of_int i)^"_"
(*
 		raise (Invalid_argument
			"Modal positions are not supposed to be converted to symbols or strings")
*)

let rec string_to_kind s =
   let len = String.length s in
   if len = 0 then EmptyVar
   else
      if len > 1 then
         let sub = String.sub s 0 (len - 8) in
         GammaPos (string_to_kind sub)
      else
         match String.get s 0 with
            'a' -> Atom
          | 'c' -> Const 0
          | 'd' -> Dummy
          | 'e' -> EigenVar
          | 'n' -> NewVar 0
          | 'q' -> NewVarQ 0
          | 'v' -> Var 0
          | 'w' -> Root
          |  _  ->
               raise (Invalid_argument ("Unknown kind of position: "^s))

(*
let safe_a2i x =
   try int_of_string x with
      e ->
         raise (Invalid_argument ("Can't extract index from "^x))

let rec extract_gamma k s =
   let len = String.length s in
   if len > 8 then
      extract_gamma (GammaPos k) (String.sub s 8 (len - 8))
   else
      k, safe_a2i s

let string_to_pos s =
   let len = String.length s in
   if len = 0 then EmptyVar,0
   else
      if len > 1 then
         match String.get s 1 with
            '0'..'9' ->
               let n = safe_a2i (String.sub s 1 (len-1)) in
               begin match String.get s 0 with
                  'a' -> Atom,n
                | 'c' -> Const 0, n
                | 'd' -> Dummy,n
                | 'e' -> EigenVar,n
                | 'n' -> NewVar 0, n
                | 'q' -> NewVarQ,n
                | 'v' -> Var 0, n
                |  _  ->
                     raise (Invalid_argument ("Unknown kind of position: "^s))
               end
          | '_' ->
               let sub = String.sub s 1 (len-1) in
               let k =
                  match String.get s 0 with
                     'a' -> Atom
                   | 'c' -> Const 0
                   | 'd' -> Dummy
                   | 'e' -> EigenVar
                   | 'n' -> NewVar 0
                   | 'q' -> NewVarQ
                   | 'v' -> Var 0
                   |  _  ->
                     raise (Invalid_argument ("Unexpected kind of position: "^s))
               in
               extract_gamma k sub
          |  _  ->
               raise (Invalid_argument ("Unknown kind of position: "^s))
      else
         if s="w" then
            Root, 0
         else
            raise (Invalid_argument ("Unknown kind of position: "^s))

let string_to_symbol s = pos_to_symbol (string_to_pos s)

let string_to_gamma s =
   pos_to_string (simple_to_gamma (string_to_pos s))

*)

let pos_to_symbol (k,i) =
   Lm_symbol.make (kind_to_string k) i

let symbol_to_pos sym =
   let k = string_to_kind (to_string sym) in
   match k with
      Atom | Const _ | Dummy | Var _ | NewVar _ | NewVarQ _ | EigenVar | GammaPos _ ->
         k, to_int sym
    | Root | EmptyVar ->
         k, 0
(*
	 | Var _ | Const _ | NewVar _ ->
	 		raise (Invalid_argument
				"Modal positions are not supposed to be converted to/from terms")
*)

let pos_to_string (kind,i) =
   let si = string_of_int i in
   let sk = kind_to_string kind in
   match kind with
      Atom
    | Const _
    | Dummy
    | EigenVar
    | NewVar _
    | Var _
    | NewVarQ _
    | GammaPos _ ->
         sk^si
    | EmptyVar | Root ->
         sk

let gamma_to_simple p =
   let k, i = p in
   match k with
      GammaPos k ->
         k, i
    | EmptyVar|Atom|Const _|Dummy|EigenVar|
      Var _|NewVar _|NewVarQ _|Root ->
         let s = pos_to_string p in
         raise (Invalid_argument ("GammaPos was expected instead of: "^s))

let simple_to_gamma (k,i) =
   GammaPos k, i

let empty_pos = (EmptyVar, 0)
let empty_sym = pos_to_symbol empty_pos
let root_pos = (Root, 0)
let dummy_pos _ = (Dummy, Lm_symbol.new_number ())

module PosOrdering =
struct

   type t = position
(*
   let rec compare_kinds a b =
      match a,b with
       | GammaPos a, GammaPos b ->
            compare_kinds a b
       | GammaPos a,
		 	(EmptyVar|Atom|Const _|Dummy|EigenVar|Var _|NewVar _|NewVarQ _|Root) ->
            let c = compare_kinds a b in
            if c = 0 then
               -1
            else
               c
       | (EmptyVar|Atom|Const _|Dummy|EigenVar|Var _|NewVar _|NewVarQ _|Root),
		 	GammaPos b ->
            let c = compare_kinds a b in
            if c = 0 then
               1
            else
               c
       | EmptyVar,EmptyVar -> 0
       | EmptyVar, _ -> -1
       | Atom,EmptyVar -> 1
       | Atom,Atom -> 0
       | Atom,_ -> -1
       | Const _,(EmptyVar|Atom) -> 1
       | Const i, Const j -> Stdlib.compare i j
       | Const _, _ -> -1
       | Dummy,(EmptyVar|Atom|Const _) -> 1
       | Dummy, Dummy -> 0
       | Dummy, _ -> -1
       | EigenVar,(EmptyVar|Atom|Const _|Dummy) -> 1
       | EigenVar, EigenVar -> 0
       | EigenVar, _ -> -1
       | NewVar _,(Atom|Const _|Dummy|EmptyVar|EigenVar) -> 1
       | NewVar i, NewVar j -> Stdlib.compare i j
       | NewVar _, _ -> -1
       | NewVarQ _, (Atom|Const _|Dummy|EmptyVar|EigenVar|NewVar _) -> 1
       | NewVarQ i, NewVarQ j -> Stdlib.compare i j
       | NewVarQ _, _ -> -1
       | Var _,(Atom|Const _|Dummy|EmptyVar|EigenVar|NewVar _|NewVarQ _) -> 1
       | Var i, Var j -> Stdlib.compare i j
       | Var _, _ -> -1
       | Root,
         (Atom|Const _|Dummy|EmptyVar|EigenVar
         |NewVar _|NewVarQ _|Var _) -> 1
       | Root,Root -> 0
*)
   let compare (a,(i:int)) (b,j) =
		Stdlib.compare i j

end

module Set = Lm_set.LmMake(PosOrdering)

module PMap = Lm_map.LmMake(PosOrdering)

let position_eq (p1: position) p2 =
   PosOrdering.compare p1 p2 = 0

let nodups k a b =
   if a = b then a else raise (Invalid_argument "no dupes allowed in map")

let rec is_const (k,i)  =
  match k with
     GammaPos k -> is_const (k,i)
   | Const _ | EigenVar -> true
   | Atom | EmptyVar | Root
   | Var _ | NewVar _ | NewVarQ _ | Dummy -> false

let rec is_var (k,i)  =
  match k with
     GammaPos k -> is_var (k,i)
   | Var _ | NewVar _ | NewVarQ _ -> true
   | Atom | Const _ | Dummy | EigenVar | EmptyVar | Root -> false

let list_pos_to_string = List.map pos_to_string
(*
let list_string_to_pos = List.map string_to_pos

let set_pos_to_string set =
   StringSet.of_list (List.map pos_to_string (Set.to_list set))
let set_string_to_pos set =
   Set.of_list (List.map string_to_pos (StringSet.to_list set))
*)

let rec print_stringlist slist =
   match slist with
      [] ->
         print_string ""
    | f::r ->
         begin
            let f = pos_to_string f in
            print_string (f^".");
            print_stringlist r
         end

let print_ordering_item pos fset =
   let pos = pos_to_string pos in
   print_string (pos^": ");   (* first element = node which successors depend on *)
   print_stringlist (Set.elements fset);
   force_newline ()

let rec print_list_sets list_of_sets =
   match list_of_sets with
      [] -> print_string ""
    | (pos,fset)::r ->
         begin
            print_ordering_item pos fset;
            print_list_sets r
         end

let print_ordering list_of_sets =
   begin
      open_box 0;
      print_list_sets list_of_sets;
      print_flush ()
   end

let print_ordering_map map =
   if PMap.is_empty map then
      print_endline "empty ordering map"
   else
      PMap.iter (fun p set -> print_ordering_item p set) map;
   print_flush ()


module MkJOrdering (JLogic : JLogicSig) =
struct

   module JTy = MkJTypes(JLogic)

   let rec collect_delta_terms consts accumulator = function
      [] ->	accumulator
    | t::r ->
	 		let all_vars = TermSubst.free_vars_set t in
			let jprover_vars = SymbolSet.diff all_vars consts in
			let delta_set =
				SymbolSet.fold
					(fun acc v ->
						let p = symbol_to_pos v in
						if is_const p then
							Set.add acc p
						else
							acc
					)
					accumulator
					jprover_vars
			in
         collect_delta_terms consts delta_set r

(* ***************** REDUCTION ORDERING -- both types **************************** *)

   exception Reflexive

   let transitive_irreflexive_closure_aux addset const pos fset =
      if (pos = const) or (Set.mem fset const) then
(* check reflexsivity during transitive closure wrt. addset ONLY!!! *)
         if Set.mem addset pos then
            raise Reflexive
         else
            Set.union fset addset
      else
         fset

   let transitive_irreflexive_closure addset const ordering =
      PMap.mapi (transitive_irreflexive_closure_aux addset const) ordering

   let search_set var ordering =
      let fset = PMap.find ordering var in
      Set.add fset var

   let add_sets var const ordering =
      let addset =  search_set var ordering in
      transitive_irreflexive_closure addset const ordering

(* ************* J ordering ********************************************** *)

   let rec add_arrowsJ v ordering = function
      [] -> ordering
    | ((Const _,i) as f)::r ->
         let new_ordering = add_sets v f ordering in
            add_arrowsJ v new_ordering r
    | _::r ->
         add_arrowsJ v ordering r

   let rec add_substJ calculus replace_vars replace_string ordering atom_set =
      match replace_vars, calculus with
         [],_ -> ordering
       | ((NewVar _| NewVarQ _),_)::r, _ -> (* don't integrate new variables *)
            add_substJ calculus r replace_string ordering atom_set
       | v::r, Intuit _ (* no reduction ordering at atoms *)
            when Set.mem atom_set v ->
               if !debug_jtunify then
                  print_endline ("no reduction ordering at atoms: "^(pos_to_string v));
               add_substJ calculus r replace_string ordering atom_set
       | v::r, _ ->
            let next_ordering = add_arrowsJ v ordering replace_string in
               add_substJ calculus r replace_string next_ordering atom_set

   let build_orderingJ calculus replace_vars replace_string ordering atom_set =
      try
         add_substJ calculus replace_vars replace_string ordering atom_set
      with Reflexive ->        (* only possible in the FO case *)
         raise Not_unifiable    (*search for alternative string unifiers *)

   let rec build_orderingJ_list calculus substJ ordering atom_set =
      match substJ with
         [] -> ordering
       | (v,vlist)::r ->
            let next_ordering = build_orderingJ calculus [v] vlist ordering atom_set in
            build_orderingJ_list calculus r next_ordering atom_set

(* ************* J ordering  END ********************************************** *)

(* ************* quantifier ordering ********************************************** *)

   let add_arrowsQ v clist ordering =
      Set.fold (fun acc p -> add_sets v p acc) ordering clist

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

   let rec add_sigmaQ consts new_elements ordering =
      match new_elements with
         [] -> ([],ordering)
       | (v,termlist)::r ->
            let dterms = collect_delta_terms consts Set.empty termlist in
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
               let (rest_pairs,rest_ordering) = add_sigmaQ consts r new_ordering in
               ((v,dterms)::rest_pairs),rest_ordering
            end

   let build_orderingQ consts new_elements ordering counter =
(* new_elements is of type (string * term list) list, since one variable can receive more than *)
(* a single term due to substitution multiplication *)
      try
(*   print_endline "build orderingQ in"; *)
         add_sigmaQ consts new_elements ordering;
      with Reflexive ->
         raise (Failed(counter))                (* new connection, please *)

(* ************* quantifier ordering  END ********************************************** *)

end
