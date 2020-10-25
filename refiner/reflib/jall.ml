(*
 * JProver first-order automated prover. See the interface file
 * for more information let a list of references for JProver.
 *
 * ----------------------------------------------------------------
 *
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml let other languages.
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
 *)

 (*
  * TODO:
  * 1. add predecessor to pos type
  * 2. optimize S4-computation in blocked
  *)
open Lm_debug
open Lm_symbol
open Lm_printf

open Refiner.Refiner
open Term
open TermType
open TermSubst
open TermMan
open RefineError

open Unify_mm

open Jlogic_sig
open Jtypes
open Jordering
open Jtunify

let _ =
   show_loading "Loading Jall%t"

let debug_jprover =
   create_debug (**)
      { debug_name = "jprover";
        debug_description = "Display Jprover operations";
        debug_value = false
      }

(* unused
let debug_jtunify =
   create_debug (**)
      { debug_name = "jtunify";
        debug_description = "Display J-Prover T-unification operations";
        debug_value = false
      }
*)

let debug_profile_tactics =
   create_debug (**)
      { debug_name = "profile_tactics";
        debug_description = "Collect and report profiling information for top-level tactics (not theads-safe)";
        debug_value = false
      }

(************************************************************************
 * Compatibility layer for abstract vars.
 *)
let free_vars_list t consts =
   SymbolSet.fold (fun acc v ->
         if is_var (symbol_to_pos v) then
            v::acc
         else
            acc) [] (SymbolSet.diff (free_vars_set t) consts)

(* unused
let mk_symbol_term opname s =
   let p = make_param (Term_sig.Var s) in
   let op = mk_op opname [p] in
      mk_any_term op []

let mk_pos_term opname p = mk_symbol_term opname (pos_to_symbol p)
*)

let mk_pos_var p =
   Term.mk_var_term (pos_to_symbol p)

(************************************************************************
 * Original JProver.
 *)

let ruletable = function
   Fail -> "Fail"
 | Ax ->     "Ax"
 | Negl ->  "Negl"
 | Negr ->  "Negr"
 | Andl ->  "Andl"
 | Andr ->  "Andr"
 | Orl ->   "Orl"
 | Orr ->   "Orr"
 | Orr1 ->   "Orr1"
 | Orr2 ->   "Orr2"
 | Impl ->   "Impl"
 | Impr ->   "Impr"
 | Exl ->   "Exl"
 | Exr ->   "Exr"
 | Alll ->   "Alll"
 | Allr ->   "Allr"
 | Boxl -> "Boxl"
 | Boxr -> "Boxr"

module JProver (JLogic : JLogicSig) =
struct
   module JTy = MkJTypes(JLogic)
   module JOrder = MkJOrdering(JLogic)
(* unused
   module JQuantifier = JQuantifier(JLogic)
*)
   module JTUnifyQ = JTUnifyQ(JLogic)
   module JTUnifyProp = JTUnifyProp(JLogic)

   open JTy
   open JOrder
(* unused
   open JQuantifier
*)
   open JTUnifyQ

   module OrderedAtom =
   struct
      type t = atom

		let compare a1 a2 = PosOrdering.compare a1.apos a2.apos
   end

   module AtomSet = Lm_set.LmMake(OrderedAtom)
	module AtomMap = Lm_map.LmMake(OrderedAtom)

   (*
   * It seems that connection set should be either unordered with O(1) union
   * operation or a sparse symmetrical(?) bit matrix (position * position) with
   * fast search in a row and in a column
    *)
   module OrderedConnection =
   struct
      type t = position * position

      let sort_pair p =
         let a, b = p in
         if PosOrdering.compare a b <= 0 then
            p
         else
            b,a

     let compare a1 a2 =
        let a11,a12 = sort_pair a1 in
        let a21,a22 = sort_pair a2 in
        let c = PosOrdering.compare a11 a21 in
        if c = 0 then
           PosOrdering.compare a12 a22
        else
           c
   end

   module ConnSet = Lm_set.LmMake(OrderedConnection)

   module OrderedPositionPair =
   struct
      type t= position * position

      let compare (a11,a12) (a21,a22) =
         let c = PosOrdering.compare a11 a21 in
         if c = 0 then
            PosOrdering.compare a12 a22
         else
            c
   end

   module Rel = Lm_set.LmMake(OrderedPositionPair)

	module OrderedPos =
	struct
		type t = pos

		let compare a b = PosOrdering.compare a.pospos b.pospos
	end

	module PosSet = Lm_set.LmMake(OrderedPos)

	module OrdSubtree =
   struct
		type t = (position * position) * int

		let compare (p1,i1) (p2,i2) =
			let c = Stdlib.compare (i1:int) i2 in
			if c = 0 then
				OrderedPositionPair.compare p1 p2
			else
				c
	end

	module SubrelSet = Lm_set.LmMake(OrdSubtree)

(* unused
   let atom_eq a1 a2 =
      position_eq a1.apos a2.apos
*)

   let pos_eq p1 p2 =
      position_eq p1.pospos p2.pospos

   let rec ftree_eq t1 t2 =
      match t1, t2 with
         Empty, Empty -> true
       | NodeAt p1, NodeAt p2 -> pos_eq p1 p2
       | NodeA (p1, pa1), NodeA (p2, pa2) -> pos_eq p1 p2 && Lm_list_util.for_all2 ftree_eq pa1 pa2
       | _ -> false

   let rule_eq (a1, (b1 : rule), c1, d1) (a2, b2, c2, d2) =
      a1 = a2 && b1 = b2 && alpha_equal c1 c2 && alpha_equal d1 d2

(*****************************************************************)

(************* printing function *************************************)

(************ printing T-string unifiers ****************************)

(* ******* printing ********** *)

(* unused
   let rec list_to_string s =
      match s with
         [] -> ""
       | f::r ->
            f^"."^(list_to_string r)

   let rec print_eqlist eqlist =
      match eqlist with
         [] ->
            print_endline ""
       | (atnames,f)::r ->
            let (s,t) = f in
            let ls = list_to_string s in
            let lt = list_to_string t in
            begin
               print_endline ("Atom names: "^(list_to_string atnames));
               print_endline (ls^" = "^lt);
               print_eqlist r
            end

  let print_equations eqlist =
      begin
         open_box 0;
         force_newline ();
         print_endline "Equations:";
         print_eqlist eqlist;
         force_newline ();
      end

   let rec print_subst sigma =
      match sigma with
         [] ->
            print_endline ""
       | f::r ->
            let (v,s) = f in
            let ls = list_to_string s in
            begin
               print_endline (v^" = "^ls);
               print_subst r
            end

   let print_tunify sigma =
      let (n,subst) = sigma in
      begin
         print_endline " ";
         print_endline ("MaxVar = "^(string_of_int (n-1)));
         print_endline " ";
         print_endline "Substitution:";
         print_subst subst;
         print_endline " "
      end
*)

(*****************************************************)

(********* printing atoms let their relations ***********************)

(* unused
   let print_stype st =
      match st with
         Alpha_1 -> print_string "Alpha_1"
       | Alpha_2 -> print_string "Alpha_2"
       | Beta_1  -> print_string "Beta_1"
       | Beta_2  -> print_string "Beta_2"
       | Gamma_0 -> print_string "Gamma_0"
       | Delta_0 -> print_string "Delta_0"
       | Phi_0   -> print_string "Phi_0"
       | Psi_0   -> print_string "Psi_0"
       | PNull_0 -> print_string "PNull_0"
       | Pi_0 i  -> print_string ("Pi_0"^(string_of_int i))
       | Nu_0 i  -> print_string ("Nu_0"^(string_of_int i))

   let print_pol pol =
      match pol with
         Zero ->
            print_string "Zero"
       | One ->
            print_string "One"

   let rec print_address int_list =
      match int_list with
         [] ->
            print_string ""
       | hd::rest ->
            begin
               print_int hd;
               print_address rest
            end

   let rec print_prefix prefix_list =
      match prefix_list with
         [] -> print_string ""
       | f::r ->
            begin
               print_string f;
               print_prefix r
            end

   let print_atom at tab =
      let ({apos=pos; aaddress=y; apredicate=p; apol=a; ast=b; alabel=label}) = at in
      begin
         print_string ("{aname="^(pos_to_string pos)^"; address=");
         print_address y;
         print_string "; ";
         force_newline ();
         print_break (tab+1) (tab+1);
(*         print_string "prefix=";
         print_prefix z;
*)
         print_string "; predicate=<abstr>; ";
         print_break (tab+1) (tab+1);
         print_break (tab+1) (tab+1);
         print_string "pol=";
         print_pol a;
         print_string "; stype=";
         print_stype b;
         print_string "; arguments=[<abstr>]";
         print_string "}"
      end

   let rec print_atom_list set tab =
      match set with
         []  -> print_string ""
       | (f::r) ->
            begin
               force_newline ();
               print_break (tab) (tab);
               print_atom f tab;
               print_atom_list r (tab)
            end

   let rec print_atom_info atom_relation =
      match atom_relation with
         [] -> print_string ""
       | (a,b,c)::r ->
            begin
               print_string "atom:";
               force_newline ();
               print_break 3 3;
               print_atom a 3;
               force_newline ();
               print_break 0 0;
               print_string "alpha_set:";
               print_atom_list b 3;
               force_newline ();
               print_break 0 0;
               print_string "beta_set:";
               print_atom_list c 3;
               force_newline ();
               force_newline ();
               print_break 0 0;
               print_atom_info r
            end
*)

(*************** print formula tree, tree ordering etc. ***********)

(* unused
   let print_ptype pt =
      match pt with
         Alpha -> print_string "Alpha"
       | Beta  -> print_string "Beta"
       | Gamma -> print_string "Gamma"
       | Delta -> print_string "Delta"
       | Phi   -> print_string "Phi"
       | Psi   -> print_string "Psi"
       | PNull -> print_string "PNull"
       | Nu i  -> print_string ("Nu"^(string_of_int i))
       | Pi i  -> print_string ("Pi"^(string_of_int i))

   let print_op op =
      match op with
         At   -> print_string "Atom"
       | Neg  -> print_string "Neg"
       | And  -> print_string "And"
       | Or   -> print_string "Or"
       | Imp  -> print_string "Imp"
       | Ex   -> print_string "Ex"
       | All  -> print_string "All"
       | Null -> print_string "Null"
       | Box i  -> print_string ("Box"^(string_of_int i))

   let print_position position tab =
      let ({address=y; op=z; pol=a; pt=b; st=c; label=t}) = position in
      begin
         (*print_string ("{name="^x^"; address=");*)
         print_address y;
         print_string "; ";
         force_newline ();
         print_break (tab+1) 0;
(*   print_break 0 3; *)
         print_string "op=";
         print_op z;
         print_string "; pol=";
         print_pol a;
         print_string "; ptype=";
         print_ptype b;
         print_string "; stype=";
         print_stype c;
         print_string ";";
         force_newline ();
         print_break (tab+1) 0;
         print_string "label=";
         print_break 0 0;
         force_newline ();
         print_break tab 0;
         print_term stdout t;
         print_string "}"
      end

  let rec pp_ftree_list tree_list tab =
      let rec pp_ftree ftree new_tab =
         let dummy = String.make (new_tab-2) ' ' in
         match ftree with
            Empty -> print_string ""
          | NodeAt(position) ->
               begin
                  force_newline ();
                  print_break new_tab 0;
                  print_string (dummy^"AtomNode: ");
(*      force_newline ();
   print_break 0 3;
*)
                  print_position position new_tab;
                  force_newline ();
                  print_break new_tab 0
               end
          | NodeA(position,subtrees) ->
               begin
                  force_newline ();
                  print_break new_tab 0;
                  print_break 0 0;
                  print_string (dummy^"InnerNode: ");
                  print_position position new_tab;
                  force_newline ();
                  print_break 0 0;
                  pp_ftree_list subtrees (new_tab-3)
               end
      in
      let new_tab = tab+5 in
      match tree_list with
         [] -> print_string ""
       | first::rest ->
            begin
               pp_ftree first new_tab;
               pp_ftree_list rest tab
            end

   let print_ftree ftree =
      begin
         open_box 0;
         print_break 3 0;
         pp_ftree_list [ftree] 0;
         print_flush ()
      end

   let rec stringlist_to_string stringlist =
      match stringlist with
         [] -> "."
       | f::r ->
            let rest_s = stringlist_to_string r in
            ((pos_to_string f)^"."^rest_s)

   let rec print_poslist slist =
      match slist with
         [] ->
            print_endline " ";
            print_flush ()
       | f::r ->
            begin
               let f = pos_to_string f.pospos in
               print_string (f^".");
               print_poslist r
            end
*)

   let print_positionset set =
      Set.iter (fun p -> print_string ((pos_to_string p)^".")) set;
      print_endline ".";
      print_flush ()

   let print_pos_set set =
      PosSet.iter (fun p -> print_string ((pos_to_string p.pospos)^".")) set;
      print_endline ".";
      print_flush ()

(* unused
   let rec pp_bproof_list tree_list tab =
      let rec pp_bproof ftree new_tab =
         let dummy = String.make (new_tab-2) ' ' in
         match ftree with
            BEmpty -> print_string ""
          | CNode((c1,c2)) ->
               begin
                  open_box 0;
                  force_newline ();
                  print_break (new_tab-10) 0;
                  open_box 0;
                  force_newline ();
                  print_string
							(dummy^"CloseNode: connection = ("^(pos_to_string c1)^","^(pos_to_string c2)^")");
                  print_flush();
(*      force_newline ();
   print_break 0 3;
*)
                  open_box 0;
                  print_break new_tab 0;
                  print_flush()
               end
          | AtNode(posname,(c1,c2)) ->
               begin
                  open_box 0;
                  force_newline ();
                  print_break (new_tab-10) 0;
                  open_box 0;
                  force_newline ();
                  print_string (dummy^"AtNode: pos = "^(pos_to_string posname)^" conneciton = ("^(pos_to_string c1)^","^(pos_to_string c2)^")");
                  print_flush();
(*      force_newline ();
   print_break 0 3;
*)
                  open_box 0;
                  print_break new_tab 0;
                  print_flush()
               end
          | RNode(alpha_layer,bproof) ->
               let alpha_string = stringlist_to_string alpha_layer in
               begin
                  open_box 0;
                  force_newline ();
                  print_break new_tab 0;
                  print_break 0 0;
                  force_newline ();
                  print_flush();
                  open_box 0;
                  print_string (dummy^"RootNode: "^alpha_string);
                  print_flush();
                  open_box 0;
                  print_break 0 0;
                  print_flush();
                  pp_bproof_list [bproof] (new_tab-3)
               end
          | BNode(posname,(alph1,bproof1),(alph2,bproof2)) ->
               let alpha_string1 = stringlist_to_string alph1 in
               let alpha_string2 = stringlist_to_string alph2 in
               begin
                  open_box 0;
                  force_newline ();
                  print_break new_tab 0;
                  print_break 0 0;
                  force_newline ();
                  print_flush();
                  open_box 0;
                  print_string (dummy^"BetaNode: pos = "^(pos_to_string posname)^" layer1 = "^alpha_string1^" layer2 = "^alpha_string2);
                  print_flush();
                  open_box 0;
                  print_break 0 0;
                  print_flush();
                  pp_bproof_list [bproof1;bproof2] (new_tab-3)
               end
      in
      let new_tab = tab+5 in
      match tree_list with
         [] -> print_string ""
       | first::rest ->
            begin
               pp_bproof first new_tab;
               pp_bproof_list rest tab
            end

   let rec print_pairlist pairlist =
      match pairlist with
         [] -> print_string ""
       | (a,b)::rest ->
            begin
               print_break 1 1;
               print_string ("("^a^","^b^")");
               print_pairlist rest
            end

   let print_beta_proof bproof =
      begin
         open_box 0;
         force_newline ();
         force_newline ();
         print_break 3 0;
         pp_bproof_list [bproof] 0;
         force_newline ();
         force_newline ();
         force_newline ();
         print_flush ()
      end

   let rec print_treelist treelist =
      match treelist with
         [] ->
            print_endline "END";
       | f::r ->
            begin
               print_ftree f;
               open_box 0;
               print_endline "";
               print_endline "";
               print_endline "NEXT TREE";
               print_endline "";
               print_endline "";
               print_treelist r;
               print_flush ()
            end

   let rec print_set_list set_list =
      match set_list with
         [] -> ""
       | f::r ->
            (pos_to_string f.apos)^" "^(print_set_list r)

   let print_set set =
      let set_list = AtomSet.elements set in
      if set_list = [] then "empty"
      else
         print_set_list set_list

   let print_string_set set =
      let set_list = Set.elements set in
      print_stringlist set_list

   let rec print_triplelist triplelist =
      match triplelist with
         [] -> print_string ""
       | ((a,b),i)::rest ->
            begin
               print_break 1 1;
               print_string ("(("^a^","^b^"),"^(string_of_int i)^")");
               print_triplelist rest
            end

   let print_pos_n pos_n =
      print_int pos_n

   let print_formula_info ftree ordering pos_n =
      begin
         print_ftree ftree;
         open_box 0;
         force_newline ();
         print_ordering ordering;
         force_newline ();
         force_newline ();
         print_string "number of positions: ";
         print_pos_n pos_n;
         force_newline ();
         print_endline "";
         print_endline "";
         print_flush ()
      end

(* print sequent proof tree *)

   let pp_rule (pos,r,formula,term) tab =
      let pos = pos_to_string pos in
      let rep = ruletable r in
      if List.mem rep ["Alll";"Allr";"Exl";"Exr"] then
         begin
            open_box 0;
(*          force_newline (); *)
            print_break tab 0;
            print_string (pos^": "^rep^" ");
            print_flush ();
(*          print_break tab 0;
   force_newline ();
   print_break tab 0;
*)

            open_box 0;
            print_term stdout formula;
            print_flush ();
            open_box 0;
            print_string "  ";
            print_flush ();
            open_box 0;
            print_term stdout term;
            force_newline ();
            force_newline ();
            print_flush ()
         end
      else
         begin
            open_box 0;
            print_break tab 0;
            print_string (pos^": "^rep^" ");
            print_flush ();
            open_box 0;
(*          print_break tab 0; *)
            force_newline ();
(*          print_break tab 0; *)
            print_term stdout formula;
            force_newline ()
         end

   let rec tpp seqtree tab addr =
      match seqtree with
       | PEmpty -> raise (Invalid_argument "jall.tpp: bug")
       | PNodeAx(rule) ->
            let (pos,r,p,pa) = rule in
            begin
               pp_rule (pos,r,p,pa) tab;
(*      force_newline (); *)
(*      let mult = get_r_chain addr in *)
(*        print_break 100 (tab - (3 * mult)) *)
            end
       | PNodeA(rule,left) ->
            let (pos,r,p,pa) = rule in
            begin
               pp_rule (pos,r,p,pa) tab;
               tpp left tab addr
            end
       | PNodeB(rule,left,right) ->
            let (pos,r,p,pa) = rule in
            let newtab = tab + 3 in
            begin
               pp_rule (pos,r,p,pa) tab;
(*             force_newline (); *)
(*             print_break 100 newtab; *)
               (tpp left newtab (Left :: addr));
               (tpp right newtab (Right :: addr))
            end

   let tt seqtree =
      begin
         open_box 0;
         tpp seqtree 0 [];
         force_newline ();
         close_box ();
         print_newline ()
      end
*)

(************ END printing functions  *********************************)

(************ Beta proofs let redundancy deletion **********************)

   let beta_pure alpha_layer connections beta_expansions =
      begin
(*       open_box 0;
         print_endline "";
         print_stringlist alpha_layer;
         print_flush();
         open_box 0;
         print_endline "";
         print_stringlist test_list;
         print_endline "";
         print_flush();
*)
         not (List.exists
            (fun x ->
               Set.mem beta_expansions x or
               ConnSet.exists (fun (a,b) -> position_eq a x || position_eq b x) connections
            )
            alpha_layer
         )
      end

   let rec apply_bproof_purity bproof =
      match bproof with
         BEmpty ->
            raise (Invalid_argument "jall.apply_bproof_purity: bug")
       | CNode((c1,c2)) ->
            bproof,ConnSet.singleton (c1,c2),Set.empty
       | AtNode(_,(c1,c2)) ->
            bproof,ConnSet.singleton (c1,c2),Set.empty
       | RNode(alpha_layer,subproof) ->
            let opt_subproof,min_connections,beta_expansions =
               apply_bproof_purity subproof in
            RNode(alpha_layer,opt_subproof),min_connections,beta_expansions
       | BNode(pos,(alph1,subp1),(alph2,subp2)) ->
            let opt_subp1,min_conn1,beta_exp1 = apply_bproof_purity subp1 in
            if beta_pure alph1 min_conn1 beta_exp1 then
               begin
(*       print_endline ("Left layer of "^pos); *)
                  opt_subp1,min_conn1,beta_exp1
               end
            else
               let opt_subp2,min_conn2,beta_exp2 = apply_bproof_purity subp2 in
               if beta_pure alph2 min_conn2 beta_exp2 then
                  begin
(*       print_endline ("Right layer of "^pos); *)
                     opt_subp2,min_conn2,beta_exp2
                  end
               else
                  let min_conn = ConnSet.union min_conn1 min_conn2 in
						let beta_exp = Set.add (Set.union beta_exp1 beta_exp2) pos in
                  BNode(pos,(alph1,opt_subp1),(alph2,opt_subp2)),min_conn,beta_exp

   let bproof_purity bproof =
      let (opt_bproof,min_connections,_) = apply_bproof_purity bproof in
      opt_bproof,min_connections

(*********** split permutation *****************)

   let rec apply_permutation bproof rep_name direction act_blayer =
      match bproof with
         BEmpty | RNode(_,_) ->
            raise (Invalid_argument "jall.apply_permutation: bug")
       | AtNode(cx,(c1,c2)) ->
            bproof,act_blayer
       | CNode((c1,c2)) ->
            bproof,act_blayer
       | BNode(pos,(alph1,subp1),(alph2,subp2)) ->
            if position_eq rep_name pos then
               let new_blayer, replace_branch =
                  match direction with
                     Left -> alph1, subp1
                   | Right -> alph2, subp2
               in
               (match replace_branch with
                  CNode((c1,c2)) ->
                     (AtNode(c1,(c1,c2))),new_blayer (* perform atom expansion at c1 *)
                | _ ->
                     replace_branch,new_blayer
               )
            else
               let pproof1,new_blayer1 = apply_permutation subp1 rep_name direction act_blayer in
               let pproof2,new_blayer2 = apply_permutation subp2 rep_name direction new_blayer1 in
               (BNode(pos,(alph1,pproof1),(alph2,pproof2))),new_blayer2

   let split_permutation pname opt_bproof =
      match opt_bproof with
         RNode(alayer,BNode(pos,(alph1,opt_subp1),(alph2,opt_subp2))) ->
            if position_eq pos pname then
(* if topmost beta expansion agrees with pname, then *)
(* only split the beta proof let give back the two subproofs *)
               let (osubp1,min_con1) = bproof_purity opt_subp1 in
               let (osubp2,min_con2) = bproof_purity opt_subp2 in
(* there will be no purity reductions in the beta subproofs. We use this *)
(* predicate to collect the set of used leaf-connections in each subproof*)
               ((RNode((List.rev_append alayer alph1),osubp1),min_con1),
                (RNode((List.rev_append alayer alph2),osubp2),min_con2)
               )
(* we combine the branch after topmost beta expansion at pos into one root alpha layer *)
(* -- the beta expansion node pos will not be needed in this root layer *)
            else
               let perm_bproof1,balph1 = apply_permutation
                     (BNode(pos,(alph1,opt_subp1),(alph2,opt_subp2))) pname Left []
               in
               let perm_bproof2,balph2 = apply_permutation
                     (BNode(pos,(alph1,opt_subp1),(alph2,opt_subp2))) pname Right []
               in
               begin
(*                print_endline " ";
                  print_beta_proof perm_bproof1;
                  print_endline" " ;
                  print_beta_proof perm_bproof2;
                  print_endline" ";
*)
                  let (osubp1,min_con1) = bproof_purity perm_bproof1 in
                  let (osubp2,min_con2) = bproof_purity perm_bproof2 in
                  ((RNode((List.rev_append alayer balph1),osubp1),min_con1),
                   (RNode((List.rev_append alayer balph2),osubp2),min_con2)
                  )
               end
(* we combine the branch after the NEW topmost beta expansion at bpos *)
(* into one root alpha layer -- the beta expansion node bpos will not be *)
(* needed in this root layer *)
       | _ ->
            raise (Invalid_argument "jall.split_permutation: bug")

(*********** END split permutation *****************)

   let rec list_diff del_list check_list =
      match del_list with
         [] ->
            []
       | f::r ->
            if List.mem f check_list then
               list_diff r check_list
            else
               f::(list_diff r check_list)

(* let rec compute_alpha_layer ftree_list =
      match ftree_list with
         [] ->
            [],[],[]
       | f::r ->
            (match f with
               Empty ->
                  raise jprover_bug
             | NodeAt(pos) ->
                  let pn = pos.name
                  let (rnode,ratom,borderings) = compute_alpha_layer r in
                  ((pn::rnode),(pn::ratom),borderings)
             | NodeA(pos,suctrees) ->
                  let pn = pos.name in
                  if pos.pt = Beta then
                     let (rnode,ratom,borderings) = compute_alpha_layer r in
                     ((pn::rnode),(ratom),(f::borderings))
                  else
                     let suclist = Array.to_list suctrees in
                     compute_alpha_layer (suclist @ r)
            )

   let rec compute_connection alpha_layer union_atoms connections =
      match connections with
         [] -> ("none","none")
       | (c,d)::r ->
            if (List.mem c union_atoms) & (List.mem d union_atoms) then
               let (c1,c2) =
                  if List.mem c alpha_layer then
                     (c,d)
                  else
                     if List.mem d alpha_layer then
                        (d,c) (* then, d is supposed to occur in alpha_layer *)
                     else
                        raise (Invalid_argument "Jprover bug: connection match failure")
               in
               (c1,c2)
            else
               compute_connection alpha_layer union_atoms r

   let get_beta_suctrees btree =
      match btree with
         Empty | NodeAt(_) -> raise jprover_bug
       | NodeA(pos,suctrees) ->
            let b1tree = suctrees.(0)
            let b2tree = suctrees.(1) in
            (pos.name,b1tree,b2tree)

   let rec build_beta_proof alpha_layer union_atoms beta_orderings connections =
      let (c1,c2) = compute_connection alpha_layer union_atoms connections in
(* c1 is supposed to occur in the lowmost alpha layer of the branch, *)
(* i.e. aplha_layer *)
      if (c1,c2) = ("none","none") then
         (match beta_orderings with
            [] -> raise jprover_bug
          | btree::r ->
               let (beta_pos,suctree1,suctree2) = get_beta_suctrees btree in
               let (alpha_layer1, atoms1, bordering1) = compute_alpha_layer [suctree1]
               let (alpha_layer2, atoms2, bordering2) = compute_alpha_layer [suctree2] in
               let bproof1,beta1,closure1 =
                  build_beta_proof alpha_layer1 (atoms1 @ union_atoms)
                     (bordering1 @ r) connections
               in
               let bproof2,beta2,closure2 =
                  build_beta_proof alpha_layer2 (atoms2 @ union_atoms)
                     (bordering2 @ r) connections in
               (BNode(beta_pos,(alpha_layer1,bproof1),(alpha_layer2,bproof2))),(1+beta1+beta2),(closure1+closure2)
         )
      else
         CNode((c1,c2)),0,1

   let construct_beta_proof ftree connections =
      let (root_node,root_atoms,beta_orderings) = compute_alpha_layer [ftree]
      in
      let beta_proof,beta_exp,closures =
         build_beta_proof root_node root_atoms beta_orderings connections in
      (RNode(root_node,beta_proof)),beta_exp,closures
*)

(* *********** New Version with direct computation from extension proof **** *)
(* follows a DIRECT step from proof histories via pr-connection orderings to opt. beta-proofs *)

   let rec compute_alpha_layer ftree_list =
      match ftree_list with
         [] ->
            []
       | NodeAt {pospos = pospos; _} :: r
       | NodeA({pt = Beta; pospos = pospos; _}, _) :: r ->
            pospos :: (compute_alpha_layer r)
       | NodeA(_, suctrees) :: r ->
            compute_alpha_layer (List.rev_append suctrees r)
       | Empty :: _ ->
            raise (Invalid_argument "jall.compute_alpha_layer: bug")

   let rec compute_beta_difference c1_context c2_context act_context =
      match c1_context,c2_context with
         ([],c2_context) ->
            (list_diff c2_context act_context)
(* both connection partners in the same submatrix; c1 already isolated *)
       | ((fc1::rc1),[]) ->
            []  (* c2 is a reduction step, i.e. isolated before c1 *)
       | ((fc1::rc1),(fc2::rc2)) ->
            if fc1 = fc2 then    (* common initial beta-expansions *)
               compute_beta_difference rc1 rc2 act_context
            else
               (list_diff c2_context act_context)

   let rec non_closed = function
     [] -> false
   | RNode(_,_) :: _
   | AtNode(_,_) :: _ -> raise (Invalid_argument "Jprover bug: invalid beta-proof")
   | BEmpty :: _ -> true
   | CNode _ :: rbpf -> non_closed rbpf
   | BNode(pos,(_,bp1),(_,bp2)) :: rbpf -> non_closed (bp1 :: bp2 :: rbpf)

   let rec cut_context pos = function
      ((f,num)::r) as context ->
         if position_eq pos f then
            context
         else
            cut_context pos r
    | [] ->
         raise (Invalid_argument "Jprover bug: invalid context element")

   let compute_tree_difference beta_proof c1_context =
      match beta_proof with
         RNode(_,_) -> raise (Invalid_argument "Jprover bug: invalid beta-proof")
       | CNode(_) -> raise (Invalid_argument "Jprover bug: invalid beta-proof")
       | AtNode(_,_) -> raise (Invalid_argument "Jprover bug: invalid beta-proof")
       | BEmpty -> c1_context
       | BNode(pos,_,_) ->
(*    print_endline ("actual root: "^pos); *)
            cut_context pos c1_context

(* unused
   let print_context conn bcontext =
      begin
         open_box 0;
         print_string conn;
         print_string ":    ";
         List.iter (fun x -> let (pos,num) = x in print_string (pos^" "^(string_of_int num)^"")) bcontext;
         print_endline " ";
         print_flush ()
      end
*)

   (* ext_proof here has to be ordered, otherwise proofs break *)
   let rec build_opt_beta_proof beta_proof
      (ext_proof : (position * position) list)
      (beta_atoms : (position * int) list PMap.t)
      (beta_layer_list  : (position list * position list) PMap.t)
      act_context
      =
      let rec add_c2_tree (c1,c2) c2_diff_context =
         match c2_diff_context with
            [] ->
               (CNode(c1, c2),0)
          | (f,num)::c2_diff_r ->
               let next_beta_proof,next_exp =
                  add_c2_tree (c1,c2) c2_diff_r in
               let (layer1,layer2) = PMap.find beta_layer_list f in
               let new_bproof =
                  if num = 1 then
                     BNode(f,(layer1,next_beta_proof),(layer2,BEmpty))
                  else (* num = 2*)
                     BNode(f,(layer1,BEmpty),(layer2,next_beta_proof))
               in
               (new_bproof,(next_exp+1))
      in
      let rec add_beta_expansions
         ((c1,c2) : position * position)
         rest_ext_proof c1_diff_context c2_diff_context new_act_context =
         match c1_diff_context with
            [] ->
               let (n_c1,n_c2) =
                  if c2_diff_context = [] then  (* make sure that leaf-connection is first element *)
                     (c1,c2)
                  else
                     (c2,c1)
               in
               let c2_bproof,c2_exp = add_c2_tree (n_c1,n_c2) c2_diff_context in
               if c2_exp <> 0 then (* at least one open branch was generated to isloate c2 *)
                  begin
(*   print_endline "start with new beta-proof"; *)
                     let new_bproof,new_exp,new_closures,new_rest_proof =
                        build_opt_beta_proof
									c2_bproof rest_ext_proof beta_atoms beta_layer_list
									(List.rev_append act_context new_act_context)
							in
                     (new_bproof,(new_exp+c2_exp),(new_closures+1),new_rest_proof)
                  end
               else
                  begin
(*   print_endline "proceed with old beta-proof"; *)
                     (c2_bproof,c2_exp,1,rest_ext_proof)
                  end
          | (f,num)::c1_diff_r ->
               let (layer1,layer2) = PMap.find beta_layer_list f in
               let next_beta_proof,next_exp,next_closures,next_ext_proof =
                  add_beta_expansions (c1,c2) rest_ext_proof c1_diff_r c2_diff_context new_act_context in
               let new_bproof =
                  if num = 1 then
                     BNode(f,(layer1,next_beta_proof),(layer2,BEmpty))
                  else (* num = 2*)
                     BNode(f,(layer1,BEmpty),(layer2,next_beta_proof))
               in
               (new_bproof,(next_exp+1),next_closures,next_ext_proof)

      in
      let rec insert_connection
         beta_proof
         ((c1,c2) : position * position)
         rest_ext_proof c1_diff_context c2_diff_context
         (act_context : (position * int) list)
         =
         begin
(*   print_context c1 c1_diff_context;
   print_endline "";
   print_context c2 c2_diff_context;
   print_endline "";
*)
            match beta_proof with
               RNode(_,_) -> raise (Invalid_argument "Jprover bug: invalid beta-proof")
             | CNode(_) -> raise (Invalid_argument "Jprover bug: invalid beta-proof")
             | AtNode(_,_) -> raise (Invalid_argument "Jprover bug: invalid beta-proof")
             | BEmpty ->
                  add_beta_expansions (c1,c2) rest_ext_proof c1_diff_context c2_diff_context act_context
             | BNode(pos,(layer1,sproof1),(layer2,sproof2)) ->
(*   print_endline (c1^" "^c2^" "^pos); *)
                  (match c1_diff_context with
                     [] ->
                        raise (Invalid_argument "Jprover bug: invalid beta-proof")
                   | (f,num)::rest_context -> (* f = pos must hold!! *)
                        if num = 1 then
                           let (next_bproof,next_exp,next_closure,next_ext_proof) =
                              insert_connection sproof1 (c1,c2) rest_ext_proof rest_context c2_diff_context act_context in
                           (BNode(pos,(layer1,next_bproof),(layer2,sproof2)),next_exp,next_closure,next_ext_proof)
                        else (* num = 2 *)
                           let (next_bproof,next_exp,next_closure,next_ext_proof) =
                              insert_connection sproof2 (c1,c2) rest_ext_proof rest_context c2_diff_context act_context in
                           (BNode(pos,(layer1,sproof1),(layer2,next_bproof)),next_exp,next_closure,next_ext_proof)
                  )
         end

      in
      match ext_proof with
         [] ->
            beta_proof,0,0,[]
       | (c1,c2)::rproof ->
(*  print_endline ("actual connection: "^c1^" "^c2); *)
            let c1_context = PMap.find beta_atoms c1 in
            let c2_context = PMap.find beta_atoms c2 in
            let c2_diff_context = compute_beta_difference c1_context c2_context act_context in
            let c1_diff_context = compute_tree_difference beta_proof c1_context in (* wrt. actual beta-proof *)
            let (next_beta_proof,next_exp,next_closures,next_ext_proof) =
               insert_connection beta_proof (c1,c2) rproof c1_diff_context c2_diff_context c1_diff_context in
            if non_closed [next_beta_proof] then  (* at least one branch was generated to isolate c1 *)
               let rest_beta_proof,rest_exp,rest_closures,rest_ext_proof =
                  build_opt_beta_proof next_beta_proof next_ext_proof beta_atoms beta_layer_list act_context in
               rest_beta_proof,(next_exp+rest_exp),(next_closures+rest_closures),rest_ext_proof
            else
               next_beta_proof,next_exp,next_closures,next_ext_proof

   let rec annotate_atoms beta_context atlist treelist =
      let annotate_tree
         (beta_context : (position * int) list)
         tree atlist
         =
         match tree with
            Empty ->
               atlist, PMap.empty, PMap.empty
          | NodeAt({pospos=pospos; _}) ->
               if Set.mem atlist pospos then
                  let new_atlist = Set.remove atlist pospos in
                  new_atlist, PMap.add PMap.empty pospos beta_context, PMap.empty
               else
                  atlist, PMap.empty, PMap.empty
          | NodeA({pt = Beta; pospos = pospos; _}, [s1;s2]) ->
               let alayer1 = compute_alpha_layer [s1] in
               let alayer2 = compute_alpha_layer [s2] in
					(* rev_appends in the next two lines would break proofs *)
               let new_beta_context1 = beta_context @ [(pospos,1)] in
               let new_beta_context2 = beta_context @ [(pospos,2)] in
               let atlist1,annotates1,blayer_list1 =
                  annotate_atoms new_beta_context1 atlist [s1] in
               let atlist2,annotates2,blayer_list2 =
                  annotate_atoms new_beta_context2 atlist1 [s2]
               in
                  atlist2, PMap.union nodups annotates1 annotates2,
						PMap.add (PMap.union nodups blayer_list1 blayer_list2) pospos (alayer1,alayer2)
          | NodeA({pt = Beta; _}, _) ->
               raise (Invalid_argument "jall.annotate_atoms: bug")
          | NodeA(_, suctrees) ->
               annotate_atoms beta_context atlist suctrees
      in
      match treelist with
         [] -> atlist, PMap.empty, PMap.empty
       | f::r ->
            let next_atlist, f_annotates, f_beta_layers = annotate_tree beta_context f atlist in
            let rest_atlist, rest_annotates, rest_beta_layers =
               annotate_atoms beta_context next_atlist r
            in
            rest_atlist, PMap.union nodups f_annotates rest_annotates,
				PMap.union nodups f_beta_layers rest_beta_layers

   let construct_opt_beta_proof ftree
      (ext_proof : (position * position) list)
      =
		let con_atoms =
			List.fold_left (fun set (a,b) -> Set.add (Set.add set a) b) Set.empty ext_proof
		in
      let empty_atoms,beta_atoms,beta_layer_list = annotate_atoms [] con_atoms [ftree] in
      let root_node = compute_alpha_layer [ftree] in
      let (beta_proof,beta_exp,closures,_) =
         build_opt_beta_proof BEmpty ext_proof beta_atoms beta_layer_list [] in
      (RNode(root_node,beta_proof)),beta_exp,closures

(************* permutation ljmc -> lj *********************************)

(* REAL PERMUTATION STAFF *)

   let subf1 n m subrel = SubrelSet.mem subrel ((n,m),1)
   let subf2 n m subrel = SubrelSet.mem subrel ((n,m),2)

	type augmented_position = Pos of position | OrlTrue

(* Transforms all normal form layers in an LJ proof *)

   let rec modify prooftree subrel tsubrel =
      match prooftree with
         PEmpty ->
            raise (Invalid_argument "jall.modify: bug")
       | PNodeAx((pos,inf,form,term)) ->
            prooftree, Pos pos
       | PNodeA((pos,inf,form,term),left)  ->
            let t,qpos = modify left subrel tsubrel in
            begin match inf, qpos with
               Impr, _ | Negr, _ | Allr, _ ->
                  PNodeA((pos,inf,form,term),t), Pos pos    (*layer bound *)
             | _, OrlTrue ->
                  PNodeA((pos,inf,form,term),t), qpos
             | Andl, _ | Alll, _ | Exl, _ ->
                  PNodeA((pos,inf,form,term),t), qpos  (*simply propagation*)
             | Exr, Pos ppos ->
                  if subf1 pos ppos subrel then
                     PNodeA((pos,inf,form,term),t), Pos pos
						else
							t,qpos
             | Negl, Pos ppos ->
                  if subf1 pos ppos subrel then
                     PNodeA((pos,inf,form,term),t), Pos empty_pos (* empty string *)
                  else
							t,qpos
             | _, Pos ppos ->          					           (* x = Orr *)
                  if subf1 pos ppos subrel then
                     PNodeA((pos,Orr1,form,term),t), Pos pos    (* make Orr for LJ *)
                  else if (subf2 pos ppos subrel) then
                     PNodeA((pos,Orr2,form,term),t), Pos pos     (* make Orr for LJ *)
                  else
							t,qpos
            end
       | PNodeB((pos,inf,form,term),left,right) ->
            let t,qpos = modify left subrel tsubrel in
				begin match inf, qpos with
					Andr, OrlTrue ->
						let s,rpos = modify right subrel tsubrel in  (* Orl-True -> subf *)
						begin match rpos with
							OrlTrue ->
								PNodeB((pos,inf,form,term),t,s), Pos pos
						 | Pos rppos when subf2 pos rppos subrel ->
						 		PNodeB((pos,inf,form,term),t,s), Pos pos
						 | Pos _ ->
						 		s,rpos
						end
				 | Andr, Pos qppos when subf1 pos qppos subrel ->
                  let s,rpos = modify right subrel tsubrel in  (* Orl-True -> subf *)
						begin match rpos with
                     OrlTrue ->
                        PNodeB((pos,inf,form,term),t,s), Pos pos
                   | Pos rppos when subf2 pos rppos subrel ->
                        PNodeB((pos,inf,form,term),t,s), Pos pos
                   | Pos _ ->
                        s,rpos
                  end
				 | Andr, Pos _ ->
				 		t,qpos 											(* not subf -> not Orl-True *)
				 | Impl, Pos qppos when subf1 pos qppos subrel ->
                  let s,rpos = modify right subrel tsubrel in
                  PNodeB((pos,inf,form,term),t,s), Pos empty_pos (* empty string *)
				 | Impl, (Pos _ | OrlTrue) ->
				 		t,qpos
				 | _ ->												(* x= Orl *)
               	let s,rpos = modify right subrel tsubrel in
	               PNodeB((pos,inf,form,term),t,s), OrlTrue
				end

(* transforms the subproof into an LJ proof between
   the beta-inference rule (excluded) and
   layer boundary in the branch ptree *)

   let rec rec_modify ptree subrel =
      match ptree with
         PEmpty ->
            raise (Invalid_argument "jall.rec_modify: bug")
       | PNodeAx((pos,inf,form,term)) ->
            ptree,pos
       | PNodeA((pos,(Impr | Negr | Allr),form,term),left)  ->
            ptree,pos    (*layer bound, stop transforming! *)
       | PNodeA((pos,((Andl | Alll | Exl) as inf),form,term),left)  ->
            let t,qpos = rec_modify left subrel in
            PNodeA((pos,inf,form,term),t),qpos   (*simply propagation*)
       | PNodeA((pos,(Exr as inf),form,term),left)  ->
            let t,qpos = rec_modify left subrel in
            if (subf1 pos qpos subrel) then
               PNodeA((pos,inf,form,term),t),pos
            else
               t,qpos
       | PNodeA((pos,(Negl as inf),form,term),left)  ->
            let t,qpos = rec_modify left subrel in
            if (subf1 pos qpos subrel) then
               PNodeA((pos,inf,form,term),t),empty_pos  (* empty string *)
            else
               t,qpos
       | PNodeA((pos,Orr,form,term),left)  ->
            let t,qpos = rec_modify left subrel in
            if (subf1 pos qpos subrel) then
               PNodeA((pos,Orr1,form,term),t),pos    (* make Orr for LJ *)
            else if (subf2 pos qpos subrel) then
               PNodeA((pos,Orr2,form,term),t),pos     (* make Orr for LJ *)
            else
               t,qpos
       | PNodeA((_, (Fail|Impl|Orl|Orr2|Orr1|Andr|Ax|Boxl|Boxr), _, _), _) ->
            raise (Invalid_argument "Jall.rec_modify: unexpected inf in PNodeA")
       | PNodeB((pos,inf,form,term),left,right) ->
            let t,qpos = rec_modify left subrel in
            match inf with
               Andr ->
                  if (subf1 pos qpos subrel) then
                     let s,rpos = rec_modify right subrel in
                     if (subf2 pos rpos subrel) then
                        PNodeB((pos,inf,form,term),t,s),pos
                     else
                        s,rpos
                  else
                     t,qpos
             | Impl -> (* x = Impl since x= Orl cannot occur in the partial layer ptree *)
                  if subf1 pos qpos subrel then
                     let s,rpos = rec_modify right subrel in
                     PNodeB((pos,inf,form,term),t,s),empty_pos (* empty string *)
                  else
                     t,qpos
             |
             (Fail|Exl|Exr|Alll|Allr|Negl|Negr|Impr|Orl|Orr2|Orr1|Orr|Andl|Ax|Boxl|Boxr) ->
                  raise (Invalid_argument "Jall.rec_modify: unexpected inf in PNodeB")

   let weak_modify rule ptree subrel =   (* recall rule = or_l *)
      match rule with
         _, Orl, _, _ -> ptree, true
       | (pos, _, _, _) ->
         let ptreem, qpos = rec_modify ptree subrel in
            ptreem, (subf1 pos qpos subrel) (* weak_modify will always be applied on left branches *)

(* Now, the permutation stuff .... *)

(* Permutation schemes *)

(* corresponds to local permutation lemma -- Lemma 3 in the paper -- *)
(* with eigenvariablen renaming let branch modification *)

(* eigenvariablen renaming let branch modification over *)
(* the whole proofs, i.e. over layer boundaries, too *)

(* global variable vor eigenvariable renaming during permutations *)

(* append renamed paramater Right to non-quantifier subformulae
   of renamed quantifier formulae *)

   let make_new_eigenvariable term =
		if is_var_term term && is_const (symbol_to_pos (dest_var term)) then
       	(*let new_eigen_var = (ofname^"_r"^(string_of_int (!eigen_counter))) in*)
		  	let new_eigen_pos = EigenVar, Lm_symbol.new_number () in
		  	(*print_endline ("New Counter :"^(string_of_int (!eigen_counter))); *)
		  	mk_pos_var new_eigen_pos
		else
			raise (Invalid_argument "jall.make_new_eigenvariable: bug")

   let replace_subterm term oldt rept =
      let dummy = pos_to_symbol (dummy_pos ()) in
      let v_term = TermSubst.var_subst term oldt dummy in
      TermSubst.subst1 v_term dummy rept

   let rec eigen_rename old_parameter new_parameter ptree =
      match ptree with
         PEmpty ->
            raise (Invalid_argument "jall.eigen_rename: bug")
       | PNodeAx((pos,inf,form,term)) ->
            let new_form = replace_subterm form old_parameter new_parameter in
            PNodeAx((pos,inf,new_form,term))
       | PNodeA((pos,inf,form,term), left) ->
            let new_form = replace_subterm form old_parameter new_parameter in
            let new_term = replace_subterm term old_parameter new_parameter in
            let ren_left = eigen_rename old_parameter new_parameter left in
            PNodeA((pos,inf,new_form,new_term), ren_left)
       | PNodeB((pos,inf,form,term),left, right) ->
            let new_form = replace_subterm form old_parameter new_parameter in
            let ren_left = eigen_rename old_parameter new_parameter left in
            let ren_right = eigen_rename old_parameter new_parameter right in
            PNodeB((pos,inf,new_form,term), ren_left, ren_right)

   let rec update_ptree rule subtree direction =
      match subtree with
         PEmpty ->
            raise (Invalid_argument "jall.update_ptree: bug")
       | PNodeAx(r) ->
            subtree
       | PNodeA((_,_,_,term) as r, left) ->
            if rule_eq r rule then
               left
          (* don't delete rule if subformula belongs to renamed instance of quantifiers; *)
          (* but this can never occur now since (renamed) formula is part of rule *)
            else
               begin match rule with
                  (_, (Exl | Allr), formn, termn) when alpha_equal term termn ->
    (* this can only occur if eigenvariable rule with same term as termn has been permuted; *)
    (* the application of the same eigenvariable introduction on the same subformula with *)
    (* different instantiated variables might occur! *)
    (* termn cannot occur in terms of permuted quantifier rules due to substitution split *)
    (* during reconstruciton of the ljmc proof *)
                     let new_term = make_new_eigenvariable term in
(*              print_endline "Eigenvariable renaming!!!"; *)
                     eigen_rename termn new_term subtree
                | _ ->
                     let left_del =
                        update_ptree rule left direction
                     in
                     PNodeA(r, left_del)
               end
       | PNodeB(r, left, right) ->
            if rule_eq r rule then
               match direction with
						Left ->
	                  left
					 | Right ->
                  	right
            else
               let left_del = update_ptree rule left direction in
               let right_del = update_ptree rule right direction in
               PNodeB(r,left_del,right_del)

(* permute layers, isolate addmissible branches *)

(* computes if an Andr is d-generatives *)

   let rec orl_free ptree =
      match ptree with
         PEmpty ->
            raise (Invalid_argument "jall.orl_free: bug")
       | PNodeAx _
       | PNodeA((_, (Impr|Negr|Allr), _, _),_) ->
            true
       | PNodeA(_, left) ->
            orl_free left
       | PNodeB((_, Orl, _, _),_,_) ->
            false
       | PNodeB(_,left,right) ->
            orl_free left && orl_free right

   let tsubf tsubrel m (n, _, _, _) = Rel.mem tsubrel (n,m)

   let dgenerative rule dglist ptree tsubrel =
      match rule with
         (_,(Exr|Orr|Negl),_,_) -> true
       | (pos, Andr, _, _) ->
            List.exists (tsubf tsubrel pos) dglist
       | (_, Impl, _, _) ->
            not (orl_free ptree)
       | _ ->
            false

(* to compute a topmost addmissible pair r,o with
   the address addr of r in the proof tree
*)

   let rec top_addmissible_pair ptree dglist act_r act_o act_addr tsubrel dummyt =
(*  print_endline "top_addmissible_pair in"; *)
      if orl_free ptree then                  (* there must be a orl BELOW an layer bound *)
         begin
(*    print_endline "orl_free"; *)
            act_r,act_o,act_addr
         end
      else
         begin
(*    print_endline "orl_full"; *)
            search_pair ptree dglist act_r act_o act_addr tsubrel dummyt
         end

   and search_pair ptree dglist act_r act_o act_addr tsubrel dummyt =
      match ptree with
         PEmpty -> raise (Invalid_argument "jall.search_pair: bug")
       | PNodeAx(_) -> raise (Invalid_argument "jall.search_pair: bug")
       | PNodeA(rule, left) ->
(*      print_endline "alpha"; *)
            if dgenerative rule dglist left tsubrel then  (* r = Exr,Orr,Negl *)
               let newdg = rule::dglist in
               search_pair left newdg act_r rule act_addr tsubrel dummyt
            else                   (* Impr, Allr, Notr only for test *)
               search_pair left dglist act_r act_o act_addr tsubrel dummyt
       | PNodeB((_, (Andr | Impl), _, _) as rule,left,right) ->
(*      print_endline "beta";  *)
            let newdg,newrule =
               if dgenerative rule dglist left tsubrel then
                  (rule::dglist),rule
               else
                  dglist,act_o
            in
            if orl_free left then
               search_pair right newdg act_r newrule (Right :: act_addr) tsubrel dummyt
            else  (* not orl_free *)
               let left_r,left_o,left_addr =
                  search_pair left newdg act_r newrule (Left :: act_addr) tsubrel dummyt in
               if left_o = (empty_pos, Orr, dummyt, dummyt) then
                  top_addmissible_pair right dglist act_r act_o (Right :: act_addr) tsubrel dummyt
               else left_r,left_o,left_addr
       | PNodeB(rule,left,right) -> (* r = Orl *)
(*      print_endline "beta";  *)
            if orl_free left then
               top_addmissible_pair right dglist rule act_o (Right :: act_addr) tsubrel dummyt
            else
               let left_r,left_o,left_addr =
                  search_pair left dglist rule act_o (Left :: act_addr) tsubrel dummyt
               in
               if left_o = (empty_pos, Orr, dummyt, dummyt) then
                  top_addmissible_pair right dglist rule act_o (Right :: act_addr) tsubrel dummyt
               else
                  left_r,left_o,left_addr

   let rec permute_branch r addr act_addr ptree dglist subrel tsubrel =
(*   print_endline "pbranch in"; *)
      match ptree, act_addr with
         PNodeA(o,PNodeA(rule,left)),_ ->                      (* one-over-one *)
            let left2 = PNodeA(o,left) in
            let pbleft = permute_branch r addr act_addr left2 dglist subrel tsubrel in
               PNodeA(rule,pbleft)
       | PNodeB(o,PNodeA(rule,left),right),Left ->               (* two-over-one, left *)
            let right_u = update_ptree rule right Left in
            let left2 = PNodeB(o, left, right_u) in
            let pbleft = permute_branch r addr act_addr left2 dglist subrel tsubrel in
               PNodeA(rule,pbleft)
       | PNodeA(o,PNodeB(rule,left,right)),_ ->                (* one-over-two *)
(*     print_endline " one-over-two ";                  *)
            if rule_eq rule r then  (* left,right are or_l free *)
               PNodeB(rule, PNodeA(o,left), PNodeA(o,right)) (* first termination case *)
            else
               begin match addr with
                  Left :: atl ->
                     let left2 = PNodeA(o,left) in
                     let right2 = PNodeA(o,right) in
                     let pbleft = permute_branch r atl Left left2 dglist subrel tsubrel in
                     let plright = permute_layer right2 dglist subrel tsubrel in
                        PNodeB(rule,pbleft,plright)
                | Right :: atl -> (* that is left of rule is or_l free *)
                     let left1,bool = weak_modify rule left subrel in
                     let left2 = PNodeA(o,left1) in
                     if bool then  (* rule is relevant *)
                        let right2 = PNodeA(o,right) in
                        let pbright = permute_branch r atl Right right2 dglist subrel tsubrel in
                           PNodeB(rule,left2,pbright)
                     else          (* rule is not relevant *)
                        left2  (* optimized termination case (1) *)
                | [] -> raise (Invalid_argument "jall.permute_branch: bug")
               end
      | PNodeB(o,left,PNodeA(rule,right)),Right ->                (* two-over-one, right *)
                                             (* left of o is or_l free *)
(*      print_endline " two-over-one, right"; *)
           let leftm,bool = weak_modify o left subrel in
           if bool then  (* rule is relevant *)
              let left_u = update_ptree rule leftm Left in
              let left2 = PNodeB(o, left_u, right) in
              let pbleft = permute_branch r addr act_addr left2 dglist subrel tsubrel in
                 PNodeA(rule,pbleft)
           else          (* rule is not relevant *)
              leftm  (* optimized termination case (2) *)
      | PNodeB(o,PNodeB(rule,left,right),right1),Left ->             (* two-over-two, left *)
(*     print_endline " two-over-two, left"; *)
           if rule_eq rule r then   (* left,right are or_l free *)
              let right_ul = update_ptree rule right1 Left in
              let right_ur = update_ptree rule right1 Right in
(*        print_endline "permute 2-o-2, left ok"; *)
              let leftm3,bool3 = weak_modify o left subrel in
              let leftm4,bool4 = weak_modify o right subrel in
              let plleft =
                 if bool3 then (* left is relevant *)
                    permute_layer (PNodeB(o,leftm3,right_ul)) dglist subrel tsubrel
                 else
                    leftm3
              in
              let plright =
                 if bool4 then (* right is relevant *)
                    permute_layer (PNodeB(o,leftm4,right_ur)) dglist subrel tsubrel
                 else
                    leftm4
              in
                 PNodeB(rule,plleft,plright)
           else
              begin match addr with
                 Left :: atl ->
                    let right_ul = update_ptree rule right1 Left in
                    let right_ur = update_ptree rule right1 Right in
                    let left2 = PNodeB(o,left,right_ul) in
                    let right2 = PNodeB(o,right,right_ur) in
                    let pbleft = permute_branch r atl act_addr left2 dglist subrel tsubrel in
                    let plright = permute_layer right2 dglist subrel tsubrel in
                       PNodeB(rule,pbleft,plright)
               | Right :: atl -> (* that is left is or_l free *)
                    let left1,bool = weak_modify rule left subrel in
                    if bool then  (* rule is relevant *)
                       let right_ul = update_ptree rule right1 Left in
                       let right_ur = update_ptree rule right1 Right in
                       let right2 = PNodeB(o,right,right_ur) in
                       let pbright = permute_branch r atl act_addr right2 dglist subrel tsubrel in
                       let leftm3,bool3 = weak_modify o left1 subrel in
                       let plleft =
                          if bool3 (* r3 relevant *) then
                             permute_layer (PNodeB(o,leftm3,right_ul)) dglist subrel tsubrel
                          else  (* r3 redundant *)
                             leftm3
                       in
                          PNodeB(rule,plleft,pbright)  (* further opt. NOT possible *)
                    else          (* rule is not relevant *)
                       permute_layer (PNodeB(o,left1,right1)) dglist subrel tsubrel (* further opt. possible *)
                                                              (* combine with orl_free *)
               | [] -> raise (Invalid_argument "jall.permute_branch: bug")
              end
    | PNodeB(o,left1,PNodeB(rule,left,right)),Right ->             (* two-over-two, right *)
(*      print_endline " two-over-two, right"; *)
         let leftm1,bool = weak_modify o left1 subrel in  (* left1 is or_l free *)
         if bool then  (* o is relevant, even after permutations *)
            if rule_eq rule r then  (* left, right or_l free *)
               let left_ul = update_ptree rule leftm1 Left in
               let left_ur = update_ptree rule leftm1 Right in
                  PNodeB(rule,PNodeB(o,left_ul,left),PNodeB(o,left_ur, right))
            else
               begin match addr with
                  Left :: atl ->
                     let left_ul = update_ptree rule leftm1 Left in
                     let left_ur = update_ptree rule leftm1 Right in
                     let left2 = PNodeB(o,left_ul,left) in
                     let right2 = PNodeB(o,left_ur, right) in
                     let pbleft = permute_branch r atl act_addr left2 dglist subrel tsubrel in
                     let plright = permute_layer right2 dglist subrel tsubrel in
                        PNodeB(rule,pbleft,plright)
                | Right :: atl -> (* that is left is or_l free *)
                     let leftm,bool = weak_modify rule left subrel in
                     if bool then  (* rule is relevant *)
                        let left_ul = update_ptree rule leftm1 Left in
                        let left_ur = update_ptree rule leftm1 Right in
                        let left2 = PNodeB(o,left_ul,left) in
                        let right2 = PNodeB(o,left_ur, right) in
                        let pbright =
                           permute_branch r atl act_addr right2 dglist subrel tsubrel
                        in
                           PNodeB(rule,left2,pbright)  (* left2 or_l free *)
                     else (* rule is not relevant *)
                        PNodeB(o,leftm1,leftm)
                | [] -> raise (Invalid_argument "jall.permute_branch: bug")
               end
         else
            leftm1
    | _ -> raise (Invalid_argument "jall.permute_branch: bug")

   and trans_add_branch r o addr act_addr ptree dglist subrel tsubrel =
      match ptree with
         (PEmpty| PNodeAx(_)) -> raise (Invalid_argument "jall.trans_add_branch: bug")
       | PNodeA(rule,left) ->
            if dgenerative rule dglist left tsubrel then
               let newdg = rule :: dglist in
               if rule_eq rule o then
                  begin
(*            print_endline "one-rule is o"; *)
                     permute_branch r addr act_addr ptree dglist subrel tsubrel
                  end
               else
                  begin
(*             print_endline "alpha - but not o"; *)
                     let tptree = trans_add_branch r o addr act_addr left newdg subrel tsubrel in
                     permute_layer (PNodeA(rule,tptree)) dglist subrel tsubrel
             (* r may not longer be valid for rule *)
                  end
            else
               let tptree =  trans_add_branch r o addr act_addr left dglist subrel tsubrel in
               PNodeA(rule,tptree)
       | PNodeB(rule,left,right) ->
            let act_addr', atl, lft =
               match addr with
                  Left :: atl ->
                     Left, atl, true
                | Right :: atl ->
                     Right, atl, false
                | [] -> raise (Invalid_argument "jall.trans_add_branch: bug")
            in
            if rule_eq rule o then
               begin
(*          print_endline "two-rule is o"; *)
                  permute_branch r atl act_addr' ptree dglist subrel tsubrel
               end
            else
               begin
(*         print_endline ("beta - but not o: address "^d); *)
                  let dglist =
                     if dgenerative rule dglist left tsubrel
                     then rule :: dglist
                     else dglist
                  in
                  let tptree =
                     trans_add_branch r o atl act_addr'
                        (if lft then left else right) dglist subrel tsubrel
                  in
                     permute_layer
                        (if lft
                         then PNodeB(rule,tptree,right)
                         else PNodeB(rule,left,tptree))
                        dglist subrel tsubrel
               end

   and permute_layer ptree dglist subrel tsubrel =
     (*  print_endline "permute_layer in"; *)
      let dummyt = mk_pos_var (dummy_pos ()) in
      let r,o,addr =
         top_addmissible_pair
            ptree dglist
            (empty_pos,Orl,dummyt,dummyt)
            (empty_pos,Orr,dummyt,dummyt)
            [] tsubrel dummyt
      in
      if rule_eq r (empty_pos,Orl,dummyt,dummyt) then
         ptree
      else if rule_eq o (empty_pos,Orr,dummyt,dummyt) then  (* Orr is a dummy for no d-gen. rule *)
         ptree
      else
(*         let (x1,x2,x3,x4) = r
         let (y1,y2,y3,y4) = o in
   print_endline ("top or_l: "^x1);
   print_endline ("or_l address: "^addr);
   print_endline ("top dgen-rule: "^y1); *)
         trans_add_branch r o (List.rev addr) Left ptree dglist subrel tsubrel

(* Isolate layer let outer recursion structure *)
(* uses weaker layer boundaries: ONLY critical inferences *)

   let rec isol_layer ptree subrel tsubrel =
      match ptree with
         PEmpty -> raise (Invalid_argument "jall.isol_layer: bug")
       | PNodeAx(inf) ->
            ptree
       | PNodeA((_,(Allr|Impr|Negr),_,_) as rule,left) ->
            let tptree = trans_layer left subrel tsubrel in
               PNodeA(rule,tptree)
       | PNodeA(rule,left) ->
            let tptree = isol_layer left subrel tsubrel in
               PNodeA(rule,tptree)
       | PNodeB(rule,left,right) ->
            let tptree_l = isol_layer left subrel tsubrel in
            let tptree_r = isol_layer right subrel tsubrel in
             PNodeB(rule,tptree_l,tptree_r)

   and trans_layer ptree subrel tsubrel =
(*   print_endline "trans_layer in"; *)
      let top_tree = isol_layer ptree subrel tsubrel in
      let back = permute_layer top_tree [] subrel tsubrel in
(*     print_endline "translauer out"; *)
      back

(* REAL PERMUTATION STAFF  --- End *)

(* build the proof tree from a list of inference rules *)

   let rec unclosed = function
      PEmpty -> true
    | PNodeAx(y) -> false
    | PNodeA(y,left) -> (unclosed left)
    | PNodeB(y,left,right) -> (or) (unclosed left) (unclosed right)

   let rec extend prooftree element =
      match prooftree with
         PEmpty ->
            begin match element with
               _, Ax, _, _ -> PNodeAx(element)
             | _, (Andr| Orl | Impl), _, _ -> PNodeB(element,PEmpty,PEmpty)
             | _ -> PNodeA(element,PEmpty)
            end
       | PNodeAx(y) ->
            PEmpty           (* that's only for exhaustive pattern matching *)
       | PNodeA(y, left) ->
            PNodeA(y, (extend left element))
       | PNodeB(y, left, right) ->
            if (unclosed left) then
               PNodeB(y, (extend left element), right)
            else
               PNodeB(y, left, (extend right element))

   let rec bptree prooftree nodelist nax=
      match nodelist with
         [] -> prooftree,nax
       | ((_,pos),(rule,formula,term))::rest -> (* kick away the first argument *)
            let newax =
               if rule = Ax then
                  1
               else
                  0
            in
            bptree (extend prooftree (pos,rule,formula,term)) rest (nax+newax)

   let bproof nodelist =
      bptree PEmpty nodelist 0

   let rec get_successor_pos set = function
      Empty :: r -> get_successor_pos set r
    | NodeA(pos,_) :: r ->
         get_successor_pos (PosSet.add set pos) r
    | [] -> set
    | NodeAt _ :: _ -> raise (Invalid_argument "jall.get_successor_pos: bug")

   let rec get_formula_tree ftreelist f predflag =
      match ftreelist with
         Empty :: rest_trees -> get_formula_tree rest_trees f predflag
       | NodeAt _ :: rest_trees -> get_formula_tree rest_trees f predflag
       | NodeA(pos,suctrees) :: rest_trees ->
            if predflag then
               if pos.pt = Gamma then
                  let succs = get_successor_pos PosSet.empty suctrees in
                  if PosSet.mem succs f then
                     NodeA(pos,suctrees),succs
                  else
                     get_formula_tree (List.rev_append suctrees rest_trees) f predflag
               else
                  get_formula_tree (List.rev_append suctrees rest_trees) f predflag
            else
               if pos_eq pos f then
                  NodeA(pos,suctrees),PosSet.empty
               else
                  get_formula_tree (List.rev_append suctrees rest_trees) f predflag
       | [] -> raise (Invalid_argument "jall.get_formula_tree: bug")

   let rec get_formula_treelist ftree = function
      [] -> []
(* a posistion has either stype Gamma_0,Psi_0,Phi_0 (non-atomic), or it has *)
(* ptype Alpha (or on the right), since there was a deadlock for proof reconstruction in LJ*)
    | ({st = Gamma_0; _} as f)::r ->
         let predtree,succs = get_formula_tree [ftree] f true in
         let new_po = List.filter (fun x -> not (PosSet.mem succs x)) r in
         predtree::(get_formula_treelist ftree new_po)
    | ({st = (Phi_0 | Psi_0); _} as f)::r
    | ({pt = Alpha; _} as f)::r ->
         let stree,_ = get_formula_tree [ftree] f false in
         stree::(get_formula_treelist ftree r)
    | _ ->
         raise (Invalid_argument "Jprover bug: non-admissible open position")

   let rec number_list n = function
      hd::tl -> (n,hd)::(number_list (succ n) tl)
    | [] -> []

   let rec build_formula_rel dir_treelist
      (slist : Set.t)
      predpos
      =
      let rec build_renamed_gamma_rel dtreelist predpos pospos d =
         match dtreelist with
            [] -> SubrelSet.empty,PMap.empty
          | (x,ft)::rdtlist ->
               let rest_rel,rest_ren = build_renamed_gamma_rel rdtlist predpos pospos d in
               (
                match ft with
                   Empty ->   (* may have empty successors due to purity in former reconstruction steps *)
                      rest_rel,rest_ren
                 | NodeAt(_) ->
                      raise (Invalid_argument "jall.build_formula_rel: bug")
							 (* gamma_0 position never is atomic *)
                 | NodeA({pospos=spospos; _},suctrees) ->
                      if Set.mem slist spospos then
(* the gamma_0 position is really unsolved *)
(* this is only relevant for the gamma_0 positions in po *)
                         let k, _ = pospos in
                         let new_pos = k, Lm_symbol.new_number () in
                         (* XXX Yegor: the old comment says: "make new unique gamma name"
                         *  but currently I simply create a fresh variable of
                         *  original kind and it seems to work alright *)
                         let new_srel_el = ((predpos,new_pos),d) in
                         let srel, sren =
                            build_formula_rel [(x,ft)] slist new_pos
                         in
                         SubrelSet.add (SubrelSet.union srel rest_rel) new_srel_el,
								 PMap.add (PMap.union nodups sren rest_ren) spospos new_pos
								 (* gamma_0 position as key first *)
                      else
                         rest_rel,rest_ren
               )

      in
      match dir_treelist with
         [] -> SubrelSet.empty,PMap.empty
       | (d,f)::dir_r ->
            let rest_rel, rest_renlist = build_formula_rel dir_r slist predpos in
            match f with
               Empty ->
                  if !debug_jprover then print_endline "Hello, an empty subtree!!!!!!";
                  rest_rel,rest_renlist
             | NodeAt({pospos=pospos; _}) ->
                  SubrelSet.add rest_rel ((predpos,pospos),d), rest_renlist
             | NodeA({ pt = Alpha | Beta | Delta; pospos=pospos; _ }, suctrees) ->
                  let dtreelist = number_list 1 suctrees in
                  let srel, sren = build_formula_rel dtreelist slist pospos in
                     SubrelSet.add (SubrelSet.union srel rest_rel) ((predpos,pospos),d),
							PMap.union nodups sren rest_renlist
             | NodeA({ pt = Psi| Phi; _ }, suctrees) ->
                  let dtreelist = (List.rev_map (fun x -> (d,x)) suctrees) in
                  let srel, sren = build_formula_rel dtreelist slist predpos in
                     SubrelSet.union srel rest_rel,
							PMap.union nodups sren rest_renlist
             | NodeA({ pt = Gamma; pospos=pospos; _ }, suctrees) ->
                  let dtreelist = (List.rev_map (fun x -> (1,x)) suctrees) in
(*                if (nonemptys suctrees 0 n) = 1 then
   let (srel,sren) = build_formula_rel dtreelist slist pos.name in
   ((((predname,pos.name),d)::srel) @ rest_rel),(sren @ rest_renlist)
   else (* we have more than one gamma instance, which means renaming *)
*)
                  let srel, sren = build_renamed_gamma_rel dtreelist predpos pospos d in
                     SubrelSet.union srel rest_rel,
							PMap.union nodups sren rest_renlist
             | NodeA({ pt = Pi _ | Nu _; _ }, suctrees) ->
                   raise (Invalid_argument "S4 nodes in build_formula_rel")
             | NodeA({ pt = PNull; _ }, _) ->
                  raise (Invalid_argument "jall.build_formula_rel: bug")

   let rec rename_gamma ljmc_proof rename_list =
      match ljmc_proof with
         [] -> []
       | ((inst,_),(((Alll | Exr),_,_) as h))::r ->
            let new_gamma = PMap.find rename_list inst in
            ((inst,new_gamma),h)::(rename_gamma r rename_list)
       | h :: r ->
            h :: (rename_gamma r rename_list)

   let compare_pair s sf transrel =
      Rel.range_fold
         (fun (a,_) -> PosOrdering.compare sf a)
         (fun acc (a,b) -> Rel.add acc (s,b))
         Rel.empty
         transrel

   let compare_pairlist rel transrel =
      Rel.fold
         (fun acc (a,b) -> Rel.union (compare_pair a b transrel) acc)
         Rel.empty
         transrel

   let rec trans_rec rel transrel =
      let trel = compare_pairlist rel transrel in
      if Rel.is_empty trel then
         transrel
      else
         Rel.union (trans_rec rel trel) transrel

   let extract_rel subrel =
		SubrelSet.fold (fun acc (p,i) -> Rel.add acc p) Rel.empty subrel

   let transitive_closure subrel =
      let rel = extract_rel subrel in
      trans_rec rel rel

   let pt ptree subrel =
      let tsubrel = transitive_closure subrel in
      let transptree = trans_layer ptree subrel tsubrel in
(*      print_endline ""; *)
      fst (modify transptree subrel tsubrel)
(*     let mtree = fst (modify transptree (subrel,tsubrel)) in *)
(*       pretty_print mtree ax *)

   let rec make_node_list ljproof =
      match ljproof with
         PEmpty ->
            raise (Invalid_argument "jall.make_node_list: bug")
       | PNodeAx((pos,inf,form,term)) ->
            [((empty_pos,pos),(inf,form,term))]
       | PNodeA((pos,inf,form,term),left) ->
            let left_list =  make_node_list left in
            ((empty_pos,pos),(inf,form,term))::left_list
       | PNodeB((pos,inf,form,term),left,right) ->
            let left_list =  make_node_list left in
            let right_list =  make_node_list right in
            ((empty_pos,pos),(inf,form,term))::(left_list @ right_list)
				(* rev_append here would break some proofs *)

   let permute_ljmc ftree po
      (slist : Set.t)
      ljmc_proof
      =
 (* ftree/po are the formula tree / open positions of the sequent that caused deadlock let permutation *)
(*  print_endline "!!!!!!!!!!!!!Permutation TO DO!!!!!!!!!"; *)
 (* the open positions in po are either phi_0, psi_0, or gamma_0 positions *)
 (* since proof reconstruction was a deadlock in LJ *)
      let po_treelist = get_formula_treelist ftree po in
      let dir_treelist = List.rev_map (fun x -> (1,x)) po_treelist in
      let formula_rel, rename_list =
         build_formula_rel dir_treelist slist (dummy_pos ())
      in
      let renamed_ljmc_proof = rename_gamma ljmc_proof rename_list in
      let (ptree,ax) =  bproof renamed_ljmc_proof in
      let ljproof = pt ptree formula_rel in
           (* this is a direct formula relation, comprising left/right subformula *)
      begin
(*   print_treelist po_treelist; *)
(*   print_endline "";
   print_endline "";
*)
(*   print_triplelist formula_rel; *)
(*   print_endline "";
   print_endline "";
   tt ljproof;
*)
(*   print_pairlist rename_list; *)
(*   print_endline "";
   print_endline "";
*)
         make_node_list ljproof
      end

(************** PROOF RECONSTRUCTION without redundancy deletion ******************************)

   let rec init_unsolved set = function
      [] -> set
    | Empty :: _ -> set
    | NodeAt pos :: r ->
         init_unsolved (Set.add set pos.pospos) r
    | NodeA(pos,suctrees) :: r ->
         let new_treelist = List.rev_append suctrees r in
            init_unsolved (Set.add set pos.pospos) new_treelist

(* only the unsolved positions will be represented --> skip additional root position *)

   let build_unsolved = function
      NodeA(pos,suctrees) ->
         ((pos.pospos),init_unsolved Set.empty suctrees)
    | Empty | NodeAt _ ->
         raise (Invalid_argument "jall.build_unsolved: bug")

(*
   let rec collect_variables tree_list =
      match tree_list with
         [] -> []
       | f::r ->
            begin match f with
               Empty -> []
             | NodeAt(pos) ->
                  if pos.st = Gamma_0 then
                     pos.name::collect_variables r
                  else
                     collect_variables r
             | NodeA(pos,suctrees) ->
                  let new_tree_list = (Array.to_list suctrees) @ r in
                  if pos.st = Gamma_0 then
                     pos.name::collect_variables new_tree_list
                  else
                     collect_variables new_tree_list
            end

   let rec extend_sigmaQ sigmaQ vlist =
      match vlist with
         [] -> []
       | f::r ->
            let vf = mk_var_term f in
            if List.exists (fun x -> (fst x = vf)) sigmaQ then
               extend_sigmaQ sigmaQ r
            else
(* first let second component are var terms in MetaPRL *)
               (vf,vf) :: (extend_sigmaQ sigmaQ r)

   let build_sigmaQ sigmaQ ftree =
      let vlist = collect_variables [ftree] in
      sigmaQ @ (extend_sigmaQ sigmaQ vlist)
*)

(* subformula relation subrel is assumed to be represented in pairs
   (a,b) *)

   let rec delete compare e = function    (* e must not necessarily occur in list *)
      [] -> []               (* e must not necessarily occur in list *)
    | first::rest ->
         if compare e first then
            rest
         else
            first::(delete compare e rest)

   let rec key_delete changed fpos pos_list =   (* in key_delete, f is a pos name (key) but sucs is a list of positions *)
      match pos_list with
         [] ->
				changed, [] (* the position with name f must not necessarily occur in pos_list *)
       | f::r ->
            if position_eq fpos f.pospos then
               true, r
            else
               let changed, rest = key_delete changed fpos r in
					changed, f::rest

   let rec get_roots treelist =
      match treelist with
         [] -> []
       | f::r ->
            match f with
               Empty -> (get_roots r)    (* Empty is posible below alpha-nodes after purity *)
             | NodeAt(pos) -> pos::(get_roots r)
             | NodeA(pos,trees) ->  pos::(get_roots r)

   let rec comp_ps padd ftree =
      match ftree with
         Empty -> raise (Invalid_argument "Jprover bug: empty formula tree")
       | NodeAt _ ->
            raise (Invalid_argument "Jprover bug: leaves have no successors")
       | NodeA(pos,strees) ->
            match padd with
               [] ->
						raise (Invalid_argument "Jprover: empty address")
             | [f] ->
				 		begin match List.nth strees f with
							Empty ->
								raise (Invalid_argument "Jprover bug: empty formula tree")
						 |	NodeAt _ ->
						 		pos, []
						 |	NodeA(_, strees) ->
								pos, get_roots strees
						end
             | f::r -> comp_ps r (List.nth strees f)

(* computes a list: first element predecessor, next elements successoes of p *)

   let tpredsucc p ftree =
      comp_ps p.address ftree

(* set an element in an array, without side effects *)

   let rec myset i element = function
      hd::tl ->
         if i = 0 then element::tl
         else hd::(myset (pred i) element tl)
    | [] -> []

   let rec compute_open treelist slist =
      match treelist with
         [] -> []
       | first::rest ->
            let elements =
               match first with
                  Empty -> []
                | NodeAt(pos) ->
                     if (Set.mem slist pos.pospos) then
                        [pos]
                     else
                        []
                | NodeA(pos,suctrees) ->
                     if (Set.mem slist pos.pospos) then
                        [pos]
                     else
                        compute_open suctrees slist
            in
            elements @ (compute_open rest slist)
				(* rev_append does not work here
				 * it changes proofs and make the whole thing slower
				 *)

   let select_connection pname connections slist =
      let unsolved = ConnSet.filter
         (fun (a,b) ->
            if position_eq a pname then
               not (Set.mem slist b)
            else if position_eq b pname then
               not (Set.mem slist a)
            else
               false
         )
         connections
      in
      if ConnSet.is_empty unsolved then
         None
      else
         Some (ConnSet.choose unsolved)

   let rec replace_element element element_set redord =
      match redord with
         [] -> raise (Invalid_argument "jall.replace_element: bug")   (* element occurs in redord *)
       | (f,fset)::r ->
            if position_eq f element then
               (f,element_set)::r
            else
               (f,fset)::(replace_element element element_set r)

   let rec collect_succ_sets sucs redord =
      match redord with
         [] -> Set.empty
       | (f,fset)::r ->
            let changed, new_sucs = key_delete false f sucs in
				if changed then
					Set.union (Set.add fset f) (collect_succ_sets new_sucs r)
				else (* position with name f did not occur in sucs -- no deletion *)
               collect_succ_sets sucs r

   let replace_ordering psucc_pos sucs redord =
      let new_psucc_set = collect_succ_sets sucs redord in
(*   print_string_set new_psucc_set; *)
      replace_element psucc_pos new_psucc_set redord

   let rec update pospos redord =
      match redord with
         [] -> []
       | (f,fset)::r ->
            if position_eq pospos f then
               r
            else
               (f,fset)::(update pospos r)

(*  rule construction *)

   let selectQ_rec spos_var csigmaQ =
      try
         PMap.find csigmaQ spos_var
      with Not_found ->
         mk_pos_var spos_var (* dynamic completion of csigmaQ *)

   let selectQ spos_pos csigmaQ =
      let spos_var = simple_to_gamma spos_pos in
      selectQ_rec spos_var csigmaQ

   let apply_sigmaQ term sigmaQ =
      let substQ =
         PMap.fold
            (fun l p t -> (pos_to_symbol p, t)::l)
            []
            sigmaQ
      in
      apply_subst substQ term

   let build_rule pos spos
      csigmaQ
      orr_flag calculus
      =
      let inst_label = apply_sigmaQ (pos.label) csigmaQ in
      match pos.op,pos.pol with
         Null,_ -> raise (Invalid_argument "Jprover: no rule")
       | At,Zero -> Ax,(inst_label),xnil_term (* to give back a term *)
       | At,One -> Ax,(inst_label),xnil_term
       | And,Zero -> Andr,(inst_label),xnil_term
       | And,One -> Andl,(inst_label),xnil_term
       | Or,Zero ->
            begin match calculus with
               Intuit SingleConcl ->
                  (if orr_flag = 1 then Orr1 else Orr2),(inst_label),xnil_term
             | _ ->
                  Orr,(inst_label),xnil_term
            end
       | Or,One -> Orl,(inst_label),xnil_term
       | Neg,Zero -> Negr,(inst_label),xnil_term
       | Neg,One -> Negl,(inst_label),xnil_term
       | Imp,Zero -> Impr,(inst_label),xnil_term
       | Imp,One -> Impl,(inst_label),xnil_term
       | All,One -> Alll,(inst_label),(selectQ spos.pospos csigmaQ)  (* elements of csigmaQ is (string * term) *)
       | Ex,Zero -> Exr,(inst_label), (selectQ spos.pospos csigmaQ)
       | All,Zero -> Allr,(inst_label),(mk_pos_var spos.pospos) (* must be a proper term *)
       | Ex,One -> Exl,(inst_label),(mk_pos_var spos.pospos) (* must be a proper term *)
       | Box _,One -> Boxl,inst_label,xnil_term
       | Box _,Zero -> Boxr,inst_label,xnil_term

(* %%%%%%%%%%%%%%%%%%%% Split begin %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% *)

   let rec nonemptys sum = function
      Empty::tl -> nonemptys sum tl
    | _::tl -> nonemptys (succ sum) tl
    | [] -> sum

   let rec collect_pure set = function
      [] -> []
    | Empty :: r ->
         collect_pure set r
    | NodeAt(pos) :: r ->
         let {pospos=pospos; _} = pos in
         if Set.mem set pospos then
            collect_pure set r
         else
            pos :: collect_pure set r
    | NodeA(pos,treearray) :: r ->
         List.rev_append (collect_pure set treearray) (collect_pure set r)

   let update_connections slist connections =
      ConnSet.filter
         (fun (a,b) -> not (Set.mem slist a or Set.mem slist b))
         connections

   let rec update_redord delset redord =   (* delset is the set of positions to be deleted *)
      match redord with
         [] -> []
       | (f,fset)::r ->
            if (Set.mem delset f) then
               update_redord delset r   (* delete all key elements f from redord which are in delset *)
            else
               let new_fset = Set.diff fset delset in  (* no successor of f from delset should remain in fset *)
               (f,new_fset)::(update_redord delset r)

   let rec get_position_names set = function
      [] -> set
    | Empty :: rests -> get_position_names set rests
    | NodeAt{pospos=pospos; _} :: rests ->
         get_position_names (Set.add set pospos) rests
    | NodeA({pospos=pospos; _},strees) :: rests ->
         get_position_names (Set.add set pospos) (List.rev_append strees rests)

(* unused
   let rec print_purelist = function
      [] ->
         begin
            print_string ".";
            print_endline " ";
         end
    | f::r ->
         print_string ((pos_to_string f.pospos)^", ");
         print_purelist r
*)

   let update_relations deltree redord connections unsolved_list =
      let pure_names = get_position_names Set.empty [deltree] in
      begin
(*        print_ftree deltree;
   open_box 0;
   print_endline " ";
   print_stringlist pure_names;
   force_newline ();
   print_flush ();
*)
         let rednew = update_redord pure_names redord in
         let connew = update_connections pure_names connections in
         let unsolnew = Set.diff unsolved_list pure_names in
         (rednew,connew,unsolnew)
      end

   let rec collect_qpos ftreelist uslist =
      match ftreelist with
         [] -> [],Set.empty
       | Empty :: rest ->
            collect_qpos rest uslist
       | NodeAt {st=Gamma_0; pospos=pospos; _} :: rest ->
            let rest_delta, rest_gamma = collect_qpos rest uslist in
            if Set.mem uslist pospos then
               rest_delta, Set.add rest_gamma pospos
            else
               rest_delta, rest_gamma
       | NodeAt {st=Delta_0; pospos=pospos; _} :: rest ->
            let rest_delta, rest_gamma = collect_qpos rest uslist in
            if Set.mem uslist pospos then
               (pospos::rest_delta), rest_gamma
            else
               rest_delta, rest_gamma
       | NodeAt _ :: rest ->
            let rest_delta, rest_gamma = collect_qpos rest uslist in
            rest_delta, rest_gamma
       | NodeA({st=Gamma_0; pospos=pospos; _},suctrees) :: rest ->
            let rest_delta, rest_gamma = collect_qpos (List.rev_append suctrees rest) uslist in
            if Set.mem uslist pospos then
               rest_delta, Set.add rest_gamma pospos
            else
               rest_delta, rest_gamma
       | NodeA({st=Delta_0; pospos=pospos; _},suctrees) :: rest ->
            let rest_delta, rest_gamma = collect_qpos (List.rev_append suctrees rest) uslist in
            if Set.mem uslist pospos then
               (pospos::rest_delta), rest_gamma
            else
               rest_delta, rest_gamma
       | NodeA(_, suctrees) :: rest ->
            let rest_delta, rest_gamma = collect_qpos (List.rev_append suctrees rest) uslist in
            rest_delta, rest_gamma

   let do_split gamma_diff sigmaQ =
      PMap.isect_mem
         sigmaQ
         (fun v -> not (Set.mem gamma_diff (gamma_to_simple v)))

(* make a term list out of a bterm list *)

   let rec check_delta_terms v p term ass_delta_diff dterms =
      match ass_delta_diff with
         [] -> term,[]
       | (var,dname)::r ->
            if Set.mem dterms dname then
               let new_var, new_pos =
	(* XXX Yegor: position_eq instead of = could be dangerous here - index 0 is not unique !!! *)
                  if position_eq var empty_pos then
                     v,p
                  else
                     pos_to_symbol var, var
               in
               let replace_var = pos_to_symbol dname in
               let next_term = TermSubst.subst1 term replace_var (mk_var_term new_var) in
               let (new_term,next_diffs) = check_delta_terms v p next_term r dterms in
               (new_term,((new_pos,dname)::next_diffs))
            else
               let (new_term,next_diffs) = check_delta_terms v p term r dterms in
               (new_term,((var,dname)::next_diffs))

   let localize_sigma_aux consts ass_delta_diff p term =
      let dterms = collect_delta_terms consts Set.empty [term] in
      let v = pos_to_symbol p in
      let new_term, new_ass_delta_diff = check_delta_terms v p term ass_delta_diff dterms in
      new_term

   let localize_sigma consts zw_sigma ass_delta_diff =
      PMap.mapi (localize_sigma_aux consts ass_delta_diff) zw_sigma

   let subst_split consts ft1 ft2 ftree uslist1 uslist2 uslist
      (sigmaQ : term PMap.t)
      =
      let delta,gamma = collect_qpos [ftree] uslist in
      let delta1,gamma1 = collect_qpos [ft1] uslist1 in
      let delta2,gamma2 = collect_qpos [ft2] uslist2 in
      let delta_diff1 = list_diff delta delta1 in
      let delta_diff2 = list_diff delta delta2 in
      let gamma_diff1 = Set.diff gamma gamma1 in
      let gamma_diff2 = Set.diff gamma gamma2 in
      let zw_sigma1 = do_split gamma_diff1 sigmaQ in
      let zw_sigma2 = do_split gamma_diff2 sigmaQ in
      let ass_delta_diff1 =
			List.rev_map (fun x -> (empty_pos, x)) delta_diff1
		in
      let ass_delta_diff2 =
			List.rev_map (fun x -> (empty_pos, x)) delta_diff2
		in
      let sigmaQ1 = localize_sigma consts zw_sigma1 ass_delta_diff1 in
      let sigmaQ2 = localize_sigma consts zw_sigma2 ass_delta_diff2 in
      (sigmaQ1,sigmaQ2)

   let rec reduce_tree addr actual_node ftree beta_flag =
      match addr, ftree with
         [], _ -> (ftree,Empty,actual_node,beta_flag)
       | a::radd, NodeA(pos,strees) ->
(*       print_endline pos.name; *)
    (* the associated node occurs above f (or the empty address) let hence, is neither atom nor empty tree *)
            let nexttree = List.nth strees a in
            if (nonemptys 0 strees) < 2 then
               begin
(*          print_endline "strees 1 or non-empties < 2"; *)
                  let (ft,dt,an,bf) =  reduce_tree radd actual_node nexttree beta_flag in
                  let nstrees = myset a ft strees in
(*            print_endline ("way back "^pos.name); *)
                  (NodeA(pos,nstrees),dt,an,bf)
               end
            else  (* nonemptys >= 2 *)
               begin
(*             print_endline "nonempties >= 2 "; *)
                  let (new_act,new_bf) =
                     if pos.pt = Beta then
                        (actual_node,true)
                     else
                        (pos.pospos,false)
                  in
                  let (ft,dt,an,bf) = reduce_tree radd new_act nexttree new_bf in
                  if position_eq an pos.pospos then
                     let nstrees = myset a Empty strees in
(*                 print_endline ("way back assocnode "^pos.name); *)
                     (NodeA(pos,nstrees),nexttree,an,bf)
                  else  (* has been replaced / will be replaced below / above pos *)
                     let nstrees = myset a ft strees in
(*                 print_endline ("way back "^pos.name); *)
                     (NodeA(pos,nstrees),dt,an,bf)
               end
       | _, Empty ->
            print_endline "Empty purity tree";
            raise (Invalid_argument "jall.reduce_tree: bug")
       | _, NodeAt(_) ->
            print_endline "Atom purity tree";
            raise (Invalid_argument "jall.reduce_tree: bug")

   let rec purity ftree redord
      (connections : ConnSet.t)
      unsolved_list
      =

      let rec purity_reduction pr ftree redord
         (connections : ConnSet.t)
         unsolved_list
         =
         begin
(*        open_box 0;
   print_endline " ";
   print_purelist pr;
   force_newline ();
   print_flush ();
*)
            match pr with
               [] -> (ftree,redord,connections,unsolved_list)
             | f::r ->
(*   print_endline ("pure position "^(f.name)); *)
                  let (ftnew,deltree,assocn,beta_flag) =
                     reduce_tree f.address empty_pos ftree false
                  in
						(*     print_endline ("assoc node "^assocn); *)
(* XXX Yegor: don't replace = with position_eq here - index 0 is not unique *)
                  if assocn = empty_pos then
                     (Empty,[],ConnSet.empty,Set.empty)  (* should not occur in the final version *)
                  else
                     let (rednew,connew,unsolnew) = update_relations deltree redord connections unsolved_list in
                     begin
(*        open_box 0;
   print_endline " ";
   print_pairlist connew;
   force_newline ();
   print_flush ();
*)
                        if beta_flag then
                           begin
(*          print_endline "beta_flag true"; *)
                              purity ftnew rednew connew unsolnew
  (* new pure positions may occur; old ones may not longer exist *)
                           end
                        else
                           purity_reduction r ftnew rednew connew unsolnew (* let's finish the old pure positions *)
                     end
         end

      in
      let solvedset = (* I guess it's a solved set, if connections is a set of solved connections *)
         ConnSet.fold
            (fun set (a,b) -> Set.add (Set.add set a) b)
            Set.empty
            connections
      in
      let pr = collect_pure solvedset [ftree] in
      purity_reduction pr ftree redord connections unsolved_list

   let rec betasplit addr ftree redord connections unsolved_list =
      match ftree, addr with
         NodeA(pos, [st1tree;st2tree]), [] ->
            (* we are at the beta node under consideration *)
            let (zw1red,zw1conn,zw1uslist) = update_relations st2tree redord connections unsolved_list in
            let (zw2red,zw2conn,zw2uslist) = update_relations st1tree redord connections unsolved_list in
               ((NodeA(pos,[st1tree;Empty])),zw1red,zw1conn,zw1uslist),
               ((NodeA(pos,[Empty;st2tree])),zw2red,zw2conn,zw2uslist)
       | NodeA(pos, _), [] ->
            raise (Invalid_argument "jall.betasplit: bug")
       | NodeA(pos, strees), f::rest ->
            let nexttree = List.nth strees f in
            let (zw1ft,zw1red,zw1conn,zw1uslist),(zw2ft,zw2red,zw2conn,zw2uslist) =
               betasplit rest nexttree redord connections unsolved_list in
(*          let scopytrees = Array.copy strees in *)
            let zw1trees = myset f zw1ft strees in
            let zw2trees = myset f zw2ft strees in
               (NodeA(pos,zw1trees),zw1red,zw1conn,zw1uslist),(NodeA(pos,zw2trees),zw2red,zw2conn,zw2uslist)
       | Empty, _  ->
            print_endline "bsplit Empty tree";
            raise (Invalid_argument "jall.betasplit: bug")
       | NodeAt(_), _ ->
            print_endline "bsplit Atom tree";
            raise (Invalid_argument "jall.betasplit: bug")
				(* the beta-node should actually occur! *)

   let split addr pname ftree redord connections unsolved_list opt_bproof =
      let (opt_bp1,min_con1),(opt_bp2,min_con2) = split_permutation pname opt_bproof in
      begin
(*
   print_endline "Beta proof 1: ";
   print_endline "";
   print_beta_proof opt_bp1;
   print_endline "";
   print_endline ("Beta proof 1 connections: ");
   open_box 0;
   print_pairlist min_con1;
   print_endline ".";
   print_flush();
   print_endline "";
   print_endline "";
   print_endline "Beta proof 2: ";
   print_endline "";
   print_beta_proof opt_bp2;
   print_endline "";
   print_endline ("Beta proof 2 connections: ");
   open_box 0;
   print_pairlist min_con2;
   print_endline ".";
   print_flush();
   print_endline "";
*)
         let (zw1ft,zw1red,zw1conn,zw1uslist),(zw2ft,zw2red,zw2conn,zw2uslist) =
            betasplit addr ftree redord connections unsolved_list in
(* zw1conn let zw2conn are not longer needed when using beta proofs *)
(*   print_endline "betasp_out"; *)
         let ft1,red1,conn1,uslist1 =  purity zw1ft zw1red min_con1 zw1uslist in
(*   print_endline "purity_one_out"; *)
         let ft2,red2,conn2,uslist2 =  purity zw2ft zw2red min_con2 zw2uslist in
(*        print_endline "purity_two_out"; *)
(* again, min_con1 = conn1 let min_con2 = conn2 should hold *)
         begin
(* print_endline "";
   print_endline "";
   print_endline ("Purity 1 connections: ");
   open_box 0;
   print_pairlist conn1;
   print_endline ".";
   print_endline "";
   print_flush();
   print_endline "";
   print_endline "";
   print_endline ("Purity 2 connections: ");
   open_box 0;
   print_pairlist conn2;
   print_endline ".";
   print_endline "";
   print_flush();
   print_endline "";
   print_endline "";
*)
            (ft1,red1,conn1,uslist1,opt_bp1),(ft2,red2,conn2,uslist2,opt_bp2)
         end
      end

(* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Splitting end %%%%%%%%%%%%%%%%  *)

(* for wait labels we collect all solved atoms with pol=Zero *)

   let rec exists_solved_Zero_At ftreelist slist =
      match ftreelist with
         [] ->
            false
       | Empty :: r ->    (* may become possible after purity *)
            exists_solved_Zero_At r slist
       | NodeAt {pospos=pospos; pol=pospol; _} :: r ->
            if ((Set.mem slist pospos) or (pospol = One)) then  (* recall slist is the unsolved list *)
               exists_solved_Zero_At r slist
            else
    (* here, we have pos solved let pos.pol = Zero) *)
               true
       | NodeA({pospos=pospos; _},treearray) :: r ->
            if Set.mem slist pospos then
(* XXX Yegor: this is probably a too agressive optimization but AFAIU
* unsolved positions can not have solved successors
*)
               exists_solved_Zero_At r slist
            else
               exists_solved_Zero_At (List.rev_append treearray r) slist

   let rec collect_solved_Zero_At ftreelist slist =
      match ftreelist with
         [] ->
            PosSet.empty
       | Empty :: r ->    (* may become possible after purity *)
            collect_solved_Zero_At r slist
       | NodeAt ({pospos=pospos; pol=pospol; _} as pos) :: r ->
            if (pospol = One) or (Set.mem slist pospos) then  (* recall slist is the unsolved list *)
               collect_solved_Zero_At r slist
            else
    (* here, we have pos solved let pos.pol = Zero) *)
               PosSet.add (collect_solved_Zero_At r slist) pos
       | NodeA({pospos=pospos; _},treearray) :: r ->
            if Set.mem slist pospos then
(* XXX Yegor: this is probably a too agressive optimization but AFAIU
* unsolved positions can not have solved successors
*)
               collect_solved_Zero_At r slist
            else
               collect_solved_Zero_At (List.rev_append treearray r) slist

   let rec collect_solved_atoms ftreelist slist =
      match ftreelist with
         [] ->
            PosSet.empty
       | Empty :: r ->    (* may become possible after purity *)
            collect_solved_atoms r slist
       | NodeAt pos :: r ->
            if Set.mem slist pos.pospos then  (* recall slist is the unsolved list *)
               collect_solved_atoms r slist
            else
				   (* here, we have pos solved *)
               PosSet.add (collect_solved_atoms r slist) pos
       | NodeA({pospos=pospos; _},treearray) :: r ->
            if Set.mem slist pospos then
(* XXX Yegor: this is probably a too agressive optimization but AFAIU
* unsolved positions can not have solved successors
*)
               collect_solved_atoms r slist
            else
               collect_solved_atoms (List.rev_append treearray r) slist

   let rec red_ord_block pospos redord =
      match redord with
         [] -> false
       | (f,fset)::r ->
            if ((position_eq f pospos) or (not (Set.mem fset pospos))) then
               red_ord_block pospos r
            else
               true   (* then, we have (StringSet.mem fset pname) *)

   let rec check_wait_succ_LJ faddress ft =
      match ft, faddress with
         NodeA({op = Or; _}, [Empty; Empty]), [] ->
            raise (Invalid_argument "Jprover: redundancies occur")
       | NodeA({op = Or; _}, [Empty; _]), [] ->
            (false,2)   (* determines the Orr2 rule *)
       | NodeA({op = Or; _}, [_; Empty]), [] ->
            (false,1)   (* determines the Orr1 ruke *)
       | NodeA({op = Or; _}, [_;_]), [] ->
            (true,0)    (* wait-label is set *)
       | NodeA({op = Or; _}, _), [] ->
            raise (Invalid_argument "jall.check_wait_succ_LJ: bug")
       | NodeA _, [] ->
            (false,0)
       | NodeA({pt = Gamma; _},strees), [f] when (nonemptys 0 strees) > 1 ->
            (true,0)
            (* we are at a gamma position (exr) with one than one successor *)
            (* -- wait label in LJ*)
       | NodeA(_,strees), f::r ->
                  check_wait_succ_LJ r (List.nth strees f)
       | Empty, _
       | NodeAt _, _ ->
		 		raise (Invalid_argument "jall.check_wait_succ_LJ: bug")
				(* we have an gamma_0 position or an or-formula *)

   let unclosed_pred popen = function
      None -> false
    | Some pos -> pos.st <> Gamma_0 && PosSet.mem popen pos

   let unclosed_cond popen pred pos =
		match pos.st with
			Nu_0 _ ->
      		(PosSet.mem popen pos || unclosed_pred popen pred)
		 | _ ->
		 		false

   let rec collect_unclosed popen ftree pred =
      match ftree with
         NodeA(f,suctrees) ->
            if unclosed_cond popen pred f then
               PosSet.singleton f
            else
               List.fold_left
                  (fun acc ftree -> PosSet.union acc (collect_unclosed popen ftree (Some f)))
                  PosSet.empty
                  suctrees
       | Empty ->
            PosSet.empty
       | NodeAt f ->
            if unclosed_cond popen pred f then
               PosSet.singleton f
            else
               PosSet.empty

   let blocked f po
      (redord : (position * Set.t) list)
      ftree connections
      (slist : Set.t)
      solved_atoms
      unclosed
      calculus opt_bproof
      =
      if !debug_s4prover then
          print_endline ("Blocking check "^(pos_to_string f.pospos));
      if red_ord_block f.pospos redord then
         begin
            if !debug_s4prover then
               print_endline "wait-1 check positive";
            true,0
         end
      else
         match calculus with
            Classical ->
               false,0  (* ready, in C only redord counts *)
          | S4 ->
               let pu = PosSet.remove (PosSet.union solved_atoms po) f in
               if !debug_s4prover then
                  begin
                     print_string "po:";
                     print_pos_set po;
                     print_string "slist:";
                     print_positionset slist;
                     print_string "unclosed:";
                     print_pos_set unclosed
                  end;
					begin match f.pt with
               	Pi sort ->
		               (*(not(List.exists
      	            (fun (p,s) -> p=f.pospos & not (Set.is_empty s)) redord)) &*)
							(*let u_nu0 y = Set.mem unclosed y || Set.subset () unclosed in
							Set.exists (fun y -> not u_nu0 y) pu, 0*)
         	   	   (not (PosSet.is_empty (PosSet.diff pu unclosed))) ||
							PosSet.exists
								(fun {st=st; _} ->
									match st with
										Nu_0 sort' -> sort'<>0 && sort'<>sort
									 | _ -> false
								)
								po,
							0
					 | _ ->
					 		false, 0
					end
          | Intuit calc ->
               let pa_Zero = solved_atoms in    (* solved atoms in ftree *)
               let po_test = PosSet.remove po f in
               (* we provide dynamic wait labels for both sequent calculi *)
               begin match calc with
                  MultiConcl ->
(*                   print_endline "wait-2 check"; *)
                     if (f.st = Psi_0)  &  (f.pt <> PNull) &
                        (not (PosSet.is_empty pa_Zero) or not (PosSet.is_empty po_test)) then
                        begin
(*                         print_endline "wait-2 positive"; *)
                           true,0 (* wait_2 label *)
                        end
                     else
                        begin
(*                         print_endline "wait-2 negative"; *)
                           false,0
                        end
                | SingleConcl ->
                     if ((f.st = Phi_0)  & ((f.op=Neg) or (f.op=Imp)) &
                          (not (PosSet.is_empty pa_Zero) or not (PosSet.is_empty po_test))
                        )
         (* this would cause an impl or negl rule with an non-empty succedent *)
                     then
                        if (f.op=Neg) then
                           true,0
                        else  (* (f.op=Imp) *)
                 (* In case of an impl rule on A => B, the wait_label must NOT be set
                    iff all succedent formulae depend exclusively on B. For this, we
                    perform a split operation let determine, if in the A-subgoal
                    all succedent formulae are pure, i.e.~have been deleted from treds.
                    Otherwise, in case of A-dependent succedent formulae, the
                    wait_label must be set.
                 *)
                           let ((_,min_con1),_) =
                              split_permutation f.pospos opt_bproof
                           in
                           let slist_fake = Set.remove slist f.pospos in
                           let ((zw1ft,zw1red,_,zw1uslist),_) =
                              betasplit f.address ftree redord connections slist_fake in
                           let ft1,_,_,uslist1 = purity zw1ft zw1red min_con1 zw1uslist in
(*                      print_endline "wait label purity_one_out"; *)
                           let ft1_root = (List.hd (snd (tpredsucc f ft1))) in
(*                    print_endline ("wait-root "^(ft1_root.name)); *)
                           let po_fake = compute_open [ft1] uslist1 in
                           let po_fake_test = delete pos_eq ft1_root po_fake in
                           let pa_Zero_fake = exists_solved_Zero_At [ft1] uslist1 in
(*                     print_purelist (po_fake_test @ pa_O_fake); *)
                              (pa_Zero_fake or
                              (List.exists (fun x -> x.pol = Zero) po_fake_test)), 0
                     else
                        if ((f.pol=Zero) & ((f.st=Gamma_0) or (f.op=Or))) then
                           let (bool,orr_flag) = check_wait_succ_LJ f.address ftree in
                           (bool,orr_flag)
              (* here is determined if orr1 or orr2 will be performed, provided bool=false) *)
              (* orr_flag can be 1 or 2 *)
                        else
                           false,0
               end

   let rec get_beta_preference list actual =
      match list with
         [] -> actual
       | (f,int)::r ->
            if f.op = Imp then
               (f,int)
            else
(*     if f.op = Or then
   get_beta_preference r (f,int)
   else
*)
               get_beta_preference r actual

   exception Gamma_deadlock

   let rec select_pos search_po po redord ftree connections
      (slist : Set.t)
      solved_atoms
      unclosed
      calculus
      candidates
      opt_bproof
      =
      match search_po with
         [] ->
            (match candidates with
               [] ->
                  if calculus = Intuit SingleConcl then
                     raise Gamma_deadlock  (* permutation may be necessary *)
                  else
                     raise (Invalid_argument "Jprover bug: overall deadlock") (* this case should not occur *)
             | c::rest ->
                  get_beta_preference (c::rest) c
            )
       | f::r ->  (* there exist an open position *)
            let bool,orr_flag =
               blocked f po redord ftree connections slist solved_atoms unclosed calculus opt_bproof
            in
		      if !debug_s4prover then
					if bool then
	      			print_endline ((pos_to_string f.pospos)^" blocked")
					else
						print_endline ((pos_to_string f.pospos)^" not blocked");
				print_flush();
            if bool then
               select_pos
                  r po redord ftree connections slist solved_atoms unclosed
                  calculus candidates opt_bproof
            else
               if f.pt = Beta then
     (* search for non-splitting rules first *)
(*      let beta_candidate =
   if candidates = []
   then
   [(f,orr_flag)]
   else
   !!!!  but preserve first found candidate !!!!!!!
   candidates
   in
   !!!!!!! this strategy is not sure the best -- back to old !!!!!!!!!
*)
                  select_pos
                     r po redord ftree connections slist solved_atoms unclosed calculus
                     ((f,orr_flag)::candidates) opt_bproof
               else
                  (f,orr_flag)

(* let rec get_position_in_tree pname treelist =
      match treelist with
         [] -> raise jprover_bug
       | f::r ->
            begin match f with
               Empty -> get_position_in_tree pname r
             | NodeAt(pos) ->
                  if pos.name = pname then
                     pos
                  else
                     get_position_in_tree pname r
             | NodeA(pos,suctrees) ->
                  get_position_in_tree pname ((Array.to_list suctrees) @ r)
            end
*)

(* total corresponds to tot in the thesis,
   tot simulates the while-loop, solve is the rest *)

(* @ in total, tot and solve can not be replaced with rev_append because they
 * are actually constructing a proof (list of rules) which is not permutable
 *)
   let rec total
      ftree
      (redord : (position * Set.t) list)
      (connections : ConnSet.t)
      (csigmaQ : term PMap.t)
      (slist : Set.t)
      calculus
		consts
      opt_bproof
      =
      let po = compute_open [ftree] slist in
      tot ftree redord connections csigmaQ po slist calculus consts opt_bproof

   and tot ftree redord connections csigmaQ po slist calculus consts opt_bproof =
      try
         (*print_poslist po;
         print_endline "select_pos:";*)
         (* last argument for guiding selection strategy *)
         let po_set, solved_atoms, unclosed =
            match calculus with
               S4 ->
                  let po = PosSet.of_list po in
                  po,
                  collect_solved_atoms [ftree] slist,
                  collect_unclosed po ftree None
             | Classical | Intuit _ ->
                 let po =
                     List.fold_left
                        (fun set pos ->
                           match pos.pol with
										Zero ->
											PosSet.add set pos
                            | One ->
											set
                        )
                        PosSet.empty
                        po
                  in
                  po, collect_solved_Zero_At [ftree] slist, PosSet.empty
         in
         let p, orr_flag =
            select_pos
               po po_set redord ftree connections slist solved_atoms unclosed calculus [] opt_bproof
         in
(*    print_endline ((pos_to_string p.pospos)^" "^(string_of_int orr_flag)); *)
         let pred, succs = tpredsucc p ftree in
            let redpo = update p.pospos redord in   (* deletes the entry (p,psuccset) from the redord *)
            let rednew =
               match p.pt with
                  Delta | Pi _ ->      (* keep the tree ordering for the successor position only *)
                     let psucc = List.hd succs in
                     let pre, sucs = tpredsucc psucc ftree in
                        replace_ordering psucc.pospos sucs redpo (* union the succsets of psucc *)
                | _ ->
                     redpo
            in
(*            print_endline "update ok"; *)
            solve
               ftree rednew connections csigmaQ p po slist
               (pred,succs) orr_flag calculus consts opt_bproof
      with Gamma_deadlock ->
         let ljmc_subproof =
            total ftree redord connections csigmaQ slist (Intuit MultiConcl) consts opt_bproof
         in
         permute_ljmc ftree po slist ljmc_subproof
           (* the permuaiton result will be appended to the lj proof constructed so far *)

   and solve
		ftree redord connections csigmaQ p po slist (pred,succs)
		orr_flag calculus consts opt_bproof
		=
      let {pospos=pospos; op=op; pt=pt; st=st; _ } = p in
      let newslist = Set.remove slist pospos in
      let rback =
         match st with
            Gamma_0 | Nu_0 _ ->
               begin
(*             print_endline "that's the gamma rule";  *)
                  [((pospos,pred.pospos),(build_rule pred p csigmaQ orr_flag calculus))]
               end
          | _ ->
               []
      in
(*        print_endline "gamma check finish";  *)
      let pnew =
         if pt <> Beta then
            succs @ (delete pos_eq p po)
         else
            po
      in
      match pt with
         Pi _ ->
            let rule = build_rule p p csigmaQ orr_flag calculus in
            let rest =
               tot ftree redord connections csigmaQ pnew newslist calculus consts opt_bproof
            in
            rback @ (((empty_pos,pospos),rule)::rest)
       | Nu _
       | Gamma ->
            rback @ (tot ftree redord connections csigmaQ pnew newslist calculus consts opt_bproof)
       | Psi ->
            begin match op, succs with
               At, succ::_ ->
                  let rest =
                     solve
                       ftree redord connections csigmaQ succ pnew newslist
                       (p,[]) orr_flag calculus consts opt_bproof
                  in
                  rback @ rest  (* solve atoms immediately *)
             | At, [] ->
                  raise (Invalid_argument "solve: op=At but succs=[]")
             | _ ->
                  rback @ (tot ftree redord connections csigmaQ pnew newslist calculus consts opt_bproof)
            end
       | Phi ->
            begin match op, succs with
               At, succ::_ ->
                  let rest =
                     solve
                        ftree redord connections csigmaQ succ pnew newslist
                        (p,[]) orr_flag calculus consts opt_bproof
                  in
                     rback @ rest  (* solve atoms immediately *)
             | At, [] ->
                  raise (Invalid_argument "total: empty succs in Phi")
             | _ ->
                  rback @ (tot ftree redord connections csigmaQ pnew newslist calculus consts opt_bproof)
            end
       | PNull ->
            let new_redord = update pospos redord in
            begin match select_connection pospos connections newslist with
               None ->
                  let rest =
                     tot ftree new_redord connections csigmaQ pnew newslist calculus consts opt_bproof
                  in
                  rback @ rest
             | Some (c1,c2) ->
                  let (ass_pos,inst_pos) =
(* need the pol=O position ass_pos of the connection for later permutation *)
(* need the pol=I position inst_pos for NuPRL instantiation *)
                     if position_eq pospos c1 then
                        match p.pol with
                           Zero ->
                              (c1,c2)
                         | One ->
                              (c2,c1)
                     else (* p.name = c2 *)
                        match p.pol with
                           Zero ->
                              (c2,c1)
                         | One ->
                              (c1,c2)
                  in
                  rback @ [((empty_pos,ass_pos),(build_rule p p csigmaQ orr_flag calculus))]
            end
   (* one possibility of recursion end *)
       | Alpha ->
            let rule = build_rule p p csigmaQ orr_flag calculus in
            let rest = tot ftree redord connections csigmaQ pnew newslist calculus consts opt_bproof in
            rback @ (((empty_pos,pospos),rule)::rest)
       | Delta ->
            begin match succs with
               sp::_ ->
                  let rule = build_rule p sp csigmaQ orr_flag calculus in
                  let rest =
                     tot ftree redord connections csigmaQ pnew newslist calculus consts opt_bproof
                  in
                  rback @ (((empty_pos,pospos),rule)::rest)
             | [] ->
                  raise (Invalid_argument "total: empty succs in Delta")
            end
       | Beta ->
(*             print_endline "split_in"; *)
            let (ft1,red1,conn1,uslist1,opt_bproof1),(ft2,red2,conn2,uslist2,opt_bproof2) =
               split p.address p.pospos ftree redord connections newslist opt_bproof
            in
            let (sigmaQ1,sigmaQ2) =
               subst_split consts ft1 ft2 ftree uslist1 uslist2 newslist csigmaQ
            in
(*           print_endline "split_out"; *)
            let p1 = total ft1 red1 conn1 sigmaQ1 uslist1 calculus consts opt_bproof1 in
(*           print_endline "compute p1 out";              *)
            let p2 = total ft2 red2 conn2 sigmaQ2 uslist2 calculus consts opt_bproof2 in
(*           print_endline "compute p2 out";              *)
            rback @ [((empty_pos,pospos),(build_rule p p csigmaQ orr_flag calculus))] @ p1 @ p2  (* second possibility of recursion end *)

   let reconstruct ftree redord sigmaQ ext_proof calculus consts =
      (* construct_opt_beta_proof seems to need ordered ext_proof *)
      let (opt_bproof,beta_exp,closures) = construct_opt_beta_proof ftree ext_proof in
(* let connections = remove_dups_connections ext_proof in
   let bproof,beta_exp,closures = construct_beta_proof ftree connections in
   let (opt_bproof,min_connections) = bproof_purity bproof in
*)
      if !debug_jprover then
         begin
            print_endline "";
            print_endline ("Beta proof with number of closures = "^(string_of_int closures)^" let number of beta expansions = "^(string_of_int beta_exp));
(*   print_endline "";
   print_endline "";
   print_beta_proof bproof;
   print_endline "";
   print_endline "";
   print_endline "Optimal beta proof: ";
   print_endline "";
   print_endline "";
   print_beta_proof opt_bproof;
   print_endline "";
   print_endline "";
   print_endline ("Beta proof connections: ");
   open_box 0;
   print_pairlist min_connections;
   print_endline ".";
   print_flush(); *)
            print_endline "";
         end;
      let (newroot_name,unsolved_list) =  build_unsolved ftree in
      let redord2 = (update newroot_name redord) in   (* otherwise we would have a deadlock *)
      if !debug_s4prover then
         begin
            print_endline "before purity:";
            print_ordering redord2
         end;
      let min_connections = ConnSet.of_list ext_proof in
      let (init_tree,init_redord,init_connections,init_unsolved_list) =
         purity ftree redord2 min_connections unsolved_list
      in
      begin
(*   print_endline "";
   print_endline "";
   print_endline ("Purity connections: ");
   open_box 0;
   print_pairlist init_connections;
   print_endline ".";
   print_endline "";
   print_flush();
   print_endline "";
   print_endline "";
*)
      if !debug_s4prover then
         begin
            print_endline "before total:";
            print_ordering init_redord;
            print_endline "total:"
         end;
(* it should hold: min_connections = init_connections *)
         total init_tree init_redord init_connections sigmaQ
            init_unsolved_list calculus consts opt_bproof
      end

(* ****** Quantifier unification ************** *)

(* For multiplication we assume always idempotent substitutions sigma, tau! *)

   let collect_assoc inst_vars tauQ =
		Set.fold
			(fun acc f ->
				try
					(PMap.find tauQ f)::acc
				with Not_found ->
					acc
			)
			[]
			inst_vars

   let rec_apply_aux consts tau_map sigma_ordering v term =
		let old_free =
			SymbolSet.fold
				(fun acc s -> Set.add acc (symbol_to_pos s))
				Set.empty
				(SymbolSet.diff (free_vars_set term) consts)
		in
      let inst_terms = collect_assoc old_free tau_map in
      if inst_terms = [] then
         sigma_ordering
      else
         (*let ordering_v = String.sub v 0 (String.index v '_') in*)
         let ordering_v = gamma_to_simple v in
         ((ordering_v,inst_terms)::sigma_ordering)

   let rec_apply
      (consts : SymbolSet.t)
      (sigmaQ : term PMap.t)
      (tau_map : term PMap.t)
      =
      PMap.fold (rec_apply_aux consts tau_map) [] sigmaQ

   let multiply
      (consts : SymbolSet.t)
      (sigmaQ : term PMap.t)
      (tau_map : term PMap.t)
      =
      let sigma_ordering = rec_apply consts sigmaQ tau_map
      in
      let ordering =
         PMap.fold
            (fun acc v t ->
					if PMap.mem sigmaQ v then
						acc
					else
						(gamma_to_simple v, [t])::acc
				)
            sigma_ordering
            tau_map
      in
      ordering

   let jqunify
      (consts : SymbolSet.t)
      term1
      term2
      (sigmaQ : term PMap.t)
		counter
		=
		let eqns = PMap.fold (fun acc p t -> (mk_pos_var p, t)::acc) [] sigmaQ in
		let eqnl = eqnlist_append_eqns eqnlist_empty ((term1, term2)::eqns) in
      try
         let tauQ = unify_eqnl eqnl consts in
         let tau_map =
            List.fold_left
               (fun map (v,t) -> PMap.add map (symbol_to_pos v) t)
               PMap.empty
               tauQ
         in
         let oel = multiply consts sigmaQ tau_map in
         tau_map, oel
      with
         RefineError _  ->  (* any unification failure *)
				if !debug_s4prover then
					eprintf "FO-unification in S4 mode!!!@.";
            raise (Failed(counter))   (* new connection, please *)

let one_equation_aux gprefix delta_0_prefixes rest_equations f =
   let fprefix = PMap.find delta_0_prefixes f in
   let (sf1,sg) = shorten fprefix gprefix in
   let v_new = NewVarQ 0, Lm_symbol.new_number () in
   let fnew = sf1 @ [v_new] in
   (([],(fnew,sg))::rest_equations)

let one_equation acc gprefix dlist delta_0_prefixes =
   Set.fold (one_equation_aux gprefix delta_0_prefixes) acc dlist

let rec make_domain_equations acc fo_pairs gamma_0_prefixes delta_0_prefixes =
   match fo_pairs with
      [] -> acc
    | (g,dlist)::r ->
         let gprefix = PMap.find gamma_0_prefixes g in
         let gequations = one_equation acc gprefix dlist delta_0_prefixes in
         make_domain_equations gequations r gamma_0_prefixes delta_0_prefixes

(* type of one unifier: int * ((string * string list) list)  *)
(* global failure: (0,[]) *)

let stringunify ext_atom try_one equations fo_pairs calculus orderingQ atom_rel qprefixes	tl counter =
   match calculus with
      Classical -> ((0,[]),[],orderingQ,[])
    | S4
    | Intuit _ ->
         let us = ext_atom.aposprefix in
         let ut = try_one.aposprefix in
         let ns = ext_atom.apos in
         let nt = try_one.apos in
         let gamma, delta = qprefixes in
         if PMap.is_empty gamma && PMap.is_empty delta then
            (* prop case *)
            (* prop unification only *)
            let new_sigma,new_eqlist,_,new_tracelist  =
               JTUnifyProp.do_stringunify calculus us ut ns nt equations [] PMap.empty Set.empty tl counter
            in
               (new_sigma,new_eqlist,PMap.empty,new_tracelist)
					(* assume the empty reduction ordering during proof search *)
         else (* "This is the FO case" *)
            (* fo_eqlist encodes the domain condition on J quantifier substitutions *)
            (* Again, always computed for the whole substitution sigmaQ *)
            let fo_eqlist = make_domain_equations [] fo_pairs gamma delta in
            (*
            open_box 0;
            print_string "domain equations in";
            print_equations fo_eqlist;
            print_string "domain equations out";
            print_flush ();
            *)
				do_stringunify calculus us ut ns nt equations fo_eqlist orderingQ atom_rel tl counter

(**************************************** add multiplicity *********************************)

let update_position position replace_n subst_list mult =
   let ({address=y; pospos=pospos; op=z; pol=p; pt=a; st=b; label=t}) = position in
   let k, _ = pospos in
   let npospos = k, Lm_symbol.new_number () in
   let nsubst_list =
      if b=Gamma_0 then
         let vx = pos_to_symbol (simple_to_gamma pospos) in
         let vnx = mk_pos_var (simple_to_gamma npospos) in
         (vx,vnx)::subst_list
      else
         if b=Delta_0 then
            let sx = pos_to_symbol pospos in
            let snx = mk_pos_var npospos in
            (sx,snx)::subst_list
         else
            subst_list
   in
   let nt = TermSubst.apply_subst nsubst_list t in
   let new_add = myset replace_n (pred mult) y in
   {address=new_add; pospos=npospos;
    op=z; pol=p; pt=a; st=b; label=nt},nsubst_list

let rec union_orderings first_orderings =
   match first_orderings with
      [] ->
         Set.empty
    | (pos,fset)::r ->
         Set.union (Set.add fset pos) (union_orderings r)

let rec select_orderings
   (add_orderings : ((position * Set.t) list) list)
   =
   match add_orderings with
      [] -> []
    | f::r ->
         (List.hd f)::select_orderings r

let combine_ordering_list
   (add_orderings : ((position * Set.t) list) list)
   (pos_name : position)
   =
   let first_orderings = select_orderings add_orderings in
   let pos_succs = union_orderings first_orderings in
   let rest_orderings = List.flatten add_orderings in
   (((pos_name,pos_succs)::rest_orderings) : (position * Set.t) list)

let rec copy_and_rename_tree last_tree replace_n mult subst_list =

   let rec rename_subtrees tree_list nposition nsubst_list =
      match tree_list with
         [] -> [],[]
       | f::r ->
            let f_subtree,f_ordering =
               copy_and_rename_tree f replace_n mult nsubst_list in
            let r_subtrees,r_ordering_list = rename_subtrees r nposition nsubst_list in
            (f_subtree::r_subtrees),(f_ordering::r_ordering_list)

   in
   match last_tree with
      Empty -> raise (Invalid_argument "Jprover: copy tree")
    | NodeAt(position) ->   (* can never be a Gamma_0 position -> no replacements *)
         let nposition,_ = update_position position replace_n subst_list mult in
         (NodeAt(nposition)),[(nposition.pospos,Set.empty)]
    | NodeA(position, suctrees) ->
         let nposition,nsubst_list = update_position position replace_n subst_list mult in
         let new_suctrees, new_ordering_list =
            rename_subtrees suctrees nposition nsubst_list in
         let new_ordering =
            combine_ordering_list new_ordering_list nposition.pospos
         in
         (NodeA(nposition,new_suctrees)),new_ordering

(* we construct for each pos a list orderings representing let correspondning to the array of succtrees *)

let rec add_multiplicity ftree
   (mult : int)
   calculus
   =
   let rec parse_subtrees tree_list =
      match tree_list with
         [] -> [],[]
       | f::r ->
            let f_subtree,f_ordering = add_multiplicity f mult calculus in
            let r_subtrees,r_ordering_list = parse_subtrees r in
            (f_subtree::r_subtrees),(f_ordering::r_ordering_list)

   in
   match ftree with
      Empty -> raise (Invalid_argument "Jprover: add mult")
    | NodeAt(pos) -> ftree,[(pos.pospos,Set.empty)]
    | NodeA(pos,suctrees) ->
         let new_suctrees, new_ordering_list = parse_subtrees suctrees in
            begin match calculus, pos with
             (* no explicit atom-instances *)
               Classical, { pt = Phi; op = All; _ }
(* XXX Yegor: this is probably a bug - there is no Phi in Classical logic *)
             | S4, { pt = Nu _; _ }
             | Intuit _, { pt = Phi; op = And | Or | Neg | Imp | All | Ex | Null (* pos.op <> At *); _ }
             (* universal quantifiers are copied at their Phi positions *)
             | _, { pt = Gamma; st =
                   Alpha_1 | Alpha_2 | Beta_1 | Beta_2 | Gamma_0 | Delta_0 | Psi_0 | PNull_0 (*pos.st <> Phi_0 *); _ } ->
                  let replace_n = List.length pos.address in  (* points to the following argument in the array_of_address *)
                  let last_tree = Lm_list_util.last new_suctrees in
                  let add_tree,add_ordering =
                     copy_and_rename_tree last_tree replace_n mult []
						in
						(* rev_append on the next line would break some proofs *)
                  let final_suctrees = new_suctrees @ [add_tree] in
                  let add_orderings = add_ordering::new_ordering_list in
                  let final_ordering =
                     combine_ordering_list add_orderings pos.pospos
                  in
                     (NodeA(pos,final_suctrees)),final_ordering
             | _ ->
                  let final_ordering =
                     combine_ordering_list new_ordering_list pos.pospos
                  in
                     (NodeA(pos,new_suctrees)),final_ordering
            end

(**************  Path checker   ****************************************************)

let get_alpha atom atom_map =
	let alpha, beta = AtomMap.find atom_map atom in
	alpha

let get_connections a alpha
   (tabulist : AtomSet.t)
   =
	let alpha' = AtomSet.diff alpha tabulist in
	AtomSet.fold
		(fun acc f ->
         if (Opname.eq a.apredicate f.apredicate) &
            (a.apol <> f.apol)
         then
            (a,f)::acc
         else
            acc
		)
		[]
		alpha'

let connections atom_rel tabulist =
	let acc, tabulist =
		AtomMap.fold
			(fun (acc, tabulist) a (alpha,_) ->
   	      List.rev_append
      	      (get_connections a alpha tabulist)
         	   acc,
				AtomSet.add tabulist a
			)
			([], tabulist)
			atom_rel
	in
	acc

let check_alpha_relation atom set atom_map =
   AtomSet.subset set (get_alpha atom atom_map)

let extset atom_map path closed =
	AtomMap.fold
		(fun acc at (alpha, beta) ->
         if (AtomSet.subset path alpha) & (AtomSet.subset closed beta) then
            AtomSet.add acc at
         else
            acc
		)
		AtomSet.empty
		atom_map

let check_ext_set ext_set fail_set atom_sets =  (* fail_set consists of one atom only *)
	AtomSet.filter	(fun f -> check_alpha_relation f fail_set atom_sets) ext_set

let fail_ext_set ext_atom ext_set atom_map =
   let fail_set = AtomSet.singleton ext_atom in
   check_ext_set ext_set fail_set atom_map

let rec ext_partners con path ext_atom reduction_partners extension_partners atom_map =
   match con with
      [] ->
         (reduction_partners,extension_partners)
    | (a,b)::r ->
         let a_partner = position_eq ext_atom.apos a.apos in
         if a_partner || position_eq ext_atom.apos b.apos then
            let ext_partner = if a_partner then b else a in
(* force reduction steps first *)
            if (AtomSet.mem path ext_partner) then
               ext_partners r path ext_atom (AtomSet.add reduction_partners ext_partner) extension_partners atom_map
            else
               let new_ext_partners =
                  if (check_alpha_relation ext_partner path atom_map) then
                     (AtomSet.add extension_partners ext_partner)
                  else
                     extension_partners
               in
               ext_partners r path ext_atom reduction_partners new_ext_partners atom_map
         else
            ext_partners r path ext_atom reduction_partners extension_partners atom_map

exception Failed_connections of int

(*
 * Connections should be stored in lists for now.
 * It is possible that what actually breka proofs is not the order of connections but
 * the fact that ConnSet sorts elements inside each pair(connection)
 *)
let path_checker
   (consts: SymbolSet.t)
   (atom_map: (AtomSet.t * AtomSet.t) AtomMap.t)
   (qprefixes: position list PMap.t * position list PMap.t)
   (init_ordering: Set.t PMap.t)
   calculus
	concl_ordering (* use only to distinguish atoms from conclusion later *)
	=

   let con = connections atom_map AtomSet.empty in
	let concl_con, hyp_only_con =
		List.partition
			(fun (a,b) -> PMap.mem concl_ordering a.apos || PMap.mem concl_ordering b.apos)
			con
	in
	let con = List.rev_append concl_con hyp_only_con in
   let atom_set =
		AtomMap.fold (fun set {apos=x; _} _ -> Set.add set x) Set.empty atom_map
	in
   let delta_list = PMap.keys (snd qprefixes) in
   let delta_syms = List.rev_map pos_to_symbol delta_list in
   let all_consts = SymbolSet.add_list consts delta_syms in
(*   print_endline "";
   print_endline ("number of connections: "^(string_of_int (List.length con)));
*)

   let rec provable
      path
      closed
      (orderingQ,reduction_ordering)
      (eqlist : equation list)
      (sigmaQ,sigmaJ)
		tracelist
		counter
      =
      let rec check_connections reduction_partners extension_partners ext_atom counter =
         let try_one =
            if AtomSet.is_empty reduction_partners then
               if AtomSet.is_empty extension_partners then
                  raise (Failed_connections(counter))
               else
                  AtomSet.choose extension_partners
            else
       (* force reduction steps always first!! *)
               AtomSet.choose reduction_partners
         in
(*         print_endline ("connection partner "^(try_one.aname)); *)
(*       print_endline ("partner path "^(print_set path));
*)
         (try
            let new_sigmaQ, new_ordering_elements =
					jqunify all_consts (ext_atom.alabel) (try_one.alabel) sigmaQ counter
				in
(* build the orderingQ incrementally from the new added substitution tau of new_sigmaQ *)
            let relate_pairs,new_orderingQ = build_orderingQ consts new_ordering_elements orderingQ counter in
(* we make in incremental reflexivity test during the string unification *)
            let (new_sigmaJ,new_eqlist,new_red_ordering,new_trace) =
(* new_red_ordering = [] in propositional case *)
               stringunify ext_atom try_one eqlist relate_pairs calculus new_orderingQ atom_set qprefixes tracelist counter
            in
(*           print_endline ("make reduction ordering "^((string_of_int (List.length new_ordering)))); *)
            let new_closed = AtomSet.add closed ext_atom in
            let ((next_orderingQ,next_red_ordering),next_eqlist,(next_sigmaQ,next_sigmaJ),subproof, next_trace),counter' =
               if AtomSet.mem path try_one then
                  provable path new_closed (new_orderingQ,new_red_ordering) new_eqlist (new_sigmaQ,new_sigmaJ) new_trace (succ counter)
                     (* always use old first-order ordering for recursion *)
               else
                  let new_path = AtomSet.add path ext_atom in
                  let extension = AtomSet.singleton try_one in
                  let ((norderingQ,nredordering),neqlist,(nsigmaQ,nsigmaJ),p1,ntrace),counter' =
                     provable new_path extension (new_orderingQ,new_red_ordering) new_eqlist (new_sigmaQ,new_sigmaJ) new_trace (succ counter) in
                  let ((nnorderingQ,nnredordering),nneqlist,(nnsigmaQ,nnsigmaJ),p2,nntrace),counter'' =
                     provable path new_closed (norderingQ,nredordering) neqlist (nsigmaQ,nsigmaJ) ntrace (succ counter')
						in
                  ((nnorderingQ,nnredordering),nneqlist,(nnsigmaQ,nnsigmaJ),(p1 @ p2),nntrace),counter''
                  (* changing to rev_append on the line above breaks some proofs *)
      (* first the extension subgoals = depth first; then other subgoals in same clause *)
            in
            ((next_orderingQ,next_red_ordering),next_eqlist,(next_sigmaQ,next_sigmaJ),(((ext_atom.apos),(try_one.apos))::subproof),next_trace),counter'
         with Failed(counter) ->
(*          print_endline ("new connection for "^(ext_atom.aname)); *)
(*            print_endline ("Failed"); *)
            check_connections (AtomSet.remove reduction_partners try_one)
                              (AtomSet.remove extension_partners try_one)
                              ext_atom counter
         )

      in
      let rec check_extension extset counter =
			if !debug_jprover then
				eprintf "atoms to explore %i/%i@." counter (AtomSet.cardinal extset);
         if AtomSet.is_empty extset then
            raise (Failed(counter))             (* go directly to a new entry connection *)
         else
            let select_one = AtomSet.choose extset in
(*        print_endline ("extension literal "^(select_one.aname)); *)
(*        print_endline ("extension path "^(print_set path));*)
            let (reduction_partners,extension_partners) =
               ext_partners con path select_one AtomSet.empty AtomSet.empty atom_map in
            (try
               check_connections reduction_partners extension_partners select_one counter
            with Failed_connections(counter) ->
(*         print_endline ("no connections for subgoal "^(select_one.aname)); *)
(*           print_endline ("Failed_connections"); *)
               let fail_ext_set = fail_ext_set select_one extset atom_map in
               check_extension fail_ext_set counter
            )

      in
      let extset = extset atom_map path closed in
      if AtomSet.is_empty extset then
         ((orderingQ,reduction_ordering),eqlist,(sigmaQ,sigmaJ),[],tracelist), counter
      else
         check_extension extset counter
   in
	let gamma, delta = qprefixes in
   let pe = PMap.empty in
   if PMap.is_empty gamma && PMap.is_empty delta then
      begin
(*      print_endline "!!!!!!!!!!! prop prover !!!!!!!!!!!!!!!!!!"; *)
(* in the propositional case, the reduction ordering will be computed AFTER proof search *)
         let (_,eqlist,(_,nsubstJ),ext_proof, tracelist), result =
            provable AtomSet.empty AtomSet.empty (pe,pe) [] (pe,(1,[])) [] 0 in
         let _,substJ = nsubstJ in
         let orderingJ = build_orderingJ_list calculus substJ init_ordering atom_set in
         if !debug_s4prover then
            begin
               print_endline "build_orderingJ_list in path_checker:";
               print_ordering_map orderingJ
            end;
         (init_ordering,orderingJ),eqlist,(pe,nsubstJ),ext_proof,tracelist
      end
   else
      let result, counter = provable AtomSet.empty AtomSet.empty (init_ordering,pe) [] (pe,(1,[])) [] 0 in
		result

(*************************** prepare let init prover *******************************************************)

let rec predecessor address_1 address_2 = function
   Empty -> PNull            (* should not occur since every pair of atoms have a common predecessor *)
 | NodeAt(position) -> PNull (* should not occur as above *)
 | NodeA(position,suctrees) ->
      match address_1,address_2 with
         [],_ -> raise (Invalid_argument "Jprover: predecessors left")
       | _,[] -> raise (Invalid_argument "Jprover: predecessors right")
       | (f1::r1),(f2::r2) ->
            if f1 = f2 then
               predecessor r1 r2 (List.nth suctrees f1)
            else
               position.pt

let rec compute_sets element ftree = function
   [] -> AtomSet.empty,AtomSet.empty
 | first::rest ->
      if position_eq first.apos element.apos then
         compute_sets element ftree rest    (* element is neithes alpha- nor beta-related to itself*)
      else
         let (alpha_rest,beta_rest) = compute_sets element ftree rest in
         if predecessor (element.aaddress) (first.aaddress) ftree = Beta then
            alpha_rest, AtomSet.add beta_rest first
         else
            AtomSet.add alpha_rest first, beta_rest

let rec compute_atomlist_relations worklist ftree alist =  (* last version of alist for total comparison *)
   let compute_atom_relations element ftree alist =
      let alpha_beta = compute_sets element ftree alist in
      element, alpha_beta
   in
   match worklist with
      [] -> AtomMap.empty
    | first::rest ->
         let atom, alpha_beta = compute_atom_relations first ftree alist in
         AtomMap.add (compute_atomlist_relations rest ftree alist) atom alpha_beta

(* unused
let mk_xnil _ = xnil_term
*)

let atom_record position posprefix =
   let {pospos=pospos;
        label=label; address=address; pol=pol; st=st; _} = position in
   let aposprefix =
      match st with
         Psi_0 | Phi_0 | Pi_0 _ | Nu_0 _ ->
            posprefix @ [pospos]
       | _ ->
            posprefix
   in
   {aaddress=address; apos=pospos;
    aposprefix=aposprefix;
    apredicate=opname_of_term label;
    apol=pol; ast=st; alabel=label}

let rec select_atoms_treelist treelist posprefix acc =
   match treelist with
      [] -> acc
    | first::rest ->
         let acc =
            select_atoms first posprefix acc
         in
         select_atoms_treelist rest posprefix acc

and select_atoms ftree posprefix acc =
	let atom_acc,gamma_0_acc,delta_0_acc = acc in
   match ftree with
      Empty -> acc
    | NodeAt(position) ->
         (atom_record position posprefix)::atom_acc, gamma_0_acc, delta_0_acc
    | NodeA({pospos = pospos; st = st; _ }, suctrees) ->
         let new_posprefix =
            match st with
               Psi_0 | Phi_0 | Pi_0 _ | Nu_0 _ ->
                  posprefix @ [pospos]
             | _ ->
                  posprefix
         in
         let acc =
            match st with
               Gamma_0 ->
						atom_acc,
                  PMap.add gamma_0_acc pospos posprefix,
                  delta_0_acc
             | Delta_0 ->
				 		atom_acc,
                  gamma_0_acc,
                  PMap.add delta_0_acc pospos posprefix
             | _ ->
				 		acc
         in
            select_atoms_treelist suctrees new_posprefix acc

(* unused
let print_item p l =
	eprintf "%s: " (pos_to_string p);
	print_stringlist l;
	eflush stderr
*)

(* unused
let print_list_map map =
	PMap.fold (fun _ p l -> print_item p l) () map
*)

let prepare_prover ftree =
   let alist,gamma_0_prefixes,delta_0_prefixes =
      select_atoms_treelist [ftree] [] ([],PMap.empty,PMap.empty)
   in
   let atom_rel = compute_atomlist_relations alist ftree alist in
   (atom_rel,(gamma_0_prefixes,delta_0_prefixes))

(* ************************ Build intial formula tree  let relations *********************************** *)
(* Building a formula tree let the tree ordering from the input formula, i.e. OCaml term *)

let make_position_name stype =
	let pos_n = Lm_symbol.new_number () in
   match stype with
      Phi_0 | Gamma_0 -> Var 0, pos_n
	 | Nu_0 i -> Var i, pos_n
    | Psi_0 | Delta_0 -> Const 0, pos_n
	 | Pi_0 i -> Const i, pos_n
    | _ -> Atom, pos_n

let dual_pol = function
   Zero -> One
 | One -> Zero

let check_subst_term variable old_term pospos stype =
   match stype with
      Gamma_0 | Delta_0 ->
         let new_variable =
            if stype = Gamma_0 then
               mk_pos_var (simple_to_gamma pospos)
            else
               mk_pos_var pospos
         in
         (TermSubst.subst1 old_term variable new_variable) (* replace variable (non-empty) in t by pos_name *)
            (* pos_name is either a variable term or a constant, f.i. a string term *)
            (* !!! check unification module how handling eingenvariables as constants !!! *)
    | _ -> old_term

let rec build_ftree calculus variable old_term pol stype address =
   let pospos = make_position_name stype in
   let term = check_subst_term variable old_term pospos stype in
   if JLogic.is_and_term term then
      let s,t = JLogic.dest_and term in
      let ptype,stype_1,stype_2 =
         match pol with
            Zero ->
               Beta,Beta_1,Beta_2
          | One ->
               Alpha,Alpha_1,Alpha_2
      in
      let position =
         {address=address; pospos=pospos;
         op=And; pol=pol; pt=ptype; st=stype; label=term}
      in
      let subtree_left,ordering_left =
         build_ftree calculus empty_sym s pol stype_1 (address@[0])
      in
      let subtree_right,ordering_right =
         build_ftree calculus empty_sym t pol stype_2 (address@[1])
      in
      let (succ_left,whole_left) = List.hd ordering_left in
      let (succ_right,whole_right) = List.hd ordering_right in
      let pos_succs =
         Set.add (Set.add (Set.union whole_left whole_right) succ_right) succ_left
      in
      (NodeA(position,[subtree_left;subtree_right]),
       ((position.pospos,pos_succs)::(List.rev_append ordering_left ordering_right))
      )
   else if JLogic.is_or_term term then
      let s,t = JLogic.dest_or term in
      let ptype,stype_1,stype_2 =
         match pol with
            Zero ->
               Alpha,Alpha_1,Alpha_2
          | One ->
               Beta,Beta_1,Beta_2
      in
      let position =
         {address=address; pospos=pospos;
         op=Or; pol=pol; pt=ptype; st=stype; label=term}
      in
      let subtree_left,ordering_left =
         build_ftree calculus empty_sym s pol stype_1 (address@[0])
      in
      let subtree_right,ordering_right =
         build_ftree calculus empty_sym t pol stype_2 (address@[1])
      in
      let (succ_left,whole_left) = List.hd ordering_left in
      let (succ_right,whole_right) = List.hd ordering_right in
      let pos_succs =
         Set.add (Set.add (Set.union whole_left whole_right) succ_right) succ_left
      in
      (NodeA(position,[subtree_left;subtree_right]),
       ((position.pospos),pos_succs)::(List.rev_append ordering_left ordering_right)
      )
   else if JLogic.is_implies_term term then
      let s,t = JLogic.dest_implies term in
      match calculus with
         Intuit _ ->
            let ptype_0,stype_0,ptype,stype_1,stype_2 =
               match pol with
                  Zero ->
                     Psi,Psi_0,Alpha,Alpha_1,Alpha_2
                | One ->
                     Phi,Phi_0,Beta,Beta_1,Beta_2
            in
            let pos2pos = make_position_name stype_0 in
            let sposition =
               {address=address; pospos=pospos;
               op=Imp; pol=pol; pt=ptype_0; st=stype; label=term}
            in
            let position = {address=address@[0]; pospos=pos2pos;
               op=Imp; pol=pol; pt=ptype; st=stype_0; label=term}
            in
            let subtree_left,ordering_left =
               build_ftree calculus empty_sym s (dual_pol pol) stype_1 (address@[0;0])
            in
            let subtree_right,ordering_right =
               build_ftree calculus empty_sym t pol stype_2 (address@[0;1])
            in
            let (succ_left,whole_left) = List.hd ordering_left in
            let (succ_right,whole_right) = List.hd ordering_right in
            let pos_succs =
               Set.add (Set.add (Set.union whole_left whole_right) succ_right) succ_left
            in
            let pos_ordering =
               (position.pospos,pos_succs) :: (List.rev_append ordering_left ordering_right)
   	      in
               NodeA(sposition,[NodeA(position,[subtree_left;subtree_right])]),
               ((sposition.pospos,(Set.add pos_succs position.pospos))::pos_ordering)
       | Classical | S4 ->
            let ptype,stype_1,stype_2 =
               match pol with
                  Zero ->
                     Alpha,Alpha_1,Alpha_2
                | One ->
                     Beta,Beta_1,Beta_2
            in
            let position =
               {address=address; pospos=pospos;
               op=Imp; pol=pol; pt=ptype; st=stype; label=term}
            in
            let subtree_left,ordering_left =
               build_ftree calculus empty_sym s (dual_pol pol) stype_1 (address@[0])
            in
            let subtree_right,ordering_right =
               build_ftree calculus empty_sym t pol stype_2 (address@[1])
            in
            let (succ_left,whole_left) = List.hd ordering_left in
            let (succ_right,whole_right) = List.hd ordering_right in
            let pos_succs =
               Set.add (Set.add (Set.union whole_left whole_right) succ_right) succ_left
            in
            let pos_ordering =
               (position.pospos,pos_succs) :: (List.rev_append ordering_left ordering_right)
            in
               NodeA(position,[subtree_left;subtree_right]),
               pos_ordering
   else if JLogic.is_not_term term then
      let s = JLogic.dest_not term in
      match calculus with
         Intuit _ ->
            let ptype_0,stype_0,ptype,stype_1=
               match pol with
                  Zero ->
                     Psi,Psi_0,Alpha,Alpha_1
                | One ->
                     Phi,Phi_0,Alpha,Alpha_1
            in
            let pos2pos = make_position_name stype_0 in
            let sposition =
               {address=address; pospos=pospos;
                op=Neg; pol=pol; pt=ptype_0; st=stype; label=term}
            in
            let position =
               {address=address@[0]; pospos=pos2pos;
                op=Neg; pol=pol; pt=ptype; st=stype_0; label=term}
            in
            let subtree_left,ordering_left =
               build_ftree calculus empty_sym s (dual_pol pol) stype_1 (address@[0;0])
            in
            let (succ_left,whole_left) = List.hd ordering_left in
            let pos_succs = Set.add whole_left succ_left in
            let pos_ordering = (position.pospos,pos_succs) :: ordering_left in
               NodeA(sposition,[NodeA(position,[subtree_left])]),
               ((sposition.pospos,(Set.add pos_succs position.pospos))::pos_ordering)
       | Classical | S4 ->
           let ptype,stype_1=
               match pol with
                  Zero ->
                     Alpha,Alpha_1
                | One ->
                     Alpha,Alpha_1
            in
            let position =
               {address=address; pospos=pospos;
                op=Neg; pol=pol; pt=ptype; st=stype; label=term}
            in
            let subtree_left,ordering_left =
               build_ftree calculus empty_sym s (dual_pol pol) stype_1 (address@[0])
            in
            let (succ_left,whole_left) = List.hd ordering_left in
            let pos_succs = Set.add whole_left succ_left in
            let pos_ordering = (position.pospos,pos_succs) :: ordering_left in
               NodeA(position,[subtree_left]),
               pos_ordering
   else if JLogic.is_box_term term then
      let i, s = JLogic.dest_box term in
      let ptype,stype_1=
         match pol with
            Zero ->
               Pi i, Pi_0 i
          | One ->
               Nu i, Nu_0 i
      in
      let position =
         {address=address; pospos=pospos;
          op=Box i; pol=pol; pt=ptype; st=stype; label=term}
      in
      let subtree_left,ordering_left =
         build_ftree calculus empty_sym s pol stype_1 (address@[0])
      in
      let (succ_left,whole_left) = List.hd ordering_left in
      let pos_succs = Set.add whole_left succ_left in
      let pos_ordering = (position.pospos,pos_succs) :: ordering_left in
         NodeA(position,[subtree_left]),
         pos_ordering
   else if JLogic.is_exists_term term then
      let v,s,t = JLogic.dest_exists term in  (* s is type of v let will be supressed here *)
      match calculus with
         Intuit _ ->
            let ptype,stype_1 =
               match pol with
                  Zero ->
                     Gamma,Gamma_0
                | One ->
                     Delta,Delta_0
            in
            let position =
               {address=address; pospos=pospos;
                op=Ex; pol=pol; pt=ptype; st=stype; label=term}
            in
            let subtree_left,ordering_left =
               build_ftree calculus v t pol stype_1 (address@[0])
            in
            let (succ_left,whole_left) = List.hd ordering_left in
            let pos_succs = Set.add whole_left succ_left in
               NodeA(position,[subtree_left]),
               ((position.pospos,pos_succs) :: ordering_left)
       | Classical | S4 ->
            raise (Invalid_argument
            "Syntax analysis of quantifiers are not implemented for Classical Logic and S4")
   else if JLogic.is_all_term term then
      let v,s,t = JLogic.dest_all term in
      match calculus with
         Intuit _ ->
            (* s is type of v let will be supressed here *)
            let ptype_0,stype_0,ptype,stype_1=
               match pol with
                  Zero ->
                     Psi,Psi_0,Delta,Delta_0
                | One ->
                     Phi,Phi_0,Gamma,Gamma_0
            in
            let pos2pos = make_position_name stype_0 in
            let sposition =
               {address=address; pospos=pospos;
                op=All; pol=pol; pt=ptype_0; st=stype; label=term}
            in
            let position =
               {address=address@[0]; pospos=pos2pos;
                op=All; pol=pol; pt=ptype; st=stype_0; label=term}
            in
            let subtree_left,ordering_left =
               build_ftree calculus v t pol stype_1 (address@[0;0])
            in
            let (succ_left,whole_left) = List.hd ordering_left in
            let pos_succs = Set.add whole_left succ_left in
            let pos_ordering = (position.pospos,pos_succs) :: ordering_left in
               NodeA(sposition,[NodeA(position,[subtree_left])]),
               ((sposition.pospos,(Set.add pos_succs position.pospos))::pos_ordering)
       | Classical | S4 ->
            raise (Invalid_argument
            "Syntax analysis of quantifiers are not implemented for Classical Logic and S4")
   else      (* finally, term is atomic *)
      match calculus with
         Intuit _ ->
            let ptype_0,stype_0 =
               match pol with
                  Zero ->
                     Psi,Psi_0
                | One ->
                     Phi,Phi_0
               in
            let pos2pos = make_position_name stype_0 in
            let sposition =
               {address=address; pospos=pospos;
                op=At; pol=pol; pt=ptype_0; st=stype; label=term}
            in
            let position =
               {address=address@[0]; pospos=pos2pos;
                op=At; pol=pol; pt=PNull; st=stype_0; label=term}
            in
               NodeA(sposition,[NodeAt(position)]),
               [(sposition.pospos,(Set.singleton position.pospos));
                  (position.pospos,Set.empty)
               ]
       | Classical | S4 ->
            let position =
               {address=address; pospos=pospos;
                op=At; pol=pol; pt=PNull; st=stype; label=term}
            in
               NodeAt(position),
               [(position.pospos,Set.empty)]

let rec construct_ftree_aux
   calculus
   termlist
   treelist
   (orderinglist : Set.t PMap.t)
   pol
   =
   match termlist with
      [] ->
         treelist, orderinglist
    | ft::rest_terms ->
         let next_address = [List.length treelist] in
         let new_tree,new_ordering =
            build_ftree calculus empty_sym ft pol Alpha_1 next_address
         in
         construct_ftree_aux
            calculus
            rest_terms
            (new_tree::treelist)
            (PMap.add_list orderinglist new_ordering)
            pol
			(* rev_append in treelist breaks some proofs *)

let construct_ftree
   calculus
   hyplist
   conclist
   goal
   =
   let concltrees, concl_ordering =
      construct_ftree_aux calculus conclist [] PMap.empty Zero
   in
   let alltrees, ordering =
      construct_ftree_aux calculus hyplist concltrees concl_ordering One
   in
   let alltrees = List.rev alltrees in
   let pt =
      match calculus with
         Intuit _ ->
            Psi
       | Classical ->
            Alpha
       | S4 ->
            Alpha
   in
   let new_root =
      {address=[]; pospos=root_pos;
       op=Null; pol=Zero; pt=pt; st=PNull_0; label=goal}
   in
   let union =
      PMap.fold (fun set key s -> Set.add (Set.union set s) key) Set.empty ordering
   in
   NodeA(new_root,alltrees),
   PMap.add ordering root_pos union,
	concl_ordering (* use only to distinguish atoms from conclusion later *)

(*************************** Main LOOP ************************************)

let unprovable = RefineError ("Jprover", StringError "formula is not provable")
let mult_limit_exn = RefineError ("Jprover", StringError "multiplicity limit reached")

let init_prover ftree =
   let atom_relation,qprefixes = prepare_prover ftree in
(*      print_atom_info atom_relation;  *)
   atom_relation, qprefixes

let rec try_multiplicity
   (consts: SymbolSet.t)
   mult_limit
   ftree
   (ordering: Set.t PMap.t)
   mult
   calculus
	concl_ordering (* use only to distinguish atoms from conclusion later *)
   =
   try
		if calculus = S4 then
			eprintf "trying multiplicity %i@." mult;
      let atom_map, qprefixes = init_prover ftree in
      let (orderingQ, red_ordering), eqlist, unifier, ext_proof, _ =
         path_checker consts atom_map qprefixes ordering calculus concl_ordering
		in
      (ftree,red_ordering,unifier,ext_proof)   (* orderingQ is not needed as return value *)
   with Failed(_) ->
      match mult_limit with
         Some m when m = mult ->
            raise mult_limit_exn
       | _ ->
            let new_mult = mult+1 in
            if !debug_jprover then begin
               open_box 0;
               force_newline ();
               print_string "Multiplicity Fail: ";
               print_string ("Try new multiplicity "^(string_of_int new_mult));
               force_newline ();
               print_flush ();
            end;
            let new_ftree, new_ordering =
               add_multiplicity ftree new_mult calculus in
            if ftree_eq new_ftree ftree then
               raise unprovable
            else
               let new_ordering =
                  List.fold_left (fun acc (p,s) -> PMap.add acc p s) PMap.empty new_ordering
               in
(*             print_formula_info new_ftree new_ordering new_pos_n;   *)
               try_multiplicity consts mult_limit new_ftree new_ordering new_mult calculus concl_ordering

let prove consts mult_limit hyplist conclist calculus =
   let ftree, ordering, concl_ordering =
      construct_ftree calculus hyplist conclist (mk_pos_var (dummy_pos ()))
   in
   if !debug_s4prover then
      print_ordering_map ordering;
(* pos_n = number of positions without new root "w" *)
(*   print_formula_info ftree ordering pos_n;    *)
   let ftree, red_ordering, (sigmaQ,sigmaJ), ext_proof =
      try_multiplicity consts mult_limit ftree ordering 1 calculus concl_ordering
   in
   ftree, red_ordering, (sigmaQ,sigmaJ), ext_proof


(********** first-order type theory interface *******************)

let rec renam_free_vars vars = function
   [] -> vars
 | f::r ->
      let new_vars = free_vars_set f in
      renam_free_vars (SymbolSet.union vars new_vars) r

let make_equal_list pattern list_object =
   List.rev_map (fun _ -> list_object) pattern

let rec create_output consts rule_list
   =
   match rule_list with
      [] -> JLogic.empty_inf
    | f::r ->
         let (pos,(rule,term1,term2)) = f in
         let unique_deltas = collect_delta_terms consts Set.empty [term1; term2] in
         let var_mapping =
            Set.fold
               (fun acc p ->
                  let pair =
							pos_to_symbol p,
                     mk_pos_var (simple_to_gamma p)
                  in
                  pair::acc
               )
					[]
               unique_deltas
         in
         let frees1 = free_vars_list term1 consts in
         let frees2 = free_vars_list term2 consts in
         let unique_object = mk_pos_var ((GammaPos (NewVar 0)),0) in
         let unique_list1 = make_equal_list frees1 unique_object in
         let unique_list2 = make_equal_list frees2 unique_object in
         let next_term1 = TermSubst.subst term1 frees1 unique_list1 in
         let next_term2 = TermSubst.subst term2 frees2 unique_list2 in
         let new_term1 = TermSubst.apply_subst var_mapping next_term1 in
         let new_term2 = TermSubst.apply_subst var_mapping next_term2 in
         (JLogic.append_inf (create_output consts r) new_term1 new_term2 rule)

let rec make_test_interface consts rule_list =
   match rule_list with
      [] -> []
    | f::r ->
         let (pos,(rule,term1,term2)) = f in
         let unique_deltas = collect_delta_terms consts Set.empty [term1; term2] in
         let var_mapping =
            Set.fold
               (fun acc p ->
                  let pair =
                     pos_to_symbol p,
                     mk_pos_var (simple_to_gamma p)
                  in
                  pair::acc
               )
               []
               unique_deltas
         in
         let frees1 = free_vars_list term1 consts in
         let frees2 = free_vars_list term2 consts in
         let unique_object = mk_pos_var (GammaPos (NewVar 0),0) in
         let unique_list1 = make_equal_list frees1 unique_object in
         let unique_list2 = make_equal_list frees2 unique_object in
         begin
(*
   print_endline "";
   print_endline "";
   print_stringlist frees1;
   print_endline "";
   print_stringlist frees2;
   print_endline "";
   print_endline "";
*)
            let next_term1 = TermSubst.subst term1 frees1 unique_list1 in
            let next_term2 = TermSubst.subst term2 frees2 unique_list2 in
	         let new_term1 = TermSubst.apply_subst var_mapping next_term1 in
   	      let new_term2 = TermSubst.apply_subst var_mapping next_term2 in
            (pos,(rule,new_term1,new_term2))::(make_test_interface consts r)
         end

(**************************************************************)

let gen_prover mult_limit calculus hyps concls =
	(* rev_append on the next line would break some proofs *)
   let consts =
		renam_free_vars SymbolSet.empty hyps
	in
   let consts =
      renam_free_vars consts concls
   in
   let ftree, red_ordering, (sigmaQ,sigmaJ), ext_proof =
		prove consts mult_limit hyps concls calculus
	in
	if calculus = S4 then
		eprintf "matrix proof found@.";
   let red_ordering = PMap.fold (fun acc p s -> (p,s)::acc) [] red_ordering in
   (* it's too early to convert ext_proof to a set *)
   let sequent_proof = reconstruct ftree red_ordering sigmaQ ext_proof calculus consts in
         (* transform types let rename constants *)
     (* we can transform the eigenvariables AFTER proof reconstruction since *)
     (* new delta_0 constants may have been constructed during rule permutation *)
     (* from the LJmc to the LJ proof *)
   create_output consts sequent_proof

let gen_prover =
   if !debug_profile_tactics then
      (fun mult_limit calculus hyps concls ->
         timing_wrap "JProver (Jall.gen_prover)" (gen_prover mult_limit calculus hyps) concls)
   else
      gen_prover

let prover mult_limit hyps concl = gen_prover mult_limit (Intuit SingleConcl) hyps [concl]

(************* test with propositional proof reconstruction ************)

(* unused
let rec count_axioms seq_list =
   match seq_list with
      [] -> 0
    | f::r ->
         let (rule,_,_) = f in
         if rule = Ax then
            1 + count_axioms r
         else
            count_axioms r
*)

let do_prove mult_limit hyps concls calculus =
   try begin
      let consts = renam_free_vars SymbolSet.empty hyps in
      let consts =
         renam_free_vars consts concls
      in
      let ftree, red_ordering, (sigmaQ,sigmaJ), ext_proof =
         prove consts mult_limit hyps concls calculus
      in
      open_box 0;
      force_newline ();
      force_newline ();
      print_string "Extension proof ready";
      force_newline ();
      force_newline ();
      print_string ("Length of Extension proof: "^((string_of_int (List.length ext_proof)))^
                           " Axioms");
      force_newline ();
      force_newline ();
      print_endline "Extension proof:";
      open_box 0;
      (*print_pairlist ext_proof;*) (* print list of type (string * string) list *)
      force_newline ();
      force_newline ();
      force_newline ();
      print_flush ();
      print_flush ();
      open_box 0;
      (*print_ordering red_ordering;*)
      print_flush ();
      open_box 0;
      force_newline ();
(* ----------------------------------------------- *)
      open_box 0;
      (*print_tunify sigmaJ;*)
      print_flush ();
      print_endline "";
      print_endline "";
(*      print_sigmaQ sigmaQ;*)
      print_endline "";
      print_endline "";
(* --------------------------------------------------------- *)
      print_string "Break ... ";
      print_endline "";
      print_endline "";
      print_flush ();
      let _ = input_char stdin in
      let red_ordering = PMap.fold (fun acc p s -> (p,s)::acc) [] red_ordering in
      let reconstr_proof =
			reconstruct ftree red_ordering sigmaQ ext_proof calculus consts
		in
      let _sequent_proof = make_test_interface consts reconstr_proof in
      open_box 0;
      force_newline ();
      force_newline ();
      print_string "Sequent proof ready";
      force_newline ();
      force_newline ();
      print_flush ();
      (*let (ptree,count_ax) = bproof sequent_proof in*)
      open_box 0;
      (*print_string
      * ("Length of sequent proof: "^((string_of_int count_ax))^" Axioms");
      *)
      force_newline ();
      force_newline ();
      force_newline ();
      force_newline ();
      print_flush ();
      (*tt ptree;*)
      print_flush ();
      print_endline "";
      print_endline ""
   end with exn -> begin
      print_endline "Jprover got an exception:";
      print_endline (Printexc.to_string exn)
   end

let test concl calculus =  (* calculus should be LJmc or LJ for J, let LK for C *)
   do_prove None [] [concl] calculus

(* for sequents *)

let seqtest list_term calculus =
   let termlist = subterms_of_term list_term in
   do_prove None termlist [] calculus

(*****************************************************************)

end (* of struct *)
