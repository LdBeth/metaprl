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
 * See the file doc/index.html for information on Nuprl,
 * OCaml, let more information about this system.
 *
 * Copyright (C) 2000 Stephan Schmitt
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
 *)
open Lm_debug
open Lm_symbol
open Lm_printf
open Lm_string_set

open Term_sig
open Refiner.Refiner
(*
open Term
*)
open TermType
(*
open TermOp
open TermSubst
open TermMan
*)
open RefineError
(*
open Opname
*)

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

(************************************************************************
 * Compatibility layer for abstract vars.
 *)
let free_vars_list t consts =
   SymbolSet.to_list (SymbolSet.diff (TermSubst.free_vars_set t) consts)

let mk_symbol_term opname s =
   let p = Term.make_param (Term_sig.Var s) in
   let op = Term.mk_op opname [p] in
   Term.mk_any_term op []

let mk_pos_term opname p = mk_symbol_term opname (pos_to_symbol p)

let mk_pos_var p =
   Term.mk_var_term (pos_to_symbol p)

let print_term = Term.print_term
let dest_term = Term.dest_term
let dest_op = Term.dest_op
let dest_opname = Opname.dest_opname
let alpha_equal = TermSubst.alpha_equal
let apply_subst = TermSubst.apply_subst
let xnil_term = TermMan.xnil_term
let all_contexts = TermMan.all_contexts
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

module JProver (JLogic : JLogicSig) =
struct
   module JTypes = JTypes(JLogic)
   module JOrdering = JOrdering(JLogic)
   module JQuantifier = JQuantifier(JLogic)
   module JTUnifyQ = JTUnifyQ(JLogic)
   module JTUnifyProp = JTUnifyProp(JLogic)

   open JTypes
   open JOrdering
   open JQuantifier
   open JTUnifyQ

   module OrderedAtom =
   struct
      type t = atom
      (*let compare a1 a2 =
         let sc = Pervasives.compare a1.aname a2.aname in
         let pc = PosOrdering.compare a1.apos a2.apos in
         if sc * pc < 0 then
            begin
               print_endline ("Inconsistent ordering: "^a1.aname^" "^a2.aname);
               sc
            end
         else
            sc
      *)
      let compare a1 a2 = Pervasives.compare a1.aname a2.aname
   end

   module AtomSet = Lm_set.LmMake(OrderedAtom)

   let jprover_bug = Invalid_argument "Jprover bug (Jall module)"

   (* XXX: Nogin: as far as I understand, names are unique, but I am not sure *)
   let atom_eq a1 a2 =
      a1.apos = a2.apos

   let pos_eq p1 p2 =
      p1.pospos = p2.pospos

   let position_eq (p1: position) p2 =
      p1 = p2

   let string_eq (s1: string) s2 =
      s1 = s2

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

(*****************************************************)

(********* printing atoms let their relations ***********************)

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
      let ({aname=x; aaddress=y; apredicate=p; apol=a; ast=b; alabel=label}) = at in
      begin
         print_string ("{aname="^x^"; address=");
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

(*************** print formula tree, tree ordering etc. ***********)

   let print_ptype pt =
      match pt with
         Alpha -> print_string "Alpha"
       | Beta  -> print_string "Beta"
       | Gamma -> print_string "Gamma"
       | Delta -> print_string "Delta"
       | Phi   -> print_string "Phi"
       | Psi   -> print_string "Psi"
       | PNull -> print_string "PNull"

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

   let rec print_stringlist slist =
      match slist with
         [] ->
            print_string ""
       | f::r ->
            begin
               print_string (f^".");
               print_stringlist r
            end

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
            (f.aname)^" "^(print_set_list r)

   let print_set set =
      let set_list = AtomSet.elements set in
      if set_list = [] then "empty"
      else
         print_set_list set_list

   let print_string_set set =
      let set_list = StringSet.elements set in
      print_stringlist set_list

   let rec print_list_sets list_of_sets =
      match list_of_sets with
         [] -> print_string ""
       | (pos,fset)::r ->
            begin
               print_string (pos^": ");   (* first element = node which successors depend on *)
               print_stringlist (StringSet.elements fset);
               force_newline ();
               print_list_sets r
            end

   let print_ordering list_of_sets =
      begin
         open_box 0;
         print_list_sets list_of_sets;
         print_flush ()
      end

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
       | PEmpty -> raise jprover_bug
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

(************ END printing functions  *********************************)

(************ Beta proofs let redundancy deletion **********************)

   let rec remove_dups_connections = function
      [] -> []
    | (c1,c2)::r ->
         if (List.mem (c1,c2) r) or (List.mem (c2,c1) r) then
  (* only one direction variant of a connection stays *)
            remove_dups_connections r
         else
            (c1,c2)::(remove_dups_connections r)

   let rec remove_dups_list = function
      [] -> []
    | f::r ->
         if List.mem f r then
            remove_dups_list r
         else
            f::(remove_dups_list r)

   let subst_eq (s1, t1) (s2, t2) =
      (Lm_symbol.eq s1 s2) && alpha_equal t1 t2

   let rec remove_subst_dups = function
      [] -> []
    | f::r ->
         if List.exists (subst_eq f) r then
            remove_subst_dups r
         else
            f::(remove_subst_dups r)

	let rec append_from_pairs accumulator = function
		[] -> accumulator
	 | (a,b)::tl -> append_from_pairs (a::b::accumulator) tl

   let beta_pure alpha_layer connections beta_expansions =
      let test_list = append_from_pairs beta_expansions connections in
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
         not (List.exists (fun x -> (List.mem x test_list)) alpha_layer)
      end

   let rec apply_bproof_purity bproof =
      match bproof with
         BEmpty ->
            raise jprover_bug
       | CNode((c1,c2)) ->
            bproof,[(c1,c2)],[]
       | AtNode(_,(c1,c2)) ->
            bproof,[(c1,c2)],[]
       | RNode(alpha_layer,subproof) ->
            let (opt_subproof,min_connections,beta_expansions) =
               apply_bproof_purity subproof in
            (RNode(alpha_layer,opt_subproof),min_connections,beta_expansions)
       | BNode(pos,(alph1,subp1),(alph2,subp2)) ->
            let (opt_subp1,min_conn1,beta_exp1) = apply_bproof_purity subp1 in
            if beta_pure alph1 min_conn1 beta_exp1 then
               begin
(*       print_endline ("Left layer of "^pos); *)
                  (opt_subp1,min_conn1,beta_exp1)
               end
            else
               let (opt_subp2,min_conn2,beta_exp2) = apply_bproof_purity subp2 in
               if beta_pure alph2 min_conn2 beta_exp2 then
                  begin
(*       print_endline ("Right layer of "^pos); *)
                     (opt_subp2,min_conn2,beta_exp2)
                  end
               else
                  let min_conn =
							remove_dups_connections (List.rev_append min_conn1 min_conn2)
						in
                  let beta_exp =
                     remove_dups_list (pos :: (List.rev_append beta_exp1 beta_exp2))
                  in
                  (BNode(pos,(alph1,opt_subp1),(alph2,opt_subp2)),min_conn,beta_exp)

   let bproof_purity bproof =
      let (opt_bproof,min_connections,_) = apply_bproof_purity bproof in
      opt_bproof,min_connections

(*********** split permutation *****************)

   let rec apply_permutation bproof rep_name direction act_blayer =
      match bproof with
         BEmpty | RNode(_,_) ->
            raise jprover_bug
       | AtNode(cx,(c1,c2)) ->
            bproof,act_blayer
       | CNode((c1,c2)) ->
            bproof,act_blayer
       | BNode(pos,(alph1,subp1),(alph2,subp2)) ->
            if rep_name = pos then
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
            if pos = pname then
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
            raise jprover_bug

(*********** END split permutation *****************)

   let rec list_del list_el el_list =
      match el_list with
         [] ->
            raise jprover_bug
       | f::r ->
            if list_el = f then
               r
            else
               f::(list_del list_el r)

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
       | NodeAt {pospos = pospos} :: r
       | NodeA({pt = Beta; pospos = pospos}, _) :: r ->
            pospos :: (compute_alpha_layer r)
       | NodeA(_, suctrees) :: r ->
            compute_alpha_layer (List.rev_append suctrees r)
       | Empty :: _ ->
            raise jprover_bug

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
         if pos = f then
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

   let print_context conn bcontext =
      begin
         open_box 0;
         print_string conn;
         print_string ":    ";
         List.iter (fun x -> let (pos,num) = x in print_string (pos^" "^(string_of_int num)^"")) bcontext;
         print_endline " ";
         print_flush ()
      end

   let rec build_opt_beta_proof beta_proof
      (ext_proof : (position * position) list)
      (beta_atoms : (position * (position * int) list) list)
      (beta_layer_list  : (position * (position list * position list)) list)
      act_context
      =
      let rec add_c2_tree (c1,c2) c2_diff_context =
         match c2_diff_context with
            [] ->
               (CNode(c1, c2),0)
          | (f,num)::c2_diff_r ->
               let next_beta_proof,next_exp =
                  add_c2_tree (c1,c2) c2_diff_r in
               let (layer1,layer2) = List.assoc f beta_layer_list in
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
               let (layer1,layer2) = List.assoc f beta_layer_list in
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
            let c1_context = List.assoc c1 beta_atoms in
            let c2_context = List.assoc c2 beta_atoms in
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
      let rec annotate_tree
         (beta_context : (position * int) list)
         tree atlist
         =
         match tree with
            Empty ->
               (atlist,[],[])
          | NodeAt({pospos=pospos}) ->
               if List.mem pospos atlist then
                  let new_atlist = list_del pospos atlist in
                  (new_atlist,[(pospos,beta_context)],[])
               else
                  (atlist,[],[])
          | NodeA({pt = Beta; pospos = pospos}, [s1;s2]) ->
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
                  (atlist2,(List.rev_append annotates1 annotates2),
						((pospos,(alayer1,alayer2))::(List.rev_append blayer_list1 blayer_list2)))
          | NodeA({pt = Beta}, _) ->
                raise jprover_bug
          | NodeA(_, suctrees) ->
                  annotate_atoms beta_context atlist suctrees
      in
      match treelist with
         [] -> (atlist,[],[])
       | f::r ->
            let (next_atlist,f_annotates,f_beta_layers) = annotate_tree beta_context f atlist in
            let (rest_atlist,rest_annotates,rest_beta_layers) = (annotate_atoms beta_context next_atlist r)
            in
            (rest_atlist, (List.rev_append f_annotates rest_annotates),
				(List.rev_append f_beta_layers rest_beta_layers))

   let construct_opt_beta_proof ftree
      (ext_proof : (position * position) list)
      =
      let con1,con2 = List.split ext_proof in
      let con_atoms = remove_dups_list (List.rev_append con1 con2) in
      let (empty_atoms,beta_atoms,beta_layer_list) = annotate_atoms [] con_atoms [ftree] in
      let root_node = compute_alpha_layer [ftree] in
      let (beta_proof,beta_exp,closures,_) =
         build_opt_beta_proof BEmpty ext_proof beta_atoms beta_layer_list [] in
      (RNode(root_node,beta_proof)),beta_exp,closures

(************* permutation ljmc -> lj *********************************)

(* REAL PERMUTATION STAFF *)

   let subf1 n m subrel = List.mem ((n,m),1) subrel
   let subf2 n m subrel = List.mem ((n,m),2) subrel

	type augmented_position = Pos of position | OrlTrue

(* Transforms all normal form layers in an LJ proof *)

   let rec modify prooftree subrel tsubrel =
      match prooftree with
         PEmpty ->
            raise jprover_bug
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
            raise jprover_bug
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
       | PNodeA((_, (Fail|Impl|Orl|Orr2|Orr1|Andr|Ax), _, _), _) ->
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
             | (Fail|Exl|Exr|Alll|Allr|Negl|Negr|Impr|Orl|Orr2|Orr1|Orr|Andl|Ax) ->
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

   let eigen_counter = ref 1

(* append renamed paramater Right to non-quantifier subformulae
   of renamed quantifier formulae *)

   let make_new_eigenvariable term =
      let op = (dest_term term).term_op in
      let opn = (dest_op op).op_name in
      let opnam = dest_opname opn in
      match opnam with
         ofirst::ofname::_ ->
            (*let new_eigen_var = (ofname^"_r"^(string_of_int (!eigen_counter))) in*)
            let new_eigen_pos = EigenVar, !eigen_counter in
            eigen_counter := !eigen_counter + 1;
(*        print_endline ("New Counter :"^(string_of_int (!eigen_counter))); *)
            mk_pos_term jprover_op new_eigen_pos
       | [] | [_] ->
            raise jprover_bug

   let replace_subterm term oldt rept =
      let dummy = pos_to_symbol (dummy_pos ()) in
      let v_term = TermSubst.var_subst term oldt dummy in
      TermSubst.subst1 v_term dummy rept

   let rec eigen_rename old_parameter new_parameter ptree =
      match ptree with
         PEmpty ->
            raise jprover_bug
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
            raise jprover_bug
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
               if direction = Left then
                  left
               else
                  right   (* direction = Right *)
            else
               let left_del = update_ptree rule left direction in
               let right_del = update_ptree rule right direction in
               PNodeB(r,left_del,right_del)

(* permute layers, isolate addmissible branches *)

(* computes if an Andr is d-generatives *)

   let rec orl_free ptree =
      match ptree with
         PEmpty ->
            raise jprover_bug
       | PNodeAx _
       | PNodeA((_, (Impr|Negr|Allr), _, _),_) ->
            true
       | PNodeA(_, left) ->
            orl_free left
       | PNodeB((_, Orl, _, _),_,_) ->
            false
       | PNodeB(_,left,right) ->
            orl_free left && orl_free right

   let tsubf tsubrel m (n, _, _, _) = List.mem (n,m) tsubrel

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
         PEmpty -> raise jprover_bug
       | PNodeAx(_) -> raise jprover_bug
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
                | [] -> raise jprover_bug
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
               | [] -> raise jprover_bug
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
                | [] -> raise jprover_bug
               end
         else
            leftm1
    | _ -> raise jprover_bug

   and trans_add_branch r o addr act_addr ptree dglist subrel tsubrel =
      match ptree with
         (PEmpty| PNodeAx(_)) -> raise jprover_bug
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
                | [] -> raise jprover_bug
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
         PEmpty -> raise jprover_bug
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

   let rec get_successor_pos = function
      Empty :: r -> get_successor_pos r
    | NodeA(pos,_) :: r ->
         pos::(get_successor_pos r)
    | [] -> []
    | NodeAt _ :: _ -> raise jprover_bug

   let rec get_formula_tree ftreelist f predflag =
      match ftreelist with
         Empty :: rest_trees -> get_formula_tree rest_trees f predflag
       | NodeAt _ :: rest_trees -> get_formula_tree rest_trees f predflag
       | NodeA(pos,suctrees) :: rest_trees ->
            if predflag then
               if pos.pt = Gamma then
                  let succs = get_successor_pos suctrees in
                  if List.mem f succs then
                     NodeA(pos,suctrees),succs
                  else
                     get_formula_tree (List.rev_append suctrees rest_trees) f predflag
               else
                  get_formula_tree (List.rev_append suctrees rest_trees) f predflag
            else
               if pos_eq pos f then
                  NodeA(pos,suctrees),[]
               else
                  get_formula_tree (List.rev_append suctrees rest_trees) f predflag
       | [] -> raise jprover_bug

   let rec get_formula_treelist ftree = function
      [] -> []
(* a posistion has either stype Gamma_0,Psi_0,Phi_0 (non-atomic), or it has *)
(* ptype Alpha (or on the right), since there was a deadlock for proof reconstruction in LJ*)
    | ({st = Gamma_0} as f)::r ->
         let (predtree,succs) = get_formula_tree [ftree] f true in
         let new_po = list_diff r succs in
         predtree::(get_formula_treelist ftree new_po)
    | ({st = (Phi_0 | Psi_0)} as f)::r
    | ({pt = Alpha} as f)::r ->
         let (stree,_) = get_formula_tree [ftree] f false in
         stree::(get_formula_treelist ftree r)
    | _ ->
         raise (Invalid_argument "Jprover bug: non-admissible open position")

   let rec number_list n = function
      hd::tl -> (n,hd)::(number_list (succ n) tl)
    | [] -> []

   let rec build_formula_rel dir_treelist slist predpos =

      let rec build_renamed_gamma_rel dtreelist predpos pospos d =
         match dtreelist with
            [] -> [],[]
          | (x,ft)::rdtlist ->
               let rest_rel,rest_ren = build_renamed_gamma_rel rdtlist predpos pospos d in
               (
                match ft with
                   Empty ->   (* may have empty successors due to purity in former reconstruction steps *)
                      rest_rel,rest_ren
                 | NodeAt(_) ->
                      raise jprover_bug (* gamma_0 position never is atomic *)
                 | NodeA({pospos=spospos},suctrees) ->
                      if List.mem spospos slist then
(* the gamma_0 position is really unsolved *)
(* this is only relevant for the gamma_0 positions in po *)
                         let k, _ = pospos in
                         let new_pos = k, Lm_symbol.new_number () in
                         (* XXX Yegor: the old comment says: "make new unique gamma name"
                         *  but currently I simply create a fresh variable of
                         *  original kind and it seems to work alright *)
                         let new_srel_el = ((predpos,new_pos),d) in
                         let new_rename_el = (spospos,new_pos)  (* gamma_0 position as key first *) in
                         let (srel,sren) =
                            build_formula_rel [(x,ft)] slist new_pos
                         in
                         (new_srel_el::(List.rev_append srel rest_rel)),
								 (new_rename_el::(List.rev_append sren rest_ren))
                      else
                         rest_rel,rest_ren
               )

      in
      match dir_treelist with
         [] -> [],[]
       | (d,f)::dir_r ->
            let (rest_rel,rest_renlist) = build_formula_rel dir_r slist predpos in
            match f with
               Empty ->
                  if !debug_jprover then print_endline "Hello, an empty subtree!!!!!!";
                  rest_rel,rest_renlist
             | NodeAt({pospos=pospos}) ->
                  (((predpos,pospos),d)::rest_rel),rest_renlist
             | NodeA({ pt = Alpha | Beta | Delta; pospos=pospos }, suctrees) ->
                  let dtreelist = number_list 1 suctrees in
                  let (srel,sren) = build_formula_rel dtreelist slist pospos in
                     (((predpos,pospos),d)::(List.rev_append srel rest_rel)),
							(List.rev_append sren rest_renlist)
             | NodeA({ pt = Psi| Phi }, suctrees) ->
                  let dtreelist = (List.map (fun x -> (d,x)) suctrees) in
                  let (srel,sren) = build_formula_rel dtreelist slist predpos in
                     (List.rev_append srel rest_rel),
							(List.rev_append sren rest_renlist)
             | NodeA({ pt = Gamma; pospos=pospos }, suctrees) ->
                  let dtreelist = (List.map (fun x -> (1,x)) suctrees) in
(*                if (nonemptys suctrees 0 n) = 1 then
   let (srel,sren) = build_formula_rel dtreelist slist pos.name in
   ((((predname,pos.name),d)::srel) @ rest_rel),(sren @ rest_renlist)
   else (* we have more than one gamma instance, which means renaming *)
*)
                  let (srel,sren) = build_renamed_gamma_rel dtreelist predpos pospos d in
                     (List.rev_append srel rest_rel), (List.rev_append sren rest_renlist)
             | NodeA({ pt = PNull }, _) ->
                  raise jprover_bug

   let rec rename_gamma ljmc_proof rename_list =
      match ljmc_proof with
         [] -> []
       | ((inst,_),(((Alll | Exr),_,_) as h))::r ->
            let new_gamma = List.assoc inst rename_list in
            ((inst,new_gamma),h)::(rename_gamma r rename_list)
       | h :: r ->
            h :: (rename_gamma r rename_list)

   let rec compare_pair s sf = function
      [] ->
         []
    | (s_1,sf_1)::restlist ->
         if sf = s_1 then
            (s,sf_1)::(compare_pair s sf restlist)
         else
            compare_pair s sf restlist

   let rec compare_pairlist list1 list2 =
      match list1 with
         [] -> []
       | (s1,sf1) :: restlist1 ->
            List.rev_append (compare_pair s1 sf1 list2) (compare_pairlist restlist1 list2)

   let rec trans_rec pairlist translist =
      let tlist = compare_pairlist pairlist translist in
      if tlist = [] then
         translist
      else
         List.rev_append (trans_rec pairlist tlist) translist

   let transitive_closure subrel =
      let pairlist,nlist = List.split subrel in
      trans_rec pairlist pairlist

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
            raise jprover_bug
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
      (slist : position list)
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

   let rec init_unsolved = function
      [] -> []
    | Empty :: _ -> []
    | NodeAt pos :: r ->
         (pos.pospos)::(init_unsolved r)
    | NodeA(pos,suctrees) :: r ->
         let new_treelist = List.rev_append suctrees r in
            (pos.pospos)::(init_unsolved new_treelist)

(* only the unsolved positions will be represented --> skip additional root position *)

   let build_unsolved = function
      NodeA(pos,suctrees) ->
         ((pos.pospos),init_unsolved suctrees)
    | Empty | NodeAt _ ->
         raise jprover_bug

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

   let rec key_delete fpos pos_list =   (* in key_delete, f is a pos name (key) but sucs is a list of positions *)
      match pos_list with
         [] -> []               (* the position with name f must not necessarily occur in pos_list *)
       | f::r ->
            if fpos = f.pospos then
               r
            else
               f::(key_delete fpos r)

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
       | NodeAt(pos) ->
            []
       | NodeA(pos,strees) ->
            match padd with
               [] -> get_roots strees
             | [f] -> pos::(comp_ps [] (List.nth strees f))
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
                     if (List.mem pos.pospos slist) then
                        [pos]
                     else
                        []
                | NodeA(pos,suctrees) ->
                     if (List.mem pos.pospos slist) then
                        [pos]
                     else
                        compute_open suctrees slist
            in
            elements @ (compute_open rest slist)
				(* rev_append does not work here
				 * it changes proofs and make the whole thing slower
				 *)

   let rec select_connection pname connections slist =
      match connections with
         [] -> None
       | f::r ->
            let a,b = f in
            let partner =
               if a = pname then
                  Some b
               else
                  if b = pname then
                     Some a
                  else
                     None
            in
            match partner with
               None ->
                  select_connection pname r slist
             | Some p when List.mem p slist ->
                  select_connection pname r slist
             | _ ->
                  Some f

   let rec replace_element element element_set redord =
      match redord with
         [] -> raise jprover_bug   (* element occurs in redord *)
       | (f,fset)::r ->
            if f = element then
               (f,element_set)::r
            else
               (f,fset)::(replace_element element element_set r)

   let rec collect_succ_sets sucs redord =
      match redord with
         [] -> Set.empty
       | (f,fset)::r ->
            let new_sucs = key_delete f sucs in
            if (List.length sucs) = (List.length new_sucs) then   (* position with name f did not occur in sucs -- no deletion *)
               (collect_succ_sets sucs r)
            else
               Set.union (Set.add fset f) (collect_succ_sets new_sucs r)

   let replace_ordering psucc_pos sucs redord =
      let new_psucc_set = collect_succ_sets sucs redord in
(*   print_string_set new_psucc_set; *)
      replace_element psucc_pos new_psucc_set redord

   let rec update pospos redord =
      match redord with
         [] -> []
       | (f,fset)::r ->
            if pospos=f then
               r
            else
               (f,fset)::(update pospos r)

(*  rule construction *)

   let rec selectQ_rec spos_var csigmaQ =
      match csigmaQ with
         [] ->  Term.mk_var_term spos_var   (* dynamic completion of csigmaQ *)
       | (var,term)::r ->
            if Lm_symbol.eq spos_var var then
               term
            else
               selectQ_rec spos_var r

   let selectQ spos_pos csigmaQ =
      let spos_var = pos_to_symbol (simple_to_gamma spos_pos) in
      selectQ_rec spos_var csigmaQ

   let apply_sigmaQ term sigmaQ =
      apply_subst sigmaQ term

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
       | All,Zero -> Allr,(inst_label),(mk_pos_term jprover_op spos.pospos) (* must be a proper term *)
       | Ex,One -> Exl,(inst_label),(mk_pos_term jprover_op spos.pospos) (* must be a proper term *)

(* %%%%%%%%%%%%%%%%%%%% Split begin %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% *)

   let rec nonemptys sum = function
      Empty::tl -> nonemptys sum tl
    | _::tl -> nonemptys (succ sum) tl
    | [] -> sum

   let rec collect_pure flist slist = function
      [] -> []
    | Empty :: r ->
         collect_pure flist slist r
    | NodeAt(pos) :: r ->
         let {pospos=pospos} = pos in
         if ((List.mem pospos flist) or (List.mem pospos slist)) then
            collect_pure flist slist r
         else
            pos :: collect_pure flist slist r
    | NodeA(pos,treearray) :: r ->
         List.rev_append (collect_pure flist slist treearray) (collect_pure flist slist r)

   let rec update_list testlist list =
      match testlist with
         [] -> list
       | f::r ->
            let newlist = delete position_eq f list in    (* f may not occur in list; then newlist=list *)
            update_list r newlist

   let rec update_pairlist p pairlist =
      match pairlist with
         [] -> []
       | f::r ->
            if ((fst f) = p) or ((snd f) = p) then
               update_pairlist p r
            else
               f::(update_pairlist p r)

   let rec update_connections slist connections =
      match slist with
         [] -> connections
       | f::r ->
            let connew = update_pairlist f connections in
            update_connections r connew

   let rec update_redord delset redord =   (* delset is the set of positions to be deleted *)
      match redord with
         [] -> []
       | (f,fset)::r ->
            if (Set.mem delset f) then
               update_redord delset r   (* delete all key elements f from redord which are in delset *)
            else
               let new_fset = Set.diff fset delset in  (* no successor of f from delset should remain in fset *)
               (f,new_fset)::(update_redord delset r)

   let rec get_position_names = function
      [] -> []
    | Empty :: rests -> get_position_names rests
    | NodeAt{pospos=pospos} :: rests ->
         pospos::get_position_names rests
    | NodeA({pospos=pospos},strees) :: rests ->
         pospos::(get_position_names (List.rev_append strees rests))

   let rec print_purelist = function
      [] ->
         begin
            print_string ".";
            print_endline " ";
         end
    | f::r ->
         print_string ((pos_to_string f.pospos)^", ");
         print_purelist r

   let update_relations deltree redord connections unsolved_list =
      let pure_names = get_position_names [deltree] in
      begin
(*        print_ftree deltree;
   open_box 0;
   print_endline " ";
   print_stringlist pure_names;
   force_newline ();
   print_flush ();
*)
         let rednew = update_redord (Set.of_list pure_names) redord in
         let connew = update_connections pure_names connections in
         let unsolnew = update_list pure_names unsolved_list in
         (rednew,connew,unsolnew)
      end

   let rec collect_qpos ftreelist uslist =
      match ftreelist with
         [] -> [],[]
       | Empty :: rest ->
            collect_qpos rest uslist
       | NodeAt {st=Gamma_0; pospos=pospos} :: rest ->
            let rest_delta, rest_gamma = collect_qpos rest uslist in
            if List.mem pospos uslist then
               rest_delta, (pospos::rest_gamma)
            else
               rest_delta, rest_gamma
       | NodeAt {st=Delta_0; pospos=pospos} :: rest ->
            let rest_delta, rest_gamma = collect_qpos rest uslist in
            if List.mem pospos uslist then
               (pospos::rest_delta), rest_gamma
            else
               rest_delta, rest_gamma
       | NodeAt _ :: rest ->
            let rest_delta, rest_gamma = collect_qpos rest uslist in
            rest_delta, rest_gamma
       | NodeA({st=Gamma_0; pospos=pospos},suctrees) :: rest ->
            let rest_delta, rest_gamma = collect_qpos (List.rev_append suctrees rest) uslist in
            if List.mem pospos uslist then
               rest_delta, (pospos::rest_gamma)
            else
               rest_delta, rest_gamma
       | NodeA({st=Delta_0; pospos=pospos},suctrees) :: rest ->
            let rest_delta, rest_gamma = collect_qpos (List.rev_append suctrees rest) uslist in
            if List.mem pospos uslist then
               (pospos::rest_delta), rest_gamma
            else
               rest_delta, rest_gamma
       | NodeA(_, suctrees) :: rest ->
            let rest_delta, rest_gamma = collect_qpos (List.rev_append suctrees rest) uslist in
            rest_delta, rest_gamma

   let rec do_split gamma_diff sigmaQ =
      match sigmaQ with
         [] -> []
       | (v,term)::r ->
            (*let _ = print_endline ("do_split: "^v) in*)
            let simple = gamma_to_simple v in
            if (List.mem simple gamma_diff) then
               do_split gamma_diff r
            else
               (v,term)::(do_split gamma_diff r)

(* make a term list out of a bterm list *)

   let rec check_delta_terms (v,term) ass_delta_diff dterms =
      match ass_delta_diff with
         [] -> term,[]
       | (var,dname)::r ->
            if List.mem dname dterms then
               let new_var =
                  if var = empty_sym then
                     v
                  else
                     var
               in
               let replace_term = mk_symbol_term jprover_op dname in
               let next_term = TermSubst.var_subst term replace_term new_var in
               let (new_term,next_diffs) = check_delta_terms (v,next_term) r dterms in
               (new_term,((new_var,dname)::next_diffs))
            else
               let (new_term,next_diffs) = check_delta_terms (v,term) r dterms in
               (new_term,((var,dname)::next_diffs))

   let rec localize_sigma zw_sigma ass_delta_diff =
      match zw_sigma with
         [] -> []
       | (v,term)::r ->
            let dterms = collect_delta_terms [term] in
            let new_term, new_ass_delta_diff = check_delta_terms (v,term) ass_delta_diff dterms in
            (v,new_term)::(localize_sigma r new_ass_delta_diff)

   let subst_split ft1 ft2 ftree uslist1 uslist2 uslist
      (sigmaQ : (symbol * term) list)
      =
      let sigmaQ = List.map (fun (s,t) -> symbol_to_pos s, t) sigmaQ in
      let delta,gamma = collect_qpos [ftree] uslist in
      let delta1,gamma1 = collect_qpos [ft1] uslist1 in
      let delta2,gamma2 = collect_qpos [ft2] uslist2 in
      let delta_diff1 = list_diff delta delta1 in
      let delta_diff2 = list_diff delta delta2 in
      let gamma_diff1 = list_diff gamma gamma1 in
      let gamma_diff2 = list_diff gamma gamma2 in
      let zw_sigma1 = do_split gamma_diff1 sigmaQ in
      let zw_sigma2 = do_split gamma_diff2 sigmaQ in
      let zw_sigma1 =
         List.map (fun (s,t) -> pos_to_symbol s, t) zw_sigma1
      in
      let zw_sigma2 =
         List.map (fun (s,t) -> pos_to_symbol s, t) zw_sigma2
      in
      let ass_delta_diff1 = List.map (fun x -> (empty_sym,pos_to_symbol x)) delta_diff1 in
      let ass_delta_diff2 = List.map (fun x -> (empty_sym,pos_to_symbol x)) delta_diff2 in
      let sigmaQ1 = localize_sigma zw_sigma1 ass_delta_diff1 in
      let sigmaQ2 = localize_sigma zw_sigma2 ass_delta_diff2 in
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
                  if an = pos.pospos then
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
            raise jprover_bug
       | _, NodeAt(_) ->
            print_endline "Atom purity tree";
            raise jprover_bug

   let rec purity ftree redord
      (connections : (position*position) list)
      unsolved_list
      =

      let rec purity_reduction pr ftree redord
         (connections : (position*position) list)
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
                  if assocn = empty_pos then
                     (Empty,[],[],[])  (* should not occur in the final version *)
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
      let flist,slist = List.split connections in
      let pr = collect_pure flist slist [ftree] in
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
            raise jprover_bug
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
            raise jprover_bug
       | NodeAt(_), _ ->
            print_endline "bsplit Atom tree";
            raise jprover_bug   (* the beta-node should actually occur! *)

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

   let rec collect_solved_Zero_At ftreelist slist =
      match ftreelist with
         [] ->
            []
       | Empty :: r ->    (* may become possible after purity *)
            collect_solved_Zero_At r slist
       | NodeAt ({pospos=pospos; pol=pospol} as pos) :: r ->
            if ((List.mem pospos slist) or (pospol = One)) then  (* recall slist is the unsolved list *)
               collect_solved_Zero_At r slist
            else
    (* here, we have pos solved let pos.pol = Zero) *)
               pos::(collect_solved_Zero_At r slist)
       | NodeA(pos,treearray) :: r ->
            collect_solved_Zero_At (List.rev_append treearray r) slist

   let rec red_ord_block pospos redord =
      match redord with
         [] -> false
       | (f,fset)::r ->
            if ((f = pospos) or (not (Set.mem fset pospos))) then
               red_ord_block pospos r
            else
               true   (* then, we have (StringSet.mem fset pname) *)

   let rec check_wait_succ_LJ faddress ft =
      match ft, faddress with
         NodeA({op = Or}, [Empty; Empty]), [] ->
            raise (Invalid_argument "Jprover: redundancies occur")
       | NodeA({op = Or}, [Empty; _]), [] ->
            (false,2)   (* determines the Orr2 rule *)
       | NodeA({op = Or}, [_; Empty]), [] ->
            (false,1)   (* determines the Orr1 ruke *)
       | NodeA({op = Or}, [_;_]), [] ->
            (true,0)    (* wait-label is set *)
       | NodeA({op = Or}, _), [] ->
            raise jprover_bug
       | NodeA _, [] ->
            (false,0)
       | NodeA({pt = Gamma},strees), [f] when (nonemptys 0 strees) > 1 ->
            (true,0)
            (* we are at a gamma position (exr) with one than one successor *)
            (* -- wait label in LJ*)
       | NodeA(_,strees), f::r ->
                  check_wait_succ_LJ r (List.nth strees f)
       | Empty, _
       | NodeAt _, _ -> raise jprover_bug (* we have an gamma_0 position or an or-formula *)

   let blocked f po
      (redord : (position * Set.t) list)
      ftree connections
      (slist : position list)
      calculus opt_bproof
      =
(* print_endline ("Blocking check "^(f.name)); *)
      if red_ord_block f.pospos redord then
         begin
(*     print_endline "wait-1 check positive"; *)
            true,0
         end
      else
         match calculus with
            Classical ->
               false,0  (* ready, in C only redord counts *)
          | Intuit calc ->
               let pa_Zero = collect_solved_Zero_At [ftree] slist in    (* solved atoms in ftree *)
               let po_test = (delete pos_eq f po) in
               (* we provide dynamic wait labels for both sequent calculi *)
               begin match calc with
                  MultiConcl ->
(*                   print_endline "wait-2 check"; *)
                     if (f.st = Psi_0)  &  (f.pt <> PNull) &
                        ((pa_Zero <> []) or (List.exists (fun x -> x.pol = Zero) po_test)) then
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
                          ((pa_Zero <> []) or
                          (List.exists (fun x -> x.pol = Zero) po_test))
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
                           let slist_fake = delete position_eq f.pospos slist in
                           let ((zw1ft,zw1red,_,zw1uslist),_) =
                              betasplit f.address ftree redord connections slist_fake in
                           let ft1,_,_,uslist1 =  purity zw1ft zw1red min_con1 zw1uslist in
(*                      print_endline "wait label purity_one_out"; *)
                           let ft1_root = (List.hd (List.tl (tpredsucc f ft1))) in
(*                    print_endline ("wait-root "^(ft1_root.name)); *)
                           let po_fake = compute_open [ft1] uslist1 in
                           let po_fake_test = delete pos_eq ft1_root po_fake in
                           let pa_Zero_fake = collect_solved_Zero_At [ft1] uslist1 in
(*                     print_purelist (po_fake_test @ pa_O_fake); *)
                              ((pa_Zero_fake <> []) or
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
      (slist : position list)
      calculus candidates
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
            let bool,orr_flag = blocked f po redord ftree connections slist calculus opt_bproof in
            if bool then
               select_pos r po redord ftree connections slist calculus candidates opt_bproof
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
                  select_pos r po redord ftree connections slist calculus
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
      (connections : (position * position) list)
      (csigmaQ : (symbol * term) list)
      (slist : position list)
      calculus
      opt_bproof
      =
      let po = compute_open [ftree] slist in
      tot ftree redord connections csigmaQ po slist calculus opt_bproof

   and tot ftree redord connections csigmaQ po slist calculus opt_bproof =
      try
         (* last argument for guiding selection strategy *)
         let p, orr_flag =
            select_pos po po redord ftree connections slist calculus [] opt_bproof
         in
(*    print_endline ((p.name)^" "^(string_of_int orr_flag)); *)
         match tpredsucc p ftree with
            pred :: succs ->
               let redpo = update p.pospos redord in   (* deletes the entry (p,psuccset) from the redord *)
               let rednew =
                  if (p.pt = Delta) then                 (* keep the tree ordering for the successor position only *)
                     let psucc = List.hd succs in
                     match tpredsucc psucc ftree with
                        pre :: sucs ->
                           replace_ordering psucc.pospos sucs redpo (* union the succsets of psucc *)
                      | [] ->
                           raise jprover_bug
                  else
                     redpo
               in
(*            print_endline "update ok"; *)
               solve
                  ftree rednew connections csigmaQ p po slist
                  (pred,succs) orr_flag calculus opt_bproof
          | [] ->
               raise jprover_bug
      with Gamma_deadlock ->
         let ljmc_subproof =
            total ftree redord connections csigmaQ slist (Intuit MultiConcl) opt_bproof
         in
         eigen_counter := 1;
         permute_ljmc ftree po slist ljmc_subproof
           (* the permuaiton result will be appended to the lj proof constructed so far *)

   and solve ftree redord connections csigmaQ p po slist (pred,succs) orr_flag calculus opt_bproof =
      let {pospos=pospos; op=op; pt=pt; st=st } = p in
      let newslist = delete position_eq pospos slist in
      let rback =
         if st = Gamma_0 then
            begin
(*          print_endline "that's the gamma rule";  *)
               [((pospos,pred.pospos),(build_rule pred p csigmaQ orr_flag calculus))]
            end
         else
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
         Gamma ->
            rback @ (tot ftree redord connections csigmaQ pnew newslist calculus opt_bproof)
       | Psi ->
            begin match op, succs with
               At, succ::_ ->
                  let rest =
                     solve
                       ftree redord connections csigmaQ succ pnew newslist
                       (p,[]) orr_flag calculus opt_bproof
                  in
                  rback @ rest  (* solve atoms immediately *)
             | At, [] ->
                  raise (Invalid_argument "solve: op=At but succs=[]")
             | _ ->
                  rback @ (tot ftree redord connections csigmaQ pnew newslist calculus opt_bproof)
            end
       | Phi ->
            begin match op, succs with
               At, succ::_ ->
                  let rest =
                     solve
                        ftree redord connections csigmaQ succ pnew newslist
                        (p,[]) orr_flag calculus opt_bproof
                  in
                     rback @ rest  (* solve atoms immediately *)
             | At, [] ->
                  raise (Invalid_argument "total: empty succs in Phi")
             | _ ->
                  rback @ (tot ftree redord connections csigmaQ pnew newslist calculus opt_bproof)
            end
       | PNull ->
            let new_redord = update pospos redord in
            begin match select_connection pospos connections newslist with
               None ->
                  let rest =
                     tot ftree new_redord connections csigmaQ pnew newslist calculus opt_bproof
                  in
                  rback @ rest
             | Some (c1,c2) ->
                  let (ass_pos,inst_pos) =
(* need the pol=O position ass_pos of the connection for later permutation *)
(* need the pol=I position inst_pos for NuPRL instantiation *)
                     if pospos = c1 then
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
            let rest = tot ftree redord connections csigmaQ pnew newslist calculus opt_bproof in
            rback @ (((empty_pos,pospos),rule)::rest)
       | Delta ->
            begin match succs with
               sp::_ ->
                  let rule = build_rule p sp csigmaQ orr_flag calculus in
                  let rest =
                     tot ftree redord connections csigmaQ pnew newslist calculus opt_bproof
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
               subst_split ft1 ft2 ftree uslist1 uslist2 newslist csigmaQ
            in
(*           print_endline "split_out"; *)
            let p1 = total ft1 red1 conn1 sigmaQ1 uslist1 calculus opt_bproof1 in
(*           print_endline "compute p1 out";              *)
            let p2 = total ft2 red2 conn2 sigmaQ2 uslist2 calculus opt_bproof2 in
(*           print_endline "compute p2 out";              *)
            rback @ [((empty_pos,pospos),(build_rule p p csigmaQ orr_flag calculus))] @ p1 @ p2  (* second possibility of recursion end *)

   let reconstruct ftree redord sigmaQ ext_proof calculus =
      let min_connections = remove_dups_connections ext_proof in
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
      let (init_tree,init_redord,init_connections,init_unsolved_list) =
         purity ftree redord2 min_connections unsolved_list in
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
(* it should hold: min_connections = init_connections *)
         total init_tree init_redord init_connections sigmaQ
            init_unsolved_list calculus opt_bproof
      end

(* ****** Quantifier unification ************** *)

(* For multiplication we assume always idempotent substitutions sigma, tau! *)

   let rec collect_assoc inst_vars tauQ =
      match inst_vars with
         [] -> []
       | f::r ->
            let f_term = List.assoc f tauQ in
            f_term::(collect_assoc r tauQ)

let pos_subst t vars tl =
   TermSubst.subst t (List.map pos_to_symbol vars) tl

   let rec rec_apply
      (consts : SymbolSet.t)
      (sigmaQ : (position * term) list)
      (tauQ : (position * term) list)
      (tau_vars : position list)
      (tau_terms : term list)
      =
      match sigmaQ with
         [] -> [],[]
       | (v,term)::r ->
            let app_term = pos_subst term tau_vars tau_terms in
            let old_free = free_vars_list term consts in
            let new_free = free_vars_list app_term consts in
            let inst_vars = list_diff old_free new_free in
            let inst_positions = List.map symbol_to_pos inst_vars in
            let inst_terms = collect_assoc inst_positions tauQ in
            let (rest_sigma,rest_sigma_ordering) = rec_apply consts r tauQ tau_vars tau_terms in
            if inst_terms = [] then
               ((v,app_term)::rest_sigma),rest_sigma_ordering
            else
               (*let ordering_v = String.sub v 0 (String.index v '_') in*)
               let ordering_v = gamma_to_simple v in
               ((v,app_term)::rest_sigma),((ordering_v,inst_terms)::rest_sigma_ordering)

(* let multiply sigmaQ tauQ =
   let tau_vars,tau_terms = List.split tauQ
   let sigma_vars,sigma_terms = List.split sigmaQ in
   let apply_terms = rec_apply sigma_terms tau_vars tau_terms in
   (List.combine sigma_vars apply_terms) @ tauQ
*)

   let multiply
      (consts : SymbolSet.t)
      (sigmaQ : (position * term) list)
      (tauQ : (position * term) list)
      =
      let tau_vars,tau_terms = List.split tauQ in
      let new_sigmaQ,sigma_ordering = rec_apply consts sigmaQ tauQ tau_vars tau_terms
      in
      let tau_ordering = List.map (fun (v,t) -> gamma_to_simple v, [t]) tauQ in
      List.rev_append new_sigmaQ tauQ,
      List.rev_append sigma_ordering tau_ordering

   let apply_2_sigmaQ term1 term2 sigmaQ =
      let sigma_vars,sigma_terms = List.split sigmaQ in
      (pos_subst term1 sigma_vars sigma_terms),(pos_subst term2 sigma_vars sigma_terms)

   let jqunify
      (consts : SymbolSet.t)
      term1
      term2
      (sigmaQ : (position * term) list) =
      let app_term1,app_term2 = apply_2_sigmaQ term1 term2 sigmaQ in
(*  print_term stdout app_term1;
   print_term stdout app_term2;
*)
      try
         let tauQ = unify app_term1 app_term2 consts in
         let pos_tauQ = List.map (fun (v,t) -> symbol_to_pos v, t) tauQ in
         let (mult,oel) = multiply consts sigmaQ pos_tauQ in
  (*   print_sigmaQ mult; *)
         (mult,oel)
      with
         RefineError _  ->  (* any unification failure *)
(*    print_endline "fo-unification fail"; *)
            raise Failed   (* new connection, please *)

let rec one_equation gprefix dlist delta_0_prefixes n =
   match dlist with
      [] -> ([],n)
    | f::r ->
         let fprefix = List.assoc f delta_0_prefixes in
         let (sf1,sg) = shorten fprefix gprefix in
         let v_new = NewVarQ, n in
         let fnew = sf1 @ [v_new] in
         let (rest_equations,new_n) = one_equation gprefix r delta_0_prefixes (n+1) in
         (([],(fnew,sg))::rest_equations),new_n

let rec make_domain_equations fo_pairs (gamma_0_prefixes,delta_0_prefixes) n =
   match fo_pairs with
      [] -> ([],n)
    | (g,dlist)::r ->
         let gprefix = List.assoc g gamma_0_prefixes in
         let (gequations,max) = one_equation gprefix dlist delta_0_prefixes n in
         let (rest_equations,new_max) =
            make_domain_equations r (gamma_0_prefixes,delta_0_prefixes) max in
         List.rev_append gequations rest_equations, new_max

(* type of one unifier: int * ((string * string list) list)  *)
(* global failure: (0,[]) *)

let stringunify ext_atom try_one (qmax,equations) fo_pairs calculus orderingQ atom_rel qprefixes =
   match calculus with
      Classical -> ((0,[]),(0,[]),orderingQ)
    | Intuit _ ->
         let us = ext_atom.aposprefix in
         let ut = try_one.aposprefix in
         let ns = ext_atom.apos in
         let nt = try_one.apos in
            match qprefixes with
               [], [] -> (* prop case *)
                  (* prop unification only *)
                  let new_sigma,new_eqlist,_  =
                     JTUnifyProp.do_stringunify us ut ns nt equations [] [] [] 1
                  in
                     (new_sigma,new_eqlist,[]) (* assume the empty reduction ordering during proof search *)
             | _ -> (* "This is the FO case" *)
                  (* fo_eqlist encodes the domain condition on J quantifier substitutions *)
                  (* Again, always computed for the whole substitution sigmaQ *)
                  let fo_eqlist, new_max = make_domain_equations fo_pairs qprefixes qmax in
                     (*
                     open_box 0;
                     print_string "domain equations in";
                     print_equations fo_eqlist;
                     print_string "domain equations out";
                     print_flush ();
                     *)
                     do_stringunify us ut ns nt equations fo_eqlist orderingQ atom_rel new_max

(**************************************** add multiplicity *********************************)

let rec subst_replace subst_list t =
   match subst_list with
      [] -> t
    | (old_t,new_t)::r ->
         let dummy = pos_to_symbol (dummy_pos ()) in
         let inter_term = TermSubst.var_subst t old_t dummy  in
         let new_term = TermSubst.subst1 inter_term dummy new_t in
         subst_replace r new_term

let update_position position m replace_n subst_list mult =
   let ({address=y; pospos=pospos; op=z; pol=p; pt=a; st=b; label=t}) = position in
   let k, _ = pospos in
   let npospos = k, m in
   let nsubst_list =
      if b=Gamma_0 then
         let vx = mk_pos_var (simple_to_gamma pospos) in
         let vnx = mk_pos_var (simple_to_gamma npospos) in
         (vx,vnx)::subst_list
      else
         if b=Delta_0 then
            let sx = mk_pos_term jprover_op pospos in
            let snx = mk_pos_term jprover_op npospos in
            (sx,snx)::subst_list
         else
            subst_list
   in
   let nt = subst_replace nsubst_list t in
   let new_add = myset replace_n (pred mult) y in
   {address=new_add; pospos=npospos;
    op=z; pol=p; pt=a; st=b; label=nt},m,nsubst_list

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

let rec copy_and_rename_tree last_tree replace_n pos_n mult subst_list =

   let rec rename_subtrees tree_list nposition s_pos_n nsubst_list =
      match tree_list with
         [] -> ([],[],s_pos_n)
       | f::r ->
            let (f_subtree,f_ordering,f_pos_n) =
               copy_and_rename_tree f replace_n s_pos_n mult nsubst_list in
            let (r_subtrees,r_ordering_list,r_pos_n) = rename_subtrees r nposition f_pos_n nsubst_list in
            ((f_subtree::r_subtrees),(f_ordering::r_ordering_list),r_pos_n)

   in
   match last_tree with
      Empty -> raise (Invalid_argument "Jprover: copy tree")
    | NodeAt(position) ->   (* can never be a Gamma_0 position -> no replacements *)
         let (nposition,npos_n,_) = update_position position (pos_n+1) replace_n subst_list mult in
         ((NodeAt(nposition)),[(nposition.pospos,Set.empty)],npos_n)
    | NodeA(position, suctrees) ->
         let (nposition,npos_n,nsubst_list) = update_position position (pos_n+1) replace_n subst_list mult in
         let (new_suctrees, new_ordering_list, new_pos_n) =
            rename_subtrees suctrees nposition npos_n nsubst_list in
         let new_ordering =
            combine_ordering_list new_ordering_list nposition.pospos
         in
         ((NodeA(nposition,new_suctrees)),new_ordering,new_pos_n)

(* we construct for each pos a list orderings representing let correspondning to the array of succtrees *)

let rec add_multiplicity ftree pos_n
   (mult : int)
   calculus
   =
   let rec parse_subtrees tree_list s_pos_n =
      match tree_list with
         [] -> ([],[],s_pos_n)
       | f::r ->
            let (f_subtree,f_ordering,f_pos_n) = add_multiplicity f s_pos_n mult calculus in
            let (r_subtrees,r_ordering_list,r_pos_n) = parse_subtrees r f_pos_n in
            ((f_subtree::r_subtrees),(f_ordering::r_ordering_list),r_pos_n)

   in
   match ftree with
      Empty -> raise (Invalid_argument "Jprover: add mult")
    | NodeAt(pos) -> (ftree,[(pos.pospos,Set.empty)],pos_n)
    | NodeA(pos,suctrees) ->
         let new_suctrees, new_ordering_list, new_pos_n = parse_subtrees suctrees pos_n in
            begin match calculus, pos with
             (* no explicit atom-instances *)
               Classical, { pt = Phi; op = All}
             | Intuit _, { pt = Phi; op = And | Or | Neg | Imp | All | Ex | Null (* pos.op <> At *) }
             (* universal quantifiers are copied at their Phi positions *)
             | _, { pt = Gamma; st =
                   Alpha_1 | Alpha_2 | Beta_1 | Beta_2 | Gamma_0 | Delta_0 | Psi_0 | PNull_0 (*pos.st <> Phi_0 *) } ->
                  let replace_n = List.length pos.address in  (* points to the following argument in the array_of_address *)
                  let last_tree = Lm_list_util.last new_suctrees in
                  let (add_tree,add_ordering,final_pos_n) =
                     copy_and_rename_tree last_tree replace_n new_pos_n mult [] in
                  let final_suctrees = new_suctrees @ [add_tree] in
                  let add_orderings = add_ordering::new_ordering_list in
                  let final_ordering =
                     combine_ordering_list add_orderings pos.pospos
                  in
                     ((NodeA(pos,final_suctrees)),final_ordering,final_pos_n)
             | _ ->
                  let final_ordering =
                     combine_ordering_list new_ordering_list pos.pospos
                  in
                     ((NodeA(pos,new_suctrees)),final_ordering,new_pos_n)
            end

(**************  Path checker   ****************************************************)

let rec get_alpha atom = function
   [] -> raise (Invalid_argument "Jprover bug: atom not found")
 | (a, alpha, _) :: _ when atom_eq a atom -> alpha
 | _ :: r -> get_alpha atom r

let rec get_connections a alpha tabulist =
   match alpha with
      [] -> []
    | f::r ->
         if (a.apredicate = f.apredicate) & (a.apol <> f.apol) & (not (List.mem f tabulist)) then
            (a,f)::(get_connections a r tabulist)
         else
            (get_connections a r tabulist)

let rec connections atom_rel tabulist =
   match atom_rel with
      [] -> []
    | (a,alpha,_)::r ->
         List.rev_append (get_connections a alpha tabulist) (connections r (a::tabulist))

let check_alpha_relation atom set atom_sets =
   AtomSet.subset set (get_alpha atom atom_sets)

let rec extset atom_sets path closed =
   match atom_sets with
      [] -> AtomSet.empty
    | (at,alpha,beta)::r ->
         if (AtomSet.subset path alpha) & (AtomSet.subset closed beta) then
            AtomSet.add (extset r path closed) at
         else
            extset r path closed

let rec check_ext_list ext_list fail_set atom_sets =  (* fail_set consists of one atom only *)
   match ext_list with
      [] -> AtomSet.empty
    | f::r ->
         if (check_alpha_relation f fail_set atom_sets) then
            AtomSet.add (check_ext_list r fail_set atom_sets) f
         else
            (check_ext_list r fail_set atom_sets)

let fail_ext_set ext_atom ext_set atom_sets =
   let ext_list = AtomSet.elements ext_set in
   let fail_set = AtomSet.singleton ext_atom in
   check_ext_list ext_list fail_set atom_sets

let rec ext_partners con path ext_atom reduction_partners extension_partners atom_sets =
   match con with
      [] ->
         (reduction_partners,extension_partners)
    | (a,b)::r ->
         let a_partner = (ext_atom.apos = a.apos) in
         if a_partner || (ext_atom.apos = b.apos) then
            let ext_partner = if a_partner then b else a in
(* force reduction steps first *)
            if (AtomSet.mem path ext_partner) then
               ext_partners r path ext_atom (AtomSet.add reduction_partners ext_partner) extension_partners atom_sets
            else
               let new_ext_partners =
                  if (check_alpha_relation ext_partner path atom_sets) then
                     (AtomSet.add extension_partners ext_partner)
                  else
                     extension_partners
               in
               ext_partners r path ext_atom reduction_partners new_ext_partners atom_sets
         else
            ext_partners r path ext_atom reduction_partners extension_partners atom_sets

exception Failed_connections

let path_checker
   (consts: SymbolSet.t)
   (atom_rel: (atom * atom list * atom list) list)
   (atom_sets: (atom * AtomSet.t * 'a) list)
   (qprefixes: ((position * position list) list) * ((position * position list) list))
   (init_ordering: (position * Set.t) list)
   calculus =

   let con = connections atom_rel [] in
   let atom_rel =
      List.map (fun ({apos=x},y,z) -> x, y, z) atom_rel
   in
(*   print_endline "";
   print_endline ("number of connections: "^(string_of_int (List.length con)));
*)

   let rec provable
      path
      closed
      (orderingQ,reduction_ordering)
      (eqlist : int * (position list * (position list * position list)) list)
      (sigmaQ,sigmaJ)
      =
      let rec check_connections reduction_partners extension_partners ext_atom =
         let try_one =
            if AtomSet.is_empty reduction_partners then
               if AtomSet.is_empty extension_partners then
                  raise Failed_connections
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
            let (new_sigmaQ,new_ordering_elements) = jqunify consts (ext_atom.alabel) (try_one.alabel) sigmaQ in
(* build the orderingQ incrementally from the new added substitution tau of new_sigmaQ *)
            let relate_pairs,new_orderingQ = build_orderingQ new_ordering_elements orderingQ in
(* we make in incremental reflexivity test during the string unification *)
            let (new_sigmaJ,new_eqlist,new_red_ordering) =
(* new_red_ordering = [] in propositional case *)
               stringunify ext_atom try_one eqlist relate_pairs calculus new_orderingQ atom_rel qprefixes
            in
(*           print_endline ("make reduction ordering "^((string_of_int (List.length new_ordering)))); *)
            let new_closed = AtomSet.add closed ext_atom in
            let ((next_orderingQ,next_red_ordering),next_eqlist,(next_sigmaQ,next_sigmaJ),subproof) =
               if AtomSet.mem path try_one then
                  provable path new_closed (new_orderingQ,new_red_ordering) new_eqlist (new_sigmaQ,new_sigmaJ)
                     (* always use old first-order ordering for recursion *)
               else
                  let new_path = AtomSet.add path ext_atom in
                  let extension = AtomSet.singleton try_one in
                  let ((norderingQ,nredordering),neqlist,(nsigmaQ,nsigmaJ),p1) =
                     provable new_path extension (new_orderingQ,new_red_ordering) new_eqlist (new_sigmaQ,new_sigmaJ) in
                  let ((nnorderingQ,nnredordering),nneqlist,(nnsigmaQ,nnsigmaJ),p2) =
                     provable path new_closed (norderingQ,nredordering) neqlist (nsigmaQ,nsigmaJ) in
                  ((nnorderingQ,nnredordering),nneqlist,(nnsigmaQ,nnsigmaJ),(p1 @ p2))
                  (* changing to rev_append on the line above breaks some proofs *)
      (* first the extension subgoals = depth first; then other subgoals in same clause *)
            in
            ((next_orderingQ,next_red_ordering),next_eqlist,(next_sigmaQ,next_sigmaJ),(((ext_atom.apos),(try_one.apos))::subproof))
         with Failed ->
(*          print_endline ("new connection for "^(ext_atom.aname)); *)
(*            print_endline ("Failed"); *)
            check_connections (AtomSet.remove reduction_partners try_one)
                              (AtomSet.remove extension_partners try_one)
                              ext_atom
         )

      in
      let rec check_extension extset =
         if AtomSet.is_empty extset then
            raise Failed             (* go directly to a new entry connection *)
         else
            let select_one = AtomSet.choose extset in
(*        print_endline ("extension literal "^(select_one.aname)); *)
(*        print_endline ("extension path "^(print_set path));*)
            let (reduction_partners,extension_partners) =
               ext_partners con path select_one AtomSet.empty AtomSet.empty atom_sets in
            (try
               check_connections reduction_partners extension_partners select_one
            with Failed_connections ->
(*         print_endline ("no connections for subgoal "^(select_one.aname)); *)
(*           print_endline ("Failed_connections"); *)
               let fail_ext_set = fail_ext_set select_one extset atom_sets in
               check_extension fail_ext_set
            )

      in
      let extset = extset atom_sets path closed in
      if AtomSet.is_empty extset then
         ((orderingQ,reduction_ordering),eqlist,(sigmaQ,sigmaJ),[])
      else
         check_extension extset
   in
   if qprefixes = ([],[]) then
      begin
(*      print_endline "!!!!!!!!!!! prop prover !!!!!!!!!!!!!!!!!!"; *)
(* in the propositional case, the reduction ordering will be computed AFTER proof search *)
         let (_,eqlist,(_,nsubstJ),ext_proof) =
            provable AtomSet.empty AtomSet.empty ([],[]) (1,[]) ([],(1,[])) in
         let _,substJ = nsubstJ in
         let orderingJ = build_orderingJ_list substJ init_ordering atom_rel in
         ((init_ordering,orderingJ),eqlist,([],nsubstJ),ext_proof)
      end
   else
      provable AtomSet.empty AtomSet.empty (init_ordering,[]) (1,[]) ([],(1,[]))

(*************************** prepare let init prover *******************************************************)

let rec make_atom_sets = function
   [] -> []
 | (a,alpha,beta)::r ->
      (a,(AtomSet.of_list alpha),(AtomSet.of_list beta))::(make_atom_sets r)

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
   [] -> [],[]
 | first::rest ->
      if first.apos = element.apos then
         compute_sets element ftree rest    (* element is neithes alpha- nor beta-related to itself*)
      else
         let (alpha_rest,beta_rest) = compute_sets element ftree rest in
         if predecessor (element.aaddress) (first.aaddress) ftree = Beta then
            (alpha_rest,(first::beta_rest))
         else
            ((first::alpha_rest),beta_rest)

let rec compute_atomlist_relations worklist ftree alist =  (* last version of alist for total comparison *)
   let rec compute_atom_relations element ftree alist =
      let alpha_set,beta_set = compute_sets element ftree alist in
      (element,alpha_set,beta_set)
   in
   match worklist with
      [] -> []
    | first::rest ->
         let first_relations = compute_atom_relations first ftree alist in
         first_relations::(compute_atomlist_relations rest ftree alist)

let atom_record position posprefix =
   let {pospos=pospos;
        label=label; address=address; pol=pol; st=st} = position in
   let aname = pos_to_string pospos in
   let aop = (dest_term label).term_op in
   {aname=aname; aaddress=address; apos=pospos;
    aposprefix=posprefix @ [pospos];
    apredicate=aop;
    apol=pol; ast=st; alabel=label}

let rec select_atoms_treelist treelist posprefix =
   match treelist with
      [] -> [],[],[]
    | first::rest ->
         let first_alist,first_gprefixes,first_dprefixes =
            select_atoms first posprefix
         in
         let rest_alist,rest_gprefixes,rest_dprefixes =
            select_atoms_treelist rest posprefix
         in
         List.rev_append first_alist rest_alist,
         List.rev_append first_gprefixes rest_gprefixes,
         List.rev_append first_dprefixes rest_dprefixes

and select_atoms ftree posprefix =
   match ftree with
      Empty -> [],[],[]
    | NodeAt(position) ->
         [atom_record position posprefix],[],[]
    | NodeA(position,suctrees) ->
         let {pospos = pospos; st = st } = position in
         let new_posprefix =
            match st with
               Psi_0 | Phi_0 ->
                  posprefix @ [pospos]
             | _ ->
                  posprefix
         in
         let (rest_alist,rest_gamma_0_prefixes,rest_delta_0_prefixes) =
            select_atoms_treelist suctrees new_posprefix
         in
         let gamma_0_prefixes, delta_0_prefixes =
            match st with
               Gamma_0 ->
                  (position.pospos,posprefix)::rest_gamma_0_prefixes,
                  rest_delta_0_prefixes
             | Delta_0 ->
                  rest_gamma_0_prefixes,
                  (position.pospos,posprefix)::rest_delta_0_prefixes
             | _ ->
                  rest_gamma_0_prefixes,
                  rest_delta_0_prefixes
         in
            rest_alist, gamma_0_prefixes, delta_0_prefixes

let prepare_prover ftree =
   let alist,gamma_0_prefixes,delta_0_prefixes =
      select_atoms_treelist [ftree] []
   in
   let atom_rel = compute_atomlist_relations alist ftree alist in
   (atom_rel,(gamma_0_prefixes,delta_0_prefixes))

(* ************************ Build intial formula tree  let relations *********************************** *)
(* Building a formula tree let the tree ordering from the input formula, i.e. OCaml term *)

let make_position_name stype pos_n =
   match stype with
      Phi_0 | Gamma_0 -> Var, pos_n
    | Psi_0 | Delta_0 -> Const, pos_n
    | _ -> Atom, pos_n

let dual_pol = function
   Zero -> One
 | One -> Zero

let check_subst_term variable old_term pospos stype =
   match stype with
      Gamma_0 | Delta_0 ->
         let new_variable =
            if stype = Gamma_0 then
               (mk_pos_var (simple_to_gamma pospos))
            else
               (mk_pos_term jprover_op pospos)
         in
         (TermSubst.subst1 old_term variable new_variable) (* replace variable (non-empty) in t by pos_name *)
            (* pos_name is either a variable term or a constant, f.i. a string term *)
            (* !!! check unification module how handling eingenvariables as constants !!! *)
    | _ -> old_term

let rec build_ftree variable old_term pol stype address pos_n =
   let pospos = make_position_name stype pos_n in
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
      let subtree_left,ordering_left,posn_left =
         build_ftree empty_sym s pol stype_1 (address@[0]) (pos_n+1)
      in
      let subtree_right,ordering_right,posn_right =
         build_ftree empty_sym t pol stype_2 (address@[1]) (posn_left+1)
      in
      let (succ_left,whole_left) = List.hd ordering_left in
      let (succ_right,whole_right) = List.hd ordering_right in
      let pos_succs =
         Set.add (Set.add (Set.union whole_left whole_right) succ_right) succ_left
      in
      (NodeA(position,[subtree_left;subtree_right]),
       ((position.pospos,pos_succs)::(ordering_left @ ordering_right)),
       posn_right
      )
   else
      if JLogic.is_or_term term then
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
         let subtree_left,ordering_left,posn_left =
            build_ftree empty_sym s pol stype_1 (address@[0]) (pos_n+1)
         in
         let subtree_right,ordering_right,posn_right =
            build_ftree empty_sym t pol stype_2 (address@[1]) (posn_left+1)
         in
         let (succ_left,whole_left) = List.hd ordering_left in
         let (succ_right,whole_right) = List.hd ordering_right in
         let pos_succs =
            Set.add (Set.add (Set.union whole_left whole_right) succ_right) succ_left
         in
         (NodeA(position,[subtree_left;subtree_right]),
          ((position.pospos),pos_succs)::(List.rev_append ordering_left ordering_right),
          posn_right
         )
      else
         if JLogic.is_implies_term term then
            let s,t = JLogic.dest_implies term in
            let ptype_0,stype_0,ptype,stype_1,stype_2 =
               match pol with
                  Zero ->
                     Psi,Psi_0,Alpha,Alpha_1,Alpha_2
                | One ->
                     Phi,Phi_0,Beta,Beta_1,Beta_2
            in
            let pos2pos = make_position_name stype_0 (pos_n+1) in
            let sposition =
               {address=address; pospos=pospos;
               op=Imp; pol=pol; pt=ptype_0; st=stype; label=term}
            in
            let position = {address=address@[0]; pospos=pos2pos;
               op=Imp; pol=pol; pt=ptype; st=stype_0; label=term}
            in
            let subtree_left,ordering_left,posn_left =
               build_ftree empty_sym s (dual_pol pol) stype_1 (address@[0;0])
                  (pos_n+2)
            in
            let subtree_right,ordering_right,posn_right =
               build_ftree empty_sym t pol stype_2 (address@[0;1])
                  (posn_left+1) in
            let (succ_left,whole_left) = List.hd ordering_left in
            let (succ_right,whole_right) = List.hd ordering_right in
            let pos_succs =
               Set.add (Set.add (Set.union whole_left whole_right) succ_right) succ_left in
            let pos_ordering =
					(position.pospos,pos_succs) :: (List.rev_append ordering_left ordering_right)
				in
            (NodeA(sposition,[NodeA(position,[subtree_left;subtree_right])]),
             ((sposition.pospos,(Set.add pos_succs position.pospos))::pos_ordering),
             posn_right
            )
         else
            if JLogic.is_not_term term then
               let s = JLogic.dest_not term in
               let ptype_0,stype_0,ptype,stype_1=
                  match pol with
                     Zero ->
                        Psi,Psi_0,Alpha,Alpha_1
                   | One ->
                        Phi,Phi_0,Alpha,Alpha_1
               in
               let pos2pos = make_position_name stype_0 (pos_n+1) in
               let sposition =
                  {address=address; pospos=pospos;
                  op=Neg; pol=pol; pt=ptype_0; st=stype; label=term}
               in
               let position =
                  {address=address@[0]; pospos=pos2pos;
                  op=Neg; pol=pol; pt=ptype; st=stype_0; label=term}
               in
               let subtree_left,ordering_left,posn_left =
                  build_ftree empty_sym s (dual_pol pol) stype_1 (address@[0;0])
                     (pos_n+2)
               in
               let (succ_left,whole_left) = List.hd ordering_left in
               let pos_succs =
                  Set.add whole_left succ_left in
               let pos_ordering = (position.pospos,pos_succs) :: ordering_left in
               (NodeA(sposition,[NodeA(position,[subtree_left])]),
                ((sposition.pospos,(Set.add pos_succs position.pospos))::pos_ordering),
                posn_left
               )
            else
               if JLogic.is_exists_term term then
                  let v,s,t = JLogic.dest_exists term in  (* s is type of v let will be supressed here *)
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
                  let subtree_left,ordering_left,posn_left =
                     build_ftree v t pol stype_1 (address@[0]) (pos_n+1)
                  in
                  let (succ_left,whole_left) = List.hd ordering_left in
                  let pos_succs =
                     Set.add whole_left succ_left in
                  (NodeA(position,[subtree_left]),
                   ((position.pospos,pos_succs) :: ordering_left),
                   posn_left
                  )
               else
                  if JLogic.is_all_term term then
                     let v,s,t = JLogic.dest_all term in
       (* s is type of v let will be supressed here *)
                     let ptype_0,stype_0,ptype,stype_1=
                        match pol with
                           Zero ->
                              Psi,Psi_0,Delta,Delta_0
                         | One ->
                              Phi,Phi_0,Gamma,Gamma_0
                     in
                     let pos2pos = make_position_name stype_0 (pos_n+1) in
                     let sposition =
                        {address=address; pospos=pospos;
                        op=All; pol=pol; pt=ptype_0; st=stype; label=term}
                     in
                     let position =
                        {address=address@[0]; pospos=pos2pos;
                        op=All; pol=pol; pt=ptype; st=stype_0; label=term}
                     in
                     let subtree_left,ordering_left,posn_left =
                        build_ftree v t pol stype_1 (address@[0;0]) (pos_n+2)
                     in
                     let (succ_left,whole_left) = List.hd ordering_left in
                     let pos_succs =
                        Set.add whole_left succ_left in
                     let pos_ordering =
                        (position.pospos,pos_succs) :: ordering_left
                     in
                     (NodeA(sposition,[NodeA(position,[subtree_left])]),
                      ((sposition.pospos,(Set.add pos_succs position.pospos))::pos_ordering),
                      posn_left
                     )
                  else      (* finally, term is atomic *)
                     let ptype_0,stype_0 =
                        match pol with
                           Zero ->
                              Psi,Psi_0
                         | One ->
                              Phi,Phi_0
                     in
                     let pos2pos = make_position_name stype_0 (pos_n+1) in
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
                      (position.pospos,Set.empty)],
                     pos_n+1

let rec construct_ftree
   termlist
   treelist
   (orderinglist : (position * Set.t) list)
   pos_n
   goal
   =
   match termlist with
      [] ->
         let new_root =
            {address=[]; pospos=root_pos;
            op=Null; pol=Zero; pt=Psi; st=PNull_0; label=goal}
         in
         NodeA(new_root,treelist),
         ((root_pos,(union_orderings orderinglist))::orderinglist),pos_n
    | ft::rest_terms ->
         let next_address = [List.length treelist] in
         let next_pol,next_goal =
            if rest_terms = []  then
               Zero,ft (* construct tree for the conclusion *)
            else
               One,goal
         in
         let new_tree,new_ordering,new_pos_n =
            build_ftree empty_sym ft next_pol Alpha_1 next_address (pos_n+1) in
         construct_ftree rest_terms (treelist @ [new_tree])
            (List.rev_append orderinglist new_ordering) new_pos_n next_goal
			(* rev_append in treelist breaks some proofs *)

(*************************** Main LOOP ************************************)

let unprovable = RefineError ("Jprover", StringError "formula is not provable")
let mult_limit_exn = RefineError ("Jprover", StringError "multiplicity limit reached")

let init_prover ftree =
   let atom_relation,qprefixes = prepare_prover ftree in
(*      print_atom_info atom_relation;  *)
   let atom_sets = make_atom_sets atom_relation in
   (atom_relation,atom_sets,qprefixes)

let rec try_multiplicity
   (consts: SymbolSet.t)
   mult_limit
   ftree
   (ordering:(position * Set.t) list)
   pos_n
   mult
   calculus
   =
   try
      let (atom_relation,atom_sets,qprefixes) = init_prover ftree in
      let ((orderingQ,red_ordering),eqlist,unifier,ext_proof) =
         path_checker consts atom_relation atom_sets qprefixes ordering calculus in
      (ftree,red_ordering,eqlist,unifier,ext_proof)   (* orderingQ is not needed as return value *)
   with Failed ->
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
            let (new_ftree,new_ordering,new_pos_n) =
               add_multiplicity ftree pos_n new_mult calculus in
            if ftree_eq new_ftree ftree then
               raise unprovable
            else
(*             print_formula_info new_ftree new_ordering new_pos_n;   *)
               try_multiplicity consts mult_limit new_ftree new_ordering new_pos_n new_mult calculus

let prove consts mult_limit termlist calculus =
   let (ftree,ordering,pos_n) =
      construct_ftree termlist [] [] 0 (mk_pos_var (dummy_pos ()))
   in
(* pos_n = number of positions without new root "w" *)
(*   print_formula_info ftree ordering pos_n;    *)
   let ftree,red_ordering,eqlist,(sigmaQ,sigmaJ),ext_proof =
      try_multiplicity consts mult_limit ftree ordering pos_n 1 calculus
   in
   let sym_sigmaQ = List.map
      (fun (p,t) -> pos_to_symbol p, t) sigmaQ
   in
   ftree, red_ordering, eqlist, (sym_sigmaQ,sigmaJ), ext_proof


(********** first-order type theory interface *******************)

let rec renam_free_vars termlist =
   match termlist
   with [] -> [],[],SymbolSet.empty
    | f::r ->
         let conts = all_contexts f in
         let var_names = free_vars_list f conts in
         let mapping =
            List.map
               (fun s -> s, mk_symbol_term free_var_op s)
               var_names
         in
         let new_f = TermSubst.apply_subst mapping f in
         let rest_mapping,rest_renamed,rest_conts = renam_free_vars r in
         let unique_mapping = remove_subst_dups (List.rev_append mapping rest_mapping) in
         (unique_mapping,(new_f::rest_renamed),SymbolSet.union conts rest_conts)

let rec apply_var_subst term = function
   [] -> term
 | (v,t)::r ->
      let next_term = TermSubst.var_subst term t v in
      apply_var_subst next_term r

let rec make_equal_list pattern list_object =
   List.rev_map (fun _ -> list_object) pattern

let rec create_output consts rule_list
   (input_map : (symbol * term) list)
   =
   match rule_list with
      [] -> JLogic.empty_inf
    | f::r ->
         let (pos,(rule,term1,term2)) = f in
         let delta1_names = collect_delta_terms [term1] in
         let delta2_names = collect_delta_terms [term2] in
         let unique_deltas =
            remove_dups_list (List.rev_append delta1_names delta2_names)
         in
         let delta_map =
            List.rev_map
               (fun s ->
                  let p = symbol_to_pos s in
                  pos_to_symbol (simple_to_gamma p),
                  mk_symbol_term jprover_op s
               )
               unique_deltas
         in
         let var_mapping = List.rev_append input_map delta_map in
         let frees1 = free_vars_list term1 consts in
         let frees2 = free_vars_list term2 consts in
         let unique_object = mk_pos_var ((GammaPos NewVar),0) in
         let unique_list1 = make_equal_list frees1 unique_object in
         let unique_list2 = make_equal_list frees2 unique_object in
         let next_term1 = TermSubst.subst term1 frees1 unique_list1 in
         let next_term2 = TermSubst.subst term2 frees2 unique_list2 in
         let new_term1 = apply_var_subst next_term1 var_mapping in
         let new_term2 = apply_var_subst next_term2 var_mapping in
(* kick away the first argument, the position *)
         (JLogic.append_inf (create_output consts r input_map) new_term1 new_term2 rule)

let rec make_test_interface consts rule_list input_map =
   match rule_list with
      [] -> []
    | f::r ->
         let (pos,(rule,term1,term2)) = f in
         let delta1_names = collect_delta_terms [term1] in
         let delta2_names = collect_delta_terms [term2] in
         let unique_deltas =
            remove_dups_list (List.rev_append delta1_names delta2_names)
         in
         let delta_map =
            List.rev_map
               (fun s ->
                  let p =symbol_to_pos s in
                  pos_to_symbol (simple_to_gamma p),
                  mk_symbol_term jprover_op s
               )
               unique_deltas
         in
         let var_mapping = List.rev_append input_map delta_map in
         let frees1 = free_vars_list term1 consts in
         let frees2 = free_vars_list term2 consts in
         let unique_object = mk_pos_var (GammaPos NewVar,0) in
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
            let new_term1 = apply_var_subst next_term1 var_mapping in
            let new_term2 = apply_var_subst next_term2 var_mapping in
            (pos,(rule,new_term1,new_term2))::(make_test_interface consts r input_map)
         end

(**************************************************************)

let gen_prover mult_limit calculus hyps concls =
   let (input_map,renamed_termlist,consts) = renam_free_vars (hyps @ concls) in
   let (ftree,red_ordering,eqlist,(sigmaQ,sigmaJ),ext_proof) = prove consts mult_limit renamed_termlist calculus in
   let sequent_proof = reconstruct ftree red_ordering sigmaQ ext_proof calculus in
         (* transform types let rename constants *)
     (* we can transform the eigenvariables AFTER proof reconstruction since *)
     (* new delta_0 constants may have been constructed during rule permutation *)
     (* from the LJmc to the LJ proof *)
   create_output consts sequent_proof input_map

let prover mult_limit hyps concl = gen_prover mult_limit (Intuit SingleConcl) hyps [concl]

(************* test with propositional proof reconstruction ************)

let rec count_axioms seq_list =
   match seq_list with
      [] -> 0
    | f::r ->
         let (rule,_,_) = f in
         if rule = Ax then
            1 + count_axioms r
         else
            count_axioms r

let do_prove mult_limit termlist calculus =
   try begin
      let (input_map,renamed_termlist,consts) = renam_free_vars termlist in
      let (ftree,red_ordering,eqlist,(sigmaQ,sigmaJ),ext_proof) = prove consts mult_limit renamed_termlist calculus in
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
      open_box 0;
      let (qmax,equations) = eqlist in
      print_endline ("number of quantifier domains :"^(string_of_int (qmax-1)));
      print_endline "";
      (*print_equations equations;*)
      print_flush ();
      print_endline "";
      print_endline "";
      print_endline ("Length of equations : "^((string_of_int (List.length equations))));
      print_endline "";
      print_endline "";
(* --------------------------------------------------------- *)
      print_string "Break ... ";
      print_endline "";
      print_endline "";
      print_flush ();
      let _ = input_char stdin in
      let reconstr_proof = reconstruct ftree red_ordering sigmaQ ext_proof calculus in
      let sequent_proof = make_test_interface consts reconstr_proof input_map in
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
   do_prove None [concl] calculus

(* for sequents *)

let seqtest list_term calculus =
   let termlist = collect_subterms [] (dest_term list_term).term_terms in
   do_prove None termlist calculus

(*****************************************************************)

end (* of struct *)
