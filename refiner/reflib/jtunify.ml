(*
 * Unification procedures for JProver. See jall.mli for more
 * information on JProver.
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
 * Modified by: Yegor Bryukhov <ybryukhov@gc.cuny.edu>
 *)
open Lm_printf

open Refiner.Refiner.TermType

open Jlogic_sig
open Jtypes
open Jordering

let jprover_bug = Invalid_argument "Jprover bug (Jtunify module)"

(* ************ T-STRING UNIFICATION *********************************)


(* ******* printing ********** *)

let rec list_to_string s =
   match s with
      [] -> ""
    | f::r ->
         (pos_to_string f)^"."^(list_to_string r)

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
		print_flush ();
   end

let rec print_subst sigma =
   match sigma with
      [] ->
         print_endline ""
    | f::r ->
         let (v,s) = f in
         let ls = list_to_string s in
         begin
            print_endline ((pos_to_string v)^" = "^ls);
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

let rec sort_of_aux k1 k2 =
	match k1, k2 with
		GammaPos k1, _ -> sort_of_aux k1 k2
	 | _, GammaPos k2 -> sort_of_aux k1 k2
	 | (Var 0 | NewVar 0 | NewVarQ 0), (Var j | NewVar j | NewVarQ j) -> j
	 | (Var i | NewVar i | NewVarQ i), (Var 0 | NewVar 0 | NewVarQ 0) -> i
	 | (Var i | NewVar i | NewVarQ i), (Var j | NewVar j | NewVarQ j) when i=j -> i
	 | _ ->
		 	print_endline ((kind_to_string k1)^" - "^(kind_to_string k2));
		 	raise (Invalid_argument "sort_of: Incompatible sorts")

let sort_of (k1,i1) (k2,i2) =
	sort_of_aux k1 k2

let rec var_le_aux k1 k2 =
  	match k1, k2 with
      GammaPos k1, _ -> var_le_aux k1 k2
    | _, GammaPos k2 -> var_le_aux k1 k2
    | (Var 0 | NewVar 0 | NewVarQ 0), _ -> true
    | (Var i | NewVar i | NewVarQ i),
	 	(Var j | NewVar j | NewVarQ j | Const j) -> i = j
	 | Const _, _ -> false
	 | _ -> raise (Invalid_argument
	 		("var_le: Unexpected position kinds: "^(kind_to_string k1)^" "^(kind_to_string k2)))

let var_le (k1,i1) (k2,i2) =
	var_le_aux k1 k2

let rec compatible_vars_aux k1 k2 =
   match k1, k2 with
      GammaPos k1, _ -> compatible_vars_aux k1 k2
    | _, GammaPos k2 -> compatible_vars_aux k1 k2
    | (Var 0 | NewVar 0 | NewVarQ 0), (Var _ | NewVar _ | NewVarQ _)
	 | (Var _ | NewVar _ | NewVarQ _), (Var 0 | NewVar 0 | NewVarQ 0) -> true
    | (Var i | NewVar i | NewVarQ i), (Var j | NewVar j | NewVarQ j) -> i = j
	 | Const _, _
	 | _, Const _ -> false
    | _ -> raise (Invalid_argument
         ("compatible_vars: Unexpected position kinds: "^(kind_to_string k1)^" "^(kind_to_string k2)))

let compatible_vars (k1,i1) (k2,i2) =
   compatible_vars_aux k1 k2

let rec com_subst ov ovlist = function
   [] -> []
 | f::r as l->
      if position_eq f ov then
         (List.rev_append ovlist r)
      else
         let rest = com_subst ov ovlist r in
            if rest == r then l else f :: rest

(*
 * rev_appends here seem to bring expand_all into an infinite loop
 *)
let rec apply_element_aux v slist acc = function
	[] -> acc
 | hd::tl ->
		if hd = v then (* XXX Yegor : replacement of (=) with position_eq here breaks some proofs *)
			apply_element_aux v slist (List.rev_append slist acc) tl
		else
			apply_element_aux v slist (hd::acc) tl

let rec apply_element v slist fs ft =
	let new_fs = apply_element_aux v slist [] fs in
	let new_ft = apply_element_aux v slist [] ft in
	List.rev new_fs, List.rev new_ft

let rec shorten (us : position list) ut =
   match us, ut with
      (fs::rs), (ft::rt) when position_eq fs ft ->
         shorten rs rt
    | usut ->
         usut

type equation = position list * (position list * position list)

module type JQuantifierSig =
sig

   val build_ordering :
      calculus ->
      position list ->
      position list ->
      Set.t PMap.t ->
		Set.t ->
      Set.t PMap.t

   val add_fo_eqlist : equation list -> equation list -> equation list
end

module JQuantifier (JLogic : JLogicSig) =
struct

   module JTypes = JTypes(JLogic)
   module JOrdering = JOrdering(JLogic)
   open JOrdering

   let build_ordering = build_orderingJ

	let add_fo_eqlist a b = List.rev_append b a
   (* rev_append here changes proofs but does not break them *)
end

module JPropositionalQuantifier (JLogic : JLogicSig) =
struct

   module JTypes = JTypes(JLogic)

	let build_ordering _ _ _ _ _ = PMap.empty

	let add_fo_eqlist a b = a
end


module JTUnify (JLogic : JLogicSig)(JQuantifier: JQuantifierSig) =
struct

   module JOrdering = JOrdering(JLogic)
   open JOrdering
   open JQuantifier

let rec combine ov oslist = function
   [] -> [],[]
 | ((v, slist) as f) :: r ->
      let com_element = com_subst ov oslist slist in
      let rest_vlist,rest_combine = combine ov oslist r in
         if com_element == slist then
            rest_vlist, f::rest_combine
         else
            (v::rest_vlist),((v,com_element)::rest_combine)

let compose (n,subst) ((ov,oslist) as one_subst) =
   let trans_vars,com = combine ov oslist subst in
(* begin
   print_endline "!!!!!!!!!test print!!!!!!!!!!";
   print_subst [one_subst];
   print_subst subst;
   print_endline "!!!!!!!!! END test print!!!!!!!!!!";
*)
   if List.mem one_subst subst then
      (trans_vars,(n,com))
   else
(* ov may multiply as variable in subst with DIFFERENT values *)
(* in order to avoid explicit atom instances!!! *)
      (trans_vars,(n,(one_subst :: com)))
(* end *)

let rec apply_subst_list eq_rest v slist =
   match eq_rest with
      [] ->
         []
    | (atomnames,(fs,ft))::r ->
         let (n_fs,n_ft) = apply_element v slist fs ft in
         let (new_fs,new_ft) = shorten n_fs n_ft in (* delete equal first elements *)
         match (new_fs,new_ft) with
            [],[] ->
               let new_eq_rest = apply_subst_list r v slist in
               (atomnames,([],[]))::new_eq_rest
          | [],(fft::_) ->
               if (is_const fft) then
                  raise Not_unifiable
               else
                  let new_eq_rest = apply_subst_list r v slist in
                  (atomnames,([],new_ft)) :: new_eq_rest
          | (ffs::_),[] ->
               if (is_const ffs) then
                  raise Not_unifiable
               else
                  let new_eq_rest = apply_subst_list r v slist in
                  (atomnames,(new_fs,[])) :: new_eq_rest
          | (ffs::_),(fft::_) ->
               if var_le ffs fft || var_le fft ffs  then
     (* at least one of firsts is a variable *)
                  let new_eq_rest = apply_subst_list r v slist in
                  (atomnames,(new_fs,new_ft)) :: new_eq_rest
					else
                  raise Not_unifiable
     (* different first constants cause local fail *)

let apply_subst eq_rest v slist atomnames =
   if (List.mem v atomnames) then (* don't apply subst to atom variables !! *)
      eq_rest
   else
      apply_subst_list eq_rest v slist

(* let all_variable_check eqlist = false   needs some discussion with Jens! -- NOT done *)

(*
let rec all_variable_check eqlist =
   match eqlist with
      [] -> true
    | ((_,(fs,ft))::rest_eq) ->
         if (fs <> []) & (ft <> []) then
            let fs_first = List.hd fs
            and ft_first = List.hd ft
            in
            if (is_const fs_first) or (is_const ft_first) then
               false
            else
               all_variable_check rest_eq
            else
               false
*)

let r_10 s rt =
   match s,rt with
      v::stl, x::rtl ->
         var_le v x && (v <> x) &&
         ((stl =[]) or (is_const x) or (rtl <> []))
    | _ -> false

let rec tunify_list calculus eqlist init_sigma ordering atom_set =
   let rec tunify atomnames fs ft rt rest_eq sigma ordering =
      let apply_r1 rest_eq sigma =
	  (* print_endline "r1"; *)
         tunify_list calculus rest_eq sigma ordering atom_set

      in
      let apply_r2 fs ft rt rest_eq sigma =
      (* print_endline "r2"; *)
         tunify atomnames rt fs ft rest_eq sigma ordering

      in
      let apply_r3 fs ft rt rest_eq sigma =
      (* print_endline "r3"; *)
         let rfs =  (List.tl fs) in
         let rft =  (List.tl rt) in
         tunify atomnames rfs ft rft rest_eq sigma ordering

      in
      let apply_r4 fs ft rt rest_eq sigma =
      (* print_endline "r4"; *)
         tunify atomnames rt ft fs rest_eq sigma ordering

      in
      let apply_r5 fs ft rt rest_eq sigma =
      (* print_endline "r5"; *)
         let v = (List.hd fs) in
         let compose_vars, new_sigma = compose sigma (v,ft) in
         let new_rest_eq = apply_subst rest_eq v ft atomnames in
         let new_ordering = build_ordering calculus (v::compose_vars) ft ordering atom_set in
            tunify atomnames (List.tl fs) rt rt new_rest_eq new_sigma new_ordering

      in
      let apply_r6 fs ft rt rest_eq sigma =
      (* print_endline "r6"; *)
         let v = (List.hd fs) in
         let _, new_sigma = compose sigma (v,[]) in
         let new_rest_eq = apply_subst rest_eq v [] atomnames in
         (* Q-case: no relation update since [] has been replaced for v *)
            tunify atomnames (List.tl fs) ft rt new_rest_eq new_sigma ordering

      in
      let apply_r7 fs ft rt rest_eq sigma =
      (* print_endline "r7"; *)
         let v = List.hd fs in
         let c1 = List.hd rt in
         let c2t =List.tl rt in
         let ft_c1 = ft @ [c1] in
         let compose_vars,new_sigma = compose sigma (v,ft_c1) in
         let new_rest_eq = apply_subst rest_eq v ft_c1 atomnames in
         let new_ordering = build_ordering calculus (v::compose_vars) ft_c1 ordering atom_set in
            tunify atomnames (List.tl fs) []  c2t new_rest_eq new_sigma new_ordering

      in
      let apply_r8 fs ft rt rest_eq sigma =
      (* print_endline "r8"; *)
         tunify atomnames rt [(List.hd fs)] (List.tl fs) rest_eq sigma ordering

      in
      let apply_r9 sort fs ft rt rest_eq sigma =
      (* print_endline "r9"; *)
         let v = List.hd fs in
         let (max,subst) = sigma in
         let v_new = (NewVar sort,max) in
         let ft_vnew = ft @ [v_new] in
         let compose_vars,new_sigma = compose ((max+1),subst) (v,ft_vnew) in
         let new_rest_eq = apply_subst rest_eq v ft_vnew atomnames in
         let new_ordering = build_ordering calculus (v::compose_vars) ft_vnew ordering atom_set in
         tunify atomnames rt [v_new] (List.tl fs) new_rest_eq new_sigma new_ordering

      in
      let apply_r10 fs ft rt rest_eq sigma =
      (* print_endline "r10"; *)
         let x = List.hd rt in
         tunify atomnames fs (ft @ [x]) (List.tl rt) rest_eq sigma ordering

      in
		let apply_r11 fs ft rt rest_eq sigma =
         let v = List.hd fs in
         let compose_vars,new_sigma = compose sigma (v,ft) in
         let new_rest_eq = apply_subst rest_eq v ft atomnames in
         let new_ordering = build_ordering calculus (v::compose_vars) ft ordering atom_set in
            tunify atomnames (List.tl fs) [] rt new_rest_eq new_sigma new_ordering
		in
      match fs,ft,rt with
         [], [], [] -> (* r1 *)
            apply_r1 rest_eq sigma
       | [], [], _::_ -> (* r2 *)
            apply_r2 fs ft rt rest_eq sigma
       | s1::_, [], r1::_ when s1=r1 -> (* r3 *)
            apply_r3 fs ft rt rest_eq sigma
       | s1::_, [], r1::_ when is_var r1 && is_const s1 -> (* r4 *)
            apply_r4 fs ft rt rest_eq sigma
       | s1::_, _, [] when is_var s1 -> (* r5 *)
            apply_r5 fs ft rt rest_eq sigma
       | s1::_, [], r1::rtl when is_var s1 & is_const r1 -> (* r6 *)
            begin try apply_r6 fs ft rt rest_eq sigma with
               Not_unifiable ->
						if var_le s1 r1 then
	                  match rtl with
   	                  r2::_ when is_const r2 ->
      	                  (* r7 applicable if r6 was and tr6 = C2t' *)
         	               (try
            	               apply_r7 fs ft rt rest_eq sigma
               	         with Not_unifiable ->
                  	         apply_r10 fs ft rt rest_eq sigma
                     	      (* r10 always applicable if r6 was *)
	                        )
   	                | _ ->
      (* r10 could be represented only once if we would try it before r7.*)
      (* but looking at the transformation rules, r10 should be tried at last in any case *)
	                        apply_r10 fs ft rt rest_eq sigma  (* r10 always applicable r6 was *)
						else
							apply_r11 fs ft rt rest_eq sigma
            end
       | s1::_, _, r1::r2::_ (* r7 *)
         when var_le s1 r1 && is_const r1 && is_const r2 ->
            (* r7, should be the same as in the catch part after apply_r6 *)
            (try
               apply_r7 fs ft rt rest_eq sigma
            with Not_unifiable ->
               apply_r10 fs ft rt rest_eq sigma  (* r10 always applicable if r7 was *)
            )
       | v::_::_, [], v1::_ (* r8 *)
         when var_le v1 v && is_var v && (v <> v1) -> (* r8 *)
            (try
               apply_r8 fs ft rt rest_eq sigma
            with Not_unifiable ->
               if r_10 fs rt then (* r10 applicable if r8 was and tr8 <> [] *)
                  apply_r10 fs ft rt rest_eq sigma
               else
                  raise Not_unifiable (* simply back propagation *)
            )
       | v::_::_, _::_, v1::_ (* r9 *)
         when compatible_vars v v1 && (v <> v1) -> (* r9 *)
            (try
               apply_r9 (sort_of v v1) fs ft rt rest_eq sigma
            with Not_unifiable ->
	            if r_10 fs rt then (* r10 applicable if r9 was and tr9 <> [] *)
   	            apply_r10 fs ft rt rest_eq sigma
      	      else
						if not (var_le v v1) then
							apply_r11 fs ft rt rest_eq sigma
						else
							raise Not_unifiable (* simply back propagation *)
            )
       | _ when r_10 fs rt ->
            (* r10 *)
         	apply_r10 fs ft rt rest_eq sigma
		 | v::_, _, r1::_ when is_var v && not (var_le v r1) ->
		 		apply_r11 fs ft rt rest_eq sigma
       | _ ->
         	(*NO rule applicable *)
            raise Not_unifiable
   in
   match eqlist with
      [] ->
         init_sigma,ordering
    | f::rest_eq ->
         if !debug_s4prover then
            begin
               open_box 0;
               print_equations [f];
               print_flush ()
            end;
         let (atomnames,(fs,ft)) = f in
         tunify atomnames fs [] ft rest_eq init_sigma ordering

let rec test_apply_eq atomnames eqs eqt subst =
   match subst with
      [] -> (eqs,eqt)
    | (f,flist)::r ->
         let (first_appl_eqs,first_appl_eqt) =
            if List.mem f atomnames then
               eqs, eqt
            else
               apply_element f flist eqs eqt
         in
         test_apply_eq atomnames first_appl_eqs first_appl_eqt r

let rec test_apply_eqsubst eqlist subst =
   match eqlist with
      [] -> []
    | (atomnames,(eqs,eqt))::r ->
         let applied_element = test_apply_eq atomnames eqs eqt subst in
         (atomnames,applied_element)::(test_apply_eqsubst r subst)

let ttest calculus us ut ns nt eqlist ordering atom_set =
   let (short_us,short_ut) = shorten us ut in (* apply intial rule R3 *)
                                           (* to eliminate common beginning *)
   let new_element = ([ns;nt],(short_us,short_ut)) in
   let full_eqlist =
      if List.mem new_element eqlist then
         eqlist
      else
         new_element::eqlist
   in
   let sigma, _ = tunify_list calculus full_eqlist (1,[]) ordering atom_set in
   let _, subst = sigma in
   let test_apply = test_apply_eqsubst full_eqlist subst in
   begin
      print_endline "";
      print_endline "Final equations:";
      (*print_equations full_eqlist;*)
      print_endline "";
      print_endline "Final substitution:";
      (*print_tunify sigma;*)
      print_endline "";
      print_endline "Applied equations:";
      (*print_equations test_apply*)
   end

let do_stringunify calculus us ut ns nt equations fo_eqlist orderingQ atom_set =
    if !debug_s4prover then
		 begin
	       print_endline "do_stringunify:";
			 print_flush ()
		 end;
    let short_us, short_ut = shorten us ut in (* apply intial rule R3 to eliminate common beginning *)
	let atomnames = [ns;nt] in
   let new_element = atomnames, (short_us,short_ut) in
	let original_eqlist = add_fo_eqlist equations fo_eqlist in
   let full_eqlist = new_element::original_eqlist in
	if !debug_s4prover then
		print_equations full_eqlist;
   try
(* Q-case: max-1 new variables have been used for the domain equations *)
      let new_sigma, new_ordering = tunify_list calculus full_eqlist (1,[]) orderingQ atom_set in
(* Q-case: sigmaQ will not be returned in eqlist *)
      if !debug_s4prover then
         begin
            print_tunify new_sigma;
            print_ordering_map new_ordering
         end;
      new_sigma, full_eqlist, new_ordering
   with Not_unifiable ->
      raise Failed            (* new connection please *)

end

module JTUnifyQ(L: JLogicSig) = JTUnify(L)(JQuantifier(L))
module JTUnifyProp(L: JLogicSig) = JTUnify(L)(JPropositionalQuantifier(L))

(* type of one unifier: int * (string * string) list  *)
