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
open Lm_debug

open Refiner.Refiner.TermType

open Jlogic_sig
open Jtypes
open Jordering

let jprover_bug = Invalid_argument "Jprover bug (Jtunify module)"

let debug_jtunify =
   create_debug (**)
      { debug_name = "jtunify";
        debug_description = "Display J-Prover T-unification operations";
        debug_value = false
      }

(* ************ T-STRING UNIFICATION *********************************)


(* ******* printing ********** *)

let rec list_to_string s =
   match s with
      [] -> ""
    | f::r ->
         (pos_to_string f)^"."^(list_to_string r)

let print_eq (atomnames,fs,ft,rt) =
	let fs = list_to_string fs in
	let ft = list_to_string ft in
	let rt = list_to_string rt in
	print_endline (fs^" = "^ft^" | "^rt)

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
         ovlist @ r
      else
         let rest = com_subst ov ovlist r in
            if rest == r then l else f :: rest

(*
 * rev_appends here seem to bring expand_all into an infinite loop
 *)
let rec apply_element_aux v slist acc = function
	[] -> acc
 | hd::tl ->
		if position_eq hd v then
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

   module JTypes = MkJTypes(JLogic)
   module JOrder = MkJOrdering(JLogic)
   open JOrder

   let build_ordering = build_orderingJ

	let add_fo_eqlist a b = List.rev_append b a
   (* rev_append here changes proofs but does not break them *)
end

module JPropositionalQuantifier (JLogic : JLogicSig) =
struct

   module JTypes = MkJTypes(JLogic)

	let build_ordering _ _ _ _ _ = PMap.empty

	let add_fo_eqlist a b = a
end

type trace_entry =
    ((position list * position list * position list * position list) *
     (equation list * (int * (position * position list) list) * Set.t PMap.t) *
     equation list * int list
    )

type tracelist = trace_entry list list

module JTUnify (JLogic : JLogicSig)(JQuantifier: JQuantifierSig) =
struct

   module JOrder = MkJOrdering(JLogic)
   open JOrder
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
						(if !debug_jtunify then
							print_endline "failure";
                  raise Not_unifiable)
               else
                  let new_eq_rest = apply_subst_list r v slist in
                  (atomnames,([],new_ft)) :: new_eq_rest
          | (ffs::_),[] ->
               if (is_const ffs) then
						(if !debug_jtunify then
							print_endline "failure";
                  raise Not_unifiable)
               else
                  let new_eq_rest = apply_subst_list r v slist in
                  (atomnames,(new_fs,[])) :: new_eq_rest
          | (ffs::_),(fft::_) ->
               if var_le ffs fft || var_le fft ffs  then
     (* at least one of firsts is a variable *)
                  let new_eq_rest = apply_subst_list r v slist in
                  (atomnames,(new_fs,new_ft)) :: new_eq_rest
					else
						(if !debug_jtunify then
							print_endline "failure";
                  raise Not_unifiable)
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

let r1 eq =
      match eq with
         _, [], [], [] -> (* r1 *)
            true
       | _ ->
            false

let r2 eq =
      match eq with
       | _, [], [], _::_ -> (* r2 *)
            true
       | _ ->
            false

let r3 eq =
      match eq with
       | _, s1::_, [], r1::_ when s1=r1 -> (* r3 *)
            true
       | _ ->
            false

let r4 eq =
      match eq with
       | _, s1::_, [], r1::_ ->
		 		is_var r1 && is_const s1 (* r4 *)
       | _ ->
            false

let r5 eq =
      match eq with
       | _, s1::_, _, [] ->
		 		is_var s1 (* r5 *)
       | _ ->
            false

let r6 eq =
      match eq with
       | _, s1::_, [], r1::rtl ->
		 		is_var s1 & is_const r1 (* r6 *)
       | _ ->
            false

let r7 eq =
      match eq with
       | _, s1::_, _, r1::r2::_ -> (* r7 *)
         	var_le s1 r1 && is_const r1 && is_const r2
       | _ ->
            false

let r8 eq =
      match eq with
       | _, v::_::_, [], v1::_ -> (* r8 *)
         	var_le v1 v && is_var v && (v <> v1) (* r8 *)
       | _ ->
            false

let r9 eq =
      match eq with
       | _, v::_::_, _::_, v1::_ -> (* r9 *)
         	compatible_vars v v1 && (v <> v1) (* r9 *)
       | _ ->
            false

let r10 eq =
   match eq with
      _, v::stl, _, x::rtl ->
         var_le v x && (v <> x) &&
         ((stl =[]) or (is_const x) or (rtl <> []))
    | _ -> false

(*
let r11 eq =
      match eq with
       | _, v::_, _, r1::_ ->
		 		is_var v && is_var r1 && not (var_le v r1)
       | _ ->
            false
*)

let r11 eq =
      match eq with
       | _, v::_, _::_, r1::_ ->
            is_var v && (*is_var r1 &&*) not (var_le v r1)
       | _, v::_, [], r1::_ ->
            is_var v && is_var r1 && not (compatible_vars v r1)
       | _ ->
            false

let r12 eq =
   match eq with
    | _, [v], [], r1::_ ->
         is_var v && var_le r1 v && not (var_le v r1)
    | _ ->
         false

let never _ = false

let apply_r1 atom_set calculus eq state =
	if !debug_jtunify then
		begin
			print_eq eq;
			print_endline "r1";
		end;
	true, eq, state

let apply_r2 atom_set calculus (atomnames, fs, ft, rt) state =
	if !debug_jtunify then
		begin
			print_eq (atomnames, fs, ft, rt);
			print_endline "r2";
		end;
	false, (atomnames, rt, fs, ft), state

let apply_r3 atom_set calculus eq state =
	if !debug_jtunify then
		begin
			print_eq eq;
			print_endline "r3";
		end;
	match eq with
		atomnames, _::rfs, ft, _::rft ->
			false, (atomnames, rfs, ft, rft), state
	 | _ ->
	 		raise (Invalid_argument "Jtunify.apply_r3 applied to an incorrect equation")

let apply_r4 atom_set calculus (atomnames, fs, ft, rt) state =
	if !debug_jtunify then
		begin
			print_eq (atomnames, fs, ft, rt);
			print_endline "r4";
		end;
	false, (atomnames, rt, ft, fs), state

let apply_r5 atom_set calculus (atomnames, fs, ft, rt) (rest_eq, sigma, ordering) =
	if !debug_jtunify then
		begin
			print_eq (atomnames, fs, ft, rt);
			print_endline "r5";
		end;
	match fs with
		v::nfs ->
		   let compose_vars, new_sigma = compose sigma (v,ft) in
		   let new_rest_eq = apply_subst rest_eq v ft atomnames in
		   let new_ordering = build_ordering calculus (v::compose_vars) ft ordering atom_set in
		   false, (atomnames, nfs, rt, rt), (new_rest_eq, new_sigma, new_ordering)
	 | _ ->
	 		raise (Invalid_argument "Jtunify.apply_r5 applied to an incorrect equation")

let apply_r6 atom_set calculus (atomnames, fs, ft, rt) (rest_eq, sigma, ordering) =
	if !debug_jtunify then
		begin
			print_eq (atomnames, fs, ft, rt);
			print_endline "r6";
		end;
	match fs with
		v::nfs ->
		   let _, new_sigma = compose sigma (v,[]) in
		   let new_rest_eq = apply_subst rest_eq v [] atomnames in
		   (* Q-case: no relation update since [] has been replaced for v *)
		   false, (atomnames, nfs, ft, rt), (new_rest_eq, new_sigma, ordering)
	 | _ ->
	 		raise (Invalid_argument "Jtunify.apply_r6 applied to an incorrect equation")

let apply_r7 atom_set calculus eq (rest_eq, sigma, ordering) =
	if !debug_jtunify then
		begin
			print_eq eq;
			print_endline "r7";
		end;
	match eq with
		atomnames, v::nfs, ft, c1::c2t ->
		   let ft_c1 = ft @ [c1] in
		   let compose_vars,new_sigma = compose sigma (v,ft_c1) in
		   let new_rest_eq = apply_subst rest_eq v ft_c1 atomnames in
		   let new_ordering =
				build_ordering calculus (v::compose_vars) ft_c1 ordering atom_set
			in
		   false, (atomnames, nfs, [], c2t), (new_rest_eq, new_sigma, new_ordering)
	 | _ ->
	 		raise (Invalid_argument "Jtunify.apply_r7 applied to an incorrect equation")

let apply_r8 atom_set calculus (atomnames, fs, ft, rt) state =
	if !debug_jtunify then
		begin
			print_eq (atomnames, fs, ft, rt);
			print_endline "r8";
		end;
	match fs with
		nft::nrt ->
		   false, (atomnames, rt, [nft], nrt), state
	 | _ ->
	 		raise (Invalid_argument "Jtunify.apply_r8 applied to an incorrect equation")

let apply_r9 atom_set calculus eq (rest_eq, sigma, ordering) =
	if !debug_jtunify then
		begin
			print_eq eq;
			print_endline "r9";
		end;
	match eq with
		atomnames, v::nft, ft, (rt1::_ as rt) ->
			let sort = sort_of v rt1 in
		   let (max,subst) = sigma in
		   let v_new = (NewVar sort,max) in
		   let ft_vnew = ft @ [v_new] in
		   let compose_vars,new_sigma = compose ((max+1),subst) (v,ft_vnew) in
		   let new_rest_eq = apply_subst rest_eq v ft_vnew atomnames in
		   let new_ordering =
				build_ordering calculus (v::compose_vars) ft_vnew ordering atom_set
			in
		   false, (atomnames, rt, [v_new], nft), (new_rest_eq, new_sigma, new_ordering)
	 | _ ->
	 		raise (Invalid_argument "Jtunify.apply_r9 applied to an incorrect equation")

let apply_r10 atom_set calculus eq state =
	if !debug_jtunify then
		begin
			print_eq eq;
			print_endline "r10";
		end;
	match eq with
		atomnames, fs, ft, x::nrt ->
		   false, (atomnames, fs, (ft @ [x]), nrt), state
	 | _ ->
	 		raise (Invalid_argument "Jtunify.apply_r10 applied to an incorrect equation")

let apply_r11 atom_set calculus eq (rest_eq, sigma, ordering) =
	if !debug_jtunify then
		begin
			print_eq eq;
			print_endline "r11";
		end;
	match eq with
		atomnames, v::nfs, ft, rt ->
		   let compose_vars,new_sigma = compose sigma (v,ft) in
		   let new_rest_eq = apply_subst rest_eq v ft atomnames in
		   let new_ordering = build_ordering calculus (v::compose_vars) ft ordering atom_set in
		   false, (atomnames, nfs, [], rt), (new_rest_eq, new_sigma, new_ordering)
	 | _ ->
	 		raise (Invalid_argument "Jtunify.apply_r11 applied to an incorrect equation")

let apply_r12b atom_set calculus (atomnames, fs, ft, rt) (rest_eq, sigma, ordering) =
	if !debug_jtunify then
		begin
			print_eq (atomnames, fs, ft, rt);
			print_endline "r12b";
		end;
	match fs with
	 | [v] ->
			let sort = sort_of v v in
		   let (max,subst) = sigma in
		   let v_new = (NewVar sort,max) in
		   let ft_vnew = [v_new] in
		   let compose_vars,new_sigma = compose ((max+1),subst) (v,ft_vnew) in
		   let new_rest_eq = apply_subst rest_eq v ft_vnew atomnames in
		   let new_ordering =
				build_ordering calculus (v::compose_vars) ft_vnew ordering atom_set
			in
		   false, (atomnames, rt, ft_vnew, []), (new_rest_eq, new_sigma, new_ordering)
	 | _ ->
	 		raise (Invalid_argument "Jtunify.apply_r12b applied to an incorrect equation")

let apply_r12 atom_set calculus (atomnames, fs, ft, rt) (rest_eq, sigma, ordering) =
	if !debug_jtunify then
		begin
			print_eq (atomnames, fs, ft, rt);
			print_endline "r12";
		end;
	match rt with
	 | v1::t ->
		   let compose_vars,new_sigma = compose sigma (v1,[]) in
		   let new_rest_eq = apply_subst rest_eq v1 [] atomnames in
		   let new_ordering = build_ordering calculus (v1::compose_vars) [] ordering atom_set in
		   false, (atomnames, fs, [], t), (new_rest_eq, new_sigma, new_ordering)
	 | _ ->
	 		raise (Invalid_argument "Jtunify.apply_r12 applied to an incorrect equation")

let t_rules = [|
   (r1,apply_r1,[]);
   (r2,apply_r2,[]);
   (r3,apply_r3,[]);
   (r4,apply_r4,[]);
   (r5,apply_r5,[]);
   (r6,apply_r6,[6;9]);
   (r7,apply_r7,[9]);
   (r8,apply_r8,[9]);
   (r9,apply_r9,[9]);
   (r10,apply_r10,[]);
   (r11,apply_r11,[]);
	(r12,apply_r12,[12]);
	(never,apply_r12b,[])
|]

let all_rules = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12]

let apply_subst_list atom_names (n,sigma) new_eql =
	List.fold_left
		(fun eql (v,l) ->	apply_subst eql v l atom_names)
		new_eql
		sigma

let rec tunify atom_set calculus = function
   (eq, state, new_eql, rule_index::rules_rest)::trace ->
		let _, left, _, right = eq in
		if List.length left > 0 && List.length right > 0 &&
			(
				let ll = Lm_list_util.last left in
				let lr = Lm_list_util.last right in
				is_const ll && is_const lr && not (position_eq ll lr)
			) then
				begin
					if !debug_jtunify then
						eprintf "incompatible tails@.";
					tunify atom_set calculus trace
				end
		else
		let condition, action, preferred_rules = t_rules.(rule_index) in
      if condition eq then
         try
				let (old_eql, sigma, ordering) = state in
				let atomnames, _, _, _ = eq in
				let full_eql =
					List.rev_append (apply_subst_list atomnames sigma new_eql) old_eql
				in
				let updated_state = full_eql, sigma, ordering in
            let stop, neq, nstate =
               action atom_set calculus eq updated_state
            in
				let ntrace = (eq, updated_state, [], preferred_rules)::trace in
               if stop then
                  Some(nstate, ntrace)
               else
                  tunify atom_set calculus ((neq, nstate, [], all_rules)::ntrace)
         with Not_unifiable ->
				(if !debug_jtunify then
					print_endline "failure";
            tunify atom_set calculus ((eq, state, new_eql, preferred_rules)::trace)
				)
      else
         tunify atom_set calculus ((eq, state, new_eql, rules_rest)::trace)
 | (_,_,_,[])::trace ->
		if !debug_jtunify then
			print_endline "failure";
      tunify atom_set calculus trace
 | [] ->
 		None

let rec tunify_list atom_set calculus = function
	[] -> None
 | trace::tracelist ->
 		let result = tunify atom_set calculus trace in
		match result with
			Some((f::rest_eq, sigma, ordering), trace) ->
	         let (atomnames,(fs,ft)) = f in
      	   tunify_list atom_set calculus
         	   ((((atomnames, fs, [], ft), (rest_eq, sigma, ordering), [], all_rules)::trace
					 )::tracelist
					)
		 | Some(([], nsigma, nordering), ntrace) ->
		 		Some(nsigma, nordering, (ntrace::tracelist))
		 | None ->
		 		tunify_list atom_set calculus tracelist

let rec add_new_equation_aux eql acc = function
	(eq, state, neql, rules)::trace ->
		add_new_equation_aux eql ((eq, state, List.rev_append eql neql, rules)::acc) trace
 | [] ->
 		acc

let rec add_new_equations eql acc = function
	trace::tracelist ->
		add_new_equations eql ((add_new_equation_aux eql [] trace)::acc) tracelist
 | [] ->
		acc

let do_stringunify calculus us ut ns nt equations fo_eqlist orderingQ atom_set tracelist counter =
	if !debug_s4prover then
		begin
	      print_endline "do_stringunify:";
			print_flush ()
		end;
   let short_us, short_ut = shorten us ut in (* apply intial rule R3 to eliminate common beginning *)
	let atomnames =
		match calculus with
			S4 -> []
		 | Intuit _ -> [ns;nt]
		 | Classical -> raise (Invalid_argument "No prefix unification for classical logic")
	in
   let new_element = atomnames, (short_us,short_ut) in
	let original_eqlist = add_fo_eqlist equations fo_eqlist in
   let full_eqlist = new_element::original_eqlist in
	if !debug_s4prover then
		print_equations full_eqlist;
	let tracelist' = add_new_equations (new_element::fo_eqlist) [] tracelist in
	match full_eqlist with
		(atomnames, (fs, rt))::rest_eq ->
	   	begin match
				tunify_list atom_set calculus
					([(atomnames, fs, [], rt), (rest_eq, (1,[]), orderingQ), [], all_rules]::tracelist'
					)
			with
				(* Q-case: max-1 new variables have been used for the domain equations *)
      		(* Q-case: sigmaQ will not be returned in eqlist *)
				Some(new_sigma, new_ordering, new_tracelist) ->
   	   		if !debug_s4prover then
      	   		begin
         	   		print_tunify new_sigma;
		         	   print_ordering_map new_ordering
		   	      end;
   		   	new_sigma, full_eqlist, new_ordering, new_tracelist
			 | None ->
					if !debug_jtunify then
						print_endline "failed";
   		   	raise (Failed(counter)) (* new connection please *)
			end
	 | [] ->
	 		raise (Invalid_argument "Jtunify.do_stringunify: Non-empty list expected")

end

module JTUnifyQ(L: JLogicSig) = JTUnify(L)(JQuantifier(L))
module JTUnifyProp(L: JLogicSig) = JTUnify(L)(JPropositionalQuantifier(L))

(* type of one unifier: int * (string * string) list  *)
