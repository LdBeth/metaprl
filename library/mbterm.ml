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

(*conversion between nuprl-light terms and mathbus terms*)

open Lm_debug
open Lm_symbol

open Lint32
open Lm_num
open MathBus
open Opname
open Term_sig
open Refiner.Refiner.Term
open Refiner.Refiner.TermType
open Registry

let _ =
   show_loading "Loading Mbterm%t"

let use_table = ref true

exception InvalidMathBusLabel of (int32 * int32)

let mbs_Term = ref (create  0X0000);;
let mbs_Variable = ref (create  0X0000);;
let mbs_Token = ref (create  0X0000);;
let mbs_Bindings = ref (create  0X0000);;
let mbs_Level = ref (create  0X0000);;
let mbs_ObjectId = ref (create  0X0000);;
let mbs_ParamList = ref (create  0X0000);;
let mbs_TermIndex = ref (create  0X0000);;
let mbs_TokenIndex = ref (create  0X0000);;
let mbs_StringIndex = ref (create  0X0000);;
let mbs_MString = ref (create  0X0000);;
let mbs_MLongInteger = ref (create  0X0000);;
let mbs_MLevel = ref (create  0X0000);;
let mbs_MToken = ref (create  0X0000);;

let assign_mbs_terms () =
  mbs_Term := numeric_label "Term";
  mbs_Variable := numeric_label "Variable";
  mbs_Token := numeric_label "Token";
  mbs_Bindings := numeric_label "Bindings";
  mbs_Level := numeric_label "Level";
  mbs_ObjectId := numeric_label "ObjectId";
  mbs_ParamList := numeric_label "ParmList";
  mbs_TermIndex := numeric_label "TermIndex";
  mbs_TokenIndex := numeric_label "TokenIndex";
  mbs_StringIndex := numeric_label "StringIndex";
  mbs_MString := numeric_label "MString";
  mbs_MLongInteger := numeric_label "MLongInteger";
  mbs_MLevel := numeric_label "MLevel";
  mbs_MToken := numeric_label "MToken";
  ()
;;

(* nuprl-light -> mathbus*)

let param_of_opname opname =
   let rec loop l nodes =
    match l with
      [] -> nodes
    | h::t -> loop t ((make_param (String h))::nodes)
  in make_param (ParamList (loop (dest_opname opname) []))

let rec mbparameter_of_param param =
  match (dest_param param) with
    Number p -> mb_number p
  | String p -> if !use_table then (let v = (try Hashtbl.find token_table p with Not_found -> 0) in if v=0 then (mb_string p) else mb_integerq v !mbs_StringIndex) else mb_string p
  | Token p ->
       let p = string_of_opname p in
          if !use_table then (let v = (try Hashtbl.find token_table p with Not_found -> 0) in if v=0 then (mb_stringq p !mbs_Token) else mb_integerq v !mbs_TokenIndex) else mb_stringq p !mbs_Token
  | Var p -> mb_stringq (string_of_symbol p) !mbs_Variable
  | ObId p -> mbnode !mbs_ObjectId (List.map mbparameter_of_param (dest_object_id p))
  | MNumber p -> mb_stringq (string_of_symbol p) !mbs_MLongInteger
  | MString p -> mb_stringq (string_of_symbol p) !mbs_MString
  | MToken p -> mb_stringq (string_of_symbol p) !mbs_MToken
  | MLevel p ->
      let aux = function
	  {le_const = i; le_vars = vars } ->
	    (let rec loop l nodes=
	      (match l with
		[] -> mbnode !mbs_Level ((mb_integer i)::nodes)
	      | hd::tl -> let aux2 = function
		    { le_var = v; le_offset = i2 } ->
		      loop tl ((mb_string (string_of_symbol v))::((mb_integer i2)::nodes))
	      in aux2 (dest_level_var hd))
	    in loop vars [])
      in aux (dest_level p)
  | ParamList p -> mbnode !mbs_ParamList (List.map mbparameter_of_param p)
  | Shape _ | MShape _ | Quote ->
       raise(Invalid_argument "Mbterm.mbparameter_of_param: quote and shape parameters not supported")

let mbbinding_of_binding binding = mb_stringq binding !mbs_Variable (* term in the future?*)

let append l p =
  let rec aux l1 l2 =
      match l1 with
      h1::t1 -> aux t1 (h1::l2)
    | [] -> l2 in
  aux (List.rev l) p

let mbbindings_of_bvars bvars =
  let rec loop l1 l2 =
    match l1 with
      "nuprl5_implementation1"::(h1::t1) -> loop t1 ((mbbinding_of_binding h1)::l2)
    | "nuprl5_implementation2"::(h1::(h2::t1)) ->
	loop t1 ((mbnode !mbs_ParamList (List.map mbbinding_of_binding [h1; h2]))::l2)
    |  "nuprl5_implementation3"::(h1::(h2::(h3::t1))) ->
	loop t1 ((mbnode !mbs_ParamList (List.map mbbinding_of_binding [h1; h2; h3]))::l2)
    |  h1::t1 -> loop t1 ((mbbinding_of_binding h1)::l2)
    | [] -> List.rev l2
  in loop bvars []

let rec mbterm_of_term term =
  let { term_op = operator; term_terms = bterms} = Lib_term.dest_term term in
  let { op_name = opname; op_params = params } = dest_op operator in
  let temp =
    if (dest_opname opname) = ["!nuprl5_implementation!"] then params
    else ((param_of_opname opname)::params) in
  let mbparams = List.map mbparameter_of_param temp
  and mbsubterms_of_bterm = function
      { bvars = bvars; bterm = t } ->
       	match bvars with
	  []-> [(mbterm_of_term t)]
       	| h::tl -> [(mbnode !mbs_Bindings (mbbindings_of_bvars (List.map string_of_symbol bvars)));
			    (* (List.map mbbinding_of_binding bvars)*)
		     (mbterm_of_term t)] in
  let rec loop l blist =
    (match blist with
      []-> l
    | h::t -> loop (append (mbsubterms_of_bterm (dest_bterm h)) l) t) in
  mbnode !mbs_Term (append mbparams (loop [] bterms)) (*LAL*)

(* mathbus -> nuprl-light*)

let rec param_of_mbparameter mbparameter =
  let b = (mbnode_label mbparameter) in
  if bequal b !mbs_String then make_param (String (string_value mbparameter))
  else if bequal b !mbs_Variable then make_param (Var (Lm_symbol.add (string_value mbparameter)))
  else if bequal b !mbs_Token then make_param (Token (mk_opname (string_value mbparameter) nil_opname))
  else if bequal b !mbs_TokenIndex then make_param (Token (mk_opname (Hashtbl.find index_table (integer_value mbparameter)) nil_opname))
  else if bequal b !mbs_StringIndex then make_param (String (Hashtbl.find index_table (integer_value mbparameter)))
  else if bequal b !mbs_TermIndex then make_param (String (Hashtbl.find index_table (integer_value mbparameter)))
  else if bequal b !mbs_LongInteger then
    let n = number_value mbparameter in make_param (Number n)
  else if bequal b !mbs_ParamList then
    let rec loop i l =
      if i = 0 then l
      else match (mbnode_subtermq mbparameter i) with
	Mnode n -> loop (i-1) ((param_of_mbparameter n)::l)
      |	Mbint b -> failwith "subterm should be a node"
    in make_param (ParamList (loop (mbnode_nSubtermsq mbparameter) []))

  else if bequal b !mbs_ObjectId then
    let rec loop i l =
      if i = 0 then l
      else match (mbnode_subtermq mbparameter i) with
	Mnode n -> loop (i-1) ((param_of_mbparameter n)::l)
      |	Mbint b -> failwith "subterm should be a node"
    in make_param (ObId (make_object_id (loop (mbnode_nSubtermsq mbparameter) [])))

  else if bequal b !mbs_Level or bequal b !mbs_MLevel then
    let nsubterms = mbnode_nSubtermsq mbparameter in
    match mbnode_subtermq mbparameter 1 with
      Mnode n1 -> let constant = integer_value n1 and
	    le_vars = let rec loop i l =
	      if i <= 1 then l
	      else (match mbnode_subtermq mbparameter i with
	      	Mnode n -> let x = integer_value n in
	      	(match mbnode_subtermq mbparameter (i-1) with
	      	  Mnode n2-> loop (i-2) ((mk_level_var (Lm_symbol.add (string_value n2)) x)::l)
	      	| Mbint b -> failwith "subterm should be a node")
	      | Mbint b -> failwith "subterm should be a node")

	    in loop nsubterms []

      	in make_param (MLevel (mk_level constant le_vars))
      | Mbint b -> failwith "subterm should be a node"

  else if bequal b !mbs_MString then make_param (MString (Lm_symbol.add (string_value mbparameter)))
  else if bequal b !mbs_MToken then make_param (MToken (Lm_symbol.add (string_value mbparameter)))
  else if bequal b !mbs_MLongInteger then make_param (MNumber (Lm_symbol.add (string_value mbparameter)))
  else let ((x, y) as fg) = dest_lint32 b in failwith "param_of_mbparameter1"(* ["mbparameter_of_parameter1"; "not"] [] [(inatural_term x); (inatural_term y)]*)



let opname_of_param p =
  match (dest_param p) with
    ParamList p ->
      let rec loop l opname =
	(match l with
	  [] -> opname
	| h::t -> (match (dest_param h) with
	    String s -> loop t (mk_opname s opname)
	  | _ -> raise (Invalid_argument "opname_of_param"))) in
      loop p nil_opname
  | _ -> raise (Invalid_argument "opname_of_param")

let op_of_params params =
  match params with
    [] -> mk_op (mk_opname "!nuprl5_implementation!" nil_opname) []
  | h::t -> (match (dest_param h) with
      ParamList p ->
	mk_op (opname_of_param h) t
    | _ -> mk_op (mk_opname "!nuprl5_implementation!" nil_opname) (h::t))

let bvars_of_mbbindings mbterm =
  let b = (mbnode_label mbterm)
  in if not (bequal b !mbs_Bindings) then failwith "bindings label" else
  let rec loop index bvars =
    if index = 0 then bvars
    else match mbterm.(index) with
      Mnode n -> let b2 = (mbnode_label n) in
      if (bequal b2 !mbs_Variable) then loop (index - 1) ((string_value n)::bvars)
      else (*parmlist node*)
 	let s1 = (match n.(1) with
	  Mnode n1 -> (string_value n1)
	| Mbint b -> failwith "bvars_of_mbindings") and s2 =
	  (match n.(2) with
	    Mnode n2 -> (string_value n2)
	  | Mbint b -> failwith "bvars_of_mbindings") in
	if (mbnode_nSubtermsq n) = 2 then
	  loop (index - 1) ("nuprl5_implementation2"::(s1::(s2::bvars)))
	else let s3 = (match n.(3) with
	  Mnode n3 -> (string_value n3)
	| Mbint b -> failwith "bvars_of_mbindings") in
	loop (index - 1) ("nuprl5_implementation3"::(s1::(s2::(s3::bvars))))

    | Mbint b -> failwith "bvars_of_mbindings"
  in List.map Lm_symbol.add (loop (mbnode_nSubtermsq mbterm) [])

let bvar_of_binding binding = binding

let bterms_of_sb subterms bindings =
  let f bindings = List.map bvar_of_binding bindings in
  List.map2  mk_bterm (List.map f bindings) subterms

let rec term_of_mbterm mbterm =
  let b = mbnode_label mbterm in
  if not (bequal b !mbs_Term) then failwith "term of mbterm label"
  else let nsubterms = mbnode_nSubtermsq mbterm in
  let rec loop index leaves  =
    match mbterm.(index) with
      Mnode node -> if
	(bequal (mbnode_label node) !mbs_Term) or
	(bequal (mbnode_label node) !mbs_Bindings) or
	(bequal (mbnode_label node) !mbs_TermIndex) then
	let rec loop1 i b =
	  (match mbterm.(i) with
	    Mnode n ->
	      if (i = nsubterms) then
	      	let bterms =
		  (if (bequal (mbnode_label n) !mbs_Term) then
		    ((mk_bterm [] (term_of_mbterm n))::b)
		  else raise (Invalid_argument "last subterm should be a term")) in
	      	Lib_term.mk_term (op_of_params (List.rev leaves)) bterms

 	      else (if (bequal (mbnode_label n) !mbs_Term) then
		loop1 (i + 1) ((mk_bterm [] (term_of_mbterm n))::b)
	      else (if (bequal (mbnode_label n) !mbs_Bindings) then
		(if (i + 1) = nsubterms then
		  (match mbterm.(i+ 1) with
		    Mnode n2 ->  let bterms = ((mk_bterm (bvars_of_mbbindings n)
						  (term_of_mbterm n2))::b) in
		    Lib_term.mk_term (op_of_params (List.rev leaves)) bterms
		  | Mbint b -> raise (Invalid_argument " subterm should be a node"))

	 	else
		  (match mbterm.(i+ 1) with
		    Mnode n2 -> loop1 (i + 2) ((mk_bterm (bvars_of_mbbindings n)
						  (term_of_mbterm n2))::b)
		  | Mbint b -> raise (Invalid_argument " subterm should be a node")))
	      else raise (Invalid_argument " subterm should be a binding")))
	  | Mbint b -> raise (Invalid_argument " subterm should be a node")) in
	loop1 index []

      else if (index = nsubterms) then
	let op = (op_of_params (List.rev ((param_of_mbparameter node)::leaves))) in
	Lib_term.mk_term op []
      else loop (index + 1) ((param_of_mbparameter node)::leaves)
    | Mbint b -> raise (Invalid_argument " subterm should be a node") in
  loop 1 []


(* printing functions *)

let rec print_param param =
   match (dest_param param) with
      Number p -> (print_string (string_of_num p)  ; print_string ":n ")
    | String p -> (print_string p ; print_string ":s ")
    | Token p -> (print_string (string_of_opname p) ; print_string ":t ")
    | Var p -> (print_string (string_of_symbol p) ; print_string ":v ")
    | ObId p -> (print_string "["; List.iter print_param (dest_object_id p); print_string "]";
                 print_string ":oid ")
    | MNumber p -> (print_string (string_of_symbol p); print_string ":mn ")
    | MString p -> (print_string (string_of_symbol p); print_string ":ms ")
    | MToken p -> (print_string (string_of_symbol p); print_string ":mt ")
    | MLevel p ->
         let rec loop l =
            (match l with
                [] -> print_string "]}"
              | hd::tl -> let
                             { le_var = v; le_offset = i2 } = (dest_level_var hd) in
                             print_string "(";
                             print_string (string_of_symbol v); print_string ", ";print_int i2;
                             print_string ")";
                             loop tl)
         in let aux = function
            {le_const = i; le_vars = vars } ->
               (print_string "{"; print_int i ;print_string " [";
                loop vars)

         in aux (dest_level p)
    | ParamList p -> (print_string "["; List.iter print_param p; print_string "]";
                      print_string ":pl ")
    | Quote | Shape _ | MShape _ ->
         raise(Invalid_argument "Mbterm.print_param: quote and shape parameters not supported")

let rec print_term term =
  print_string "print_term";
  let { term_op = operator; term_terms = bterms} = Lib_term.dest_term term in
  let { op_name = opname; op_params = params } = dest_op operator in
  let print_subterms = function
      { bvars = bvars; bterm = t } -> begin
	print_newline ();
	print_string "    ";
	if bvars <> [] then print_string "bindings:";
	List.iter (fun v -> print_string (string_of_symbol v)) bvars;
	print_term t
      end in
  List.iter print_string (dest_opname opname);
  print_string "{"; List.iter print_param params; print_string "}";
  print_string "("; List.iter print_subterms (List.map dest_bterm bterms);
  print_string ")" ;
  ()


(*LAL TODO: conditionalize on whether or not nuprl 5 implementation term-not needed*)
(*LAL ok, done on nuprl side*)


let write_node_to_file node filename =
  let out_channel = open_out filename in
  write_node node out_channel;
  close_out out_channel;
  ()

let read_node_from_file filename =
  let in_channel = open_in filename in
  let node = read_node in_channel in
  close_in in_channel;
  node
