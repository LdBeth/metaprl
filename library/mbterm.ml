
(*conversion between nuprl-light terms and mathbus terms*)


open BigInt
open MathBus
open Term
open Opname


let mbs_Term = numeric_label2 "Term"
let mbs_Variable = numeric_label2 "Variable"
let mbs_Token = numeric_label2 "Token"
let mbs_Bindings = numeric_label2 "Bindings"
let mbs_Level = numeric_label2 "Level"
let mbs_ObjectId = numeric_label2 "ObjectId"
let mbs_ParmList = numeric_label2 "ParmList"
let mbs_TermIndex = numeric_label2 "TermIndex"
let mbs_MString = numeric_label2 "MString"
let mbs_MLongInteger = numeric_label2 "MLongInteger"
let mbs_MVariable = numeric_label2 "Variable"
let mbs_MLevel = numeric_label2 "MLevel"
let mbs_MToken = numeric_label2 "MToken"


(* new term representation*)

let param_of_opname opname =
  let rec loop l nodes =
    match l with
      [] ->  nodes
    | h::t -> loop t ((make_param (String h))::nodes)
  in make_param (ParmList (loop (dest_opname opname) []))
	
(* nuprl-light -> mathbus*)

let rec mbparameter_of_param param =
  match (dest_param param) with
    Number (Num.Int p) -> mb_integer p
  | String p -> mb_string p
  | Token p -> mb_stringq p mbs_Token
  | Level p ->
      let aux = function
	  {le_const = i; le_vars = vars } ->
	    (let rec loop l nodes=
	      (match l with
		[] -> mbnode mbs_Level ((mb_integer i)::nodes)
	      | hd::tl -> let aux2 = function
		    { le_var = v; le_offset = i2 } ->
		      loop tl ((mb_string v)::((mb_integer i2)::nodes))
	      in aux2 (dest_level_var hd))
	    in loop vars [])
      in aux (dest_level p)
  | Var p -> mb_stringq p mbs_Variable
  | ObId p -> mbnode mbs_ObjectId (List.map mbparameter_of_param (dest_object_id p))
	 (*mb_integerq (dest_object_id p) mbs_ObjectId*)
  | MNumber p -> mb_stringq p mbs_MLongInteger
  | MString p -> mb_stringq p mbs_MString
  | MToken p -> mb_stringq p mbs_MToken
  | MLevel p -> mb_stringq p mbs_MLevel
  | MVar p -> mb_stringq p mbs_MVariable
  | ParmList p -> mbnode mbs_ParmList (List.map mbparameter_of_param p)
  | _ -> failwith "unauthorized parameter type"

let rec print_param param =
  match (dest_param param) with
    Number (Num.Int p) -> (print_int p  ; print_string " ")
  | String p ->   (print_string p ; print_string " ")
  | Token p -> (print_string p ; print_string " ")
  | Level p ->  print_string "level" (* let aux = function
					{le_const = i; le_vars = vars } ->
					(let rec loop l nodes=
					(match l with
					[] -> mbnode mbs_Level  ((mb_integer i)::nodes)
					| hd::tl ->  let aux2 = function
					{ le_var = v; le_offset = i2 } ->
 					loop tl  ((mb_string v)::((mb_integer i2)::nodes))
					in aux2 (dest_level_var hd)) in loop vars []) in aux  (dest_level p)*)
  | Var p -> (print_string p ; print_string " ")
  | ObId p -> (print_string "["; List.map print_param (dest_object_id p); print_string "]"; print_string ":obid"; ())
	 (*print_int (dest_object_id p)*)
  | MNumber p -> print_string p
  | MString p -> print_string p
  | MToken p -> print_string p
  | MLevel p -> print_string p
  | MVar p -> print_string p
  | ParmList p -> (print_string "parm_list:" ;print_string "["; List.map print_param p;print_string "]"; ())
  | _ -> failwith "unauthorized parameter type"

let mbbinding_of_binding binding = mb_stringq binding mbs_Variable (*term in the future? yes, far*)

let append l p =
  let rec aux l1 l2 =
    match l1 with
      h1::t1 -> aux t1 (h1::l2)
    | [] -> l2
  in
  aux (List.rev l) p

let mbbindings_of_bvars bvars =
  let rec loop l1 l2 =
    match l1 with
      "nuprl5_implementation"::(h2::t1) -> (match t1 with
      | h3::t2 -> (match t2 with
	  h4::x -> loop x ((mbnode mbs_ParmList (List.map mb_string [h2; h3; h4]))::l2)
 	| [] -> ((mbnode mbs_ParmList (List.map mb_string [h2; h3]))::l2))
      | [] -> ((mb_stringq h2 mbs_MVariable)::l2))
    | h1::t1 -> loop t1 ((mb_stringq h1 mbs_Variable)::l2)
    | [] -> l2
  in
  loop bvars []
      
let rec mbterm_of_term term =
  let { term_op = operator; term_terms = bterms} = dest_term term
  in
  let { op_name = opname; op_params = params } = dest_op operator
  in
  let temp =  if (dest_opname opname) = ["!nuprl5_implementation!"] then params
  else ((param_of_opname opname)::params) 
  in
  let mbparams = List.map mbparameter_of_param temp
  and mbsubterms_of_bterm = function
      { bvars = bvars; bterm = t } ->
	match bvars with
	  []-> [(mbterm_of_term t)]
	| h::tl -> [(mbnode mbs_Bindings (mbbindings_of_bvars bvars));
			    (* (List.map mbbinding_of_binding bvars)*)
		     (mbterm_of_term t)]
  in
  let rec loop l blist =
    (match blist with
      []-> l
    | h::t -> loop (append (mbsubterms_of_bterm (dest_bterm h)) l) t)
  in
  mbnode mbs_Term (append mbparams (loop [] bterms)) (*LAL*)
    
let rec print_term term =
  let { term_op = operator; term_terms = bterms} = dest_term term in
  let { op_name = opname; op_params = params } = dest_op operator in
  let print_subterms = function
      { bvars = bvars; bterm = t } -> begin
	print_newline ();
	print_string "    ";
	if bvars <> [] then print_string "bindings:";
	List.map print_string bvars;
	print_term t
      end in
  List.map print_string (dest_opname opname);
  print_string "{"; List.map print_param params; print_string "}";
  print_string "("; List.map print_subterms (List.map dest_bterm bterms);
  print_string ")" ;
  ()
		 

(* mathbus -> nuprl-light*)

let rec param_of_mbparameter mbparameter =
  let b = (mbnode_label mbparameter) in
  if bequal b mbs_String then make_param (String (string_value mbparameter)) 
  else if bequal b mbs_Variable then make_param (Var (string_value mbparameter))
  else if bequal b mbs_Token then make_param (Token (string_value mbparameter))
  else if bequal b mbs_LongInteger then 
    let b = integer_value mbparameter in make_param (Number (Num.Int b))
  else if bequal b mbs_ParmList then
    let rec loop i l =
      if i = 0 then l
      else match (mbnode_subtermq mbparameter i) with
	Mnode n -> loop (i-1) ((param_of_mbparameter n)::l)
      |	Mbint b -> failwith "subterm should be a node" 
    in make_param (ParmList (loop (mbnode_nSubtermsq mbparameter) []))

  else if bequal b mbs_ObjectId then
    let rec loop i l =
      if i = 0 then l
      else match (mbnode_subtermq mbparameter i) with
	Mnode n -> loop (i-1) ((param_of_mbparameter n)::l)
      |	Mbint b -> failwith "subterm should be a node" 
	in make_param (ObId (make_object_id (loop (mbnode_nSubtermsq mbparameter) [])))

  else if bequal b mbs_Level then
    let nsubterms = (mbnode_nSubtermsq mbparameter) in
    match (mbnode_subtermq mbparameter 1) with
      Mnode n -> let constant = integer_value n and
	  le_vars = let rec loop i l =
	    if i <= 1 then l
	    else (match (mbnode_subtermq mbparameter i) with
	      Mnode n -> let s = (string_value n) in
	      (match (mbnode_subtermq mbparameter (i-1)) with
	      	Mnode n2-> loop (i-2) ((mk_level_var s (integer_value n2))::l)
	      |	Mbint b2 -> failwith "subterm should be a node")
	    | Mbint b -> failwith "subterm should be a node")
	  in loop nsubterms []
      in make_param (Level (mk_level constant le_vars))
    | Mbint b -> failwith "subterm should be a node"
	  

  else if bequal b mbs_MString then make_param (MString (string_value mbparameter))
  else if bequal b mbs_MVariable then make_param (MVar (string_value mbparameter))
  else if bequal b mbs_MToken then make_param (MToken (string_value mbparameter))
  else if bequal b mbs_MLongInteger then make_param (MNumber (string_value mbparameter))
  else if bequal b mbs_MLevel then make_param (MLevel (string_value mbparameter))
  else failwith "mbparameter_of_parameter" 
 (*        
let param_of_mbparameter mbparameter =
   match (mbnode_label mbparameter) with
	  i when i = mbs_String -> make_param (String (string_value mbparameter)) 
	 | (mbs_Variable) -> make_param (Var (string_value mbparameter))
         | (mbs_Token) -> make_param (Token (string_value mbparameter))
	 | (mbs_LongInteger) -> let (a, b) = integer_value mbparameter in make_param (Number b)
	 | (mbs_Level) -> 
	 let nsubterms = (mbnode_nSubtermsq mbparameter) in
		  (match (mbnode_subtermq mbparameter 1) with
	 Mnode n->let constant= integer_value n) and
		le_vars =
	let rec loop i l =
		 if i  <= 1 then l
		else (match (mbnode_subtermq mbparameter i) with
	 Mnode n->let s = (string_value n) in
	 (match (mbnode_subtermq mbparameter (i-1)) with
	 Mnode n2-> loop (i-2) ((mk_level_var s (integer_value n2))::l)))
	    in  loop nsubterms [] in
	make_param (Level (mk_level constant le_vars)))
	| (mbs_MString) -> make_param (MString (string_value mbparameter))
	| (mbs_MVariable) -> make_param (MVar (string_value mbparameter))
	| (mbs_MToken) -> make_param (MToken (string_value mbparameter))
	| (mbs_MLongInteger) -> make_param (MNumber (string_value mbparameter))
	| (mbs_MLevel) -> make_param (MLevel (string_value mbparameter))
	| _ -> failwith "mbparameter_to_parameter" 
	*) 


let opname_of_param p =
  match (dest_param p) with
    ParmList p ->
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
      ParmList p ->
	mk_op (opname_of_param h) t
    | _ -> mk_op (mk_opname "!nuprl5_implementation!" nil_opname) (h::t))

let bvars_of_mbbindings mbterm =
  let b = (mbnode_label mbterm)
  in if not (bequal b mbs_Bindings) then failwith "bindings label" else
  let rec loop index bvars =
    if index = 0 then bvars
    else match mbterm.(index) with
      Mnode n -> let b2 = (mbnode_label n) in
      if (bequal b2 mbs_Variable) then loop (index - 1) ((string_value n)::bvars)
      else if (bequal b2 mbs_MVariable) then
      	loop (index - 1) ("nuprl5_implementation"::((string_value n)::bvars))
      else
 	let s1 = (match n.(1) with
	  Mnode n1 -> (string_value n1)
	| Mbint b -> failwith "bvars_of_mbindings") and s2 =
	  (match n.(2) with
	    Mnode n2 -> (string_value n2)
	  | Mbint b -> failwith "bvars_of_mbindings") in
	if 2 = (mbnode_nSubtermsq n) then
	  loop (index - 1) ("nuprl5_implementation"::(s1::(s2::bvars)))
	else let s3 = (match n.(3) with
	  Mnode n3 -> (string_value n3)
	| Mbint b -> failwith "bvars_of_mbindings") in
	loop (index - 1) ("nuprl5_implementation"::(s1::(s2::(s3::bvars))))
	  
    | Mbint b -> failwith "bvars_of_mbindings"
  in loop (mbnode_nSubtermsq mbterm) []

let bvar_of_binding binding = binding

let bterms_of_sb subterms bindings =
  let f bindings = List.map bvar_of_binding bindings in
  List.map2  mk_bterm (List.map f bindings) subterms


 (*
let rec term_of_mbterm mbterm =
	let b = (mbnode_label mbterm) in
	if not (bequal b mbs_Term) then failwith "term of mbterm label"
	else let nsubterms = (mbnode_nSubtermsq mbterm) in
	 let rec loop index leaves  =
		match mbterm.(index) with
		Mnode node -> if (index = nsubterms) or
			(bequal (mbnode_label node) mbs_Term) or
			(bequal (mbnode_label node) mbs_Bindings) or
			(bequal (mbnode_label node) mbs_TermIndex) then
		let rec loop1 i s b p =
			if (i = nsubterms) then
			let op = (op_of_params (List.rev ((param_of_mbparameter node)::leaves))) and bterms = (bterms_of_sb s b) in
			mk_term op bterms
			else loop1 (i + 1) (if (bequal (mbnode_label node) mbs_Term) then ((term_of_mbterm node)::s) else s) (if (bequal (mbnode_label node) mbs_Bindings) then ((bvars_of_mbindings node)::b) else (if p then ([]::b) else b)) (bequal (mbnode_label node) mbs_Term) in
		loop1 index [] [] true
	else loop (index + 1) ((param_of_mbparameter node)::leaves) in
	 loop 1 []
 *)
	 
let rec term_of_mbterm mbterm =
  let b = (mbnode_label mbterm) in
  if not (bequal b mbs_Term) then failwith "term of mbterm label"
  else let nsubterms = (mbnode_nSubtermsq mbterm) in
  let rec loop index leaves  =
    match mbterm.(index) with
      Mnode node -> if
	(bequal (mbnode_label node) mbs_Term) or
	(bequal (mbnode_label node) mbs_Bindings) or
	(bequal (mbnode_label node) mbs_TermIndex) then
	let rec loop1 i b =
	  (match mbterm.(i) with
	    Mnode n ->
	      if (i = nsubterms) then
	      	let bterms =
		  (if (bequal (mbnode_label n) mbs_Term) then
		    ((mk_bterm [] (term_of_mbterm n))::b)
		  else raise (Invalid_argument "last subterm should be a term")) in
	      	mk_term (op_of_params (List.rev leaves)) bterms

 	      else (if (bequal (mbnode_label n) mbs_Term) then
		loop1 (i + 1) ((mk_bterm [] (term_of_mbterm n))::b)
	      else (if (bequal (mbnode_label n) mbs_Bindings) then
		(if (i + 1) = nsubterms then
		  (match mbterm.(i+ 1) with
		    Mnode n2 ->  let bterms = ((mk_bterm (bvars_of_mbbindings n) 
						  (term_of_mbterm n2))::b) in
		    mk_term (op_of_params (List.rev leaves)) bterms
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
	mk_term op []
      else loop (index + 1) ((param_of_mbparameter node)::leaves)
    | Mbint b -> raise (Invalid_argument " subterm should be a node") in
  loop 1 []
	 
 (*LAL TODO: conditionalize on whether or not nuprl 5 implementation term-not needed*)
 (*LAL ok, done on nuprl side*)
	

(*old term representation*)
(*nuprl_light -> mathbus*)
(*

let bterm_of_mbboundterm mbboundterm =
	 mk_bterm (bvars_of_mbboundterm mbboundterm) (term_of_mbterm  (mbnode_subtermq mbboundterm 1)
)
 ;;

let bvars_of_mbboundterm mbboundterm =
	let bvars (*do loop*)  List.cons (string_value (mbnode_subtermq mbboundterm i)) ;;

*)






