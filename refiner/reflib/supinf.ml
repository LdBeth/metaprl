open Printf
open Lm_debug
open Refiner.Refiner.Term
open Refiner.Refiner.TermSubst

let debug_supinf_trace =
   create_debug (**)
      { debug_name = "debug_supinf_trace";
        debug_description = "Print out (low-level) trace of supinf execution";
        debug_value = false
      }

module type BoundFieldSig =
sig
	type bfield

	val fieldUnit : bfield
	val fieldZero : bfield
	val plusInfinity : bfield
	val minusInfinity : bfield
	val mul : bfield -> bfield -> bfield
	val add : bfield -> bfield -> bfield
	val neg : bfield -> bfield
	val sub : bfield -> bfield -> bfield
	val inv : bfield -> bfield
	val div : bfield -> bfield -> bfield
	val compare : bfield -> bfield -> int
	val isInfinite : bfield -> bool

	val term_of : bfield -> term
	val mul_term : term -> term -> term
	val add_term : term -> term -> term
	val neg_term : term -> term
	val sub_term : term -> term -> term
	val inv_term : term -> term
	val div_term : term -> term -> term
	val ge_term : term -> term -> term
	val max_term : term -> term -> term
	val min_term : term -> term -> term

	val print : bfield -> unit
end

module VarType =
struct
	type t=int
   let compare a b = a-b

	let print v =
		if v>0 then printf "v%i" v
		else if v=0 then printf "1"
		else raise (Invalid_argument "Variable index should be non-negative")
end

module Var =
struct
	type t = term
	let equal = alpha_equal
	let hash = Hashtbl.hash
end

module Var2Index(BField : BoundFieldSig) =
struct
	module Table=Hashtbl.Make(Var)

	type t=int ref * int Table.t

	let create n = (ref 0, Table.create n)

	let lookup (info:t) v =
		let (count,table)=info in
		if Table.mem table v then
			Table.find table v
		else
			let index=(!count)+1 in
			begin
				Table.add table v index;
				count:=index;
				index
			end

	let print info =
		let count,table=info in
		let aux k d = printf "%a ->v%i%t" print_term k d eflush in
		(*printf "count=%i%t" !count eflush;*)
		Table.iter aux table

	let invert ((count,table) : t) =
		let ar=Array.make !count (BField.term_of BField.fieldZero) in
		let aux key data = (ar.(data-1)<-key) in
		Table.iter aux table;
		ar

	let restore inverted index =
		if index=0 then
			BField.term_of (BField.fieldUnit)
		else
			inverted.(index-1)
end

module MakeMonom(BField : BoundFieldSig) =
struct
	type elt = VarType.t
	type data = BField.bfield

	let compare = VarType.compare

	let print (v:elt) (kl: data list) =
		match kl with
			[k] -> BField.print k; (*printf"*";*) VarType.print v
		 | _ -> raise (Invalid_argument "More than one coefficient is associated with one variable")

	let append l1 l2 =
		match l1,l2 with
			[],[] -> [BField.fieldZero]
		 | [],[a] -> [a]
		 | [a],[] -> [a]
		 | [a],[b] -> [BField.add a b]
		 | _,_ -> raise (Invalid_argument "Addition non-trivial lists are not supported")

end

module type AF_Sig =
sig
	type vars=int
	type af
	type bfield

	val constvar : vars

   val mk_number: bfield -> af
   val mk_var: vars -> af
   val scale: bfield -> af -> af
   val add: af -> af -> af

   val coef: af -> vars -> bfield
   val remove: af -> vars -> af
   val split: af -> (bfield * vars * af)
   val isNumber: af -> bool
	val isInfinite: af -> bool

	val minusInfinity : af
	val plusInfinity : af

	val term_of : (term array) -> af -> term

	val print : af -> unit
	val print_var : vars -> unit
end

module MakeAF(BField : BoundFieldSig)
	: AF_Sig with type bfield=BField.bfield and type vars=VarType.t =
struct
	module Monom=MakeMonom(BField)
	module Table=Lm_splay_table.MakeTable(Monom)
	module VI=Var2Index(BField)

	type bfield=BField.bfield
	type vars=Monom.elt
	type af=Table.t

	let constvar = 0

	let print_var = VarType.print

	let print f =
		let aux key data =
			printf "+"; Monom.print key [data]
		in
		(*printf "(";*) Table.iter aux f; (*printf")";*) flush stdout

	let mk_number k = Table.add Table.empty constvar k
   let mk_var v = Table.add Table.empty v BField.fieldUnit

	let scale_aux k v d =
		BField.mul k d

   let scale k f =
		if BField.compare k BField.fieldZero =0 then Table.empty
		else if BField.compare k BField.fieldUnit =0 then f
		else Table.map (scale_aux k) f

   let add f1 f2 = Table.union f1 f2

   let coef f v =
		try Table.find f v
		with Not_found -> BField.fieldZero

   let remove = Table.remove

   let split f =
		if !debug_supinf_trace then
			(printf "split "; print f; eflush stdout);
		let (v,coefs,rest)=Table.deletemax f in
		match coefs with
			[c] ->
				if !debug_supinf_trace then
					(Monom.print v coefs; printf" "; print rest; eflush stdout);
				(c,v,rest)
		 | _ -> raise (Invalid_argument "More than one coefficient associated with a variable")

   let isNumber f =
		let test=ref true in
		let aux v c =
			if v<>constvar && compare c BField.fieldZero <>0 then
				test:=false
		in
		Table.iter aux f;
		!test

	let minusInfinity = Table.add Table.empty constvar BField.minusInfinity
	let plusInfinity = Table.add Table.empty constvar BField.plusInfinity

	let isInfinite f =
		BField.isInfinite (coef f constvar)

	let term_of_monom info k v =
		if v=constvar then
			BField.term_of k
		else
			BField.mul_term (BField.term_of k) (VI.restore info v)

	let rec term_of_aux info = function
		[] -> BField.term_of BField.fieldZero
	 | [(v,k)] -> term_of_monom info k v
	 | (v,k)::tl -> BField.add_term (term_of_monom info k v) (term_of_aux info tl)

	let rec term_of info f =
		let l=Table.list_of f in
		let aux = function
			(k,[d]) -> (k,d)
		 | (k,[]) -> raise (Invalid_argument "MakeAF.term_of - empty data list linked to a key in list_of")
		 | (k,_) -> raise (Invalid_argument "MakeAF.term_of - more than one data item per key in list_of")
		in
		let aux2 (k,d) = if BField.compare d BField.fieldZero = 0 then false else true in
		term_of_aux info (List.filter aux2 (List.map aux l))

end

module type SAF_Sig =
sig
	type bfield
	type vars
	type af
	type saf = Affine of af | Max of saf*saf | Min of saf*saf
	type 'a step =
		Assert of string * saf * saf * 'a
	 |	Transitive of string * saf * saf * saf
	 | Tactic of string * 'a

	val affine: af -> saf
   val min: saf -> saf -> saf
   val max: saf -> saf -> saf

   val scale: bfield -> saf -> saf
   val add: saf -> saf -> saf

   val occurs: vars -> saf -> bool
	val isInfinite: saf -> bool
	val isAffine: saf -> bool

	val term_of: (term array) -> saf -> term

	val print : saf -> unit
end

module MakeSAF(BField : BoundFieldSig)(AF : AF_Sig  with type bfield=BField.bfield)
	: SAF_Sig with type bfield=BField.bfield and type vars=AF.vars and type af=AF.af =
struct
	open BField

	type vars=AF.vars
	type af=AF.af
	type bfield=BField.bfield

	type saf = Affine of af | Max of saf*saf | Min of saf*saf
	type 'a step =
		Assert of string * saf * saf * 'a
	 |	Transitive of string * saf * saf * saf
	 | Tactic of string * 'a

	let affine af = Affine af

	let min_aff_simple f1 f =
		match f1 with
			Affine f1' ->
				if AF.isNumber f1' then
					let c=AF.coef f1' AF.constvar in
					if BField.compare c BField.plusInfinity = 0 then Affine f
					else if BField.compare c BField.minusInfinity =0 then f1
					else Min (f1, Affine f)
				else
					Min (f1, Affine f)
		 |	_ -> Min (f1, Affine f)

	let min_aff f1 f =
		if AF.isNumber f then
			let c=AF.coef f AF.constvar in
			if BField.compare c BField.plusInfinity = 0 then f1
			else if BField.compare c BField.minusInfinity =0 then Affine f
			else min_aff_simple f1 f
		else
			min_aff_simple f1 f

   let min f1 f2 =
		match f1,f2 with
			_, Affine f -> min_aff f1 f
		 | Affine f, _ -> min_aff f2 f
		 | Min (f11,f12), Min (f21,f22) -> Min (f1,f2)
		 | _,_ -> raise (Invalid_argument "SAF.min: detected a mixture of min and max")

	let max_aff_simple f1 f =
		match f1 with
			Affine f1' ->
				if AF.isNumber f1' then
					let c=AF.coef f1' AF.constvar in
					if BField.compare c BField.plusInfinity = 0 then f1
					else if BField.compare c BField.minusInfinity =0 then Affine f
					else Max (f1, Affine f)
				else
					Max (f1, Affine f)
		 |	_ -> Max (f1, Affine f)

	let max_aff f1 f =
		if AF.isNumber f then
			let c=AF.coef f AF.constvar in
			if BField.compare c BField.plusInfinity = 0 then Affine f
			else if BField.compare c BField.minusInfinity =0 then f1
			else max_aff_simple f1 f
		else
			max_aff_simple f1 f

   let max f1 f2 =
		match f1,f2 with
			_, Affine f -> max_aff f1 f
		 | Affine f, _ -> max_aff f2 f
		 | Max(f11,f12),Max(f21,f22) -> Max (f1,f2)
		 | _,_ -> raise (Invalid_argument "SAF.min: detected a mixture of min and max")


   let rec scale k f =
		match f with
			Affine f' -> Affine (AF.scale k f')
		 | Min (a,b) ->
				let cmp=compare k fieldZero in
				if cmp<0 then Max (scale k a, scale k b)
				else if cmp=0 then Affine(AF.mk_number(fieldZero))
				else Min (scale k a, scale k b)
		 | Max (a,b) ->
				let cmp=compare k fieldZero in
				if cmp<0 then Min (scale k a, scale k b)
				else if cmp=0 then Affine(AF.mk_number(fieldZero))
				else Max (scale k a, scale k b)

   let rec add f1 f2 =
		match f1,f2 with
			Affine f1', Affine f2' -> Affine (AF.add f1' f2')
		 | Min (a,b), _ -> Min (add a f2, add b f2)
		 | _, Min (a,b) -> Min (add f1 a, add f1 b)
		 | Max (a,b), _ -> Max (add a f2, add b f2)
		 | _, Max (a,b) -> Max (add f1 a, add f1 b)

   let rec occurs v f =
		match f with
			Affine f' -> (compare (AF.coef f' v) fieldZero <>0)
		 | Min (a,b) -> (occurs v a) || (occurs v b)
		 | Max (a,b) -> (occurs v a) || (occurs v b)

	let isInfinite = function
		Affine f ->
			AF.isInfinite f
	 | _ -> false

	let isAffine = function
		Affine _ -> true
	 | _ -> false

	let rec print f =
		match f with
			Affine f' -> AF.print f'
		 | Max (a,b) ->
				printf "max("; print a; printf"; "; print b; printf ")"
		 | Min (a,b) ->
				printf "min("; print a; printf"; "; print b; printf ")"

	let rec term_of info = function
		Affine f -> AF.term_of info f
	 | Max (a,b) -> BField.max_term (term_of info a) (term_of info b)
	 | Min (a,b) -> BField.min_term (term_of info a) (term_of info b)
end
