open Lm_debug
open Lm_printf
open Refiner.Refiner.TermType
open Refiner.Refiner.Term
open Refiner.Refiner.TermSubst

let debug_supinf_trace =
   create_debug (**)
      { debug_name = "supinf_trace";
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

   val print : out_channel -> bfield -> unit
end

module VarType =
struct
   type t=int
   let compare a b = a-b

   let print out v =
      if v>0 then fprintf out "v%i" v
      else if v=0 then fprintf out "1"
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

   let print out info =
      let count,table=info in
      let aux k d = fprintf out "%a ->v%i%t" print_term k d eflush in
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

   let print out (v:elt) (kl: data list) =
      match kl with
         [k] -> BField.print out k; (*printf"*";*) VarType.print out v
       | _ -> raise (Invalid_argument "More than one coefficient is associated with one variable")

   let append l1 l2 =
      match l1,l2 with
         [],[] -> [BField.fieldZero]
       | [],[a] -> [a]
       | [a],[] -> [a]
       | [a],[b] -> [BField.add a b]
       | _,_ -> raise (Invalid_argument "Addition non-trivial lists are not supported")

end

module type SourceSig =
sig
	type bfield
	type vars

	type source =
		Signore
	 | Shypothesis of int
	 | Sleft of source
	 | Sright of source
	 | Smin of source * source
	 | Smax of source * source
	 | StransitiveLeft of source * source * vars
	 | StransitiveRight of vars * source * source
	 | Sextract2left of vars * source
	 | Sextract2right of vars * source
	 | StrivialConst of bfield
	 | StrivialVar of vars
	 | Scontradiction of source
	 | Sscale of bfield * source
	 | SaddVar of bfield * vars * source
	 | Ssum of source * source

	exception IncompatibleSources of source * source * string

	val print : out_channel -> source -> unit

end

module MakeSource(BField : BoundFieldSig)
   : SourceSig with type bfield=BField.bfield and type vars=VarType.t =
struct
	type bfield = BField.bfield
	type vars = VarType.t

	type source =
		Signore
	 | Shypothesis of int
	 | Sleft of source
	 | Sright of source
	 | Smin of source * source
	 | Smax of source * source
	 | StransitiveLeft of source * source * vars
	 | StransitiveRight of vars * source * source
	 | Sextract2left of vars * source
	 | Sextract2right of vars * source
	 | StrivialConst of bfield
	 | StrivialVar of vars
	 | Scontradiction of source
	 | Sscale of bfield * source
	 | SaddVar of bfield * vars * source
	 | Ssum of source * source

	exception IncompatibleSources of source * source * string

	let rec print out = function
		Signore -> fprintf out "ignore"
	 | Shypothesis(h) -> fprintf out "hyp %i" h
	 | Sleft(s) -> fprintf out "left(%a)" print s
	 | Sright(s) -> fprintf out "right(%a)" print s
	 | Smin(s1,s2) -> fprintf out "min(%a,%a)" print s1 print s2
	 | Smax(s1,s2) -> fprintf out "max(%a,%a)" print s1 print s2
	 | StransitiveLeft(s1,s2,v) -> fprintf out "transL(%i,%a,%a)" v print s1 print s2
	 | StransitiveRight(v,s1,s2) -> fprintf out "transR(%i,%a,%a)" v print s1 print s2
	 | Sextract2left(v,s) -> fprintf out "extr2left(%i,%a)" v print s
	 | Sextract2right(v,s) -> fprintf out "extr2right(%i,%a)" v print s
	 | StrivialConst(c) -> fprintf out "trivConst(%a)" BField.print c
	 | StrivialVar(v) -> fprintf out "trivVar(%i)" v
	 | Scontradiction(s) -> fprintf out "contrad(%a)" print s
	 | Sscale(c,s) -> fprintf out "scale(%a,%a)" BField.print c print s
	 | SaddVar(c,v,s) -> fprintf out "addVar(%a,%i,%a)" BField.print c v print s
	 | Ssum(s1,s2) -> fprintf out "sum(%a,%a)" print s1 print s2

end

module Tree =
struct
	type 'a tree = Ignore | Leaf of 'a | Left of 'a tree | Right of 'a tree | Pair of ('a tree) * ('a tree)

	let rec string_of_tree = function
		Ignore -> "ignore"
	 | Leaf _ -> "leaf"
	 | Left subtree -> sprintf "left(%s)" (string_of_tree subtree)
	 | Right subtree -> sprintf "right(%s)" (string_of_tree subtree)
	 | Pair(left,right) -> sprintf "pair(%s,%s)" (string_of_tree left) (string_of_tree right)

	let rec treeMap f = function
		Ignore -> Ignore
	 | Leaf e -> Leaf(f e)
	 | Left subtree -> Left(treeMap f subtree)
	 | Right subtree -> Right(treeMap f subtree)
	 | Pair(left,right) -> Pair(treeMap f left, treeMap f right)

	let leftBranch = function
		Ignore -> Ignore
	 |	Leaf _ -> raise (Invalid_argument "leftBranch applied to a leaf")
	 | Left subtree -> subtree
	 | Right subtree -> Ignore
	 | Pair(left, right) -> left

	let rightBranch = function
		Ignore -> Ignore
	 | Leaf _ -> raise (Invalid_argument "rightBranch applied to a leaf")
	 | Left subtree -> Ignore
	 | Right subtree -> subtree
	 | Pair(left, right) -> right

	let rec treeProduct f tree1 tree2 =
		match tree1, tree2 with
			Ignore, _ -> Ignore
		 | _, Ignore -> Ignore
		 | Leaf e1, Leaf e2 ->
				Leaf(f e1 e2)
		 | Left(subtree), _ -> Left(treeProduct f subtree tree2)
		 | Right(subtree), _ -> Right(treeProduct f subtree tree2)
		 | Pair(a,Ignore), _ -> Pair(treeProduct f a tree2, Ignore)
		 | Pair(Ignore,b), _ -> Pair(Ignore, treeProduct f b tree2)
		 | Pair(a,b), _ -> Pair(treeProduct f a tree2, treeProduct f b tree2)
		 | _, Left(subtree) -> Left(treeProduct f tree1 subtree)
		 | _, Right(subtree) -> Right(treeProduct f tree1 subtree)
		 | _, Pair(a,Ignore) -> Left(treeProduct f tree1 a)
		 | _, Pair(Ignore,b) -> Right(treeProduct f tree1 b)
		 | _, Pair(a,b) -> Pair(treeProduct f tree1 a, treeProduct f tree1 b)

	let rec treeMergeLeft f tree1 tree2 =
		match tree1, tree2 with
			Ignore, _ -> Ignore
		 | _, Ignore -> Ignore
		 | Leaf e1, Leaf e2 -> Leaf(f e1 e2)
		 | _, Left sub2 -> treeMergeLeft f tree1 sub2
		 | _, Right sub2 -> treeMergeLeft f tree1 sub2
		 | Left sub1,Pair(left,right) -> (treeMergeLeft f sub1 left)
		 | Right sub1,Pair(left,right) -> (treeMergeLeft f sub1 right)
		 | Pair(left1,right1),Pair(Ignore,right2) -> treeMergeLeft f right1 right2
		 | Pair(left1,right1),Pair(left2,Ignore) -> treeMergeLeft f left1 left2
		 | Pair(Ignore,right1),Pair(left2,right2) -> treeMergeLeft f right1 right2
		 | Pair(left1,Ignore),Pair(left2,right2) -> treeMergeLeft f left1 left2
		 | Pair(left1,Ignore),Leaf _ -> treeMergeLeft f left1 tree2
		 | Pair(Ignore,right1),Leaf _ -> treeMergeLeft f right1 tree2
		 | Pair(left1,right1),Pair(left2,right2) -> Pair(treeMergeLeft f left1 left2, treeMergeLeft f right1 right2)
		 | Pair(left1,right1),Leaf _ -> Pair(treeMergeLeft f left1 tree2, treeMergeLeft f right1 tree2)
		 | _, _ ->
				let s = sprintf "Incompatible trees %s %s in treeMergeLeft@." (string_of_tree tree1) (string_of_tree tree2) in
				raise (Invalid_argument s)

	let rec treeMergeRight f tree1 tree2 =
		match tree1, tree2 with
			Ignore, _ -> Ignore
		 | _, Ignore -> Ignore
		 | Leaf e1, Leaf e2 -> Leaf(f e1 e2)
		 | Left sub1, _ -> treeMergeRight f sub1 tree2
		 | Right sub1, _ -> treeMergeRight f sub1 tree2
		 | Pair(left,right),Left sub2 -> (*Left*)(treeMergeRight f left sub2)
		 | Pair(left,right),Right sub2 -> (*Right*)(treeMergeRight f right sub2)
		 | Pair(left1,right1),Pair(Ignore,right2) -> treeMergeRight f right1 right2
		 | Pair(left1,right1),Pair(left2,Ignore) -> treeMergeRight f left1 left2
		 | Pair(Ignore,right1),Pair(left2,right2) -> treeMergeRight f right1 right2
		 | Pair(left1,Ignore),Pair(left2,right2) -> treeMergeRight f left1 left2
		 | Leaf _,Pair(left2,Ignore) -> treeMergeRight f tree1 left2
		 | Leaf _,Pair(Ignore,right2) -> treeMergeLeft f tree1 right2
		 | Pair(left1,right1),Pair(left2,right2) -> Pair(treeMergeRight f left1 left2, treeMergeRight f right1 right2)
		 | Leaf _,Pair(left2,right2) -> Pair(treeMergeRight f tree1 left2, treeMergeRight f tree1 right2)
		 | _, _ ->
				let s = sprintf "Incompatible trees %s %s in treeMergeRight@." (string_of_tree tree1) (string_of_tree tree2) in
				raise (Invalid_argument s)

	let rec treeFlatten = function
		Ignore -> []
	 | Leaf l -> [l]
	 | Left subtree -> treeFlatten subtree
	 | Right subtree -> treeFlatten subtree
	 | Pair(left, right) -> (treeFlatten left)@(treeFlatten right)
end

module type AF_Sig =
sig
   type bfield
   type vars=int
   type af
   type source

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
   val isMinusInfinity: af -> bool
   val isPlusInfinity: af -> bool

   val minusInfinity : af
   val plusInfinity : af

   val term_of : (term array) -> af -> term

	val setSource : source -> af -> af
	val getSource : af -> source
	val extract2leftSource : vars -> af -> af
	val extract2rightSource : vars -> af -> af
(*	val trivialConstSource : bfield -> af
	val trivialVarSource : vars -> af
*)
	val contrSource : af -> af -> af
	val hypSource : int -> af -> af
(*	val scaleSource : bfield -> af -> af *)
	val addVarSource : bfield -> vars -> af -> af
	val sumSource : af -> af -> af

   val print : out_channel -> af -> unit
   val print_var : out_channel -> vars -> unit
end

module MakeAF(BField : BoundFieldSig)
   (Source: SourceSig with
      type bfield=BField.bfield and
      type vars=VarType.t)
   : AF_Sig with
	type bfield=BField.bfield and
	type vars=VarType.t and
	type source=Source.source =
struct
   module Monom=MakeMonom(BField)
   module Table=Lm_splay_table.MakeTable(Monom)
   module VI=Var2Index(BField)

   type bfield=BField.bfield
   type vars=Monom.elt
	type source = Source.source
	open Source

   type af=source * Table.t

	let constvar = 0

   let print_var = VarType.print

   let print out (s,f) =
      let aux key data =
         fprintf out "+"; Monom.print out key [data]
      in
      fprintf out "("; Table.iter aux f; fprintf out ")%t" eflush

   let minusInfinity = (Signore, Table.add Table.empty constvar BField.minusInfinity)
   let plusInfinity = (Signore, Table.add Table.empty constvar BField.plusInfinity)

   let mk_number k =
		if BField.isInfinite k then
			if BField.compare BField.minusInfinity k =0 then
				minusInfinity
			else
				plusInfinity
		else
			StrivialConst(k), Table.add Table.empty constvar k

   let mk_var v = StrivialVar(v), Table.add Table.empty v BField.fieldUnit

   let scale_aux k v d =
      BField.mul k d

   let scale k (s,f) =
      if BField.compare k BField.fieldZero =0 then StrivialConst(BField.fieldZero),Table.empty
      else if BField.compare k BField.fieldUnit =0 then s,f
      else Sscale(k,s),Table.map (scale_aux k) f

   let coef (s,f) v =
      try Table.find f v
      with Not_found -> BField.fieldZero

   let isInfinite f =
      BField.isInfinite (coef f constvar)

   let isMinusInfinity f =
      BField.compare BField.minusInfinity (coef f constvar) =0

   let isPlusInfinity f =
      BField.compare BField.plusInfinity (coef f constvar) =0

   let add f1 f2 =
		if (isInfinite f1) or (isInfinite f2) then
			if isInfinite f1 then
				if isInfinite f2 then
					if (isMinusInfinity f1) && (isMinusInfinity f2) then
						f1
					else
						if (isPlusInfinity f1) && (isPlusInfinity f2) then
							f1
						else
							raise (Invalid_argument "MinusInfinity+PlusInfinity is undefined")
				else
					f1
			else
				f2
		else
			let (s1,f1)=f1 in
			let (s2,f2)=f2 in
			let f = Table.union f1 f2 in
			if s1==s2 then
				s1,f
			else
				match s1,s2 with
					Signore, s -> s,f
				 | s,Signore -> s,f
				 | _ -> (Ssum(s1,s2),f)

   let remove (s,f) vs = s,Table.remove f vs

   let rec split (s,f) =
      if !debug_supinf_trace then
			begin
				eprintf "split"; Table.print stderr f;
				eprintf "@.split %a@.%t" print (s,f) eflush;
			end;
      let (v,coefs,rest)=Table.deletemax f in
      match coefs with
         [c] ->
            if !debug_supinf_trace then
               (Monom.print stderr v coefs; eprintf " %a@." print (s,rest));
				if v!=constvar && (BField.compare c BField.fieldZero =0) then
					split (s,rest)
				else
					(c,v,(s,rest))
       | _ -> raise (Invalid_argument "More than one coefficient associated with a variable")

   let isNumber (s,f) =
      let test=ref true in
      let aux v c =
         if v<>constvar && compare c BField.fieldZero <>0 then
            test:=false
      in
      Table.iter aux f;
      !test

   let term_of_monom info k v =
      if v=constvar then
         BField.term_of k
      else
         BField.mul_term (BField.term_of k) (VI.restore info v)

   let rec term_of_aux info = function
      [] -> BField.term_of BField.fieldZero
    | [(v,k)] -> term_of_monom info k v
    | (v,k)::tl -> BField.add_term (term_of_monom info k v) (term_of_aux info tl)

   let rec term_of info (s,f) =
      let l=Table.list_of f in
      let aux = function
         (k,[d]) -> (k,d)
       | (k,[]) -> raise (Invalid_argument "MakeAF.term_of - empty data list linked to a key in list_of")
       | (k,_) -> raise (Invalid_argument "MakeAF.term_of - more than one data item per key in list_of")
      in
      let aux2 (k,d) = if BField.compare d BField.fieldZero = 0 then false else true in
      term_of_aux info (List.filter aux2 (List.map aux l))

	let setSource s (s',f) = (s,f)

	let getSource (s,f) = s

	let extract2leftSource v f = setSource (Sextract2left(v,getSource f)) f

	let extract2rightSource v f = setSource (Sextract2right(v,getSource f)) f

	let trivialConstSource c = setSource (StrivialConst c) (mk_number c)

	let trivialVarSource v = setSource (StrivialVar v) (mk_var v)

	let contrSource src f = setSource (Scontradiction (getSource src)) f

	let hypSource h f = setSource (Shypothesis h) f

	let scaleSource coef f = setSource (Sscale(coef, getSource f)) f

	let addVarSource coef v f = setSource (SaddVar(coef, v, getSource f)) f

	let sumSource f1 f2 = setSource (Ssum(getSource f1, getSource f2)) f2

end

module type SAF_Sig =
sig
   type bfield
   type vars
	type source

   type af

   type saf' = Affine of af | Max of saf*saf | Min of saf*saf
	and saf = source * saf'

   type 'a step =
      Assert of string * saf * saf * 'a
    |   Transitive of string * saf * saf * saf
    | Tactic of string * 'a

   val affine: af -> saf
   val min: saf -> saf -> saf
   val max: saf -> saf -> saf

   val scale: bfield -> saf -> saf
   val add: saf -> saf -> saf

   val occurs: vars -> saf -> bool
   val isInfinite: saf -> bool
   val isMinusInfinity: saf -> bool
   val isPlusInfinity: saf -> bool
   val isAffine: saf -> bool

   val term_of: (term array) -> saf -> term

	val getSource : saf -> source
	val setSource : source -> saf -> saf
	val transitiveLeftSource : saf -> saf -> vars -> saf
	val transitiveRightSource : vars -> saf -> saf -> saf
	val addVarSource : bfield -> vars -> saf -> saf
(*
	val scaleSource : bfield -> saf -> saf
	val sumSource : saf -> saf -> saf
*)
   val print : out_channel -> saf -> unit
end

module MakeSAF(BField : BoundFieldSig)
	   (Source: SourceSig with
   	   type bfield=BField.bfield and
      	type vars=VarType.t)
	(AF : AF_Sig with type bfield=BField.bfield and
							type source=Source.source)
: SAF_Sig with
	type bfield=BField.bfield and
	type vars=AF.vars and
	type af=AF.af and
	type source=Source.source =
struct
   open BField

   type bfield=BField.bfield
   type vars=AF.vars
	type source = Source.source
   type af=AF.af
   open Source

   type saf' = Affine of af | Max of saf*saf | Min of saf*saf
	and saf = source * saf'

   type 'a step =
      Assert of string * saf * saf * 'a
    | Transitive of string * saf * saf * saf
    | Tactic of string * 'a

	let getSource (s,f') = s
	let setSource s (_,f) = (s,f)

   let affine af = (AF.getSource af), Affine af

	let min_aff_simple f1 s f =
      match f1 with
         s1,Affine f1' ->
            if AF.isNumber f1' then
               let c=AF.coef f1' AF.constvar in
                  if BField.compare c BField.plusInfinity = 0 then
							Signore, s, Affine f
                  else if BField.compare c BField.minusInfinity =0 then
							s1, Signore, (Affine f1')
                  else
							s1, s, Min (f1, (s, Affine f))
            else
               s1, s, Min (f1, (s, Affine f))
       | s1,_ -> s1, s, Min (f1, (s, Affine f))

   let min_aff f1 s f =
   	let s1,f1'=f1 in
      if AF.isNumber f then
         let c=AF.coef f AF.constvar in
            if BField.compare c BField.plusInfinity = 0 then
					s1, Signore, f1'
            else if BField.compare c BField.minusInfinity =0 then
					Signore, s, Affine f
            else
					min_aff_simple f1 s f
      else
         min_aff_simple f1 s f

   let min f1 f2 =
      match f1,f2 with
         (_,_), (s, Affine f) ->
				let s1,s2,minf=min_aff f1 s f in
					Smin(s1,s2), minf
       | (s, Affine f), (_,_) ->
				let s2,s1,minf=min_aff f2 s f in
					Smin(s1,s2), minf
       | (s1, Min(f11,f12)), (s2, Min(f21,f22)) ->
				Smin(s1,s2), Min (f1,f2)
       | _,_ -> raise (Invalid_argument "SAF.min: detected a mixture of min and max")

   let max_aff_simple f1 s f =
      match f1 with
         s1, Affine f1' ->
            if AF.isNumber f1' then
               let c=AF.coef f1' AF.constvar in
                  if BField.compare c BField.plusInfinity = 0 then
							s1, Signore, (Affine f1')
                  else if BField.compare c BField.minusInfinity =0 then
							Signore, s, Affine f
                  else
							s1, s, Max (f1, (s,Affine f))
            else
               s1, s, Max (f1, (s,Affine f))
       | s1, _ -> s1, s, Max (f1, (s,Affine f))

   let max_aff f1 s f =
   	let s1,f1'=f1 in
      if AF.isNumber f then
         let c=AF.coef f AF.constvar in
            if BField.compare c BField.plusInfinity = 0 then
					Signore, s, Affine f
            else if BField.compare c BField.minusInfinity =0 then
					getSource f1, Signore, f1'
            else
					max_aff_simple f1 s f
      else
         max_aff_simple f1 s f

   let max f1 f2 =
      match f1,f2 with
         (_,_), (s, Affine f) ->
				let s1,s2,maxf=max_aff f1 s f in
				Smin(s1,s2), maxf
       | (s, Affine f), _ ->
				let s2,s1,maxf=max_aff f2 s f in
				Smin(s1,s2), maxf
       | (s1,Max(f11,f12)),(s2,Max(f21,f22)) ->
				Smax(s1,s2), Max (f1,f2)
       | _,_ -> raise (Invalid_argument "SAF.min: detected a mixture of min and max")


   let rec scale k f =
      match f with
         s, Affine f' -> Sscale(k,s), Affine (AF.scale k f')
       | s, Min (a,b) ->
            let cmp=compare k fieldZero in
               if cmp<0 then
						Sscale(k,s), Max (scale k a, scale k b)
               else if cmp=0 then
						StrivialConst(fieldZero), Affine(AF.mk_number(fieldZero))
               else
						Sscale(k,s), Min (scale k a, scale k b)
       | s, Max (a,b) ->
            let cmp=compare k fieldZero in
               if cmp<0 then
						Sscale(k,s), Min (scale k a, scale k b)
               else if cmp=0 then
						StrivialConst(fieldZero), Affine(AF.mk_number(fieldZero))
               else
						Sscale(k,s), Max (scale k a, scale k b)

   let rec add f1 f2 =
      match f1,f2 with
         (s1,Affine f1'), (s2,Affine f2') ->
         	let saf'=Affine (AF.add f1' f2') in
				(match s1,s2 with
					Signore, s -> s,saf'
				 | s,Signore -> s,saf'
				 | _ -> Ssum(s1,s2),saf'
	 			)
       | (s1, Min(a,b)), (s2,_) -> Ssum(s1,s2), Min(add a f2, add b f2)
       | (s1,_), (s2, Min(a,b)) -> Ssum(s1,s2), Min(add f1 a, add f1 b)
       | (s1, Max(a,b)), (s2,_) -> Ssum(s1,s2), Max(add a f2, add b f2)
       | (s1,_), (s2, Max(a,b)) -> Ssum(s1,s2), Max(add f1 a, add f1 b)

   let rec occurs v (_,f) =
      match f with
         Affine f' -> (compare (AF.coef f' v) fieldZero <>0)
       | Min (a,b) -> (occurs v a) || (occurs v b)
       | Max (a,b) -> (occurs v a) || (occurs v b)

   let isInfinite = function
      _,Affine f ->
         AF.isInfinite f
    | _ -> false

   let isMinusInfinity = function
      _,Affine f ->
         AF.isMinusInfinity f
    | _ -> false

   let isPlusInfinity = function
      _,Affine f ->
         AF.isPlusInfinity f
    | _ -> false

   let isAffine = function
      _,Affine _ -> true
    | _ -> false

	let transitiveLeftSource (s,f) (s0,f0) v =
		StransitiveLeft(s,s0,v), f

	let transitiveRightSource v (s0,f0) (s,f) =
		StransitiveRight(v,s0,s), f

	let addVarSource coef v (s,f) = SaddVar(coef,v,s),f

(*
	let rec scaleSource coef = function
		Affine f -> Affine (AF.scaleSource coef f)
	 | Min (f1,f2) -> Min (scaleSource coef f1, scaleSource coef f2)
	 | Max (f1,f2) -> Max (scaleSource coef f1, scaleSource coef f2)

	let rec sumSource_aux f0 = function
		Affine f -> Affine (AF.sumSource f f0)
	 | Min (f1,f2) -> Min (sumSource_aux f0 f1, sumSource_aux f0 f2)
	 | Max (f1,f2) -> Max (sumSource_aux f0 f1, sumSource_aux f0 f2)

	let rec sumSource f = function
		Affine f0 -> sumSource_aux f0 f
	 | Min (f1,f2) -> Min (sumSource f f1, sumSource f f2)
	 | Max (f1,f2) -> Max (sumSource f f1, sumSource f f2)
*)
	let rec print out (_,f) =
      match f with
         Affine f' -> AF.print out  f'
       | Max (a,b) ->
            fprintf out "max(%a; %a)" print a print b
       | Min (a,b) ->
            fprintf out "min(%a; %a)" print a print b

   let rec term_of info = function
      _,Affine f -> AF.term_of info f
    | _,Max (a,b) -> BField.max_term (term_of info a) (term_of info b)
    | _,Min (a,b) -> BField.min_term (term_of info a) (term_of info b)
end
