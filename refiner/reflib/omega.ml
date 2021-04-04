open Lm_printf
open Refiner.Refiner.TermType
open Refiner.Refiner.Term

module type RingSig =
sig
   type ring

   val ringUnit : ring
   val ringZero : ring
   val abs : ring -> ring
   val mul : ring -> ring -> ring
   val div : ring -> ring -> ring
   val rem : ring -> ring -> ring
   val add : ring -> ring -> ring
   val neg : ring -> ring
   val sub : ring -> ring -> ring
   val compare : ring -> ring -> int
   val equal : ring -> ring -> bool
   val isNegative : ring -> bool
   val gcd : ring -> ring -> ring

   val term_of : ring -> term
   val mul_term : term -> term -> term
   val add_term : term -> term -> term
   val neg_term : term -> term
   val sub_term : term -> term -> term
   val ge_term : term -> term -> term

   val print : out_channel -> ring -> unit
end

module VarType =
struct
   type t=int
   let compare = Int.compare

   let print out v =
      if v>0 then fprintf out "v%i" v
      else if v=0 then fprintf out "1"
      else raise (Invalid_argument "Variable index should be non-negative")
end

module type Index_Sig =
sig
   type t
   val create : int -> t
   val length : t -> int
   val lookup : t -> term -> int
   val print : out_channel -> t -> unit
   val invert : t -> term array
   val restore : term array -> int -> term
end

module type AF_Sig =
sig
   module VI : Index_Sig

   type ring
   type vars=int
   type af

   val constvar : vars

   val dim : af -> int
   val mk_number: int -> ring -> af
   val mk_var: int -> vars -> af
   val grow: int -> af -> af
   val scale: ring -> af -> af
   val div: af -> ring -> af
   val add: af -> af -> af
   val sub: af -> af -> af
   val add_scaled: af -> ring -> af -> af
   val sub_scaled: af -> ring -> af -> af
   val sub_number : af -> ring -> af

   val coef: af -> vars -> ring
   val get: af -> vars -> ring
   val remove: af -> vars -> af
   val split: af -> (ring * vars * af)
   val any_var : af -> vars
   val isNumber: af -> bool
   val gcd: af -> ring

   val value_of : af -> ring
   val term_of : (term array) -> af -> term

   val print : out_channel -> af -> unit
   val print_var : out_channel -> vars -> unit
end

module Var2Index(Var : Hashtbl.HashedType with type t = term)(Ring : RingSig) =
struct
   module Table=Hashtbl.Make(Var)

   type t=int Table.t
   type key=term

   let create = Table.create

   let length = Table.length

   let lookup (info:t) v =
      match Table.find_opt info v with
         Some a -> a
       | None -> let a = length info + 1 in
                    Table.add info v a;
                    a

   let print out info =
      let aux k d = fprintf out "%a ->v%i%t" print_term k d eflush in
      (*printf "count=%i%t" !count eflush;*)
      Table.iter aux info

   let invert (table : t) =
      let ar=Array.make (length table) (Ring.term_of Ring.ringZero) in
      let aux key data = (ar.(pred data) <- key) in
      Table.iter aux table;
      ar

   let restore inverted index =
      if index=0 then
         Ring.term_of (Ring.ringUnit)
      else
         inverted.(pred index)
end

module MakeMonom(Ring : RingSig) =
struct
   type elt = VarType.t
   type data = Ring.ring

   let compare = VarType.compare

   let print out (v:elt) (kl: data list) =
      match kl with
         [k] -> Ring.print out k; (*printf"*";*) VarType.print out v
       | _ -> raise (Invalid_argument "More than one coefficient is associated with one variable")

   let append l1 l2 =
      match l1,l2 with
         [],[] -> [Ring.ringZero]
       | [],[a] -> [a]
       | [a],[] -> [a]
       | [a],[b] -> [Ring.add a b]
       | _,_ -> raise (Invalid_argument "Addition non-trivial lists are not supported")

end

module MakeAF(Var : Hashtbl.HashedType with type t = term)(Ring : RingSig)
   : AF_Sig with
   type ring=Ring.ring and
   type vars=VarType.t =
struct
   module Monom=MakeMonom(Ring)
   module Table=Lm_splay_table.MakeTable(Monom)
   module VI=Var2Index(Var)(Ring)

   type ring=Ring.ring
   type vars=Monom.elt

   type af=Table.t

   let constvar = 0

   let print_var = VarType.print

   let print out f =
      let aux key data =
         fprintf out "+"; Monom.print out key [data]
      in
      fprintf out "("; Table.iter aux f; fprintf out ")%t" flush

   let mk_number _ k =
      Table.add Table.empty constvar k

   let mk_var _ v = Table.add Table.empty v Ring.ringUnit

   let grow _ f = f

   let dim f = pred (Table.length f)

   let scale_aux k v d =
      Ring.mul k d

   let scale k f =
      if Ring.equal k Ring.ringZero then Table.empty
      else if Ring.equal k Ring.ringUnit then f
      else Table.map (scale_aux k) f

   let coef f v =
      try Table.find f v
      with Not_found -> Ring.ringZero

   let get f v = coef f v

   let add_aux v k f =
      let k' = coef f v in
      Table.replace f v [Ring.add k' k]

   let add f1 f2 =
      Table.fold_map add_aux f1 f2

   let sub_aux v k f =
      let k' = coef f v in
      Table.replace f v [Ring.sub k' k]

   let sub f1 f2 =
      Table.fold_map sub_aux f1 f2
(*
      let neg_f2 = scale (Ring.neg Ring.ringUnit) f2 in
      add f1 neg_f2
*)

   let add_scaled_aux c v k f =
      let k' = coef f v in
      Table.replace f v [Ring.add k' (Ring.mul c k)]

   let add_scaled f1 c f2 =
      Table.fold_map (add_scaled_aux c) f1 f2

   let sub_scaled_aux c v k f =
      let k' = coef f v in
      Table.replace f v [Ring.sub k' (Ring.mul c k)]

   let sub_scaled f1 c f2 =
      Table.fold_map (sub_scaled_aux c) f1 f2

   let sub_number f k =
      let k' = Table.find f constvar in
      Table.replace f constvar [Ring.sub k' k]
(*    let k' = Table.find f constvar in
      let f' = Table.remove f constvar in
      Table.add f' constvar (Ring.sub k' k)
*)

   let gcd f =
      let r = ref Ring.ringZero in
      let aux v k =
         if v=constvar then
            ()
         else
            r:=Ring.gcd !r k
      in
      Table.iter aux f;
      !r

   let div f k = Table.map (fun v c -> Ring.div c k) f

   let remove f vs = Table.remove f vs

   let rec split f =
      if Table.is_empty f then
         (Ring.ringZero, constvar, mk_number 0 Ring.ringZero)
      else
         let v, coefs, rest = Table.deletemax f in
         match coefs with
            [c] ->
               if v!=constvar && (Ring.equal c Ring.ringZero) then
                  split rest
               else
                  (c,v,rest)
          | _ -> raise (Invalid_argument "More than one coefficient associated with a variable")

   let any_var f =
      let c,v,_ = split f in
      v

   let isNumber f =
      let test=ref true in
      let aux v c =
         if v<>constvar && Ring.compare c Ring.ringZero <>0 then
            test:=false
      in
      Table.iter aux f;
      !test

   let value_of f =
      if isNumber f then
         coef f constvar
      else
         begin
            eprintf "AF.value_of: applied to a non-constant form %a" print f;
            raise (Invalid_argument "AF.value_of: applied to a non-constant form")
         end

   let term_of_monom info k v =
      if v=constvar then
         Ring.term_of k
      else
         Ring.mul_term (Ring.term_of k) (VI.restore info v)

   let rec term_of_aux info = function
      [] -> Ring.term_of Ring.ringZero
    | [(v,k)] -> term_of_monom info k v
    | (v,k)::tl -> Ring.add_term (term_of_monom info k v) (term_of_aux info tl)

   let term_of info f =
      let l=Table.list_of f in
      let aux = function
         (k,[d]) -> (k,d)
       | (k,[]) -> raise (Invalid_argument "MakeAF.term_of - empty data list linked to a key in list_of")
       | (k,_) -> raise (Invalid_argument "MakeAF.term_of - more than one data item per key in list_of")
      in
      let aux2 (k,d) = not (Ring.equal d Ring.ringZero) in
      term_of_aux info (List.filter aux2 (List.map aux l))

end

(* unused
module MakeArrayAF(Ring : RingSig)
   : AF_Sig with
   type ring=Ring.ring and
   type vars=VarType.t =
struct
   module Monom=MakeMonom(Ring)
   module VI=Var2Index(Ring)

   type ring=Ring.ring
   type vars=Monom.elt

   type af=ring array

   let constvar = 0

   let print_var = VarType.print

   let print out f =
      let aux key data =
         fprintf out "+"; Monom.print out key [data]
      in
      fprintf out "("; Array.iteri aux f; fprintf out ")%t" flush

   let dim f = pred (Array.length f)

   let mk_number n k =
      Array.init (succ n) (fun i -> if i=constvar then k else Ring.ringZero)

   let mk_var n v =
      Array.init (succ n) (fun i -> if i=v then Ring.ringUnit else Ring.ringZero)

   let grow n f =
      let old = Array.length f in
      if n > old then
         Array.init n (fun i -> if i<old then f.(i) else Ring.ringZero)
      else
         f

   let scale_aux k d =
      Ring.mul k d

   let scale k f =
      Array.map (scale_aux k) f

   let div f k =
      Array.map (fun x -> Ring.div x k) f

   let coef f v =
      f.(v)

   let get f i =
      if i>= Array.length f then
         Ring.ringZero
      else
         f.(i)

   let add f1 f2 =
      if Array.length f1 > Array.length f2 then
         Array.mapi (fun i k1 -> Ring.add k1 (get f2 i)) f1
      else
         Array.mapi (fun i k2 -> Ring.add k2 (get f1 i)) f2

   let add_scaled f1 c f2 =
      if Array.length f1 > Array.length f2 then
         Array.mapi (fun i k1 -> Ring.add k1 (Ring.mul c (get f2 i))) f1
      else
         Array.mapi (fun i k2 -> Ring.add (Ring.mul c k2) (get f1 i)) f2

   let sub f1 f2 =
      if Array.length f1 > Array.length f2 then
         Array.mapi (fun i k1 -> Ring.sub k1 (get f2 i)) f1
      else
         Array.mapi (fun i k2 -> Ring.sub (get f1 i) k2) f2

   let sub_scaled f1 c f2 =
      if Array.length f1 > Array.length f2 then
         Array.mapi (fun i k1 -> Ring.sub k1 (Ring.mul c (get f2 i))) f1
      else
         Array.mapi (fun i k2 -> Ring.sub (get f1 i) (Ring.mul c k2)) f2

   let sub_number f k =
      f.(constvar) <- Ring.sub f.(constvar) k;
      f

   let remove f v =
      f.(v) <- Ring.ringZero;
      f

   let rec gcd_aux f acc i =
      if i < Array.length f then
         gcd_aux f (Ring.gcd acc f.(i)) (succ i)
      else
         acc

   (*let gcd f = Array.fold_left Ring.gcd Ring.ringZero f*)
   let gcd f = gcd_aux f Ring.ringZero (succ constvar)

   exception Found of int

   let split f =
      let n = Array.length f in
      try
         for i=(pred n) downto 1 do
            if Ring.compare f.(i) Ring.ringZero <> 0 then
               raise (Found i)
         done;
         (f.(constvar), constvar, Array.make n Ring.ringZero)
      with
         Found i ->
            let f' = Array.init n (fun j -> if i=j then Ring.ringZero else f.(j)) in
            (f.(i), i, f')

   let any_var f =
      let n = Array.length f in
      try
			for i=(pred n) downto 1 do
				if Ring.compare f.(i) Ring.ringZero <> 0 then
					raise (Found i)
			done;
			constvar
		with
			Found i -> i

   let isNumber f =
      let test=ref true in
		for i=1 to Array.length f - 1 do
         if Ring.compare f.(i) Ring.ringZero <>0 then
            test:=false
		done;
      !test

	let value_of f =
		if isNumber f then
			coef f constvar
		else
			begin
				eprintf "AF.value_of: applied to a non-constant form %a" print f;
				raise (Invalid_argument "AF.value_of: applied to a non-constant form")
			end

   let term_of_monom info k v =
      if v=constvar then
         Ring.term_of k
      else
         Ring.mul_term (Ring.term_of k) (VI.restore info v)

	let rec term_of_aux info f n t i =
		if i >= n then
			t
		else
			let k = f.(i) in
			if Ring.equal k Ring.ringZero then
				term_of_aux info f n t (succ i)
			else
				let t' = Ring.add_term t (term_of_monom info k i) in
				term_of_aux info f n t' (succ i)

   let rec term_of info f =
		term_of_aux info f (Array.length f) (Ring.term_of f.(constvar)) 1

end
 *)

(* unused
module MakeDebugAF(Ring : RingSig)
	(AF1: AF_Sig with type ring=Ring.ring) (* less trusted module *)
	(AF2: AF_Sig with type ring=Ring.ring) (* more trusted module *)
   : AF_Sig with
	type ring=Ring.ring and
	type vars=VarType.t =
struct
   module Monom=MakeMonom(Ring)
   module VI=Var2Index(Ring)

   type ring=Ring.ring
   type vars=Monom.elt

   type af=AF1.af * AF2.af

	let constvar = 0

   let print_var = VarType.print

   let print out (f1,f2) =
		fprintf out "AF1: ";
		AF1.print out f1;
		fprintf out " AF2: ";
		AF2.print out f2

	let dim (f1,f2) =
		let d1 = AF1.dim f1 in
		let d2 = AF2.dim f2 in
		max d1 d2

	let equal f1 f2 =
		let d = dim (f1,f2) in
		let r = ref true in
		for i=0 to d do
			let k1=AF1.get f1 i in
			let k2=AF2.get f2 i in
			if Ring.compare k1 k2 <> 0 then
				begin
					r := false;
					eprintf "MakeDebugAF.equal %i: %a %a@." i Ring.print k1 Ring.print k2
				end
		done;
		!r

   let mk_number n k =
		let f1 = AF1.mk_number n k in
		let f2 = AF2.mk_number n k in
		if equal f1 f2 then (f1,f2)
		else raise (Invalid_argument "MakeDebugAF.mk_number")

   let mk_var n v =
		let f1 = AF1.mk_var n v in
		let f2 = AF2.mk_var n v in
		if equal f1 f2 then (f1,f2)
		else raise (Invalid_argument "MakeDebugAF.mk_var")

	let grow n (f1,f2) =
		let f1 = AF1.grow n f1 in
		let f2 = AF2.grow n f2 in
		if equal f1 f2 then (f1,f2)
		else raise (Invalid_argument "MakeDebugAF.grow")

   let scale k (f1,f2) =
		let f1 = AF1.scale k f1 in
		let f2 = AF2.scale k f2 in
		if equal f1 f2 then (f1,f2)
		else raise (Invalid_argument "MakeDebugAF.scale")

	let div (f1,f2) k =
		let f1 = AF1.div f1 k in
		let f2 = AF2.div f2 k in
		if equal f1 f2 then (f1,f2)
		else raise (Invalid_argument "MakeDebugAF.div")

   let coef (f1,f2) v =
		let c1 = AF1.coef f1 v in
		let c2 = AF2.coef f2 v in
		if Ring.equal c1 c2 then c1
		else
			begin
				eprintf "MakeDebugAF.coef\n%a %i -> %a\n%a %i -> %a@."
					AF1.print f1 v Ring.print c1
					AF2.print f2 v Ring.print c2;
				raise (Invalid_argument "MakeDebugAF.coef")
			end

   let get (f1,f2) v =
		let c1 = AF1.get f1 v in
		let c2 = AF2.get f2 v in
		if Ring.equal c1 c2 then c1
		else
			begin
				eprintf "MakeDebugAF.get\n%a %i -> %a\n%a %i -> %a@."
					AF1.print f1 v Ring.print c1
					AF2.print f2 v Ring.print c2;
				raise (Invalid_argument "MakeDebugAF.get")
			end

   let add (f11,f12) (f21,f22) =
		let f1 = AF1.add f11 f21 in
		let f2 = AF2.add f12 f22 in
		if equal f1 f2 then (f1,f2)
		else
			begin
				eprintf "MakeDebugAF.add\n%a %a = %a\n%a %a = %a@."
				AF1.print f11 AF1.print f21 AF1.print f1
				AF2.print f12 AF2.print f22 AF2.print f2;
				raise (Invalid_argument "MakeDebugAF.add")
			end

   let add_scaled (f11,f12) c (f21,f22) =
		let f1 = AF1.add_scaled f11 c f21 in
		let f2 = AF2.add_scaled f12 c f22 in
		if equal f1 f2 then (f1,f2)
		else
			begin
				eprintf "MakeDebugAF.add_scaled\n%a %a = %a\n%a %a = %a@."
				AF1.print f11 AF1.print f21 AF1.print f1
				AF2.print f12 AF2.print f22 AF2.print f2;
				raise (Invalid_argument "MakeDebugAF.add_scaled")
			end

	let sub (f11,f12) (f21,f22) =
		let f1 = AF1.sub f11 f21 in
		let f2 = AF2.sub f12 f22 in
		if equal f1 f2 then (f1,f2)
		else
			begin
				eprintf "MakeDebugAF.sub\n%a %a = %a\n%a %a = %a@."
				AF1.print f11 AF1.print f21 AF1.print f1
				AF2.print f12 AF2.print f22 AF2.print f2;
				raise (Invalid_argument "MakeDebugAF.sub")
			end

	let sub_scaled (f11,f12) c (f21,f22) =
		let f1 = AF1.sub_scaled f11 c f21 in
		let f2 = AF2.sub_scaled f12 c f22 in
		if equal f1 f2 then (f1,f2)
		else
			begin
				eprintf "MakeDebugAF.sub_scaled\n%a %a = %a\n%a %a = %a@."
				AF1.print f11 AF1.print f21 AF1.print f1
				AF2.print f12 AF2.print f22 AF2.print f2;
				raise (Invalid_argument "MakeDebugAF.sub_scaled")
			end

	let sub_number (f1,f2) k =
		let f1 = AF1.sub_number f1 k in
		let f2 = AF2.sub_number f2 k in
		if equal f1 f2 then (f1,f2)
		else
		raise (Invalid_argument "MakeDebugAF.sub_number")

   let remove (f1,f2) v =
		let f1 = AF1.remove f1 v in
		let f2 = AF2.remove f2 v in
		if equal f1 f2 then (f1,f2)
		else raise (Invalid_argument "MakeDebugAF.remove")

	let gcd (f1,f2) =
		let r1 = AF1.gcd f1 in
		let r2 = AF2.gcd f2 in
		if Ring.equal r1 r2 then
			r1
		else
			begin
				eprintf "MakeDebugAF.gcd:\n%a -> %a\n%a -> %a@." AF1.print f1 Ring.print r1 AF2.print f2 Ring.print r2;
				raise (Invalid_argument "MakeDebugAF.gcd")
			end

	exception Found of int

	let split (f1,f2) =
		let c1, v, f1' = AF1.split f1 in
		let c2 = AF2.coef f2 v in
		let f2' = AF2.remove f2 v in
		if (equal f1' f2') && (Ring.equal c1 c2) then
			(c1,v,(f1',f2'))
		else
			begin
				eprintf "MakeDebugAF.split:\n%a -> %a %i %a\n%a -> %a %i %a@."
					AF1.print f1 Ring.print c1 v AF1.print f1'
					AF2.print f2 Ring.print c2 v AF2.print f2';
				raise (Invalid_argument "MakeDebugAF.split")
			end

	let any_var (f1,f2) =
		let v = AF1.any_var f1 in
		let c1 = AF1.coef f1 v in
		let c2 = AF2.coef f2 v in
		if Ring.equal c1 c2 then
			v
		else
			begin
				eprintf "MakeDebugAF.any_var:\n%a -> %a %i\n%a -> %a %i@."
					AF1.print f1 Ring.print c1 v
					AF2.print f2 Ring.print c2 v;
				raise (Invalid_argument "MakeDebugAF.any_var")
			end

   let isNumber (f1,f2) =
		let r1 = AF1.isNumber f1 in
		let r2 = AF2.isNumber f2 in
		if r1 = r2 then
			r1
		else
			begin
				eprintf "MakeDebugAF.isNumber:\n%a -> %b\n%a -> %b@." AF1.print f1 r1 AF2.print f2 r2;
				raise (Invalid_argument "MakeDebugAF.isNumber")
			end

	let value_of (f1,f2) =
		let r1 = AF1.value_of f1 in
		let r2 = AF2.value_of f2 in
		if Ring.equal r1 r2 then
			r1
		else
			begin
				eprintf "MakeDebugAF.value_of:\n%a -> %a\n%a->%a@." AF1.print f1 Ring.print r1 AF2.print f2 Ring.print r2;
				raise (Invalid_argument "MakeDebugAF.value_of")
			end

   let term_of info (f1,f2) =
		let _ = AF1.term_of info f1 in
		AF2.term_of info f2

end
*)
