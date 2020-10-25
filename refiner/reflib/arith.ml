(*
 *
 * Positive cycle search in a graph.
 * Used by itt_int_arith/arithT
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
 * Copyright (C) 1998 Jason Hickey, Cornell University
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
 * Author: Yegor Bryukhov @email{ynb@mail.ru}
 * Modified by: Aleksey Nogin @email{nogin@cs.cornell.edu}
 *)
open Lm_debug
open Lm_printf
open Lm_num

module type HypsSig = sig
    type var
    type 'a cmp (*= var * var * num*)
    type 'a hyps
    type addr
    val get_cmp : 'a hyps -> addr -> 'a cmp
	  val dest_cmp : 'a cmp -> var * var * num
    val get_v1 : 'a hyps -> addr -> var
    val get_v2 : 'a hyps -> addr -> var
    val get_const : 'a hyps -> addr -> num
    val compare : var -> var -> bool
    val iter : 'a hyps -> (addr -> 'a cmp -> unit) -> unit
    val print_hyps : out_channel -> 'a hyps -> unit
    val print_addr : out_channel -> addr -> unit
    val print_var : out_channel -> var -> unit
end

(* unused
module SimpleHyps = struct
    type var = string
    type 'a cmp = var * var * num
    type 'a hyps = 'a cmp array
    type addr = int

    let get_cmp h a = Array.get h a
	 let dest_cmp c = c
    let get_v1 h a = let (v,_,_)=get_cmp h a in v
    let get_v2 h a = let (_,v,_)=get_cmp h a in v
    let get_const h a = let (_,_,c)=get_cmp h a in c
    let compare = (=)
    let iter h f = Array.iteri f h
    let print_hyps _ _ = ()
    let print_addr _ _ = ()
    let print_var _ _ = ()
end
*)

let num0 = zero_num

module ArrayTools (Hyps: HypsSig) =
struct
   open Hyps

   let d2_1 (n,x,y) = n*x+y
(* unused
   let d1_2 n i =let x=i/n in (n,x,i-n*x)
*)
   let get a coord = Array.get a (d2_1 coord)
   let set a coord e = Array.set a (d2_1 coord) e
(* unused
   let init n m f = Array.init (n*m) (fun x -> (f (d1_2 n x)))
*)

(*    exception NotFound of (SimpleHyps.var * (SimpleHyps.var array))
*)

   let find a (e: var) =
      let len = Array.length a in
      let rec aux i =
         if i==len then raise Not_found;
         if compare (Array.get a i) e then i else aux (i+1)
      in aux 0
end

let debug_graph_arith1 =
   create_debug (**)
      { debug_name = "graph_arith1";
        debug_description = "Report input of Arith's solve function";
        debug_value = false
      }
let debug_graph_arith2 =
   create_debug (**)
      { debug_name = "graph_arith2";
        debug_description = "Report output of Arith's solve function";
        debug_value = false
      }
let debug_graph_arith3 =
   create_debug (**)
      { debug_name = "graph_arith3";
        debug_description = "Report Arith's input converted to internal representation";
        debug_value = false
      }

module Graph = functor (Hyps : HypsSig) ->
struct
   module ArrayTool = ArrayTools(Hyps)
   open Hyps
   open ArrayTool

(* unused
   type result = Example of (var*num) list | Cycle of addr list
*)
   type dist = Disconnected | Int of num * (addr list)

   let maxd d1 d2 = match (d1,d2) with
       (Disconnected,Disconnected) -> d1
       | (Disconnected,Int _) -> d2
       | (Int _, Disconnected) -> d1
       | (Int (i1,a1), Int (i2,a2)) -> if gt_num i2 i1 then d2 else d1

   let pos_dist d = match d with
       Disconnected -> false
       | Int (i,_) -> (gt_num i num0)

   let add_dist cij n a b c =
       let d1=get cij (n,a,b) in
       let d2=get cij (n,b,c) in
       match (d1,d2) with
       (Disconnected,Disconnected) -> d1
       | (Disconnected,Int _) -> d1
       | (Int _, Disconnected) -> d2
       | (Int (i1,a1), Int (i2,a2)) ->
				if (a=b) && (b=c) then
					d1
				else
					Int (add_num i1 i2, a1 @ a2)

  	let print_dist dst =
	   match dst with
	   	Disconnected -> eprintf "Disconnected\n%t" eflush
	    | Int(d, al) ->
	    		begin
		    		eprintf "Int %s|" (string_of_num d);
		    		List.iter (eprintf "%a:" print_addr) al;
		    		eprintf "\n%t" eflush;
		    	end;
		()

	let print_i_dist i dst =
		eprintf "%i-> " i;
		print_dist dst

   let init_c h va =
		let n=Array.length va in
(*    let ini (_,i,j) = if i=j then Int (0,[]) else Disconnected in
      let cij=init n n ini in
*)
		let cij = Array.make (n*n) Disconnected in
		let f a acmp =
			let (v1,v2,const) = dest_cmp acmp in
			let i=find va v1 in
			let j=find va v2 in
			let coord=(n,i,j) in
			set cij coord (maxd (Int(const,[a])) (get cij coord))
		in
		begin
			iter h f;
			if !debug_graph_arith3 then
				Array.iteri print_i_dist cij;
			cij
		end

   let compute h va =
       let cij=init_c h va in
       let n = Array.length va in
       let k = ref 0 in
       let i = ref 0 in
       let poscycle = ref false in
       begin
           while (not !poscycle) && !k<n do
               i:=0;
               while (not !poscycle) && !i<n do
                   for j=0 to n-1 do
                       let coord=(n,!i,j) in
                       set cij coord (maxd (get cij coord)
                                           (add_dist cij n !i !k j))
                   done;
                   poscycle:=(pos_dist (get cij (n,!i,!i)));
                   i:=!i+1
               done;
               k:=!k+1
           done;
           if !poscycle then
               let posi=(!i)-1 in
                   (get cij (n,posi,posi), cij)
           else
               (Disconnected, cij)
       end

	let print_i_var oc i v =
		fprintf oc "%i-> " i;
		print_var oc v;
  		fprintf oc "\n";
		flush oc

   let vars_of_hyps h =
   	let putv v l =
      	if List.exists (compare v) l then l
         else v::l
		in
		let putc acmp l = let (v1,v2,_)=dest_cmp acmp in putv v1 (putv v2 l) in
		let l = ref [] in
		let put a c = l:=(putc c !l) in
		begin
			iter h put;
			let result= Array.of_list !l in
			if !debug_graph_arith3 then
           	Array.iteri (print_i_var stderr) result;
         result
		end

   let solve h =
		begin
	   	if !debug_graph_arith1 then
		   	print_hyps stderr h;
	   	let result=compute h (vars_of_hyps h) in
	   	begin
			   if !debug_graph_arith2 then
					let (d,dar)=result in
					begin
						print_dist d;
						Array.iter print_dist dar
					end
				else
					();
				result
		  	end
	   end
end

open Refiner.Refiner
open TermType
open Term
open TermSubst

type 'a inequality =
   term * term * num * 'a (* represents  t1 >= t2 + n *)

module TermHyps =
struct
    type var = term
    type 'a cmp = 'a inequality
    type 'a hyps = 'a inequality array
    type addr = int

    let get_cmp h a = h.(a)
	 let dest_cmp (t1,t2,n,tac) = (t1,t2,n)
    let get_v1 h a = let (v,_,_,_)=get_cmp h a in v
    let get_v2 h a = let (_,v,_,_)=get_cmp h a in v
    let get_const h a = let (_,_,c,_)=get_cmp h a in c
    let iter h f =
       Array.iteri f h

    let compare = alpha_equal

    let print_hyps oc h =
    	let pr (t1,t2,c,tac) =
    		Lm_printf.fprintf oc "%a>=%a+%s\n" print_term t1 print_term t2 (string_of_num c)
    	in
	    	Lm_printf.fprintf oc "hyps:\n";
   	 	Array.iter pr h;
    		flush oc

    let print_addr oc a = fprintf oc "%i" a

    let print_var oc t = fprintf oc "%a" print_term t
end

module TG = Graph(TermHyps)

open RefineError

let find_contradiction l =
	let ar=Array.of_list l in
   match TG.solve ar with
      TG.Int (_,r),_ ->
         let aux3 i al = (ar.(i))::al in
         let rl = List.fold_right aux3 r [] in
		   if !debug_graph_arith2 then
				eprintf"Cycle size %i, list size %i%t" (List.length r) (List.length rl) eflush;
         rl
    | TG.Disconnected,_ ->
         raise (RefineError("arithT", StringError "Proof by contradiction - No contradiction found"))
