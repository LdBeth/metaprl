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
 * See the file doc/index.html for information on Nuprl,
 * OCaml, and more information about this system.
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

module type HypsSig = sig
    type var
    type cmp = var * var * int
    type hyps
    type addr
    val get_cmp : hyps -> addr -> cmp
    val get_v1 : hyps -> addr -> var
    val get_v2 : hyps -> addr -> var
    val get_const : hyps -> addr -> int
    val compare : var -> var -> bool
    val iter : hyps -> (addr -> cmp -> unit) -> unit
    val print_hyps : out_channel -> hyps -> unit
    val print_addr : out_channel -> addr -> unit
    val print_var : out_channel -> var -> unit
end

module SimpleHyps = struct
    type var = string
    type cmp = var * var * int
    type hyps = cmp array
    type addr = int

    let get_cmp h a = Array.get h a
    let get_v1 h a = let (v,_,_)=get_cmp h a in v
    let get_v2 h a = let (_,v,_)=get_cmp h a in v
    let get_const h a = let (_,_,c)=get_cmp h a in c
    let compare = (=)
    let iter h f = Array.iteri f h
    let print_hyps _ _ = ()
    let print_addr _ _ = ()
    let print_var _ _ = ()
end

module ArrayTools (Hyps: HypsSig) =
struct
   open Hyps

   let d2_1 (n,x,y) = n*x+y
   let d1_2 n i =let x=i/n in (n,x,i-n*x)
   let get a coord = Array.get a (d2_1 coord)
   let set a coord e = Array.set a (d2_1 coord) e
   let init n m f = Array.init (n*m) (fun x -> (f (d1_2 n x)))

(*    exception NotFound of (SimpleHyps.var * (SimpleHyps.var array))
*)

   let find a (e: var) =
      let len = Array.length a in
      let rec aux i =
         if i==len then raise Not_found;
         if compare (Array.get a i) e then i else aux (i+1)
      in aux 0
end

open Printf
open Lm_debug
let debug_graph_arith1 =
   create_debug (**)
      { debug_name = "debug_graph_arith1";
        debug_description = "Report input of solve function";
        debug_value = false
      }
let debug_graph_arith2 =
   create_debug (**)
      { debug_name = "debug_graph_arith2";
        debug_description = "Report output of solve function";
        debug_value = false
      }
let debug_graph_arith3 =
   create_debug (**)
      { debug_name = "debug_graph_arith3";
        debug_description = "Report input converted to internal representation";
        debug_value = false
      }

module Graph =
functor (Hyps : HypsSig) -> struct
   module ArrayTool = ArrayTools(Hyps)
   open Hyps
   open ArrayTool

   type result = Example of (var*int) list | Cycle of addr list
   type dist = Disconnected | Int of int * (addr list)

   let maxd d1 d2 = match (d1,d2) with
       (Disconnected,Disconnected) -> d1
       | (Disconnected,Int _) -> d2
       | (Int _, Disconnected) -> d1
       | (Int (i1,a1), Int (i2,a2)) -> if i1>i2 then d1 else d2

   let pos_dist d = match d with
       Disconnected -> false
       | Int (i,_) -> (i>0)

   let add_dist cij n a b c =
       let d1=get cij (n,a,b) in
       let d2=get cij (n,b,c) in
       match (d1,d2) with
       (Disconnected,Disconnected) -> d1
       | (Disconnected,Int _) -> d1
       | (Int _, Disconnected) -> d2
       | (Int (i1,a1), Int (i2,a2)) -> Int (i1 + i2, a1 @ a2)

  	let print_dist dst =
	   match dst with
	   	Disconnected -> eprintf "Disconnected\n%t" eflush
	    | Int(d, al) ->
	    		begin
		    		eprintf "Int %i|" d;
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
		let cij = Array.create (n*n) Disconnected in
		let f a (v1,v2,const) =
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
		let putc (v1,v2,_) l = putv v1 (putv v2 l) in
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
open TermOp
open TermSubst
open TermMan

module TermHyps =
struct
    type var = term
    type cmp = var * var * int
    type hyps = term * (int array)
    type addr = int

    let get_cmp (h,m) a =
       let t=TermMan.nth_hyp h m.(a) in
       let v1, v_and_c = two_subterms t in
       let c, v2 = two_subterms v_and_c in
(*       let t' : TermType.term' = Term.dest_term c in
       let { term_op = op ; term_terms = tl } = t' in
       let {op_params=[param]} = dest_op op in
       let Number cval = dest_param param in *)
       let n = TermOp.dest_number_any_term c in
       (v1,v2,Lm_num.int_of_num n)

    let get_v1 h a = let (v,_,_)=get_cmp h a in v
    let get_v2 h a = let (_,v,_)=get_cmp h a in v
    let get_const h a = let (_,_,c)=get_cmp h a in c
    let iter h f =
       let (t,ar)=h in
       let f1 i v = f i (get_cmp h i) in
       Array.iteri f1 ar

    let compare = alpha_equal

    let print_hyps oc (h,ar) =
    	let pr i =
    		let t=TermMan.nth_hyp h i in
    		Printf.fprintf oc "%a\n" debug_print t
    	in
	    	Printf.fprintf oc "hyps:\n";
   	 	Array.iter pr ar;
    		flush oc

    let print_addr oc a = fprintf oc "%i" a

    let print_var oc t = fprintf oc "%a" print_term t
end

let collect f gl =
   let sh = (explode_sequent gl).sequent_hyps in
   let aux' h = match h with HypBinding (_,t) | Hypothesis t -> t
    | Context (_,_,_) -> xnil_term in
   let shl = List.map aux' (SeqHyp.to_list sh) in
   let rec aux src i l =
      match src with
         [] -> l
       | h::tl -> if f h then
                     aux tl (i+1) (i::l)
                  else
                     aux tl (i+1) l
   in
      aux shl 1 []

module Test = struct
    module SG = Graph(SimpleHyps)
    open SG

    let h = Array.of_list [
    ("v1","v2",0);
    ("v5","v6",3);
    ("v4","v1",1);
    ("v1","v5",5);
    ("v4","v6",1);
    ("v3","v4",-1);
    ("v2","v3",2);
    ("v2","v2",-3)
    ]

    let v = solve h (*;("ok",Array.of_list ["ok"]))
                 with ArrayTools.NotFound(a,b) ->(a,b) *)
end

module TG = Graph(TermHyps)
