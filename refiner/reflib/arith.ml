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
 	  val dest_cmp : 'a cmp -> var * var * num
    val equal : var -> var -> bool
    val iter : 'a hyps -> (addr -> 'a cmp -> unit) -> unit
    val vars_of_hyps : 'a hyps -> var array
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
    let equal = (=)
    let iter h f = Array.iteri f h
    let print_hyps _ _ = ()
    let print_addr _ _ = ()
    let print_var _ _ = ()
end
*)

module ArrayTools (Hyps: HypsSig) =
struct
   open Hyps

   let d2_1 (n,x,y) = n*x+y
   let d1_2 n i = let x = i/n in x,i-n*x

   let get a coord = Array.unsafe_get a (d2_1 coord)
   let set a coord e = Array.unsafe_set a (d2_1 coord) e

   let find a (e: var) =
      let len = Array.length a in
      let rec aux i =
         if i==len then raise Not_found;
         if equal (Array.get a i) e then i else aux (i+1)
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

module Graph (Hyps : HypsSig) =
struct
   module ArrayTool = ArrayTools(Hyps)
   open Hyps
   open ArrayTool

(* unused
   type result = Example of (var*num) list | Cycle of addr list
*)
   type dist = Disconnected | Int of num * (addr list)

   let maxd d1 d2 =
      match (d1,d2) with
         (Int (i1,_), Int (i2,_)) -> if gt_num i2 i1 then d2 else d1
       | (Disconnected,Int _) -> d2
       | _ -> d1

   let pos_dist d =
      match d with
         Disconnected -> false
       | Int (i,_) -> is_pos i

   let add_dist cij n a b c =
       let d1=get cij (n,a,b) in
       let d2=get cij (n,b,c) in
       match (d1,d2) with
          (Int (i1,a1), Int (i2,a2)) ->
             if (a=b) && (b=c) then
                d1
             else
                Int (add_num i1 i2, a1 @ a2)
        | _  -> Disconnected

    let print_dist dst =
       match dst with
          Disconnected -> eprintf "Disconnected\n%t" eflush
        | Int(d, al) ->
             eprintf "Int %s|" (string_of_num d);
             List.iter (eprintf "%a:" print_addr) al;
             eprintf "\n%t" eflush

    let print_i_dist n i dst =
       let x, y = d1_2 n i in
          eprintf "%i %i-> " x y;
          print_dist dst

   let init_c n h va =
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
        Array.iteri (print_i_dist n) cij;
      cij
    end

   exception Positive of dist

   let compute h va =
      let n = Array.length va in
      let cij = init_c n h va in
         try
            for k=0 to n-1 do
               for i=0 to n-1 do
                  for j=0 to n-1 do
                     let coord=(n,i,j) in
                        set cij coord (maxd (get cij coord)
                                       (add_dist cij n i k j))
                  done;
                  let d = get cij (n,i,i) in
                     if pos_dist d then raise (Positive d)
               done;
            done;
            Disconnected, cij
         with
            Positive d -> d, cij

  let print_i_var oc i v =
     fprintf oc "%i-> " i;
     print_var oc v;
     fprintf oc "\n";
     flush oc

   let solve h =
      if !debug_graph_arith1 then
         print_hyps stderr h;
      let vars = vars_of_hyps h in
      let () = if !debug_graph_arith3 then
                  Array.iteri (print_i_var stderr) vars in
      let d, dar = compute h vars in
         if !debug_graph_arith2 then
         begin
            print_dist d;
            Array.iter print_dist dar
         end;
         d

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

    let dest_cmp (t1,t2,n,tac) = (t1,t2,n)
    let iter h f =
       Array.iteri f h

    let equal = alpha_equal

    let vars_of_hyps h =
       let putv v l =
          if List.exists (equal v) l then l else v :: l in
       let putc l acmp =
          let v1,v2,_ = dest_cmp acmp
          in if equal v1 v2 then putv v1 l else putv v1 (putv v2 l)
       in
       let l = Array.fold_left putc [] h in
          Array.of_list l

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
      TG.Int (_,r) ->
         let rl = List.map (fun i -> ar.(i)) r in
            if !debug_graph_arith2 then
               eprintf "Cycle size %i, list size %i%t" (List.length r) (List.length rl) eflush;
            rl
    | TG.Disconnected ->
         raise (RefineError("arithT", StringError "Proof by contradiction - No contradiction found"))
