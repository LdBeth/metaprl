(*
 * This file is part of Nuprl-Light, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/index.html for information on Nuprl,
 * OCaml, and more information about this system.
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

open Printf
open Nl_debug

let _ =
   if !debug_load then
      eprintf "Loading Int32%t" eflush

 (*module type BigIntSig =
sig
(************************************************************************
 * Types                                                                *
 ************************************************************************)

type int32

exception IntSize of string * int

val create : int -> int32

val lband : int32 -> int32 -> int32
val lbor : int32 -> int32 -> int32
val lbsl : int32 -> int -> int32
 (*val lbsr : int32 -> int -> int32*)
val basr : int32 -> int -> int32

val bequal : int32 -> int32 -> bool
val blt : int32 -> int32 -> bool
val bgt : int32 -> int32 -> bool
val blte : int32 -> int32 -> bool
val bgte : int32 -> int32 -> bool
 end

module Int32:Int32Sig =
 struct*)

type int32 = int * int

exception IntSize of string * int

let create i =
  if (i > 0xFFFF) then raise (IntSize ("create", i))
  else (0, i)

let mk_bint i =
  let a = abs i in let b = ((a asr 16) land 0xFFFF) and c = (a land 0xFFFF) in
  if i >= 0 then (b, c) else (if b = 0 then (b, -c) else (-b, c))

let make_int32 (i, k) =
  if ((i > 0xFFFF) or (k > 0xFFFF)) then raise (IntSize ("make_int32", i))
  else (i, k)
let dest_bint (a, b) =
  let c = abs a in
  if c > 0x3FFF then failwith "int32 too big"
  else let i = (c lsl 16) lor b in if a >= 0 then i else (-i)
let dest_int32 (a, b) = (a, b)

let int_of_int32 (a, b) = b
let int32_of_int int = mk_bint int

let lband (x, w) (y, z) = ((x land y),(w land z))
let lbor (x, w) (y, z) = ((x lor y), (w lor z))

let lbsl (x, w) n =
  let a = abs (w lsl n) and b = abs ((x lsl n) land 0xFFFF) in
  if a <= 0xFFFF then (b, a)
  else ((b lor (a lsr 16)), (a land 0xFFFF)) ;;
	
let lbsl (x, w) n =
  let rec aux i y z=
    (if i = n then (y, z)
    else
      let a = z lsl  1 and b = (y lsl 1) in
      (if a <= 0xFFFF then aux (i + 1) (b land 0xFFFF) a
      else aux (i + 1) ((b lor (a lsr 16)) land 0xFFFF) (a land 0xFFFF))) in
  aux 0 x w ;;

(*
let lbsr (x, w) n =
let basr (x, w) n = (x, w)
*)
	   (*LAL*)
let bplus (x, w) n = (x, w+ n)
let bminus (x, w) n = (x, w - n)
let bneg (x, w) = if x = 0 then (x, -w) else (-x, w)

let bequal (x, w) (y, z) = (x = y) && (w = z)
let blt (x, w) (y, z) = (x < y) or ((x = y) && (w < z))
let bgt (x, w) (y, z) = (x > y) or ((x = y) && (w > z))
let blte (x, w) (y, z) = (x < y) or ((x = y) && (w <= z))
let bgte (x, w) (y, z) = (x > y) or ((x = y) && (w >= z))

let bdecr b = let (x, y) = !b
in if y > 0 then b:=  (x, y-1) else b:= (x-1, 0xffff)
(* end ;;*)

let print_int32 b =
  let (a, c) = dest_int32 b in
  print_char '(';
  print_int a;
  print_char ',';
  print_int c; print_char ')'
	


