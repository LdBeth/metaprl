(* This file is part of Nuprl-Light, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/index.html for information on Nuprl,
 * OCaml, and more information about this system.
 *
 * Copyright (C) 1998 Yegor Bryukhov, Moscow State University
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
 * Author: Yegor Bryukhov
 *)

open Termmod_sig
open Simplehashtbl
open Term_compare
open Term_transfer

(*
 * Common cases.
 *)
module NormalizeTerm =
   MakeTermCopy (Simplehashtbl) (TermCompare) (Refiner_std_verb.Refiner) (Refiner.Refiner)

type normalize = NormalizeTerm.t

let create_norm = NormalizeTerm.create

let normalize_term = NormalizeTerm.copy_term
let normalize_meta_term = NormalizeTerm.copy_meta_term
let denormalize_term = NormalizeTerm.back_term
let denormalize_meta_term = NormalizeTerm.back_meta_term

let normalize_term_single = NormalizeTerm.copy_term_single
let normalize_meta_term_single = NormalizeTerm.copy_meta_term_single
let denormalize_term_single = NormalizeTerm.back_term_single
let denormalize_meta_term_single = NormalizeTerm.back_meta_term_single

(*
let normalize_term info t =
   if !debug_memo then
      eprintf "Normalizing:%t" eflush;
   let t = NormalizeTerm.copy_term info t in
      if !debug_memo then
         eprintf "Done%t" eflush;
      t

let normalize_meta_term info t =
   if !debug_memo then
      eprintf "Normalizing:%t" eflush;
   let t = NormalizeTerm.copy_meta_term info t in
      if !debug_memo then
         eprintf "Done%t" eflush;
      t

let denormalize_term info t =
   if !debug_memo then
      begin
         eprintf "Denormalizing: ";
         flush stderr;
         Simple_print.prerr_simple_term t;
         eflush stderr
      end;
   let t = NormalizeTerm.back_term info t in
      if !debug_memo then
         eprintf "Done%t" eflush;
      t

let denormalize_meta_term info t =
   if !debug_memo then
      eprintf "Deormalizing:%t" eflush;
   let t = NormalizeTerm.back_meta_term info t in
      if !debug_memo then
         eprintf "Done%t" eflush;
      t

let normalize_term_single t =
   if !debug_memo then
      eprintf "Normalizing Single:%t" eflush;
   let t = NormalizeTerm.copy_term_single t in
      if !debug_memo then
         eprintf "Done%t" eflush;
      t

let normalize_meta_term_single t =
   if !debug_memo then
      eprintf "Normalizing Single:%t" eflush;
   let t = NormalizeTerm.copy_meta_term_single t in
      if !debug_memo then
         eprintf "Done%t" eflush;
      t

let denormalize_term_single t =
   if !debug_memo then
      begin
         eprintf "Denormalizing: Single:";
         flush stderr;
         Simple_print.prerr_simple_term t;
         eflush stderr
      end;
   let t = NormalizeTerm.back_term_single t in
      if !debug_memo then
         eprintf "Done%t" eflush;
      t

let denormalize_meta_term_single t =
   if !debug_memo then
      eprintf "Deormalizing: Single%t" eflush;
   let t = NormalizeTerm.back_meta_term_single t in
      if !debug_memo then
         eprintf "Done%t" eflush;
      t
*)

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
