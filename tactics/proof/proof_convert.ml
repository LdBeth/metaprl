(*
 * Proof format conversion.
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
 * Copyright (C) 1999 Jason Hickey, Cornell University
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
 * Author: Jason Hickey
 * jyh@cs.cornell.edu
 *)

open Lm_debug
open Lm_printf

open Refiner_io
open Refiner.Refiner.TermType

open Proof_boot

type term_io = Refiner_io.TermType.term

let debug_convert =
   create_debug (**)
      { debug_name = "convert";
        debug_description = "show proof format conversions";
        debug_value = false
      }

(*
 * Convert between all the proof formats.
 * Raw proofs are in Proof.io_proof format.
 *)
module Convert =
struct
   (*
    * The type of proofs.
    *)
   type t = unit
   type raw = Proof.io_proof
   type cooked =
      Raw of Proof.io_proof
    | Term of term
    | Term_io of term_io

   (*
    * The parser and evaluator are needed, but they
    * will never be used.
    *)
   let parse text =
      raise (Invalid_argument "Proof_convert.parse: illegal call")

   let eval expr =
      raise (Invalid_argument "Proof_convert.eval: illegal call")

   (*
    * Get a raw proof from the proof.
    *)
   let to_raw () name proof =
      match proof with
         Raw proof ->
            if !debug_convert then
               eprintf "Converting proof to raw from raw%t" eflush;
            proof
       | Term t ->
            if !debug_convert then
               eprintf "Converting proof to raw from term%t" eflush;
            Proof.io_proof_of_term parse eval t
       | Term_io t ->
            if !debug_convert then
               eprintf "Converting proof to raw from term_io%t" eflush;
            Proof.io_proof_of_term_io parse eval t

   (*
    * Get a proof from the raw proof.
    *)
   let of_raw () _ proof =
      Raw (proof)

   (*
    * Convert the proof to a term.
    *)
   let to_term () name proof =
      match proof with
         Raw proof ->
            if !debug_convert then
               eprintf "Converting proof to term from raw%t" eflush;
            Proof.term_of_io_proof parse eval proof
       | Term t ->
            if !debug_convert then
               eprintf "Converting proof to term from term%t" eflush;
            t
       | Term_io t ->
            if !debug_convert then
               eprintf "Converting proof to term from term_io%t" eflush;
            Term_io.normalize_term t

   (*
    * Convert back to a proof.
    *)
   let of_term () name proof =
      if !debug_convert then
         eprintf "Creating term proof%t" eflush;
      Term proof

   (*
    * Convert the proof to a term.
    *)
   let to_term_io () name proof =
      match proof with
         Raw proof ->
            if !debug_convert then
               eprintf "Converting proof to term_io from raw%t" eflush;
            Proof.term_io_of_io_proof parse eval proof
       | Term t ->
            if !debug_convert then
               eprintf "Converting proof to term_io from term%t" eflush;
            Term_io.denormalize_term t
       | Term_io t ->
            if !debug_convert then
               eprintf "Converting proof to term_io from term_io%t" eflush;
            t

   (*
    * Convert back to a proof.
    *)
   let of_term_io () _ proof =
      if !debug_convert then
         eprintf "Creating term_io proof%t" eflush;
      Term_io proof

end

(*
 * -*-
 * Local Variables:
 * Caml-master: "nl"
 * End:
 * -*-
 *)
