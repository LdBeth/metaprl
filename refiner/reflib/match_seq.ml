(*
 * Tried to match a pair of hypothesis lists 
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
 * Copyright (C) 1998 Alexey Nogin, Cornell University
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
 * Author: Alexey Nogin
 *
 *)

open Refiner.Refiner
open RefineError

let cant_match_hyp = RefineError ("Match_seq.match_hyp", StringError "hypothesis do not match")

let match_hyps new_hyp old_hyp = 
   let old_length = Array.length old_hyp in
   let may_skip = (Array.length new_hyp) - old_length in
   let result = Array.create (Array.length new_hyp) None in
   let rec aux new_skip old_skip matching =
      if old_skip = old_length then () else
      if (new_skip - old_skip) > may_skip then raise cant_match_hyp;
      try 
         let matching = TermSubst.match_terms matching old_hyp.(old_skip) new_hyp.(new_skip) in
         aux (succ new_skip) (succ old_skip) matching;
         result.(new_skip) <- Some old_skip;
         ()
      with RefineError _ ->
         aux (succ new_skip) old_skip matching
   in aux 0 0 []; result
