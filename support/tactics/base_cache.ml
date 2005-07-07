(*
 * Define a cache resource.
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
 * Author: Jason Hickey
 * jyh@cs.cornell.edu
 *)

extends Tactic_cache
extends Summary

open Mp_resource

open Tactic_cache
open Tactic_type.Tactic

(************************************************************************
 * TYPES                                                                *
 ************************************************************************)

type cache_rule =
   Forward of tactic frule
 | Backward of tactic brule

type cache = tactic Tactic_cache.cache

(************************************************************************
 * IMPLEMENTATION                                                       *
 ************************************************************************)

(*
 * Add a rule.
 *)
let improve_data cache info =
   match info with
      Forward info ->
         add_frule cache info
    | Backward info ->
         add_brule cache info

(*
 * Resource.
 *)
let resource (cache_rule, cache) cache =
   Functional {
      fp_empty = empty_cache;
      fp_add = improve_data;
      fp_retr = (fun x -> x)
   }

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
