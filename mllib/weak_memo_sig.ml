(* This file is an interface for memoize function based on weak
 * array of results
 *
 * -----------------------------------------------------------------
 * This file is part of MetaPRL, a modular, higher order
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

module type WeakMemoSig = 
sig

(* Memo-type
 * 'param - the way for recursion
 * 'header - already transformed argument where all recursive references replaced with
 *           its target's descriptors
 * 'weak_header - already transformed argument where all recursive references replaced with
 *           its target's weak_descriptors
 * 'image - result type
 *)
   type ('param, 'header, 'weak_header, 'image) t

(*
 * External kind of descriptors to prevent GC
 *)
   type 'a descriptor 

(*
 * Creates new memo-structure
 *)
   val create : int -> int ->              (* This numbers are sizes of headers' hash table *)
                                           (* and results' array *)
          ('header -> 'weak_header) ->     (* Convert 'header to 'weak_header *)
          ('weak_header -> 'weak_header -> bool) ->  (* Headers' comparision function *)
          ('param -> 'header -> 'image) -> (* Converter from header to result *)
          ('param, 'header, 'weak_header, 'image) t      (* New memo-structure *)

(*
 * Looks for header and returns result's descriptor if succeed otherwise evaluate the result 
 * and memorize it (if result GC-ed already then it reevaluated correctly) and returns
 * its descriptor
 *)
   val lookup : ('param, 'header, 'weak_header, 'image) t -> 'param -> 'header -> 'image descriptor
 
(*
 * As previous but it suppose result is not GC-ed (if not exception raises)
 *)
   val unsafe_lookup : ('param, 'header, 'weak_header, 'image) t -> 'param -> 'header -> 'image descriptor

(*
 * Returns descriptor's denotat
 *)
   val retrieve : ('param, 'header, 'weak_header, 'image) t -> 'param -> 'image descriptor  -> 'image

(*
 * As previous but raise exception if object already GC-ed
 
   val unsafe_retrieve : ('param, 'header, 'weak_header, 'image) t -> 'param -> 'image weak_descriptor  -> 'image
 *)

end

(*
 * -*-
 * Local Variables:
 * Caml-master: "term_hash, term_header_constr"
 * End:
 * -*-
 *)
