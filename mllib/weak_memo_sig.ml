(*
 * This is a memo function where function results
 * are not saved if there are no references to them.
 *
 * Note that a WeakMemo _is_ a Memo.
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

open Splay_table

module type WeakMemoSig =
sig
   (*
    * Weak_descriptors are hash-table entries that
    * permit the values associated with them to be collected
    * by GC.
    *)
   type 'a weak_descriptor
   val wd_hash : 'a weak_descriptor -> int

   (*
    * Descriptors are analog of weak_descriptor,
    * but they do not permit their values to be collected.
    *)
   type 'a descriptor

   (* Memo-type:
    * 'param - an arbitrary argument (usually used for recursion on memo tables).
    * 'arg - the type of arguments
    * 'header - transformed argument where all recursive references are replaced with
    *           its target's descriptors
    * 'weak_header - transformed argument where all recursive references are replaced with
    *           its target's weak_descriptors
    * 'image - result type
    *)
   type ('param, 'arg, 'header, 'weak_header, 'image) t

   (*
    * Release the value to GC.
    *)
   val weaken : 'a descriptor -> 'a weak_descriptor

   (*
    * Compare descriptors.
    * This is useful for constructing sets of values.
    *
    * The use of this function is not advised for casual
    * users since some implementations of WeakMemo may
    * not provide this function.
    *)
   val compare : 'a descriptor -> 'a descriptor -> int

   (*
    * Creates new memo-structure.
    *)
   val create : int -> int ->              (* These numbers are size of header's hash table
                                            * (and halfsize of array of target objects)
                                            * and critical level of holes to restart GC.
                                            *)
      string ->                                      (* For debugging, this is the name of the table *)
      ('param -> 'arg -> 'header) ->                 (* Convert the argument to a header *)
      ('param -> 'header -> 'weak_header) ->         (* Convert 'header to 'weak_header *)
      ('weak_header -> 'weak_header -> bool) ->      (* Headers' comparision function *)
      ('param -> 'header -> 'image) ->               (* Converter from header to result *)
      ('param, 'arg, 'header, 'weak_header, 'image) t

   (*
    * Creates new memo-structure.
    * Apply is not allowed, and table is initialied to default size.
    *)
   val create_default :
      string ->                                      (* Name of the table *)
      ('param -> 'header -> 'weak_header) ->         (* Convert 'header to 'weak_header *)
      ('weak_header -> 'weak_header -> bool) ->      (* Headers' comparision function *)
      ('param -> 'header -> 'image) ->               (* Converter from header to result *)
      ('param, 'arg, 'header, 'weak_header, 'image) t

   (*
    * Looks for header and returns result's descriptor if succeed otherwise evaluate the result
    * and memorize it (if result GC-ed already then reevaluate it) and returns
    * its descriptor
    *)
   val lookup : ('param, 'arg, 'header, 'weak_header, 'image) t -> 'param -> 'header -> 'image descriptor

   (*
    * As previous but assume result has not been collected by GC
    * (if not, an exception is raised).
    *)
   val unsafe_lookup : ('param, 'arg, 'header, 'weak_header, 'image) t -> 'param -> 'header -> 'image descriptor

   (*
    * Return the value represented by the descriptor.
    *)
   val retrieve : ('param, 'arg, 'header, 'weak_header, 'image) t -> 'param -> 'image descriptor -> 'image

   (*
    * The descriptor allows dereferencing.
    *)
   val retrieve_hack : 'image descriptor -> 'image

   (*
    * Check that a descriptor is consistent.
    * This is for descriptors that may have been passed across the
    * network.
    *)
   val retrieve_check : ('param, 'atg, 'header, 'weak_header, 'image) t -> 'image descriptor -> bool

   (*
    * Compose conversion, lookup, and retrieve
    *)
   val apply : ('param, 'arg, 'header, 'weak_header, 'image) t -> 'param -> 'arg -> 'image

   val gc_on : unit -> unit

   val gc_off : unit -> unit
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "term_hash, term_header_constr"
 * End:
 * -*-
 *)
