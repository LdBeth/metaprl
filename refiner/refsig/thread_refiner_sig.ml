(*
 * This is a module that threads refiner operations.
 *
 * We provide two implementations of this signature:
 * - a simple serial one (tactics/null directory)
 * - a distributed one using Ensemble toolkit (tactics/ensemble directory)
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
 * Author: Jason Hickey
 * jyh@cs.cornell.edu
 *)

(*
 * This module is used to provide arguments to the thread module.
 *)
module type ThreadRefinerTacticalsSig =
sig
   type ('term, 'arg, 'extract) t
   type ('term, 'arg, 'extract) tactic = 'term -> ('term, 'arg, 'extract) t

   (* Fold a value *)
   val create_value : 'term list -> 'extract -> ('term, 'arg, 'extract) t

   (* Force the tactic application *)
   val force : string -> ('term, 'arg, 'extract) tactic -> ('term, 'arg, 'extract) tactic

   (* Postprocess the tactic *)
   val wrap : 'arg -> ('term, 'arg, 'extract) tactic -> ('term, 'arg, 'extract) tactic

   (* Postprocess the tactic *)
   val wrap_terms : ('term -> 'term) -> ('term, 'arg, 'extract) tactic -> ('term, 'arg, 'extract) tactic

   (* First operation that succeeds *)
   val first : ('term, 'arg, 'extract) tactic list -> ('term, 'arg, 'extract) tactic

   (* All operations should succeed *)
   val compose1 : ('term, 'arg, 'extract) tactic -> ('term, 'arg, 'extract) tactic -> ('term, 'arg, 'extract) tactic
   val compose2 : ('term, 'arg, 'extract) tactic -> ('term, 'arg, 'extract) tactic list -> ('term, 'arg, 'extract) tactic
   val composef : ('term, 'arg, 'extract) tactic -> ('term list -> ('term, 'arg, 'extract) t list) -> ('term, 'arg, 'extract) tactic
end

module type ThreadRefinerSig =
sig
   (* Access is through a server *)
   type ('term, 'share, 'arg, 'extract) server

   (*
    * These are the thread objects.
    *)
   type ('term, 'arg, 'extract) t
   type ('term, 'arg, 'extract) tactic = 'term -> ('term, 'arg, 'extract) t

   (* Shared values *)
   type 'share key

   (* Create a server *)
   val create : (Lm_printf.out_channel -> 'term -> unit) ->
      ('extract -> 'extract list -> 'extract) ->        (* compose function *)
      ('arg -> 'extract -> 'extract) ->                 (* post-processor *)
      ('term, 'share, 'arg, 'extract) server

   (* Fold a value *)
   val create_value : 'term list -> 'extract -> ('term, 'arg, 'extract) t

   (* Force the tactic application *)
   val force : string -> ('term, 'arg, 'extract) tactic -> ('term, 'arg, 'extract) tactic

   (* Post-process the extract *)
   val wrap : 'arg -> ('term, 'arg, 'extract) tactic -> ('term, 'arg, 'extract) tactic

   (* First operation that succeeds *)
   val first : ('term, 'arg, 'extract) tactic list -> ('term, 'arg, 'extract) tactic

   (* All operations should succeed *)
   val compose1 : ('term, 'arg, 'extract) tactic -> ('term, 'arg, 'extract) tactic -> ('term, 'arg, 'extract) tactic
   val compose2 : ('term, 'arg, 'extract) tactic -> ('term, 'arg, 'extract) tactic list -> ('term, 'arg, 'extract) tactic
   val composef : ('term, 'arg, 'extract) tactic -> ('term list -> ('term, 'arg, 'extract) t list) -> ('term, 'arg, 'extract) tactic

   (* Fully evaluate the thread *)
   val eval : ('term, 'share, 'arg, 'extract) server -> ('term, 'arg, 'extract) t -> 'term list * 'extract

   (* Shared memory *)
   val share : ('term, 'share, 'arg, 'extract) server -> string -> (unit -> 'share) -> 'share key
   val arg_of_key : ('term, 'share, 'arg, 'extract) server -> 'share key -> 'share

   (* Start the main loop *)
   val args : unit -> (string * Arg.spec * string) list
   val main_loop : ('term, 'share, 'arg, 'extract) server -> unit
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
