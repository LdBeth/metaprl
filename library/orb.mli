(*
 * This file is part of MetaPRL, a modular, higher order
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

open Refiner.Refiner.TermType
open Definition
open Link

type orb
type connection =
	{ link	: link;
	  orb	: orb;			(* local orb containing connection *)
	  ro_address	: string list	(* remote orb *)
	}
type environment

val orb_open	: string (* mnemonic *)  -> orb
val connect	: orb -> string (*servername*) -> string (* remote hostname *) -> int (* remote socket *)
			-> connection
val disconnect	: orb -> connection -> unit
val orb_close	: orb -> unit

val resource			: environment -> string -> termtable
val jprover_description_term	: term
val metaprl_description_term	: term
val current_description_term	: term ref

val open_library_environment	: connection
				-> string			(* "" means new lib env, otherwise -> restore *)
				-> (term -> term) 		(* local eval *)
				-> environment
val close_library_environment	: environment -> string
val join_library_environment	: connection
 				-> string list			(* mneumonic *)
				-> (term -> term) 		(* local eval *)
				-> environment
val leave_library_environment	: environment -> unit
val restore_library_environment	: connection
				-> string
				-> (term -> term) 		(* local eval *)
				-> environment

val eval_string		: environment -> term (* tid *) -> string -> unit
val eval		: environment -> term (* tid *) -> term -> unit
val eval_args		: environment -> term (* tid *) -> term -> term list -> unit
val eval_string_to_term		: environment -> term (* tid *) -> string -> term
val eval_to_term		: environment -> term (* tid *) -> term -> term
val eval_args_to_term		: environment -> term (* tid *) -> term -> term list -> term

(* all these callback variants seem overkill.
   single callback with checkpoint option  should be sufficient
   as other reqs can be done within call back.
 *)

(* twould be cleaner if tid were term *)
val eval_callback	: bool -> environment
				-> term (* tid *)
				-> (term -> unit)
				-> unit

(*
   intention is to not allow g to mask exception from f.
   g may handle failure of f but if f and g both fail then the exception is from f.

   this is important in callbacks to the library, as if f is local eval then failure
   in f needs to be passed back through the library. The original failure can not be
   marshalled/unmarshalled thus it must be cached locally. with fail protect is an
   abstract method of accomplishing this.
 *)
val with_fail_protect	: (('b -> unit) -> unit) (* g *) -> ('b -> 'a) (* f *) -> 'a

(*
val eval_with_callback		: environment
					-> bound_term (* tid *)
					-> (term -> unit) -> term
					-> unit
val eval_to_term_with_callback		: environment
					-> bound_term (* tid *)
					-> (term -> unit) -> term
					-> term
val eval_args_with_callback		: environment
					-> bound_term (* tid *)
					-> (term -> unit) -> term -> term list
					-> unit
val eval_args_to_term_with_callback	: environment
					-> bound_term (* tid *)
					-> (term -> unit) -> term -> term list
					-> term
*)

(*
val request_loop	: environment -> unit
   (* only viable if the local eval hook supplied *)
*)

val orb_req_loop 	: environment -> unit

val db_pathname		: string ref
