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

open Refiner.Refiner.Term
open Refiner.Refiner.Refine

(*
 *	uses NUPRLLIB_PORT & NUPRLLIB_HOST environment variables.
 *	
 *	lib mnemonic should be "NuprlLight";
 *	
 *	Then just use following functions to connect to lib and loop:
 *	
 *	rhook : (term (*goal*) -> term (*tactic*) -> term list(*subgoals*) )
 *	
 *	Any catchable errors thrown by the rhook will be coerced to a term and returned
 *	to caller. The coercion will result in readable errors being returned to the
 *	proof editro if the library Basic.error function is used.
 *	
 *)	


val library_open_eval	: string (* lib mnemonic *)
				-> (term -> term -> term list)
				-> unit

(*
 *	library must be open.
 *	multiple calls to loop_eval ok as long as no intevening close.
 *	doesn't return until lib sends quit term.
 *)

val library_loop_eval	: unit -> unit

val library_close	: unit -> unit



(*
 *	Following combines above three steps into one call.
 *	library closed at loop quit.
 *)

val library_open_and_loop_eval	: string -> (term -> term -> term list) -> unit


(*	a trivial refiner hook for testing : *)
val faux_refine : term -> term -> term list
val itt_bug : bool ref





(*	
 *	<ref_req>	: !nuprl5_implementation!{!refine:t}(<term{goal}>; <term{tactic}>; <obids>)
 *	
 *	<obids>		: !nuprl5_implementation!{!oid:t,<obid>:o list}()
 *	
 *	The obid list are those which would trivially cause dependency loops if referenced.
 *	In this application it seems save to ignore those.
 *	
 *	
 *	Any catchable errors thrown by the ehook will be coerced to a term and returned
 *	to caller. The coercion will result in readable errors if the library Basic.error
 *	function is used.
 *)

val msequent_to_term : msequent -> term 
val term_to_msequent : term -> term list * term
 