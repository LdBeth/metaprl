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

(*
 * This implements a filesystem interface to the library.
 *)

open Refiner.Refiner.Term
open Refiner.Refiner.TermType
open Basic

open Utils
open Library

open Printf
open Nl_debug

let _ =
   if !debug_load then
      eprintf "Loading Library_eval%t" eflush



let library = null_oref()
let connection = null_oref()

exception LibraryException of string

let library_close () =
 if oref_p library
    then (leave (oref_val library);
	  disconnect (oref_val connection);
	  oref_nullify library
	 )
    else raise (LibraryException "Close: No library open.")

let lib_open_eval env ehook host localport remoteport =

 (* eprintf "%s %d %d %t" host localport remoteport eflush; *)

 if oref_p library
    then raise (LibraryException "Open: Library already open.")
    else ( oref_set library (join_eval  (oref_set connection (connect host localport remoteport))
					[env]
					ehook
					)
	 ; at_exit (function () -> if oref_p library then library_close()))
 ; ()



let refiner_op = mk_nuprl5_op [ make_param (Token "!refine")]

let refine_req_p t = opeq refiner_op (operator_of_term t)

let refine_args t =
   match dest_term t with
	{term_op = op; term_terms = goal :: tac :: r } when (opeq op refiner_op)

          -> (term_of_unbound_term goal, term_of_unbound_term tac)

    | _ -> error ["eval"; "op"; "unrecognized"] [] [t]


let refine_ehook rhook =
  (function t ->
    let (goal, tac) = refine_args t in
     list_to_ilist (rhook goal tac))




let library_open_eval name rhook =

  (if not (oref_p library)
     then let host = Sys.getenv "NUPRLLIB_HOST"
	  and port = int_of_string (Sys.getenv "NUPRLLIB_PORT")
	  in
	  lib_open_eval name (refine_ehook rhook) host port (port+2))
 ; ()


let library_loop_eval () =

	let lib = oref_val library in

	(with_transaction lib
	   (function t ->
		(eval t
		 (null_ap (itext_term "\l. inform_message nil ``NuprlLight Loop Start`` nil")))))

	; server_loop lib



let library_open_and_loop_eval name rhook =

  library_open_eval name rhook;

  (unwind_error
     (function () -> library_loop_eval ();
	library_close ())
     (function () -> library_close ()))




open List

let faux_refine g t =
    print_newline();
    Mbterm.print_term g;
    print_newline();
    [g; g]
