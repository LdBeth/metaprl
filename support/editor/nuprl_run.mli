(*
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/htmlman/default.html or visit http://metaprl.org/
 * for more information.
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

(* to connect see QUICKSTART file *)

(* hint: for debugging create a thread
   # Thread.create run_nuprl ... ;;
*)

open Nuprl_sig

module NuprlRun (Nuprl: NuprlSig):
    sig
      val run_library : string -> unit  (* uses  NUPRL_HOST and NUPRL_PORT env vars *)
      val run_jprover : int (*library*) -> string (*host*) -> string (*library*) -> string (*db*) -> unit
      val run_connection : int (*library*) -> string (*host*) -> string (*library*) -> string (*db*) -> unit
      val run_connection_with_hook : int (*library*) -> string (*host*)-> string -> string (*dbpath*) -> (Refiner.Refiner.TermType.term -> Refiner.Refiner.TermType.term) -> unit
    end
