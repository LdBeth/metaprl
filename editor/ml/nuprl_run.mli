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

(*
  Thread.create run_library "mp_o8" ;;
  let f x = Printf.eprintf "Name = %s\n" x; flush stderr; edit_cd_thm name x;;

  # #use "nuprl.txt";;
  # NuprlRun.run_connection  3998 4992 "baldwin" "lmp_d21";;

  # #use "nuprl.txt";;
  # run_connection_with_hook 4666 5666 "baldwin" "lmp_m20" jprover_hook;;
*)

module NuprlRun :
    sig
      val run_nuprl : unit -> unit
      val run_library : string -> unit
      val run_jprover : string -> unit
      val run_connection : int (*library*) -> int -> string (*host*)-> string -> unit
      val run_dummy_connection : int (*library*) -> int -> string (*host*)-> string -> unit
      val run_connection_with_hook : int (*library*) -> int -> string (*host*)-> string -> (Refiner.Refiner.Term.term -> Refiner.Refiner.Term.term) -> unit
    end