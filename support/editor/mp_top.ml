(*
 * Build the toploop version.
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
open Lm_debug
open Lm_printf

let () =
   show_loading "Loading Mp_top"

module Shell = Shell.Shell (Shell_mp.ShellP4)

let () =
   show_loading "Loaded Shell"

module ShellBrowser = Shell_browser.ShellBrowser (Shell.Top)

let () =
   show_loading "Starting main loop"

(*
 * The first active shell will take everything.
 *)
let () = ShellBrowser.main ()
let () = Shell.Main.main ()

external stop_gmon : unit -> unit = "stop_gmon"

let _ =
   if not !Shell_state.batch_flag then eprintf "\nMetaPRL exiting\n";
   stop_gmon();
   flush stderr;
   flush stdout;
   exit 0

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
