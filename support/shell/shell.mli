(*
 * General purpose toploop.
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
 * Author: Jason Hickey <jyh@cs.cornell.edu>
 * Modifed By: Aleksey Nogin <nogin@cs.caltech.edu>
 *)

extends Mptop
extends Proof_edit
extends Package_info
extends Shell_rewrite
extends Shell_rule
extends Shell_package
extends Shell_root
extends Shell_p4_sig

open Shell_sig
open Shell_p4_sig

module Shell (ShellP4 : ShellP4Sig): ShellSig

(* Navigation and display. *)
topval cd : string -> string
topval pwd : unit -> string
topval set_dfmode : string -> unit

(* Module commands. *)
topval create_pkg : string -> unit
topval set_writeable : unit -> unit
topval save : unit -> unit
topval export : unit -> unit
topval save_all : unit -> unit

(* Other utilities *)
topval set_debug : string -> bool -> unit
topval stop_gmon : unit -> unit
topval restart_gmon : unit -> unit
topval print_gc_stats : unit -> unit

(*
 * -*-
 * Local Variables:
 * Caml-master: "editor.top"
 * End:
 * -*-
 *)
