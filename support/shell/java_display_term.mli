(*
 * This is the standard interface to the window system.
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
 * Copyright (C) 1999 Jason Hickey, Cornell University
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
open Refiner.Refiner.TermType
open Dform

(*
 * A generic term window.
 *)
type t

(*
 * A proof window has three parts.
 *)
type java_proof =
   { java_proof_goal     : t;
     java_proof_rule     : t;
     java_proof_subgoals : t
   }

(*
 * Possible output events.
 *)
type event =
   TermSelection of term        (* A term has been selected *)

type callback = t -> event -> unit

(*
 * Create new windows over the given channel.
 * The dform base used to retrieve display forms.
 *)
val create_menu : Java_mux_channel.session -> dform_mode_base -> t
val create_term : Java_mux_channel.session -> dform_mode_base -> t
val create_proof : Java_mux_channel.session -> dform_mode_base -> java_proof

(*
 * Set the callback for the window.
 *)
val set_callback : t -> callback -> unit

(*
 * Set the root directory for the window.
 * All "cd" commands are interpreted relative to this.
 *)
val set_dir : t -> string -> unit

(*
 * Set the term in the window.
 * Substitution is allowed on the displayed term.
 * The first term argument is the term to be replaced,
 * and the second is the replacement.  Substitution
 * is capturing.
 *)
val set : t -> term -> unit

(*
 * -*-
 * Local Variables:
 * Caml-master: "nl"
 * End:
 * -*-
 *)
