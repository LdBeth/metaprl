(*
 * Display forms for basic objects.
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
 * Author: Jason Hickey <jyh@cs.cornell.edu>
 * Modified by: Aleksey Nogin <nogin@cs.cornell.edu>
 *)

extends Perv
extends Mpfont
extends Mpsymbols

(*
 * Other commands.
 *)
declare bvar{'v : Dform} : Dform
declare " " : Dform
declare "^" : Dform
declare "_" : Dform
declare "{" : Dform
declare "}" : Dform
declare "$" : Dform
declare "[" : Dform
declare "]" : Dform
declare ";" : Dform
declare "\\" : Dform

(*
 * Sequent separator
 *)
declare seq_sep{'a : ty_sequent{'x; 'y; 'z}} : Dform

(*
 * List utilities.
 *)
declare df_length{'l : Dform} : Dform                   (* Prints l's length *)
declare df_down{'l : Dform} : Dform                     (* Prints l's length, cd to the directory in HTML mode *)
declare df_last{'l : Dform} : Dform                     (* Prints l's last element *)
declare df_concat{'sep : Dform; 'l : Dform} : Dform     (* Prints l's elements separated by sep *)
declare df_rev_concat{'sep : Dform; 'l : Dform} : Dform (* Same as df_concat, but prints l in reverse order *)
declare df_context_var[name:v] : Dform

(*
 * Before a term is passed to the display form mechanism, each sequent context <H...>
 * is turned into a hypothesis df_context{'H...} (where 'H is now a SO variable)
 *)
declare df_context{'t : Dform} : Dform

(* same as "szone 'e ezone" *)
declare szone{'e : Dform} : Dform

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
