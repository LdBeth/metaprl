(*
 * Display forms for basic objects.
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
 * Modified by: Aleksey Nogin <nogin@cs.cornell.edu>
 *)

extends Perv
extends Nuprl_font

(*
 * Other commands.
 *)
declare bvar{'v}
declare " "
declare "^"
declare "_"
declare "{"
declare "}"
declare "$"
declare "["
declare "]"
declare ";"
declare "\\"

(*
 * List utilities.
 *)
declare df_length{'l}          (* Prints l's length *)
declare df_last{'l}            (* Prints l's last element *)
declare df_concat{'sep;'l}     (* Prints l's elements separated by sep *)
declare df_rev_concat{'sep;'l} (* Same as df_concat, but prints l in reverse order *)
declare df_context_var[s:s]

declare szone{'e}

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
