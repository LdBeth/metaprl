(*
 * This file contains the primitive syntax and display
 * for ocaml terms.
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
 *)
extends Nuprl_font
extends Base_dform

(************************************************************************
 * DISPLAY UTILITIES                                                    *
 ************************************************************************)

(*
 * Operators.
 *)
declare "[" : Dform
declare "]" : Dform
declare "[|" : Dform
declare "|]" : Dform
declare "[<" : Dform
declare ">]" : Dform
declare "{" : Dform
declare "}" : Dform
declare "(" : Dform
declare ")" : Dform

declare "+" : Dform
declare "-" : Dform
declare "*" : Dform
declare "/" : Dform
declare "mod" : Dform

declare "&" : Dform
declare "or" : Dform
declare "=" : Dform
declare "==" : Dform
declare "::" : Dform
declare ":=" : Dform
declare "." : Dform
declare ".(" : Dform
declare ".[" : Dform
declare ":>" : Dform
declare ";" : Dform
declare "->" : Dform
declare "|" : Dform
declare "<>" : Dform
declare ":" : Dform
declare "_" : Dform
declare "#" : Dform
declare "'" : Dform
declare "\"" : Dform

declare "_if" : Dform
declare "_then" : Dform
declare "_else" : Dform

declare "_for" : Dform
declare "_while" : Dform
declare "_to" : Dform
declare "_downto" : Dform
declare "_do" : Dform
declare "_done" : Dform

declare "_new" : Dform
declare "_fun" : Dform
declare "_match" : Dform
declare "_try" : Dform
declare "_type" : Dform
declare "_exception" : Dform
declare "_let" : Dform
declare "_letrec" : Dform
declare "_in" : Dform
declare "_and" : Dform
declare "_with" : Dform
declare "_val" : Dform
declare "_as" : Dform
declare "_external" : Dform
declare "_of" : Dform

declare "_module" : Dform
declare "_moduletype" : Dform
declare "_open" : Dform
declare "_sig" : Dform
declare "_struct" : Dform
declare "_functor" : Dform
declare "_end" : Dform

declare push_indent : Dform

(*
 * Display control tags.
 *)
declare patt_format{'a : Dform; 'b} : Dform

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
