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
extends Ocaml

(************************************************************************
 * DISPLAY UTILITIES                                                    *
 ************************************************************************)

(*
 * Operators.
 *)
declare "[" : OCaml
declare "]" : OCaml
declare "[|" : OCaml
declare "|]" : OCaml
declare "[<" : OCaml
declare ">]" : OCaml
declare "{" : OCaml
declare "}" : OCaml
declare "(" : OCaml
declare ")" : OCaml

declare "+" : OCaml
declare "-" : OCaml
declare "*" : OCaml
declare "/" : OCaml
declare "mod" : OCaml

declare "&" : OCaml
declare "or" : OCaml
declare "=" : OCaml
declare "==" : OCaml
declare "::" : OCaml
declare ":=" : OCaml
declare "." : OCaml
declare ".(" : OCaml
declare ".[" : OCaml
declare ":>" : OCaml
declare ";" : OCaml
declare "->" : OCaml
declare "|" : OCaml
declare "<>" : OCaml
declare ":" : OCaml
declare "_" : OCaml
declare "#" : OCaml
declare "'" : OCaml
declare "\"" : OCaml

declare "_if" : OCaml
declare "_then" : OCaml
declare "_else" : OCaml

declare "_for" : OCaml
declare "_while" : OCaml
declare "_to" : OCaml
declare "_downto" : OCaml
declare "_do" : OCaml
declare "_done" : OCaml

declare "_new" : OCaml
declare "_fun" : OCaml
declare "_match" : OCaml
declare "_try" : OCaml
declare "_type" : OCaml
declare "_exception" : OCaml
declare "_let" : OCaml
declare "_letrec" : OCaml
declare "_in" : OCaml
declare "_and" : OCaml
declare "_with" : OCaml
declare "_val" : OCaml
declare "_as" : OCaml
declare "_external" : OCaml
declare "_of" : OCaml

declare "_module" : OCaml
declare "_moduletype" : OCaml
declare "_open" : OCaml
declare "_sig" : OCaml
declare "_struct" : OCaml
declare "_functor" : OCaml
declare "_end" : OCaml

declare push_indent : OCaml

(*
 * Display control tags.
 *)
declare patt_format{'a : OCaml; 'b : OCaml} : OCaml

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
