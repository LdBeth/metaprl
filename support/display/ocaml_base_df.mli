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
declare "[" : Ocaml
declare "]" : Ocaml
declare "[|" : Ocaml
declare "|]" : Ocaml
declare "[<" : Ocaml
declare ">]" : Ocaml
declare "{" : Ocaml
declare "}" : Ocaml
declare "(" : Ocaml
declare ")" : Ocaml

declare "+" : Ocaml
declare "-" : Ocaml
declare "*" : Ocaml
declare "/" : Ocaml
declare "mod" : Ocaml

declare "&" : Ocaml
declare "or" : Ocaml
declare "=" : Ocaml
declare "==" : Ocaml
declare "::" : Ocaml
declare ":=" : Ocaml
declare "." : Ocaml
declare ".(" : Ocaml
declare ".[" : Ocaml
declare ":>" : Ocaml
declare ";" : Ocaml
declare "->" : Ocaml
declare "|" : Ocaml
declare "<>" : Ocaml
declare ":" : Ocaml
declare "_" : Ocaml
declare "#" : Ocaml
declare "'" : Ocaml
declare "\"" : Ocaml

declare "_if" : Ocaml
declare "_then" : Ocaml
declare "_else" : Ocaml

declare "_for" : Ocaml
declare "_while" : Ocaml
declare "_to" : Ocaml
declare "_downto" : Ocaml
declare "_do" : Ocaml
declare "_done" : Ocaml

declare "_new" : Ocaml
declare "_fun" : Ocaml
declare "_match" : Ocaml
declare "_try" : Ocaml
declare "_type" : Ocaml
declare "_exception" : Ocaml
declare "_let" : Ocaml
declare "_letrec" : Ocaml
declare "_in" : Ocaml
declare "_and" : Ocaml
declare "_with" : Ocaml
declare "_val" : Ocaml
declare "_as" : Ocaml
declare "_external" : Ocaml
declare "_of" : Ocaml

declare "_module" : Ocaml
declare "_moduletype" : Ocaml
declare "_open" : Ocaml
declare "_sig" : Ocaml
declare "_struct" : Ocaml
declare "_functor" : Ocaml
declare "_end" : Ocaml

declare push_indent : Ocaml

(*
 * Display control tags.
 *)
declare patt_format{'a : Ocaml; 'b : Ocaml} : Ocaml

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
