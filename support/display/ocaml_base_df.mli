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
 *)
extends Mpsymbols
extends Base_dform
extends Ocaml

(************************************************************************
 * DISPLAY UTILITIES                                                    *
 ************************************************************************)

(*
 * Operators.
 *)
declare "[" : TyOCaml
declare "]" : TyOCaml
declare "[|" : TyOCaml
declare "|]" : TyOCaml
declare "[<" : TyOCaml
declare ">]" : TyOCaml
declare "{" : TyOCaml
declare "}" : TyOCaml
declare "(" : TyOCaml
declare ")" : TyOCaml

declare "+" : TyOCaml
declare "-" : TyOCaml
declare "*" : TyOCaml
declare "/" : TyOCaml
declare "mod" : TyOCaml

declare "&" : TyOCaml
declare "or" : TyOCaml
declare "=" : TyOCaml
declare "==" : TyOCaml
declare "::" : TyOCaml
declare ":=" : TyOCaml
declare "." : TyOCaml
declare ".(" : TyOCaml
declare ".[" : TyOCaml
declare ":>" : TyOCaml
declare ";" : TyOCaml
declare "->" : TyOCaml
declare "|" : TyOCaml
declare "<>" : TyOCaml
declare "~" : TyOCaml
declare ":" : TyOCaml
declare "_" : TyOCaml
declare "#" : TyOCaml
declare "'" : TyOCaml
declare "\"" : TyOCaml

declare "_if" : TyOCaml
declare "_then" : TyOCaml
declare "_else" : TyOCaml

declare "_for" : TyOCaml
declare "_while" : TyOCaml
declare "_to" : TyOCaml
declare "_downto" : TyOCaml
declare "_do" : TyOCaml
declare "_done" : TyOCaml

declare "_new" : TyOCaml
declare "_fun" : TyOCaml
declare "_match" : TyOCaml
declare "_try" : TyOCaml
declare "_type" : TyOCaml
declare "_exception" : TyOCaml
declare "_let" : TyOCaml
declare "_letrec" : TyOCaml
declare "_in" : TyOCaml
declare "_and" : TyOCaml
declare "_with" : TyOCaml
declare "_val" : TyOCaml
declare "_as" : TyOCaml
declare "_external" : TyOCaml
declare "_of" : TyOCaml

declare "_module" : TyOCaml
declare "_moduletype" : TyOCaml
declare "_open" : TyOCaml
declare "_sig" : TyOCaml
declare "_struct" : TyOCaml
declare "_functor" : TyOCaml
declare "_end" : TyOCaml

declare push_indent : TyOCaml

(*
 * Display control tags.
 *)
declare patt_format{'a : TyOCaml; 'b : TyOCaml} : TyOCaml

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
