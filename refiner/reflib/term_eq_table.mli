(*
 * This is a simple implementation of a hash table.
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

open Refiner_sig

(*
 * Arguments needed to make the table.
 *)
module type TableBaseSig =
sig
   type data

   val append : data list -> data list -> data list
end

module MakeEqTables (Term : RefinerSig) :
sig
   type 'data term_table
   type 'data meta_term_table
   type 'data msequent_table

   module MakeTermTable
      (Base : TableBaseSig)
   : Lm_map_sig.TableSig
     with type t = Base.data term_table
     with type elt = Term.TermNorm.term
     with type data = Base.data

   module MakeMetaTermTable
      (Base : TableBaseSig)
   : Lm_map_sig.TableSig
     with type t = Base.data meta_term_table
     with type elt = Term.TermNorm.meta_term
     with type data = Base.data

   module MakeMsequentTable
      (Base : TableBaseSig)
   : Lm_map_sig.TableSig
     with type t = Base.data msequent_table
     with type elt = Term.TermNorm.msequent
     with type data = Base.data
end

(*
 * Normal modules.
 *)
type 'data term_table
type 'data meta_term_table
type 'data msequent_table

module MakeTermTable
   (Base : TableBaseSig)
: Lm_map_sig.TableSig
  with type t = Base.data term_table
  with type elt = Refiner.Refiner.TermNorm.term
  with type data = Base.data

module MakeMetaTermTable
   (Base : TableBaseSig)
: Lm_map_sig.TableSig
  with type t = Base.data meta_term_table
  with type elt = Refiner.Refiner.TermNorm.meta_term
  with type data = Base.data

module MakeMsequentTable
   (Base : TableBaseSig)
: Lm_map_sig.TableSig
  with type t = Base.data msequent_table
  with type elt = Refiner.Refiner.TermNorm.msequent
  with type data = Base.data

(*
 * -*-
 * Local Variables:
 * Caml-master: "nl"
 * End:
 * -*-
 *)
