(*
 * Some tables based on terms.
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

open Lm_splay_table
open Term_norm
open Termmod_hash_sig

(*
 * Arguments needed to make the table.
 *)
module type TableBaseSig =
sig
   type data

   val append : data list -> data list -> data list
end

(*
 * Build tables over arbitrary hash module.
 *)
module MakeEqTables (Term : TermModuleHashSig) =
struct
   (*
    * Build from splay tables.
    *)
   type 'data term_table = (Term.TermNorm.term_index, 'data) Lm_splay_table.table
   type 'data meta_term_table = (Term.TermNorm.meta_term_index, 'data) Lm_splay_table.table
   type 'data msequent_table = (Term.TermNorm.msequent_index, 'data) Lm_splay_table.table

   module MakeTable = Lm_splay_table.MakeTable

   (*
    * Extra info we need to build the table.
    *)
   module type TableBaseExtraSig =
   sig
      type term
      type index
      val hash : term -> index
      val unhash : index -> term
      val compare : index -> index -> int
   end

   (*
    * Make the real splay set info.
    *)
   module MakeSetInfo (Base : TableBaseSig) (Extra : TableBaseExtraSig) =
   struct
      type elt = Extra.index
      type data = Base.data

      let print _ _ = ()
      let compare x y = Extra.compare x y
      let append = Base.append
   end

   (*
    * Wrap the splay set.
    *)
   module MakeTableInternal (Base : TableBaseSig) (Extra : TableBaseExtraSig) =
   struct
      module Table = MakeTable (MakeSetInfo (Base) (Extra))

      type elt = Extra.term
      type data = Base.data
      type t = (Extra.index, data) table

      let empty = Table.empty
      let add table term data = Table.add table (Extra.hash term) data
      let union = Table.union
      let mem table term = Table.mem table (Extra.hash term)
      let find table term = Table.find table (Extra.hash term)
      let find_all table term = Table.find_all table (Extra.hash term)
      let remove table term = Table.remove table (Extra.hash term)
      let iter f table =
         Table.iter (fun index data -> f (Extra.unhash index) data) table
      let map f table =
         Table.map (fun index data -> f (Extra.unhash index) data) table
      let print = Table.print
   end

   (*
    * Make the extra tables.
    *)
   module TermExtra =
   struct
      type term = Term.TermNorm.term
      type index = Term.TermNorm.term_index
      let hash = Term.TermNorm.add
      let unhash = Term.TermNorm.retrieve
      let compare = Term.TermNorm.compare_terms
   end

   module MetaTermExtra =
   struct
      type term = Term.TermNorm.meta_term
      type index = Term.TermNorm.meta_term_index
      let hash = Term.TermNorm.add_meta
      let unhash = Term.TermNorm.retrieve_meta
      let compare = Term.TermNorm.compare_meta_terms
   end

   module MsequentExtra =
   struct
      type term = Term.TermNorm.msequent
      type index = Term.TermNorm.msequent_index
      let hash = Term.TermNorm.add_msequent
      let unhash = Term.TermNorm.retrieve_msequent
      let compare = Term.TermNorm.compare_msequents
   end

   (*
    * Make the tables.
    *)
   module MakeTermTable (Base : TableBaseSig) =
      MakeTableInternal (Base) (TermExtra)

   module MakeMetaTermTable (Base : TableBaseSig) =
      MakeTableInternal (Base) (MetaTermExtra)

   module MakeMsequentTable (Base: TableBaseSig) =
      MakeTableInternal (Base) (MsequentExtra)
end

(*
 * Now build the standard ones.
 *)
module EqTables = MakeEqTables (Refiner.Refiner)

type 'data term_table = 'data EqTables.term_table
type 'data meta_term_table = 'data EqTables.meta_term_table
type 'data msequent_table = 'data EqTables.msequent_table

module MakeTermTable = EqTables.MakeTermTable
module MakeMetaTermTable = EqTables.MakeMetaTermTable
module MakeMsequentTable = EqTables.MakeMsequentTable

(*
 * -*-
 * Local Variables:
 * Caml-master: "nl"
 * End:
 * -*-
 *)
