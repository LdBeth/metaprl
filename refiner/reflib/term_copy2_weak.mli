(* This file is an interface for terms' conversion
 * From one Term-module to another
 *
 * -----------------------------------------------------------------
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/index.html for information on Nuprl,
 * OCaml, and more information about this system.
 *
 * Copyright (C) 1998 Yegor Bryukhov, Moscow State University
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
 * Author: Yegor Bryukhov, Alexey Nogin
 *)
open Refiner_sig

module TermCopy2Weak (**)
   (FromTerm : RefinerSig)
   (ToTerm : RefinerSig) :
sig
   (*
    * Pair of Term_hash's structures
    *)
   type t

   (*
    * Reimplement the indexing.
    * THESE INDEXES CANNOT BE COMPARED WITH ==!
    *)
   type term_index
   type term_weak_index
   type meta_term_index
   type meta_term_weak_index
   type msequent_index
   type msequent_weak_index

   (*
    * Operations on indexes.
    *)
   val weaken : term_index -> term_weak_index
   val weaken_meta : meta_term_index -> meta_term_weak_index
   val weaken_msequent : msequent_index -> msequent_weak_index

   val compare : term_weak_index -> term_weak_index -> bool
   val compare_meta : meta_term_weak_index -> meta_term_weak_index -> bool
   val compare_msequent : msequent_weak_index -> msequent_weak_index -> bool

   (*
    * Parameterized versions.
    *)
   val p_create : int -> t

   val p_add_src :
      t -> FromTerm.TermType.term -> term_index
   val p_add_dst :
      t -> ToTerm.TermType.term -> term_index
   val p_retrieve_src :
      t -> term_index -> FromTerm.TermType.term
   val p_retrieve_dst :
      t -> term_index -> ToTerm.TermType.term
   val p_convert :
      t -> FromTerm.TermType.term -> ToTerm.TermType.term
   val p_revert :
      t -> ToTerm.TermType.term -> FromTerm.TermType.term

   val p_add_meta_src :
      t -> FromTerm.TermType.meta_term -> meta_term_index
   val p_add_meta_dst :
      t -> ToTerm.TermType.meta_term -> meta_term_index
   val p_retrieve_meta_src :
      t -> meta_term_index -> FromTerm.TermType.meta_term
   val p_retrieve_meta_dst :
      t -> meta_term_index -> ToTerm.TermType.meta_term
   val p_convert_meta :
      t -> FromTerm.TermType.meta_term -> ToTerm.TermType.meta_term
   val p_revert_meta :
      t -> ToTerm.TermType.meta_term -> FromTerm.TermType.meta_term

   val p_add_msequent_src :
      t -> FromTerm.Refine.msequent -> msequent_index
   val p_add_msequent_dst :
      t -> ToTerm.Refine.msequent -> msequent_index
   val p_retrieve_msequent_src :
      t -> msequent_index -> FromTerm.Refine.msequent
   val p_retrieve_msequent_dst :
      t -> msequent_index -> ToTerm.Refine.msequent
   val p_convert_msequent :
      t -> FromTerm.Refine.msequent -> ToTerm.Refine.msequent
   val p_revert_msequent :
      t -> ToTerm.Refine.msequent -> FromTerm.Refine.msequent

(*
 * Pair of synonyms to Term_hash's globals
 *)
   val global_hash : t

   val add_src :
      FromTerm.TermType.term -> term_index
   val add_dst :
      ToTerm.TermType.term -> term_index
   val retrieve_src :
      term_index -> FromTerm.TermType.term
   val retrieve_dst :
      term_index -> ToTerm.TermType.term
   val convert : FromTerm.TermType.term -> ToTerm.TermType.term
   val revert : ToTerm.TermType.term -> FromTerm.TermType.term

   val add_meta_src :
      FromTerm.TermType.meta_term -> meta_term_index
   val add_meta_dst :
      ToTerm.TermType.meta_term -> meta_term_index
   val retrieve_meta_src :
      meta_term_index -> FromTerm.TermType.meta_term
   val retrieve_meta_dst :
      meta_term_index -> ToTerm.TermType.meta_term
   val convert_meta :
      FromTerm.TermType.meta_term -> ToTerm.TermType.meta_term
   val revert_meta :
      ToTerm.TermType.meta_term -> FromTerm.TermType.meta_term

   val add_msequent_src :
      FromTerm.Refine.msequent -> msequent_index
   val add_msequent_dst :
      ToTerm.Refine.msequent -> msequent_index
   val retrieve_msequent_src :
      msequent_index -> FromTerm.Refine.msequent
   val retrieve_msequent_dst :
      msequent_index -> ToTerm.Refine.msequent
   val convert_msequent :
      FromTerm.Refine.msequent -> ToTerm.Refine.msequent
   val revert_msequent :
      ToTerm.Refine.msequent -> FromTerm.Refine.msequent
end

(*
 * -*-
 * Local Variables:
 * Caml-master: ""
 * End:
 * -*-
 *)
