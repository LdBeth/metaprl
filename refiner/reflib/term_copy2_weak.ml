(* This file implements terms' conversion from one Term-module
 * to another
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
open Lm_threads

open Termmod_hash_sig

open Term_copy_weak

module TermCopy2Weak
   (SourceTerm : TermModuleHashSig)
   (TargetTerm : TermModuleHashSig) =
struct

   module SourceHash = SourceTerm.TermHash
   module TargetHash = TargetTerm.TermHash
   module SourceNorm = SourceTerm.TermNorm
   module TargetNorm = TargetTerm.TermNorm
   module Forward = TermCopyWeak (SourceTerm) (TargetTerm)
   module Backward = TermCopyWeak (TargetTerm) (SourceTerm)

   type t =
      { source_hash : SourceHash.t;
        target_hash : TargetHash.t;
        locker: Mutex.t
      }

   type term_index = SourceHash.term_index * TargetHash.term_index
   type meta_term_index = SourceHash.meta_term_index * TargetHash.meta_term_index
   type msequent_index = SourceHash.msequent_index * TargetHash.msequent_index
   type term_weak_index = SourceHash.term_weak_index * TargetHash.term_weak_index
   type meta_term_weak_index = SourceHash.meta_term_weak_index * TargetHash.meta_term_weak_index
   type msequent_weak_index = SourceHash.msequent_weak_index * TargetHash.msequent_weak_index

   let weaken (i1, i2) =
      SourceHash.weaken_term i1, TargetHash.weaken_term i2
   let weaken_meta (i1, i2) =
      SourceHash.weaken_meta_term i1, TargetHash.weaken_meta_term i2
   let weaken_msequent (i1, i2) =
      SourceHash.weaken_msequent i1, TargetHash.weaken_msequent i2

   let compare (a1, a2) (b1, b2) =
      a1 = b1 && a2 = b2

   let compare_meta = compare
   let compare_msequent = compare

   (*
    * Create the bi-directional table.
    *)
   let p_create i =
      { source_hash = SourceHash.p_create i;
        target_hash = TargetHash.p_create i;
        locker = Mutex.create ()
      }

   (*
    * Terms.
    *)
   let p_add_src info t =
      Mutex.lock info.locker;
      let i1 = SourceNorm.p_add info.source_hash t in
      let i2 = Forward.p_add info.target_hash t in
         Mutex.unlock info.locker;
         (i1, i2)

   let p_add_dst info t =
      Mutex.lock info.locker;
      let i2 = TargetNorm.p_add info.target_hash t in
      let i1 = Backward.p_add info.source_hash t in
         Mutex.unlock info.locker;
         (i1, i2)

   let p_retrieve_src info (i1, _) =
      Mutex.lock info.locker;
      let r = SourceNorm.p_retrieve info.source_hash i1 in
         Mutex.unlock info.locker;
         r

   let p_retrieve_dst info (_, i2) =
      Mutex.lock info.locker;
      let r = TargetNorm.p_retrieve info.target_hash i2 in
         Mutex.unlock info.locker;
         r

   let p_convert info t =
      Mutex.lock info.locker;
      let _ = SourceNorm.p_add info.source_hash t in
      let r = Forward.p_convert info.target_hash t in
         Mutex.unlock info.locker;
         r

   let p_revert info t =
      Mutex.lock info.locker;
      let _ = TargetNorm.p_add info.target_hash t in
      let r = Backward.p_convert info.source_hash t in
         Mutex.unlock info.locker;
         r

   (*
    * Meta-terms.
    *)
   let p_add_meta_src info t =
      Mutex.lock info.locker;
      let i1 = SourceNorm.p_add_meta info.source_hash t in
      let i2 = Forward.p_add_meta info.target_hash t in
         Mutex.unlock info.locker;
         (i1, i2)

   let p_add_meta_dst info t =
      Mutex.lock info.locker;
      let i2 = TargetNorm.p_add_meta info.target_hash t in
      let i1 = Backward.p_add_meta info.source_hash t in
         Mutex.unlock info.locker;
         (i1, i2)

   let p_retrieve_meta_src info (i1, _) =
      Mutex.lock info.locker;
      let r = SourceNorm.p_retrieve_meta info.source_hash i1 in
         Mutex.unlock info.locker;
         r

   let p_retrieve_meta_dst info (_, i2) =
      Mutex.lock info.locker;
      let r = TargetNorm.p_retrieve_meta info.target_hash i2 in
         Mutex.unlock info.locker;
         r

   let p_convert_meta info t =
      Mutex.lock info.locker;
      let _ = SourceNorm.p_add_meta info.source_hash t in
      let r = Forward.p_convert_meta info.target_hash t in
         Mutex.unlock info.locker;
         r

   let p_revert_meta info t =
      Mutex.lock info.locker;
      let _ = TargetNorm.p_add_meta info.target_hash t in
      let r = Backward.p_convert_meta info.source_hash t in
         Mutex.unlock info.locker;
         r

   (*
    * MSequents.
    *)
   let p_add_msequent_src info t =
      Mutex.lock info.locker;
      let i1 = SourceNorm.p_add_msequent info.source_hash t in
      let i2 = Forward.p_add_msequent info.target_hash t in
         Mutex.unlock info.locker;
         (i1, i2)

   let p_add_msequent_dst info t =
      Mutex.lock info.locker;
      let i2 = TargetNorm.p_add_msequent info.target_hash t in
      let i1 = Backward.p_add_msequent info.source_hash t in
         Mutex.unlock info.locker;
         (i1, i2)

   let p_retrieve_msequent_src info (i1, _) =
      Mutex.lock info.locker;
      let r = SourceNorm.p_retrieve_msequent info.source_hash i1 in
         Mutex.unlock info.locker;
         r

   let p_retrieve_msequent_dst info (_, i2) =
      Mutex.lock info.locker;
      let r = TargetNorm.p_retrieve_msequent info.target_hash i2 in
         Mutex.unlock info.locker;
         r

   let p_convert_msequent info t =
      Mutex.lock info.locker;
      let _ = SourceNorm.p_add_msequent info.source_hash t in
      let r = Forward.p_convert_msequent info.target_hash t in
         Mutex.unlock info.locker;
         r

   let p_revert_msequent info t =
      Mutex.lock info.locker;
      let _ = TargetNorm.p_add_msequent info.target_hash t in
      let r = Backward.p_convert_msequent info.source_hash t in
         Mutex.unlock info.locker;
         r

   let global_hash = { source_hash = SourceHash.global_hash;
                       target_hash = TargetHash.global_hash;
                       locker = Mutex.create ()
                     }

   let add_src = p_add_src global_hash
   let add_dst = p_add_dst global_hash
   let retrieve_src = p_retrieve_src global_hash
   let retrieve_dst = p_retrieve_dst global_hash
   let convert = p_convert global_hash
   let revert = p_revert global_hash

   let add_meta_src = p_add_meta_src global_hash
   let add_meta_dst = p_add_meta_dst global_hash
   let retrieve_meta_src = p_retrieve_meta_src global_hash
   let retrieve_meta_dst = p_retrieve_meta_dst global_hash
   let convert_meta = p_convert_meta global_hash
   let revert_meta = p_revert_meta global_hash

   let add_msequent_src = p_add_msequent_src global_hash
   let add_msequent_dst = p_add_msequent_dst global_hash
   let retrieve_msequent_src = p_retrieve_msequent_src global_hash
   let retrieve_msequent_dst = p_retrieve_msequent_dst global_hash
   let convert_msequent = p_convert_msequent global_hash
   let revert_msequent = p_revert_msequent global_hash
end

(*
 * -*-
 * Local Variables:
 * Caml-master: ""
 * End:
 * -*-
 *)
