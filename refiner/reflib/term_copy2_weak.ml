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

open Term_hash
open Term_norm
open Term_copy_weak
open Infinite_weak_array

module TermCopy2Weak
   (SourceTerm : Termmod_hash_sig.TermModuleHashSig)
   (TargetTerm : Termmod_hash_sig.TermModuleHashSig) =
struct

   module SourceHash = SourceTerm.TermHash
   module TargetHash = TargetTerm.TermHash
   module SourceNorm = SourceTerm.TermNorm
   module TargetNorm = TargetTerm.TermNorm
   module Forward = TermCopyWeak(SourceTerm)(TargetTerm)
   module Backward = TermCopyWeak(TargetTerm)(SourceTerm)

   type t = { source_hash : SourceHash.t; 
              target_hash : TargetHash.t
            }

   let p_create i j = { source_hash = SourceHash.p_create i j;
                        target_hash = TargetHash.p_create i j;
                      }

   let p_convert info t = 
      let _ = SourceNorm.p_add info.source_hash t in
      Forward.p_convert info.target_hash t
   
   let p_revert info t = 
      let _ = TargetNorm.p_add info.target_hash t in
      Backward.p_convert info.source_hash t

   let p_convert_meta info t = 
      let _ = SourceNorm.p_add_meta info.source_hash t in 
      Forward.p_convert_meta info.target_hash t

   let p_revert_meta info t = 
      let _ = TargetNorm.p_add_meta info.target_hash t in 
      Backward.p_convert_meta info.source_hash t

   let global_hash = { source_hash = SourceHash.global_hash;
                       target_hash = TargetHash.global_hash;
                     }

   let convert = p_convert global_hash
   let revert = p_revert global_hash

   let convert_meta = p_convert_meta global_hash
   let revert_meta = p_revert_meta global_hash

end

(*
 * -*-
 * Local Variables:
 * Caml-master: ""
 * End:
 * -*-
 *)
