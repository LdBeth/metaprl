(* This file implements terms' conversion from one Term-module
 * to another
 *
 * -----------------------------------------------------------------
 * This file is part of Nuprl-Light, a modular, higher order
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
 * Author: Yegor Bryukhov
 *)

open Term_hash
open Term_norm
open Term_copy_weak

module TermCopy2Weak =
  functor(Source : Termmod_sig.TermModuleSig) ->
  functor(Target   : Termmod_sig.TermModuleSig) ->
struct

   module SourceHash = TermHash(Source)
   module TargetHash = TermHash(Target)
   module SourceNorm = TermNorm(Source)
   module TargetNorm = TermNorm(Target)
   module Forward = TermCopyWeak(Source)(Target)
   module Backward = TermCopyWeak(Target)(Source)

   type t = { source_hash : SourceHash.t; 
              target_hash : TargetHash.t
            }

   let p_create i j = { source_hash = SourceHash.p_create i j;
                        target_hash = TargetHash.p_create i j;
                      }

   let p_convert info t = SourceNorm.p_add info.source_hash t; Forward.p_convert info.target_hash t
   let p_revert info t = TargetNorm.p_add info.target_hash t; Backward.p_convert info.source_hash t

   let p_convert_meta info t = SourceNorm.p_add_meta info.source_hash t; Forward.p_convert_meta info.target_hash t
   let p_revert_meta info t = TargetNorm.p_add_meta info.target_hash t; Backward.p_convert_meta info.source_hash t

   let global_hash = { source_hash = SourceHash.global_hash;
                       target_hash = TargetHash.global_hash;
                     }

   let convert = p_convert global_hash
   let revert = p_revert global_hash

   let convert_meta = p_convert_meta global_hash
   let revert_meta = p_revert_meta global_hash

end

module NormalizeTerm =
   TermCopy2Weak (Refiner_std.Refiner) (Refiner.Refiner)

let normalize_term = NormalizeTerm.convert
let normalize_meta_term = NormalizeTerm.convert_meta
let denormalize_term = NormalizeTerm.revert
let denormalize_meta_term = NormalizeTerm.revert_meta

(*
 * -*-
 * Local Variables:
 * Caml-master: ""
 * End:
 * -*-
 *)
