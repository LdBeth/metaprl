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
 * Author: Yegor Bryukhov, Alexey Nogin
 *)

open Term_hash
open Term_norm
open Term_copy_weak
open Infinite_weak_array

module TermCopy2Weak
   (FromTerm : Termmod_sig.TermModuleSig)

   (FromHeader : Term_header_sig.TermHeaderSig
      with type term = FromTerm.TermType.term
      with type param = FromTerm.TermType.param
      with type meta_term = FromTerm.TermType.meta_term

      with type 'a descriptor = 'a InfiniteWeakArray.descriptor
      with type 'a weak_descriptor = 'a InfiniteWeakArray.weak_descriptor)

   (FromHash : Term_hash_sig.TermHashSig
      with type param_header = FromHeader.param_header
      with type param_weak_header = FromHeader.param_weak_header
      with type term_header = FromHeader.term_header
      with type term_weak_header = FromHeader.term_weak_header
      with type meta_term_header = FromHeader.meta_term_header
      with type meta_term_weak_header = FromHeader.meta_term_weak_header

      with type param = FromTerm.TermType.param
      with type term = FromTerm.TermType.term
      with type meta_term = FromTerm.TermType.meta_term)

   (ToTerm : Termmod_sig.TermModuleSig)

   (ToHeader : Term_header_sig.TermHeaderSig
      with type term = ToTerm.TermType.term
      with type param = ToTerm.TermType.param
      with type meta_term = ToTerm.TermType.meta_term

      with type 'a descriptor = 'a InfiniteWeakArray.descriptor
      with type 'a weak_descriptor = 'a InfiniteWeakArray.weak_descriptor)

   (ToHash : Term_hash_sig.TermHashSig
      with type param_header = ToHeader.param_header
      with type param_weak_header = ToHeader.param_weak_header
      with type term_header = ToHeader.term_header
      with type term_weak_header = ToHeader.term_weak_header
      with type meta_term_header = ToHeader.meta_term_header
      with type meta_term_weak_header = ToHeader.meta_term_weak_header

      with type param = ToTerm.TermType.param
      with type term = ToTerm.TermType.term
      with type meta_term = ToTerm.TermType.meta_term) =

struct

   module SourceNorm = TermNorm(FromTerm)(FromHeader)(FromHash)
   module TargetNorm = TermNorm(ToTerm)(ToHeader)(ToHash)
   module Forward = TermCopyWeak(FromTerm)(ToTerm)(ToHeader)(ToHash)
   module Backward = TermCopyWeak(ToTerm)(FromTerm)(FromHeader)(FromHash)

   type t = { source_hash : FromHash.t; 
              target_hash : ToHash.t
            }

   let p_create i j = { source_hash = FromHash.p_create i j;
                        target_hash = ToHash.p_create i j;
                      }

   let p_convert info t = SourceNorm.p_add info.source_hash t; Forward.p_convert info.target_hash t
   let p_revert info t = TargetNorm.p_add info.target_hash t; Backward.p_convert info.source_hash t

   let p_convert_meta info t = SourceNorm.p_add_meta info.source_hash t; Forward.p_convert_meta info.target_hash t
   let p_revert_meta info t = TargetNorm.p_add_meta info.target_hash t; Backward.p_convert_meta info.source_hash t

   let global_hash = { source_hash = FromHash.global_hash;
                       target_hash = ToHash.global_hash;
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
