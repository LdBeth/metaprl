(* This file implements terms' conversion 
 * between term representation for IO (Refiner_std)
 * and the current term representation (Refiner)
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

open Term_copy2_weak

module Refiner_std = Refiner_std.Refiner
module Refiner = Refiner.Refiner
module Header_std = Term_header.TermHeader (Refiner_std)
module Header = Term_header.TermHeader (Refiner)
module Hash_std = Term_hash.TermHash (Refiner_std) (Header_std)
module Hash = Term_hash.TermHash (Refiner) (Header)
module NormalizeTerm =
   Term_copy2_weak.TermCopy2Weak (Refiner_std) (Header_std) (Hash_std) (Refiner) (Header) (Hash)

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
