(* This file is an interface for terms' conversion
 * between term representation for IO (Refiner_std)
 * and the current term representation (Refiner)
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
open Refiner_io
open Refiner.Refiner.TermType

val normalize_term : Refiner_io.TermType.term -> term
val normalize_meta_term : Refiner_io.TermType.meta_term -> meta_term
val denormalize_term : term -> Refiner_io.TermType.term
val denormalize_meta_term : meta_term -> Refiner_io.TermType.meta_term

(*
 * -*-
 * Local Variables:
 * Caml-master: ""
 * End:
 * -*-
 *)
