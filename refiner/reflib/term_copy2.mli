(* This file is part of Nuprl-Light, a modular, higher order
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

open Term_transfer
open Simplehashtbl
open Term_compare

type normalize

val create_norm : unit -> normalize

val normalize_term :
  normalize -> Refiner_std.Refiner.TermType.term      -> Refiner.Refiner.TermType.term
val normalize_meta_term :
  normalize -> Refiner_std.Refiner.TermType.meta_term -> Refiner.Refiner.TermType.meta_term

val denormalize_term :
  normalize -> Refiner.Refiner.TermType.term          -> Refiner_std.Refiner.TermType.term
val denormalize_meta_term :
  normalize -> Refiner.Refiner.TermType.meta_term     -> Refiner_std.Refiner.TermType.meta_term

val normalize_term_single :
  Refiner_std.Refiner.TermType.term -> Refiner.Refiner.TermType.term
val normalize_meta_term_single :
  Refiner_std.Refiner.TermType.meta_term ->
  Refiner.Refiner.TermType.meta_term

val denormalize_term_single :
  Refiner.Refiner.TermType.term -> Refiner_std.Refiner.TermType.term
val denormalize_meta_term_single :
  Refiner.Refiner.TermType.meta_term ->
  Refiner_std.Refiner.TermType.meta_term

