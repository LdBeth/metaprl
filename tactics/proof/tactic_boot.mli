(*
 * Define the common types.
 * A file with this name is required for every theory.
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
 * Copyright (C) 1998 Jason Hickey, Cornell University
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

open Tactic_boot_sig

module TacticInternalType
: TacticInternalTypeSig

module Tactic
: TacticSig
  with type tactic_arg = TacticInternalType.tactic_arg
  with type tactic = TacticInternalType.tactic
  with type conv = TacticInternalType.conv
  with type sentinal = TacticInternalType.sentinal
  with type raw_attribute = TacticInternalType.raw_attribute
  with type pre_tactic = TacticInternalType.pre_tactic
  with type extract = TacticInternalType.extract

module TacticInternal
: TacticInternalSig
  with type tactic_arg = TacticInternalType.tactic_arg
  with type tactic = TacticInternalType.tactic
  with type extract = TacticInternalType.extract
  with type conv = TacticInternalType.conv
  with type attribute_info = TacticInternalType.attribute_info
  with type raw_attribute = TacticInternalType.raw_attribute

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
