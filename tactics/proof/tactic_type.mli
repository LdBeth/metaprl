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
 * See the file doc/index.html for information on Nuprl,
 * OCaml, and more information about this system.
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

module TacticType
: TacticTypeSig

module Tactic
: TacticSig
  with type attribute = TacticType.attribute
  with type arglist = TacticType.arglist

module Tacticals
: TacticalsSig
  with type tactic = Tactic.tactic
  with type tactic_arg = Tactic.tactic_arg
  with type arglist = TacticType.arglist

module Rewrite
: RewriteSig
  with type conv = Tactic.conv
  with type tactic = Tactic.tactic
  with type tactic_arg = Tactic.tactic_arg

module Conversionals
: ConversionalsSig
  with type conv = Rewrite.conv
  with type env = Rewrite.env
  with type tactic_arg = Tactic.tactic_arg
  with type tactic = Tactic.tactic

module Sequent
: SequentSig
  with type extract = Tacticals.extract
  with type tactic = Tactic.tactic
  with type tactic_arg = Tactic.tactic_arg
  with type sentinal = Tactic.sentinal
  with type conv = Conversionals.conv
  with type raw_attribute = Tactic.raw_attribute

module Proof
: ProofSig
  with type extract = Tacticals.extract
  with type tactic_arg = Tactic.tactic_arg
  with type tactic = Tactic.tactic
  with type sentinal = Tactic.sentinal
  with type raw_attribute = Tactic.raw_attribute
  with type attribute = TacticType.attribute
  with type arglist = TacticType.arglist

module TacticExn
: TacticExnSig

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
