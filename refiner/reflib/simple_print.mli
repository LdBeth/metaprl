(*
 * Pretty printer for terms.
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
 *
 *)

open Refiner_sig
open Simple_print_sig

module MakeSimplePrint
   (Refiner : RefinerSig)
   : SimplePrintSig 
     with type term = Refiner.TermType.term
     with type level_exp = Refiner.TermType.level_exp
     with type param = Refiner.TermType.param
     with type bound_term = Refiner.TermType.bound_term
     with type address = Refiner.TermAddr.address
     with type meta_term = Refiner.TermMeta.meta_term

module SimplePrint : SimplePrintSig
                     with type term = Refiner.Refiner.TermType.term
                     with type level_exp = Refiner.Refiner.TermType.level_exp
                     with type param = Refiner.Refiner.TermType.param
                     with type bound_term = Refiner.Refiner.TermType.bound_term
                     with type address = Refiner.Refiner.TermAddr.address
                     with type meta_term = Refiner.Refiner.TermMeta.meta_term
   
