(*
 * This file includes definitions for operators,
 * terms with delayed substitution, and regular terms.
 *
 * Note: many functions in this module (most dest_* and
 * is_* functions, shape, etc) have side-effect:
 * if the term is a Subst, they push substitutions one step down
 * and _replace_ the referenced term with the resulting Term term_single.
 * alpha_equal* functions may push down and eliminate all the Substs, not
 * only the top ones.
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
 * Copyright (C) 1998 Alexey Nogin, Cornell University
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
 * Author: Alexey Nogin
 * jyh@cs.cornell.edu
 *)

open Term_ds_sig

module TermType : TermDsTypeSig
