(*
 * Used to print the FIR theory.
 *
 * ------------------------------------------------------------------------
 *
 * @begin[license]
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.  Additional
 * information about the system is available at
 * http://www.metaprl.org/
 *
 * Copyright (C) 2002 Brian Emre Aydemir, Caltech
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
 * Author: Brian Emre Aydemir
 * @email{emre@cs.caltech.edu}
 * @end[license]
 *)

set_tex_file "../../doc/latex/theories/fir/theory.tex";;

print_theory "mfir_theory";;
print_theory "mfir_basic";;
print_theory "mfir_ty";;
print_theory "mfir_exp";;
print_theory "mfir_sequent";;
print_theory "mfir_tr_base";;
print_theory "mfir_tr_types";;
print_theory "mfir_tr_atom";;
print_theory "mfir_tr_store";;
print_theory "mfir_tr_exp";;
print_theory "mfir_test";;
