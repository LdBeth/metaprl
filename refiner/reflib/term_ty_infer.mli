(*
 * Type inference.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2005 Mojave Group, Caltech
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
 * @email{jyh@cs.caltech.edu}
 * @end[license]
 *)
open Lm_symbol

open Opname

open Refiner.Refiner.TermType
open Refiner.Refiner.TermShape
open Refiner.Refiner.TermTy
open Refiner.Refiner.Rewrite

(*
 * Some canonical types.
 *)
val term_opname  : opname
val type_opname  : opname
val token_opname : opname

val term_type  : term
val type_type  : term
val token_type : term

val mk_ty_sequent_term   : term -> term -> term -> term
val mk_ty_hyp_term       : term -> term -> term
val mk_ty_exists_term    : var -> term -> term -> term
val mk_ty_constrain_term : term -> term -> term

(*
 * A type environment has 3 parts.
 *   tenv_typeclasses : the set of typeclasses
 *   tenv_typeenv     : the typeclass of each type
 *   tenv_termenv     : the type of each term
 *)
module Shape2Table : Lm_map_sig.LmMap with type key = shape * shape;;

type typeclasses      = OpnameSet.t OpnameTable.t
type typeenv          = opname ShapeTable.t
type typereduce       = rewrite_rule Term_match_table.term_table
type typereductions   = (term * term) Shape2Table.t
type termenv          = ty_term ShapeTable.t

type tenv =
   { tenv_typeclasses    : typeclasses;
     tenv_typereduce     : typereduce;
     tenv_typereductions : typereductions;
     tenv_typeenv        : typeenv;
     tenv_termenv        : termenv
   }

(*
 * Inference.
 *)
val infer_term         : tenv -> term -> term
val check_rule         : tenv -> meta_term -> term list -> unit
val infer_rewrite      : tenv -> meta_term -> term list -> term
val check_type_rewrite : tenv -> term -> term -> unit
val check_dform        : tenv -> term -> term -> unit
val check_iform        : tenv -> meta_term -> term list -> unit
val check_production   : tenv -> term list -> term -> unit

(*
 * Erase any terms left over for typing.
 *)
val erase_term      : term -> term
val erase_meta_term : meta_term -> meta_term

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
