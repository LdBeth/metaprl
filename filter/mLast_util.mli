(*
 * Handle commenting.
 * We parse a file, and scan in the comments and their
 * locations.  then we pass through the terms, and match their
 * locations with the comments.
 *
 * ----------------------------------------------------------------
 *
 * This file is part of Nuprl-Light, a modular, higher order
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

(*
 * Iteration functions.
 *)
type 'a fold =
   { fold_expr             : 'a -> MLast.expr -> 'a;
     fold_patt             : 'a -> MLast.patt -> 'a;
     fold_type             : 'a -> MLast.ctyp -> 'a;
     fold_sig_item         : 'a -> MLast.sig_item -> 'a;
     fold_str_item         : 'a -> MLast.str_item -> 'a;
     fold_module_expr      : 'a -> MLast.module_expr -> 'a;
     fold_module_type      : 'a -> MLast.module_type -> 'a;
     fold_with_constr      : 'a -> MLast.with_constr -> 'a;
     fold_class_type_infos : 'a -> MLast.class_type MLast.class_infos -> 'a;
     fold_class_expr_infos : 'a -> MLast.class_expr MLast.class_infos -> 'a;
     fold_class_expr       : 'a -> MLast.class_expr -> 'a;
     fold_class_field      : 'a -> MLast.class_field -> 'a;
     fold_class_type       : 'a -> MLast.class_type -> 'a;
     fold_class_type_field : 'a -> MLast.class_type_field -> 'a
   }

(*
 * Ast folders.
 *)
val fold_expr : 'a fold -> 'a -> MLast.expr -> 'a
val fold_patt : 'a fold -> 'a -> MLast.patt -> 'a
val fold_type : 'a fold -> 'a -> MLast.ctyp -> 'a
val fold_sig_item : 'a fold -> 'a -> MLast.sig_item -> 'a
val fold_str_item : 'a fold -> 'a -> MLast.str_item -> 'a
val fold_module_expr : 'a fold -> 'a -> MLast.module_expr -> 'a
val fold_module_type : 'a fold -> 'a -> MLast.module_type -> 'a
val fold_with_constr : 'a fold -> 'a -> MLast.with_constr -> 'a

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
