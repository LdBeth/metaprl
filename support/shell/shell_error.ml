(*
 * Defualt error report for edit object.
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
 * Copyright (C) 2021 LdBeth
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
 * Author: LdBeth <ldbeth@sdf.org>
 *)
open Refiner.Refiner.RefineError
open Shell_sig

type error_info =
   { package : string;
     save : string;
     check: string;
     info : string;
     interpret : string;
     get_contents : string
   }

class edit_error info : edit_action =
 let raise_error s = raise (RefineError (info.package, StringError s)) in
 object (self)
   method private raise_edit_error : 'a. string -> 'a = raise_error

   method private not_a_rule : 'a. 'a =
      raise_error "this is not a rule or rewrite"

   method edit_get_terms = self#not_a_rule
   method edit_set_goal = self#not_a_rule
   method edit_set_redex = self#not_a_rule
   method edit_set_contractum = self#not_a_rule
   method edit_set_assumptions = self#not_a_rule
   method edit_set_params = self#not_a_rule
   method edit_get_extract = self#not_a_rule
   method edit_find = self#not_a_rule

   method edit_save =
      raise_error info.save
   method edit_check =
      raise_error info.check
   method edit_info addr =
      raise_error info.info
   method edit_interpret command =
      raise_error info.interpret
   method edit_get_contents addr =
      raise_error info.get_contents

   method edit_undo addr =
      addr
   method edit_redo addr =
      addr
end