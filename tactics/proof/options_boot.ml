(*
 * Option types.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2005-2006 Mojave Group, Caltech
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
 * Author: Jason Hickey @email{jyh@cs.caltech.edu}
 * Modified by: Aleksey Nogin @email{nogin@cs.caltech.edu}
 * @end[license]
 *)
open Opname
open Refiner.Refiner.TermType
open Refiner.Refiner.TermOp
open Refiner.Refiner.RefineError

(*
 * Flags associated with options.
 *)
type option_info =
   OptionAllow
 | OptionExclude
 | OptionIgnore

type option_table =
   (int * bool) OpnameTable.t * int

type option_key = term

let string_of_option = function
   OptionAllow -> "ййallow"
 | OptionExclude -> "ййexclude"
 | OptionIgnore -> "ййignore"

(*
 * XXX: HACK: the versions without the "йй" prefix are here for the ASCII IO versions 1.0.25-26 compatibility
 *)
let option_of_string = function
   "allow" | "ййallow" -> OptionAllow
 | "exclude" | "ййexclude" -> OptionExclude
 | "ignore" | "ййignore" -> OptionIgnore
 | s -> raise (RefineError ("option_of_string", StringError (Printf.sprintf "illegal option string '%s': legal values are 'ййallow', 'ййexclude', 'ййignore'" s)))

let is_option_string = function
   "allow"
 | "exclude"
 | "ignore"
 | "ййallow"
 | "ййexclude"
 | "ййignore" ->
      true
 | _ ->
      false

let select_opname = make_opname ["select"; "Perv"]
let get_label = dest_token_term select_opname
let mk_label_term = mk_token_term select_opname

let options_empty = OpnameTable.empty, 0

let add_option (options, i) t info =
   let op = get_label t in
   let i = i + 1 in
   let options =
      match info with
         OptionAllow ->
            OpnameTable.add options op (i, true)
       | OptionExclude ->
            OpnameTable.add options op (i, false)
       | OptionIgnore ->
            OpnameTable.remove options op
   in
      options, i

let cmp ((i1:int), _) (i2, _) =
   Stdlib.compare i1 i2

let list_options (options, _) =
   let lst = OpnameTable.fold (fun l op (n, dec) -> (n, (mk_label_term op, if dec then OptionAllow else OptionExclude)) :: l) [] options in
      List.map snd (List.sort cmp lst)

let eq ((i1:int), (b1: bool)) (i2, b2) =
   i1 = i2 && b1 = b2

let options_eq ((t1, i1): option_table) (t2, i2) = (i1=i2) && OpnameTable.equal eq t1 t2

(*
 * XXX: HACK: tells whether a label was added by filter_prog or user
 *)
let automatic_labels = OpnameSet.of_list (List.map make_opname [
   [ "select_crw"; "Perv" ];
])

let automatic_label key =
   OpnameSet.mem automatic_labels (get_label key)

let rule_labels_not_allowed loc labels =
   match labels with
      None ->
         ()
    | Some l when List.for_all automatic_label l ->
         ()
    | Some _ ->
         Ploc.raise loc (RefineError ("option check", StringError "rule labels are not allowed, or the
annotation processor has not been updated"))

(************************************************************************
 * Rule labeling.
 *)

(*
 * A rule is labeled with positive and negative arguments.
 *)
type rule_labels = opname list

let rule_labels_empty = []

let test_rule_label options ((i, _) as decision) op =
   if OpnameTable.mem options op then
      let (i', _) as decision' = OpnameTable.find options op in
         if i' > i then decision' else decision
   else
      decision

let empty_start = 0, true

let rule_labels_are_allowed (options, _) labels =
   snd (List.fold_left (test_rule_label options) empty_start labels)

let rule_labels_of_terms =
   List.map get_label

let rule_labels_of_opt_terms labels =
   let labels =
      match labels with
         Some labels -> labels
       | None -> []
   in
      rule_labels_of_terms labels

(*
 * -*-
 * Local Variables:
 * End:
 * -*-
 *)
