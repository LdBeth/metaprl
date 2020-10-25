(*
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2004 Mojave Group, Caltech
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
 * Modified By: Aleksey Nogin @email{nogin@cs.caltech.edu}
 * @end[license]
 *)
open Printf

open Refiner.Refiner.RefineError

(*
 * Options to the "ls" command.
 *)
type ls_option =
   LsRewrites
 | LsRules
 | LsUnjustified
 | LsDisplay
 | LsFormal
 | LsInformal
 | LsParent
 | LsAll
 | LsDocumentation

   (*
    * File listings.
    *)
 | LsFileAll
 | LsFileModifiers

   (*
    * Browser-only modes:
    *   LsHandles: display handles to allow selection of arbitrary subterms.
    *)
 | LsHandles
 | LsExternalEditor
 | LsLineNumbers

(*
 * LS options.
 *)
module LsOptionCompare =
struct
   type t = ls_option
   let compare = Stdlib.compare
end

module LsOptionSet = Lm_set.LmMake (LsOptionCompare)

(*
 * Folding.
 *)
let string_fold f x s =
   let len = String.length s in
   let rec collect x i =
      if i = len then
         x
      else
         collect (f x s.[i]) (succ i)
   in
      collect x 0

(*
 * Get the option for a char.
 *)
let option_of_char c =
   match c with
      'R' ->
         LsRules
    | 'r' ->
         LsRewrites
    | 'u' ->
         LsUnjustified
    | 'f' ->
         LsFormal
    | 'i' ->
         LsInformal
    | 'd' ->
         LsDisplay
    | 'p' ->
         LsParent
    | 'a' ->
         LsAll
    | 'D' ->
         LsDocumentation
    | 'H' ->
         LsHandles
    | 'E' ->
         LsExternalEditor
    | 'A' ->
         LsFileAll
    | 'F' ->
         LsFileModifiers
    | 'L' ->
         LsLineNumbers
    | _ ->
         raise (RefineError ("ls", StringError (sprintf "unrecognized option '%s'" (Char.escaped c))))

(*
 * Inverse-operation.
 *)
let char_of_option option =
   match option with
      LsRules -> 'R'
    | LsRewrites -> 'r'
    | LsUnjustified -> 'u'
    | LsFormal -> 'f'
    | LsInformal -> 'i'
    | LsAll -> 'a'
    | LsDisplay -> 'd'
    | LsParent -> 'p'
    | LsDocumentation -> 'D'
    | LsHandles -> 'H'
    | LsExternalEditor -> 'E'
    | LsFileAll -> 'A'
    | LsFileModifiers -> 'F'
    | LsLineNumbers -> 'L'

(*
 * Translate string options to LS options.
 *)
let ls_options_of_string s =
   string_fold (fun options c ->
         LsOptionSet.add options (option_of_char c)) LsOptionSet.empty s

let string_of_ls_options options =
   let buf = Buffer.create 10 in
      LsOptionSet.iter (fun option ->
            Buffer.add_char buf (char_of_option option)) options;
      Buffer.contents buf

(*
 * Default options.
 *)
let ls_default_list = [LsFormal; LsParent; LsRules; LsRewrites; LsDocumentation; LsFileModifiers]

let ls_options_default =
   LsOptionSet.of_sorted_list ls_default_list

(*
 * Set some additional options.
 * Make sure the set is consistent.
 *)
let ls_options_add options s =
   string_fold (fun options c ->
         let option = option_of_char c in
            match option with
               LsFormal ->
                  let options = LsOptionSet.remove options LsUnjustified in
                     LsOptionSet.add_list options [ option; LsRules; LsRewrites ]
             | LsRules
             | LsRewrites ->
                  let options = LsOptionSet.remove options LsUnjustified in
                     LsOptionSet.add options option
             | LsUnjustified ->
                  let options = LsOptionSet.subtract_list options (**)
                     [ LsFormal; LsRules; LsRewrites; LsInformal; LsDisplay; LsParent; LsDocumentation ]
                  in
                     LsOptionSet.add options option
             | LsInformal ->
                  let options = LsOptionSet.remove options LsUnjustified in
                     LsOptionSet.add_list options [ option; LsParent; LsDisplay; LsDocumentation ]
             | LsDisplay
             | LsDocumentation
             | LsParent ->
                  let options = LsOptionSet.remove options LsUnjustified in
                     LsOptionSet.add options option
             | LsHandles
             | LsExternalEditor
             | LsFileModifiers
             | LsLineNumbers
             | LsFileAll
             | LsAll ->
                  LsOptionSet.add options option) options s

(*
 * Clear some additional options.
 * Make sure the set is consistent.
 *)

let ls_options_clear options s =
   string_fold (fun options c ->
         let option = option_of_char c in
            match option with
               LsRules
             | LsRewrites ->
                  let options = LsOptionSet.remove options LsFormal in
                     LsOptionSet.remove options option
             | LsDisplay
             | LsDocumentation
             | LsParent ->
                  let options = LsOptionSet.remove options LsInformal in
                     LsOptionSet.remove options option
             | LsFormal ->
                  let options = LsOptionSet.remove options LsRules in
                  let options = LsOptionSet.remove options LsRewrites in
                     LsOptionSet.remove options LsFormal
             | LsInformal ->
                  LsOptionSet.subtract_list options [ LsParent; LsDisplay; LsDocumentation; LsInformal ]
             | LsUnjustified ->
                  let options = LsOptionSet.add_list options ls_default_list in
                     LsOptionSet.remove options option
             | LsHandles
             | LsExternalEditor
             | LsFileModifiers
             | LsLineNumbers
             | LsFileAll
             | LsAll ->
                  LsOptionSet.remove options option) options s

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
