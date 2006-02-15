(*
 * Produce a reflected version of a theory.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2006 Mojave Group, Caltech
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
open Lm_debug
open Lm_printf

open Refiner.Refiner.TermType

open Filter_type
open Filter_summary
open Filter_summary_type
open Filter_prog
open Filter_util

open Filter_prog.ProofCaches

let debug_filter_reflect =
   create_debug (**)
      { debug_name = "filter_reflect";
        debug_description = "debug filter processing for reflected theories";
        debug_value = false
      }

(*
 * Show the file loading.
 *)
let () = show_loading "Loading Filter_reflect%t"

(*
 * Reflect theories are prefixed with "reflect_"
 *)
let reflect_prefix = "reflect_"

let reflect_filename orig_path =
   let old_base = Lm_filename_util.basename orig_path in
   let new_base = reflect_prefix ^ old_base in
   let new_path = Lm_filename_util.replace_basename orig_path new_base in
      new_base, new_path

(************************************************************************
 * Signature conversion.
 *
 * The only thing we care about is the extends directives,
 * where we need to convert the extensions to point to the
 * reflect_ theories.
 *)
let declare_parent_path cache loc path =
   (* Lots of errors can occur here *)
   let () =
      try SigFilterCache.inline_module cache () path with
         exn ->
            Stdpp.raise_with_loc loc exn
   in

   (* Add resources and grammar start symbols *)
   let info =
      { parent_name = path;
        parent_resources = SigFilterCache.sig_resources cache path
      }
   in
      SigFilterCache.add_command cache (Parent info, loc)

let declare_parent cache loc item =
   let { parent_name = path } = item in
   let head, name =
      try Lm_list_util.split_last path with
         Failure _ ->
            Stdpp.raise_with_loc loc (EmptyModulePath "Filter_reflect.declare_parent")
   in
   let name = reflect_prefix ^ String.uncapitalize name in
   let path = head @ [name] in
      declare_parent_path cache loc path

let compile_sig_item info (item, loc) =
   match item with
      (*
       * Supported items.
       *)
      Parent ({ parent_name = name } as parent) ->
         if !debug_filter_reflect then
            eprintf "Filter_reflect.extract_sig_item: parent: %s@." (string_of_path name);
         declare_parent info loc parent

      (*
       * Illegal items.
       *)
    | Module _ ->
         Stdpp.raise_with_loc loc (Failure "Filter_sig.extract_sig_item: nested modules are not implemented")
    | Improve _ ->
         Stdpp.raise_with_loc loc (Invalid_argument "Filter_reflect.extract_sig_item")

      (*
       * The rest are ignored.
       *)
    | Rewrite _
    | CondRewrite _
    | MLAxiom _
    | MLRewrite _
    | Rule _
    | SummaryItem _
    | MagicBlock _
    | ToploopItem _
    | Resource _
    | Prec _
    | InputForm _
    | DefineTerm _
    | DeclareTypeClass _
    | DeclareType _
    | DeclareTerm _
    | DeclareTypeRewrite _
    | DForm _
    | PrecRel _
    | Id _
    | Comment _
    | MLGramUpd _
    | PRLGrammar _ ->
         ()

let compile_sig info orig_info =
   declare_parent_path info dummy_loc ["itt_hoas_theory"];
   List.iter (compile_sig_item info) (info_items orig_info)

(************************************************************************
 * Implementation conversion.
 *)
let define_parent_path cache loc path =
   (* Lots of errors can occur here *)
   let () =
      try StrFilterCache.inline_module cache () path with
         exn ->
            Stdpp.raise_with_loc loc exn
   in

   (* Add resources and grammar start symbols *)
   let info =
      { parent_name = path;
        parent_resources = StrFilterCache.sig_resources cache path
      }
   in
      StrFilterCache.add_command cache (Parent info, loc)

let define_parent cache loc item =
   let { parent_name = path } = item in
   let head, name =
      try Lm_list_util.split_last path with
         Failure _ ->
            Stdpp.raise_with_loc loc (EmptyModulePath "Filter_reflect.define_parent")
   in
   let name = reflect_prefix ^ String.uncapitalize name in
   let path = head @ [name] in
      define_parent_path cache loc path

let compile_str_item info (item, loc) =
   match item with
      (*
       * Supported items.
       *)
      Parent ({ parent_name = name } as parent) ->
         if !debug_filter_reflect then
            eprintf "Filter_reflect.extract_sig_item: parent: %s@." (string_of_path name);
         define_parent info loc parent

      (*
       * Illegal items.
       *)
    | Module _ ->
         Stdpp.raise_with_loc loc (Failure "Filter_sig.extract_sig_item: nested modules are not implemented")
    | Improve _ ->
         Stdpp.raise_with_loc loc (Invalid_argument "Filter_reflect.extract_sig_item")

      (*
       * The rest are ignored.
       *)
    | Rewrite _
    | CondRewrite _
    | MLAxiom _
    | MLRewrite _
    | Rule _
    | SummaryItem _
    | MagicBlock _
    | ToploopItem _
    | Resource _
    | Prec _
    | InputForm _
    | DefineTerm _
    | DeclareTypeClass _
    | DeclareType _
    | DeclareTerm _
    | DeclareTypeRewrite _
    | DForm _
    | PrecRel _
    | Id _
    | Comment _
    | MLGramUpd _
    | PRLGrammar _ ->
         ()

let compile_str info orig_info =
   define_parent_path info dummy_loc ["itt_hoas_theory"];
   List.iter (compile_str_item info) (info_items orig_info)

(*
 * -*-
 * Local Variables:
 * Fill-column: 100
 * End:
 * -*-
 * vim:ts=3:et:tw=100
 *)
