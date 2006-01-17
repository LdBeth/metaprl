(*
 * Resource management. See doc/resources_spec.txt for more information.
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
 * Copyright (C) 2001 Aleksey Nogin, Cornell University
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
 * Author: Aleksey Nogin <nogin@cs.cornell.edu>
 *)
open Lm_debug
open Lm_symbol
open Lm_printf

open Lm_string_set

open Rewrite_sig
open Refiner.Refiner.Refine

(*
 * Show loading of the file.
 *)
let _ =
   show_loading "Loading Mp_resource%t"

let debug_resource =
   create_debug (**)
      { debug_name = "resource";
        debug_description = "display resource operations";
        debug_value = false
      }

module Table = StringMTable

(************************************************************************
 * TYPES                                                                *
 ************************************************************************)

type 'input data_cell =
   DatInclude of string
 | DatData of 'input list (* last added first *)
 | DatBookmark of string

type 'input data =
   'input data_cell list (* last added first *)

type 'input raw_data = (string, 'input data) Hashtbl.t

type bookmark = string * string

type 'input increment = {
   inc_bookmark  : bookmark;
   inc_increment : 'input Table.t
}

type ('input, 'output) processor = {
   proc_is_local  : bool;
   proc_add       : 'input -> unit;
   proc_retrieve  : unit -> 'output;
   proc_clone     : unit -> ('input, 'output) processor
}

type ('input, 'intermediate, 'output) funct_processor = {
   fp_is_local : bool;
   fp_empty    : 'intermediate;
   fp_add      : 'intermediate -> 'input -> 'intermediate;
   fp_retr     : 'intermediate -> 'output
}

type ('input, 'intermediate, 'output) imper_processor = {
   imp_is_local : bool;
   imp_create   : unit -> 'intermediate;
   imp_add      : 'intermediate -> 'input -> unit;
   imp_retr     : 'intermediate -> 'output
}

type ('input, 'output) proc_result = {
   res_proc: ('input, 'output) processor;
   mutable res_result: 'output option
}

type ('input, 'intermediate, 'output) resource_info =
   Imperative of ('input, 'intermediate, 'output) imper_processor
 | Functional of ('input, 'intermediate, 'output) funct_processor

type global_resource = bookmark

type ('pre_tactic, 'input) poly_annotation_processor =
   string ->            (* Name of the new rule *)
   rewrite_args_spec -> (* Names of the context vars parameters *)
   term list ->         (* Term parameters *)
   meta_term ->         (* Rule statement *)
   MLast.loc ->         (* Location of the rule *)
   'pre_tactic ->       (* Tactic.pre_tactic *)
   'input list

type ('prim_rewrite, 'input) poly_rw_annotation_processor =
   string ->            (* Name of the new rewrite *)
   term ->              (* Redex *)
   term ->              (* Contractum *)
   term list ->         (* Assumptions *)
   rewrite_args_spec -> (* Names of the context vars parameters *)
   term list ->         (* Term arguments *)
   MLast.loc ->         (* Location of the rewrite *)
   'prim_rewrite ->     (* Refine.prim_rewrite *)
   'input list

(*
 * The global state of all the resources in the system.
 *)
type global_state =
   { (* Theory name  ->  theory resources (local + includes names) *)
     mutable raw_data        : (string * Obj.t) raw_data;
     (* A list of DatInclude for all the theories we've seen *)
     mutable top_data        : (string * Obj.t) data;
     (* Theory name -> theory parents *)
     mutable theory_includes : (string, StringSet.t) Hashtbl.t;
     (* Bookmark -> bookmark increment *)
     mutable bookmarker      : (bookmark, Obj.t increment) Hashtbl.t;
     (* Resource name -> (bookmark -> processed resource) *)
     mutable processed_data  : (string, (bookmark, (Obj.t, Obj.t) proc_result) Hashtbl.t) Hashtbl.t;
     (* Resource name -> is local to each theory *)
     mutable local_resources : (string, bool) Hashtbl.t
   }

(*
 * State that we keep while processing data from an individual theory
 *)
type local_state =
   { mutable data  : (string * Obj.t) data;
     mutable names : StringSet.t;
   }

(************************************************************************
 * IMPLEMENTATION                                                       *
 ************************************************************************)

let state =
   { raw_data        = Hashtbl.create 19;
     top_data        = [];
     theory_includes = Hashtbl.create 19;
     bookmarker      = Hashtbl.create 19;
     processed_data  = Hashtbl.create 19;
     local_resources = Hashtbl.create 19
   }

let local_state =
   { data = [];
     names = StringSet.empty;
   }

let improve name (data:'input) =
   match local_state.data with
      (DatData l_data) :: l_tail ->
         local_state.data <- DatData ((name,data) :: l_data) :: l_tail
    | l_data ->
         local_state.data <- DatData [name, data] :: l_data

let improve_list name data =
   List.iter (improve name) data

let bookmark name =
   if StringSet.mem local_state.names name then
      raise(Invalid_argument("Mp_resource.bookmark: bookmark "^name^" aready exists in this theory"));
   local_state.data <- DatBookmark name :: local_state.data;
   local_state.names <- StringSet.add local_state.names name

let extends_theory name =
   if Hashtbl.mem state.raw_data name then
      local_state.data <- DatInclude name :: local_state.data
   else
      eprintf "Mp_resource: warning: included theory %s does not have resources%t" name eflush

let empty_bookmark = "",""
let theory_bookmark name = name, ""

let top_name = "_$top_resource$_"
let top_bookmark = theory_bookmark top_name

let clear () =
   state.theory_includes <- Hashtbl.create 19;
   state.bookmarker <- Hashtbl.create 19;
   let pd = Hashtbl.create 19 in
   let clear_res s t =
      let t' = Hashtbl.create 19 in
         Hashtbl.add t' empty_bookmark (Hashtbl.find t empty_bookmark);
         Hashtbl.add pd s t'
   in
      Hashtbl.iter clear_res state.processed_data;
      state.processed_data <- pd

let close_theory name =
   if Hashtbl.mem state.raw_data name then
      raise(Invalid_argument("Mp_resource.close_theory: theory "^name^" aready exists"));

   (*
    * Remove all the entries from resources that are local.
    *)
   let local = state.local_resources in
   let data =
      List.fold_left (fun data item ->
            match item with
               DatData items ->
                  let items =
                     List.fold_left (fun items item ->
                           let name, _ = item in
                              if Hashtbl.find local name then
                                 items
                              else
                                 item :: items) [] items
                  in
                     if items = [] then
                        data
                     else
                        DatData (List.rev items) :: data
             | DatInclude _
             | DatBookmark _ ->
                  item :: data) [] local_state.data
   in
   let data = List.rev data in
      Hashtbl.add state.raw_data name data;
      state.top_data <- DatInclude name :: state.top_data;
      local_state.data <- [];
      local_state.names <- StringSet.empty

let add_data (name, data) incr =
   Table.add incr name data

let rec collect_include_aux incr includes = function
   [] ->
      incr, includes
 | DatBookmark _ :: tail ->
      collect_include_aux incr includes tail
 | DatData data :: tail ->
      let incr, includes = collect_include_aux incr includes tail in
         List.fold_right add_data data incr, includes
 | DatInclude name :: tail ->
      let incr, includes = collect_include_aux incr includes tail in
         if StringSet.mem includes name then incr, includes
         else collect_include incr name includes

and collect_include incr name includes =
   let data = Hashtbl.find state.raw_data name in
   let incr, includes = collect_include_aux incr includes data in
      incr, StringSet.add includes name

let rec compute_aux name top_includes = function
   [] ->
      empty_bookmark, Table.empty, top_includes
 | DatInclude name' :: _ when StringSet.mem top_includes name' ->
      raise (Invalid_argument("Mp_resource: theory " ^ name ^ " extends " ^ name' ^ " twice"))
 | [DatInclude name'] ->
      if not (Hashtbl.mem state.theory_includes name') then compute_data name';
      let includes = Hashtbl.find state.theory_includes name' in
      let top_includes' = StringSet.add top_includes name' in
      if StringSet.intersectp includes top_includes then
         (* Can not take theory as a whole, have to process its data *)
         (* XXX: Could be made somewhat more efficient *)
         let incr, includes = collect_include Table.empty name' top_includes' in
         empty_bookmark, incr, includes
      else
         (* Take theory as a whole *)
         (theory_bookmark name'), Table.empty, (StringSet.union top_includes' includes)
 | DatData data :: tail ->
      let bookmark, incr , includes = compute_aux name top_includes tail in
      bookmark, List.fold_right add_data data incr, includes
 | DatBookmark bk :: tail ->
      let bookmark, incr, includes = compute_aux name top_includes tail in
      let bkmrk = (name, bk) in
      if Hashtbl.mem state.bookmarker bkmrk then
         raise (Invalid_argument ("Mp_resource: duplicate bookmark " ^ name ^ "." ^ bk));
      Hashtbl.add state.bookmarker bkmrk {
         inc_bookmark = bookmark;
         inc_increment = incr
      };
      bkmrk, Table.empty, includes
  | DatInclude name' :: tail ->
      let bookmark, incr, includes = compute_aux name (StringSet.add top_includes name') tail in
      let incr, includes = collect_include incr name' includes in
      bookmark, incr, includes

and compute_data name =
   let bookmark, incr, includes =
      compute_aux name StringSet.empty (Hashtbl.find state.raw_data name)
   in
      Hashtbl.add state.theory_includes name includes;
      Hashtbl.add state.bookmarker (theory_bookmark name) {
         inc_bookmark = bookmark;
         inc_increment = incr
      }

let make_fun_proc fun_proc =
   let rec clone data =
      let data_ref = ref data in
      let add data = data_ref := fun_proc.fp_add !data_ref data in
      let retrieve () = fun_proc.fp_retr !data_ref in
      let clone () = clone !data_ref in
         { proc_is_local = fun_proc.fp_is_local;
           proc_add      = add;
           proc_retrieve = retrieve;
           proc_clone    = clone;
         }
   in
      clone fun_proc.fp_empty

let make_proc_functional imp_proc =
   let result l =
      let dat = imp_proc.imp_create () in
         Lm_list_util.rev_iter (imp_proc.imp_add dat) l;
         imp_proc.imp_retr dat
   in
      { fp_is_local = imp_proc.imp_is_local;
        fp_empty    = [];
        fp_add      = (fun l d -> d::l);
        fp_retr     = result
      }

let make_processor = function
   Functional fp -> make_fun_proc fp
 | Imperative imp -> make_fun_proc (make_proc_functional imp)

let obj_processor (proc : ('input, 'output) processor) =
   (Obj.magic proc : (Obj.t, Obj.t) processor)

let make_resource name proc =
   if Hashtbl.mem state.processed_data name then
      raise (Invalid_argument ("Mp_resource.create_resource: resource with name " ^ name ^ "already exists!"));
   let proc_data = Hashtbl.create 19 in
   let processor = make_processor proc in
      Hashtbl.add proc_data empty_bookmark {
         res_proc = obj_processor processor;
         res_result = None
      };
      Hashtbl.add state.processed_data name proc_data;
      Hashtbl.add state.local_resources name processor.proc_is_local

let get_result = function
   { res_result = Some res } -> res
 | { res_proc = proc } as res_pr ->
      let res = proc.proc_retrieve () in
         res_pr.res_result <- Some res;
         res

let find ((name, _) as bookmark) =
   if not (Hashtbl.mem state.theory_includes name) then
      compute_data name;
   ignore(Hashtbl.find state.bookmarker bookmark);
   bookmark

let get_resource bookmark resource_name =
   let data = Hashtbl.find state.processed_data resource_name in
   let rec extract_bookmark bookmark =
      if Hashtbl.mem data bookmark then
         Hashtbl.find data bookmark
      else begin
         let incr = Hashtbl.find state.bookmarker bookmark in
         let proc = extract_bookmark incr.inc_bookmark in
         let incr = Table.find_all incr.inc_increment resource_name in
         let proc =
            if incr == [] then
               proc
            else
               let proc = proc.res_proc.proc_clone () in
                  Lm_list_util.rev_iter proc.proc_add incr;
                  { res_proc = proc;
                    res_result = None
                  }
         in
            Hashtbl.add data bookmark proc;
            proc
      end
   in
      get_result (extract_bookmark bookmark)

let create_resource name proc =
   make_resource name proc;
   (fun bookmark -> Obj.obj (get_resource bookmark name))

let recompute_top () =
   if Hashtbl.mem state.raw_data top_name then begin
      Hashtbl.remove state.raw_data top_name;
      Hashtbl.remove state.bookmarker top_bookmark;
      Hashtbl.remove state.theory_includes top_name;
      Hashtbl.iter (fun _ t -> Hashtbl.remove t top_bookmark) state.processed_data
   end;
   Hashtbl.add state.raw_data top_name state.top_data;
   ignore(find(top_bookmark))

let rec get_parents_aux prnts = function
   [] -> prnts
 | DatInclude name :: tl ->
      get_parents_aux (name::prnts) tl
 | _ :: tl ->
      get_parents_aux prnts tl

let clear_results bookmark =
   let clear_results_aux _ data =
      if Hashtbl.mem data bookmark then
         (Hashtbl.find data bookmark).res_result <- None
   in
      Hashtbl.iter clear_results_aux state.processed_data

let get_parents name =
   try get_parents_aux [] (Hashtbl.find state.raw_data name) with
      Not_found ->
         raise (Invalid_argument("Mp_resource.get_parents: unknown theory " ^ name))
