(*
 * We add a layer to filter Summary, to allow inlined modules
 * and cached info about opnames, axioms, and precedences.
 *
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
 *)

open Printf

open Mp_debug
open Opname
open Refiner.Refiner.TermType
open Mp_resource
open Simple_print

open File_base_type
open File_type_base

open Filter_type
open Filter_util
open Filter_ocaml
open Filter_summary
open Filter_summary_type
open Filter_summary_io

(*
 * Show the file loading.
 *)
let _ =
   show_loading "Loading Filter_cache_fun%t"

let debug_filter_cache =
   create_debug (**)
      { debug_name = "filter_cache";
        debug_description = "display cache operations during compiling";
        debug_value = false
      }

let debug_filter_path =
   create_debug (**)
      { debug_name = "filter_path";
        debug_description = "display path expansions";
        debug_value = false
      }

module FilterSummaryTerm = Filter_summary.FilterSummaryTerm (Refiner.Refiner)

open FilterSummaryTerm

(*
 * Make the enhanced base from a normal summary base.
 *)
module MakeFilterCache (**)
   (SigMarshal : MarshalSig)
   (StrMarshal : MarshalSig
    with type ctyp = SigMarshal.ctyp
    with type select = SigMarshal.select
    with type cooked = SigMarshal.cooked)
   (Base : SummaryBaseSig
    with type select = SigMarshal.select
    with type cooked = SigMarshal.cooked) =
struct
   (************************************************************************
    * TYPES                                                                *
    ************************************************************************)

   (*
    * Base types.
    *)
   type sig_proof = SigMarshal.proof
   type sig_ctyp  = SigMarshal.ctyp
   type sig_expr  = SigMarshal.expr
   type sig_item  = SigMarshal.item
   type sig_info  = (term, meta_term, sig_proof, sig_ctyp, sig_expr, sig_item) module_info
   type sig_elem  = (term, meta_term, sig_proof, sig_ctyp, sig_expr, sig_item) summary_item

   type str_proof = StrMarshal.proof
   type str_ctyp  = StrMarshal.ctyp
   type str_expr  = StrMarshal.expr
   type str_item  = StrMarshal.item
   type str_info  = (term, meta_term, str_proof, str_ctyp, str_expr, str_item) module_info
   type str_elem  = (term, meta_term, str_proof, str_ctyp, str_expr, str_item) summary_item

   type select = Base.select
   type arg = Base.arg

   (*
    * We save summaries with inherited attributes.
    * The resources are always sorted in order of their names.
    *
    * This is a "compressed" version of the complete info
    * for the file.
    *
    * The includes has the list of the summaries for all the immediate parents
    * of a given module, with the last included parent first in the list.
    *)
   type sig_summary =
      { sig_summary : Base.info;
        sig_resources : sig_ctyp resource_info list;
        sig_opnames : (string * opname) list;
        sig_includes : sig_summary list
      }

   (*
    * The main base keeps the filesystem base.
    * The summaries are saved with inherited attributes.
    *)
   type t =
      { lib : Base.t;
        mutable str_summaries : Base.info list;
        mutable sig_summaries : sig_summary list
      }

   (*
    * This is the extra info we keep for each module.
    * 1. There is an opname translator that maps strings
    *    to complete opnames.
    * 2. Collect all the precedences
    * 3. Collect all the resources
    * 4. A list of all inlined modules
    * 5. The current module_info
    *)
   type info =
      { opprefix : opname;
        optable : (string, opname) Hashtbl.t;
        mutable summaries : sig_summary list;

        (* Names of precedences in this module *)
        mutable precs : string list;

        (* List of resources, and where they come from *)
        mutable resources : (module_path * str_ctyp resource_info) list;

        (*
         * Info about self.
         * info is the summary of this module.
         *)
        mutable info : str_info;
        self : Base.info;
        name : string;

        (*
         * Keep a link to the summary base.
         *)
        base : t;
        select : select
      }

   (* Hook that is called whenever a module is loaded *)
   type 'a hook = info -> module_path * sig_info -> 'a -> 'a

   (************************************************************************
    * BASE OPERATIONS                                                      *
    ************************************************************************)

   (*
    * Create the bases.
    *)
   let create path =
      { lib = Base.create path;
        str_summaries = [];
        sig_summaries = []
      }

   let set_path { lib = base } path =
      Base.set_path base path

   (*
    * Take a partial pathname and expand it with all the intervening modules.
    * This function works within a summary.  Raises Not_found on failure.
    *)
   let expand_in_summary =
      let rec aux path sum = function
         [modname] ->
            (* If only one name left, it should name a term *)
            let rec search = function
               ((Opname { opname_name = str }, _)::tl) when str = modname ->
                  modname :: path
             | _::tl ->
                  search tl
             | [] ->
                  raise Not_found
            in
               search (info_items sum)

       | modname::tl ->
            (* Modname should name a module in the current summary *)
            let rec search path' = function
               (Module (n, sum''), _)::sum' ->
                  (* Check if this name matches *)
                  let path'' = n :: path' in
                     if n = modname then
                        aux path'' sum'' tl
                     else
                        begin
                           try search path'' (info_items sum'') with
                              Not_found -> search path' sum'
                        end
             | _::sum' ->
                  search path' sum'
             | [] ->
                  raise Not_found
            in
               search path (info_items sum)

       | [] ->
            raise (Invalid_argument "expand_in_summary")
      in
      let aux path sum =
         if !debug_filter_cache then
            eprintf "Filter_cache.expand_in_summary: %s%t" (string_of_path path) eflush;
         let path' = List.rev (aux [] sum path) in
            if !debug_filter_cache then
               eprintf "Filter_cache.expand_in_summary: expanded to %s%t" (string_of_path path') eflush;
            path'
      in
         aux

   (*
    * Expand the summary across all the opened modules.
    * Search for the head module in the list of modules, and
    * if that fails, search for the module in all submodules.
    *)
   let expand_path cache path =
      if !debug_filter_cache or !debug_filter_path then
         eprintf "Filter_cache.expand_path: %s%t" (string_of_path path) eflush;
      let { base = { lib = base; sig_summaries = summaries } } = cache in
         match path with
            modname::modpath ->
               (* First search for head module in top level modules *)
               let rec head_search = function
                  [] ->
                     None
                | { sig_summary = info }::t ->
                     let modname' = Base.name base info in
                        if !debug_filter_path then
                           eprintf "Filter_cache.expand_path.head_search: %s vs. %s%t" modname' modname eflush;
                        if modname' = modname then
                           Some info
                        else
                           head_search t
               in
               let rec mod_search = function
                  [] ->
                     raise Not_found
                | { sig_summary = info }::tl ->
                     let modname' = Base.name base info in
                        try modname' :: (expand_in_summary path (SigMarshal.unmarshal (Base.info base info))) with
                           Not_found ->
                              mod_search tl
               in
               let path' =
                  match head_search summaries with
                     Some info ->
                        modname :: (expand_in_summary modpath (SigMarshal.unmarshal (Base.info base info)))
                   | None ->
                        mod_search summaries
               in
                  path'
          | [] ->
               raise (Invalid_argument "expand_path")

   (************************************************************************
    * OPERATOR NAME TABLE                                                  *
    ************************************************************************)

   (*
    * These are the standard opnames.
    * This should change at some point; either
    * by moving these opnames into a global file,
    * or by making nothing global.
    *)
   let standard_opnames =
      ["lzone"; "hzone"; "szone"; "izone"; "tzone"; "ezone";
       "hbreak"; "sbreak"; "space"; "hspace"; "newline";
       "pushm"; "popm"; "pushfont"; "popfont";
       "parens"; "prec"; "internal"; "mode"; "except_mode"; "slot";
       "sequent"; "hyp"; "concl"; "var"; "context"]

   (*
    * Make a new hashtable for mapping opnames.
    *)
   let create_optable () =
      let t = Hashtbl.create 79 in
      let add s =
         Hashtbl.add t s (mk_opname s nil_opname)
      in
         List.iter add standard_opnames;
         t

   (*
    * Get an opname.
    *)
   let optable cache = function
      str ->
         Hashtbl.find cache.optable str

   (*
    * Get the prefix.
    *)
   let op_prefix cache =
      cache.opprefix

   (*
    * Add a map to the table.
    *)
   let add_opname cache str name =
      if !debug_opname then
         eprintf "Filter_cache_fun.add_opname: %s.%s%t" str (SimplePrint.string_of_opname name) eflush;
      Hashtbl.add cache.optable str name

   (*
    * Remove the map.
    *)
   let rm_opname cache str =
      Hashtbl.remove cache.optable str

   (*
    * Construct an opname.
    * If there is only one word, then this opname is declared in the current
    * module.  Otherwise it is prefixed by one or more module names, which
    * specify it more exactly.
    *)
   let mk_opname cache = function
      [] ->
         raise (Invalid_argument "mk_opname")

    | [str] ->
         (* Name in this module *)
         begin
            try
               let opname = optable cache str in
                  if !debug_opname then
                     eprintf "Filter_cache_fun.mk_opname: %s%t" (**)
                        (SimplePrint.string_of_opname opname) eflush;
                  opname
            with
               Not_found ->
                  raise (Failure (sprintf "undeclared name: %s" str))
         end

    | path ->
         (* The head serves to specify the name more precisely *)
         let path' =
            try expand_path cache path with
               Not_found ->
                  raise (BadCommand ("no object with name: " ^ (string_of_opname_list path)))
         in
         let opname = make_opname (List.rev path') in
            if !debug_opname then
               eprintf "Filter_cache_fun.mk_opname: path: %s%t" (**)
                  (SimplePrint.string_of_opname opname) eflush;
            opname

   (************************************************************************
    * ACCESS                                                               *
    ************************************************************************)

   (*
    * Projection.
    *)
   let info { info = info } =
      info

   (*
    * Find a summary by its module path.
    *)
   let find_summarized_sig_module cache path =
      let { base = { lib = base; sig_summaries = summaries } } = cache in
      let compare { sig_summary = info } =
         Base.pathname base info = path
      in
         List_util.find compare summaries

   (*
    * Get a previous module.
    *)
   let sub_info cache path =
      let info = find_summarized_sig_module cache path in
         SigMarshal.unmarshal (Base.info cache.base.lib info.sig_summary)

   (*
    * Find a summary by its module path.
    *)
   let find_summarized_str_module base path =
      let { lib = base; str_summaries = summaries } = base in
      let compare info =
         Base.pathname base info = path
      in
         List_util.find compare summaries

   (*
    * Inherited access.
    *)
   let proofs cache         = Filter_summary.get_proofs cache.info
   let parents cache        = Filter_summary.parents cache.info
   let find cache           = Filter_summary.find cache.info
   let find_axiom cache     = Filter_summary.find_axiom cache.info
   let find_rewrite cache   = Filter_summary.find_rewrite cache.info
   let find_mlrewrite cache = Filter_summary.find_mlrewrite cache.info
   let find_mlaxiom cache   = Filter_summary.find_mlaxiom cache.info
   let find_dform cache     = Filter_summary.find_dform cache.info
   let find_prec cache name = List.mem name cache.precs
   let resources cache      = cache.resources

   (*
    * Get the resources of a parent.
    *)
   let sig_resources { base = { lib = base; sig_summaries = summaries } } name =
      let rec search = function
         sum :: tl ->
            let { sig_summary = info; sig_resources = resources } = sum in
            let name' = Base.pathname base info in
               if name' = name then
                  resources
               else
                  search tl
       | [] ->
            raise Not_found
      in
         search summaries

   (************************************************************************
    * UPDATE                                                               *
    ************************************************************************)

   (*
    * Add a command to the summary.
    *)
   let add_command cache item =
      cache.info <- Filter_summary.add_command cache.info item

   let set_command cache item =
      try cache.info <- Filter_summary.set_command cache.info item with
         Not_found ->
            add_command cache item

   (*
    * Add a resource.
    *)
   let add_resource cache rsrc =
      cache.resources <- ([], rsrc) :: cache.resources

   (*
    * Add a precedence.
    *)
   let add_prec cache s =
      cache.precs <- s :: cache.precs

   (*
    * See if a resource is in a list.
    *)
   let resource_member { resource_name = name } resources =
      let rec search = function
         (_, { resource_name = name' }) :: t ->
            name = name' or search t
       | [] ->
            false
      in
         search resources

   (*
    * Resources are compared by name.
    *)
   let compare_resources { resource_name = name } { resource_name = name' } =
      name <= name'

   (*
    * Merge two lists, removing duplicates.
    *)
   let merge_resources rsrc1 rsrc2 =
      let rec merge = function
         ((h1 :: t1) as l1), ((h2 :: t2) as l2) ->
            let { resource_name = name1 } = h1 in
            let { resource_name = name2 } = h2 in
               if name1 = name2 then
                  h1 :: merge (t1, t2)
               else if name1 < name2 then
                  h1 :: merge (t1, l2)
               else
                  h2 :: merge (l1, t2)
       | [], l2 ->
            l2
       | l1, [] ->
            l1
      in
         merge (rsrc1, rsrc2)

   (*
    * Inline a module into the current one.
    * The inline_{sig,str}_components function return
    * the inherited attributes.
    *)
   let rec inline_sig_components barg arg path self items =
      let cache, _, _ = arg in

      (* Get the opname for this path *)
      let opprefix = make_opname path in

      (* Get all the sub-summaries *)
      let inline_component resources_opnames_includes (item, _) =
         match item with
            Module (n, _) ->
               (*
                * The contained summaries become top level.
                * BUG: this code needs to compute resources
                * and opnames correctly if we want nested modules.
                *)
               let base = cache.base in
               let info = Base.sub_info base.lib self n in
               let info =
                  { sig_summary = info;
                    sig_resources = [];
                    sig_opnames = [];
                    sig_includes = []
                  }
               in
                  base.sig_summaries <- info :: base.sig_summaries;
                  resources_opnames_includes

          | Opname { opname_name = str; opname_term = t } ->
               (* Hash this name to the full opname *)
               let opname = Opname.mk_opname str opprefix in
               let resources, opnames, includes = resources_opnames_includes in
                  if !debug_opname then
                     eprintf "Filter_cache_fun.inline_sig_components: add opname %s%t" (**)
                        (SimplePrint.string_of_opname opname) eflush;
                  Hashtbl.add cache.optable str opname;
                  resources, (str, opname) :: opnames, includes

          | Parent { parent_name = path } ->
               (* Recursive inline of all ancestors *)
               let info' = inline_sig_module barg arg path in
               let resources, opnames, includes = resources_opnames_includes in
               let this_resources, par_resources = resources in
               let includes = info' :: includes in
                  (this_resources, merge_resources info'.sig_resources par_resources),
                  opnames, includes

          | Resource rsrc ->
               let resources, opnames, includes = resources_opnames_includes in
               let this_resources, par_resources = resources in
                  if not (resource_member rsrc cache.resources) then
                     cache.resources <- (path, rsrc) :: cache.resources;
                  (rsrc :: this_resources, par_resources), opnames, includes

          | Prec p ->
               let precs = cache.precs in
                  if not (List.mem p precs) then
                     cache.precs <- p :: precs;
                  resources_opnames_includes

          | _ ->
               resources_opnames_includes
      in
      let (this_resources, par_resources), opnames, includes = List.fold_left inline_component (([], []), [], []) items in
      let this_resources = Sort.list compare_resources this_resources in
         { sig_summary = self;
           sig_resources = merge_resources this_resources par_resources;
           sig_opnames = opnames;
           sig_includes = includes
         }

   and inline_str_components barg arg path self (items : (term, meta_term, str_proof, str_ctyp, str_expr, str_item) summary_item_loc list) =
      let cache, _, _ = arg in

      (* Get the opname for this path *)
      let opprefix = make_opname path in

      (* Get all the sub-summaries *)
      let inline_component (item, _) =
         match item with
            Module (n, _) ->
               (*
                * The contained summaries become top level.
                * BUG: see BUG in inline_sig_components above.
                *)
               let base = cache.base in
               let info = Base.sub_info base.lib self n in
               let info = {
                  sig_summary = info;
                  sig_resources = [];
                  sig_opnames = [];
                  sig_includes = []
               } in
                  base.sig_summaries <- info :: base.sig_summaries

          | Opname { opname_name = str; opname_term = t } ->
               (* Hash this name to the full opname *)
               let opname = Opname.mk_opname str opprefix in
                  if !debug_opname then
                     eprintf "Filter_cache_fun.inline_sig_components: add opname %s%t" (**)
                        (SimplePrint.string_of_opname opname) eflush;
                  Hashtbl.add cache.optable str opname

          | Parent { parent_name = path } ->
               (* Recursive inline of all ancestors *)
               let _ = inline_sig_module barg arg path in
                  ()

          | _ ->
               ()
      in
         List.iter inline_component items

   and inline_sig_module barg arg path =
      let cache, inline_hook, vals = arg in
      let base = cache.base.lib in
      let base_info =
         try Base.find base barg path SigMarshal.select AnySuffix with
            Not_found ->
               eprintf "Can't find module %s%t" (string_of_path path) eflush;
               raise Not_found
      in
         if !debug_filter_cache then
            eprintf "FilterCache.inline_module': %s%t" (string_of_path path) eflush;
         try
            let info = find_summarized_sig_module cache path in
               if !debug_filter_cache then
                  eprintf "FilterCache.inline_module': %s: already loaded%t" (string_of_path path) eflush;
               collect_opnames cache info;
               info
         with
            Not_found ->
               if !debug_filter_cache then
                  eprintf "FilterCache.inline_module': finding: %s%t" (string_of_path path) eflush;
               let info' = SigMarshal.unmarshal (Base.info base base_info) in
               let info =
                  (* Inline the subparts *)
                  inline_sig_components barg arg path base_info (info_items info')
               in
                  (* This module gets listed in the inline stack *)
                  cache.base.sig_summaries <- info :: cache.base.sig_summaries;
                  cache.summaries <- info :: cache.summaries;

                  (* Call the hook *)
                  if !debug_filter_cache then
                     begin
                        eprintf "Summary: %s%t" (string_of_path path) eflush;
                        eprint_info info'
                     end;
                  vals := inline_hook cache (path, info') !vals;

                  info

   and collect_opnames cache info =
      if not (List.memq info cache.summaries) then begin
         List_util.rev_iter (collect_opnames cache) info.sig_includes;
         let optable = cache.optable in
         List_util.rev_iter (fun (str, op) -> Hashtbl.add optable str op) info.sig_opnames;
         cache.summaries <- info :: cache.summaries
      end

   let inline_module cache barg path inline_hook arg =
      try
         let vals = ref arg in
         let info = inline_sig_module barg (cache, inline_hook, vals) path in
            SigMarshal.unmarshal (Base.info cache.base.lib info.sig_summary), !vals
      with
         Not_found ->
            raise (BadCommand (sprintf "Theory is not found: %s" (String_util.concat "." path)))

   (*
    * To create, need:
    *    1. module_base
    *    2. Load path
    *)
   let create_cache base name self_select child_select =
      { opprefix = Opname.mk_opname (String.capitalize name) nil_opname;
        optable = create_optable ();
        summaries = [];
        precs = [];
        resources = [];
        info = new_module_info ();
        self = Base.create_info base.lib self_select "." name;
        name = name;
        base = base;
        select = self_select
      }

   (*
    * When a cache is loaded follow the steps to inline
    * the file into a new cache.
    *)
   let load base barg (name : module_name) (my_select : select) (child_select : select) (hook : 'a hook) (arg : 'a) suffix =
      let vals = ref arg in
      let path = [name] in
      let info =
         try find_summarized_str_module base path with
            Not_found ->
               let info = Base.find base.lib barg path my_select suffix in
                  base.str_summaries <- info :: base.str_summaries;
                  info
      in
      let info' = StrMarshal.unmarshal (Base.info base.lib info) in
      let cache =
         { opprefix = Opname.mk_opname (String.capitalize name) nil_opname;
           optable = create_optable ();
           summaries = [];
           precs = [];
           resources = [];
           info = info';
           self = info;
           name = name;
           base = base;
           select = my_select
         }
      in
         if !debug_filter_cache then
            begin
               eprintf "Filter_cache.load: loaded %s%t" name eflush;
               eprint_info info'
            end;
         inline_str_components barg (cache, hook, vals) [String.capitalize name] info (info_items info');
         cache, !vals (* hook cache (path, info') !vals *)

   (*
    * Get the filename of the info.
    *)
   let filename { lib = base } { self = info } =
      Base.file_name base info

   let name { name = name } =
      name

   (*
    * Get the signature for the module.
    *)
   let sig_info cache barg alt_select =
      let { base = base; self = self; name = name; optable = optable } = cache in
      let { lib = lib; sig_summaries = summaries } = base in
      let info =
         try (find_summarized_sig_module cache [name]).sig_summary with
            Not_found ->
               (*
                * nogin: This used to also create a sig_summary and add it
                * to base.sig_summaries, but we no longer have enough information
                * to do that (BUG?)
                *)
               Base.find_match lib barg self alt_select AnySuffix
      in
         SigMarshal.unmarshal (Base.info lib info)

   (*
    * Check the implementation with its interface.
    *)
   let check cache barg alt_select =
      let sig_info = sig_info cache barg alt_select in
      let id = find_id sig_info in
         add_command cache (Id id, (0, 0));
         check_implementation cache.info sig_info;
         sig_info

   (*
    * Copy the proofs from the summary.
    *)
   let copy_proofs cache barg copy_proof =
      let { name = name;
            base = base;
            info = info;
            select = my_select
          } = cache
      in
      let path = [name] in
      let info' = Base.find_file base.lib barg path my_select (OnlySuffixes ["prlb"; "prla"]) in
      let _ =
         (* Save the .prlb if the loaded file is newer *)
         Base.set_magic base.lib info' 1;
         Base.save_if_newer base.lib barg info' (OnlySuffixes ["prlb"])
      in
      let info' = StrMarshal.unmarshal (Base.info base.lib info') in
         (* Copy the proofs *)
         cache.info <- FilterSummaryTerm.copy_proofs copy_proof info info'

   (*
    * Upgrade the file mode.
    *)
   let set_mode cache mode =
      let magic =
         match mode with
            CompiledSummary ->
               0
          | InteractiveSummary ->
               1
      in
      let { base = { lib = base }; self = self } = cache in
         Base.set_magic base self magic

   (*
    * Save the cache.
    *)
   let save cache barg suffix =
      let { base = { lib = base }; self = self; info = info } = cache in
         if !debug_filter_cache then
            begin
               eprintf "Filter_cache.save: begin%t" eflush;
               eprint_info info
            end;
         Base.set_info base self (StrMarshal.marshal info);
         Base.save base barg self suffix;
         if !debug_filter_cache then
            eprintf "Filter_cache.save: done%t" eflush

   (*
    * Debugging.
    *)
   let eprint_info { info = info } =
      Filter_summary.eprint_info info
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
