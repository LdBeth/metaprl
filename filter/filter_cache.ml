(*
 * We add a layer to filterSummry, to allow inlined modules
 * and cached info about opnames, axioms, and precedences.
 *
 *)

open Printf

open Debug
open Opname
open Term

open Filter_debug
open Filter_type
open Filter_summary
open Filter_summary_io

(************************************************************************
 * TYPES                                                                *
 ************************************************************************)

(*
 * We keep a record of inlined modules and:
 *    1. Their name
 *    2. Their complete name
 *    3. Their info.
 *)
type moduleSummary =
   { mod_name : string;
     mod_fullname : module_path;
     mod_info : module_info
   }

(*
 * For a specific module, we include a list of inlined modules,
 * as well as:
 *    1. opname translation,
 *    2. axiom variable recording
 *    3. prec variable recording
 *    4. inlined modules
 *    5. summaryItems
 *)
type module_cache =
   { 
      (* 
       * Opname management:
       *    prefix: the prefix for opnames in this module.
       *    optable: a hashtable for finding opnames from strings
       *)
      opprefix : opname;
      optable : (string, opname) Hashtbl.t;

      (* Names of precedences in this module *)
      precs : (string list) ref;
      
      (* List of resources, and where they come from *)
      resources : ((module_path * resource_info) list) ref;
      
      (*
       * Summaries of modules.
       * load_path is search path for finding module summaries from the filesystem.
       * summaries is the list of inlined module summaries
       * modules is the list of modules that have been inlined
       * new_summary is the summary of this module.
       *)
      base : module_base_io;
      mutable summaries : moduleSummary list;
      mutable info : module_info
   }

(*
 * Abbreviation.
 *)
type 'a module_inline_hook = module_cache -> (module_path * module_info) -> 'a

(************************************************************************
 * OPERATOR NAME TABLE                                                  *
 ************************************************************************)

(* These are the standard opnames *)
let standard_opnames =
   ["lzone"; "hzone"; "szone"; "ezone";
    "break"; "sbreak"; "space"; "hspace"; "newline";
    "pushm"; "popm";
    "parens"; "prec"; "mode"; "slot";
    "sequent"; "hyp"; "concl"; "var"]

(*
 * Make a nw hashtable.
 *)
let new_optable () =
   let t = Hashtbl.create 79 in
   let insert s =
      Hashtbl.add t s (make_opname [s])
   in
      List.iter insert standard_opnames;
      t

(*
 * Get an opname.
 *)
let find_opname cache str =
   Hashtbl.find cache.optable str

(*
 * Get the prefix.
 *)
let get_opprefix cache = cache.opprefix

(*
 * Add a map to the table.
 *)
let add_opname cache str name =
   Hashtbl.add cache.optable str name

(*
 * Remove the map.
 *)
let rm_opname cache str =
   Hashtbl.remove cache.optable str

(************************************************************************
 * ACCESS                                                               *       
 ************************************************************************)

(*
 * Projection.
 *)
let get_module_info cache = cache.info

(*
 * Take a partial pathname and expand it with all the intervening modules.
 * This function works within a summary.  Raises Not_found on failure.
 *)
let expand_in_summary =
   let rec aux path sum = function
      [modname] ->
         (* If only one name left, it should name a term *)
         let rec search = function
            ((Opname { opname_name = str })::tl) when str = modname ->
               modname :: path
          | _::tl ->
               search tl
          | [] -> raise Not_found
         in
            search (info_items sum)

    | modname::tl ->
         (* Modname should name a module in the current summary *)
         let rec search path' = function
            Module (n, sum'')::sum' ->
               (* Check if this name matches *)
               let path'' = n :: path' in
                  if n = modname then
                     aux path'' sum'' tl
                  else
                  begin
                     try search path'' (info_items sum'') with
                        Not_found -> search path' sum'
                  end
          | x::sum' ->
               search path' sum'
          | [] ->
               raise Not_found
         in
            search path (info_items sum)

    | [] ->
         raise (Invalid_argument "expand_in_summary")
   in
   let aux path sum = List.rev (aux [] sum path) in
      aux

(*
 * Expand the summary across all the opened modules.
 * Search for the head module in the list of modules, and
 * if that fails, search for the module in all submodules.
 *)
let expand_path cache = function
   [] -> raise (Invalid_argument "expand_path")
 | (modname::modpath) as path ->
      (* First search for head module in top level modules *)
      let rec head_search = function
         [] -> None
       | { mod_name = name; mod_info = sum }::t ->
            if name = modname then
               Some sum
            else
               head_search t
      in
      let rec mod_search = function
         [] -> raise Not_found
       | { mod_name = name; mod_info = sum }::tl ->
            try name :: (expand_in_summary path sum) with
               Not_found -> mod_search tl
      in
      let summaries = cache.summaries in
      let path' =
         match head_search summaries with
            Some sum ->
               modname :: (expand_in_summary modpath sum)
          | None ->
               mod_search summaries
      in
         path'

(*
 * Find a summary in a list.
 *)
let find_summary summaries name =
   let rec aux = function
      [] -> None
    | { mod_name = name'; mod_info = info }::_ when name' = name -> Some info
    | _::t -> aux t
   in
      aux summaries

(*
 * Find a summry by its module path.
 *)
let find_summarized_module summaries path id =
   let rec aux = function
      [] -> None
    | info::tl ->
         if info.mod_fullname = path & (id = ignore_id or find_id info.mod_info = id) then
            Some info
         else
            aux tl
   in
      aux summaries

(*
 * Opname lookup.
 *)
let get_optable cache = function str -> Hashtbl.find cache.optable str

(*
 * Inherited access.
 *)
let find_axiom cache     = Filter_summary.find_axiom cache.info
let find_rewrite cache   = Filter_summary.find_rewrite cache.info
let find_mlterm cache    = Filter_summary.find_mlterm cache.info
let find_condition cache = Filter_summary.find_condition cache.info
let find_dform cache     = Filter_summary.find_dform cache.info
let find_id cache        = Filter_summary.find_id cache.info

let find_prec cache name = List.mem name !(cache.precs)

let get_all_resources cache = !(cache.resources)

(************************************************************************
 * UPDATE                                                               *
 ************************************************************************)

(*
 * Add a command to the summary.
 *)
let add_command cache item =
   let info' = Filter_summary.add_command cache.info item in
      cache.info <- info';
      item

(*
 * Add a precedence.
 *)
let add_prec cache s =
   cache.precs := s :: !(cache.precs)

(*
 * Add a resource.
 * Eliminate duplicates.
 *)
let add_resource cache path rsrc =
   let rec rsrc_member = function
      (path', rsrc')::t ->
         if path = path' & rsrc.resource_name = rsrc'.resource_name then
            true
         else
            rsrc_member t
    | [] ->
         false
   in
      if not (rsrc_member !(cache.resources)) then
         Ref_util.push (path, rsrc) cache.resources

(*
 * Inline a module into the current one.
 *)
let rec inline_components' ((cache, _, _) as arg) path items =
   (* Get the opname for this path *)
   let opprefix = make_opname path in

   (* Get all the sub-summaries *)
   let inline_component = function
      Module (n, info) ->
         (* The contained summaries become top level *)
         cache.summaries <-
            { mod_name = n;
              mod_fullname = path @ [n];
              mod_info = info
            }::cache.summaries

    | Opname { opname_name = str; opname_term = t } ->
         (* Hash this name to the full opname *)
         let opname = Opname.mk_opname str opprefix in
            Hashtbl.add cache.optable str opname

    | Prec name ->
         (* This becomes a local prec *)
         Ref_util.push name cache.precs

    | Parent path' ->
         (* Recursive inline of all ancestors *)
         inline_module' arg path' ignore_id; ()

    | _ -> ()
   in
      List_util.rev_iter inline_component items

and inline_module' ((cache, inline_hook, vals) as arg) path id =
   match find_summarized_module cache.summaries path id with
      None ->
         let info = Filter_summary_io.find_module cache.base path id in
            if debug_resource then
               begin
                  eprintf "Summary: %s%t" (string_of_path path) eflush;
                  eprint_info info
               end;

            (* Inline the parts of the summary *)
            cache.summaries <-
               { mod_name = List_util.last path;
                 mod_fullname = path;
                 mod_info = info
               }::cache.summaries;
            
            (* Inline the subparts *)
            inline_components' arg path (info_items info);

            (* Call the hook *)
            vals := (inline_hook cache (path, info)) :: !vals;
            
            info
         
    | Some summary -> summary.mod_info

let inline_components cache path items inline_hook =
   let vals = ref [] in
      inline_components' (cache, inline_hook, vals) path items;
      List.rev !vals

let inline_module cache path id inline_hook =
   let vals = ref [] in
   let info = inline_module' (cache, inline_hook, vals) path id in
      info, List.rev !vals

(*
 * Inline a module by its handle.
 *)
let find_cache info summaries =
   let rec aux = function
      h::t ->
         if h.mod_info == info then
            Some h
         else
            aux t
    | [] -> None
   in
      aux summaries

let inline_info pcache (info, vals) inline_hook =
   let base = pcache.base in
   let name = module_name base info in
   let path = module_fullname base info in
   let vals' = ref vals in
      if find_cache info pcache.summaries = None then
         begin
            (* Inline the summary *)
            pcache.summaries <-
               { mod_name = name;
                 mod_fullname = path;
                 mod_info = info
               }::pcache.summaries;
      
            (* Call the hook *)
            vals' := (inline_hook pcache (path, info)) :: vals;
         
            (* Inline the subparts *)
            inline_components' (pcache, inline_hook, vals') path (info_items info)
         end;
      info, !vals'

let inline_cache pcache ccache hook =
   inline_info pcache (ccache.info, []) hook

(*
 * Get a previous module.
 *)
let get_sub_module_info cache path =
   match find_summarized_module cache.summaries path ignore_id with
      None -> raise Not_found
    | Some summary -> summary.mod_info

(************************************************************************
 * CREATION                                                             *
 ************************************************************************)

(*
 * To create, need:
 *    1. module_base
 *    2. Load path
 *)
let new_module_cache base name =
   { opprefix = Opname.mk_opname name nil_opname;
     optable = new_optable ();
     precs = ref [];
     base = base;
     summaries = [];
     resources = ref [];
     info = new_module_info ()
   }

(*
 * To create, need:
 *    1. module_base
 *    2. Load path
 *)
let load_module cache name id hook =
   let info, vals = inline_module cache [name] id hook in
      cache.info <- info;
      info, vals

(*
 * Load a cache given a full pathname.
 *)
let get_module_cache base name filename id hook =
   let cache = new_module_cache base name in
   let info, vals = inline_info cache (load_module cache filename id hook) hook in
      cache.info <- info;
      cache, vals

(*
 * $Log$
 * Revision 1.1  1997/04/28 15:50:51  jyh
 * This is the initial checkin of Nuprl-Light.
 * I am porting the editor, so it is not included
 * in this checkin.
 *
 * Directories:
 *     refiner: logic engine
 *     filter: front end to the Ocaml compiler
 *     editor: Emacs proof editor
 *     util: utilities
 *     mk: Makefile templates
 *
 * Revision 1.3  1996/10/23 15:17:54  jyh
 * First working version of dT tactic.
 *
 * Revision 1.2  1996/09/25 22:51:55  jyh
 * Initial "tactical" commit.
 *
 * Revision 1.1  1996/09/02 19:42:45  jyh
 * Semi working package management.
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
