(*
 * We add a layer to filterSummry, to allow inlined modules
 * and cached info about opnames, axioms, and precedences.
 *
 *)

open Printf

open Debug
open Opname
open Term

open File_base_type
open File_type_base

open Filter_debug
open Filter_type
open Filter_util
open Filter_ocaml
open Filter_summary
open Filter_summary_type
open Filter_summary_io

(*
 * Make the enhanced base from a normal summary base.
 *)
module MakeFilterCache 
(SigMarshal : MarshalSig)
(StrMarshal : MarshalSig
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
   type sig_info  = (sig_proof, sig_ctyp, sig_expr, sig_item) module_info
   type sig_elem  = (sig_proof, sig_ctyp, sig_expr, sig_item) summary_item
   
   type str_proof = StrMarshal.proof
   type str_ctyp  = StrMarshal.ctyp
   type str_expr  = StrMarshal.expr
   type str_item  = StrMarshal.item
   type str_info  = (str_proof, str_ctyp, str_expr, str_item) module_info
   type str_elem  = (str_proof, str_ctyp, str_expr, str_item) summary_item
   
   type select = Base.select
   
   (*
    * The main base keeps the filesystem base,
    * as well as a list of summaries that have been loaded so far.
    *)
   type t =
      { lib : Base.t;
        mutable str_summaries : Base.info list;
        mutable sig_summaries : Base.info list
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
        base : t
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
         if debug_filter_cache then
            eprintf "Filter_cache.expand_in_summary: %s%t" (string_of_path path) eflush;
         let path' = List.rev (aux [] sum path) in
            if debug_filter_cache then
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
      if debug_filter_cache then
         eprintf "Filter_cache.expand_path: %s%t" (string_of_path path) eflush;
      let { base = { lib = base; sig_summaries = summaries } } = cache in
         match path with
            modname::modpath ->
               (* First search for head module in top level modules *)
               let rec head_search = function
                  [] ->
                     raise Not_found
                | info::t ->
                     let modname' = Base.name base info in
                        if modname' = modname then
                           Some info
                        else
                           head_search t
               in
               let rec mod_search = function
                  [] ->
                     raise Not_found
                | info::tl ->
                     let modname' = Base.name base info in
                        try modname' :: (expand_in_summary path (SigMarshal.unmarshal (Base.info base info))) with
                           Not_found -> mod_search tl
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
      ["lzone"; "hzone"; "szone"; "ezone";
       "break"; "sbreak"; "space"; "hspace"; "newline";
       "pushm"; "popm"; "pushfont"; "popfont";
       "parens"; "prec"; "mode"; "slot";
       "sequent"; "hyp"; "concl"; "var"]

   (*
    * Make a new hashtable for mapping opnames.
    *)
   let create_optable () =
      let t = Hashtbl.create 79 in
      let add s =
         Hashtbl.add t s (make_opname [s])
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
            try optable cache str with
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
            make_opname (List.rev path')
   
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
      let compare info =
         Base.pathname base info = path
      in
         List_util.find compare summaries

   (*
    * Get a previous module.
    *)
   let sub_info cache path =
      let info = find_summarized_sig_module cache path in
         SigMarshal.unmarshal (Base.info cache.base.lib info)

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
   let parents cache        = Filter_summary.parents cache.info
   let find_axiom cache     = Filter_summary.find_axiom cache.info
   let find_rewrite cache   = Filter_summary.find_rewrite cache.info
   let find_mlterm cache    = Filter_summary.find_mlterm cache.info
   let find_condition cache = Filter_summary.find_condition cache.info
   let find_dform cache     = Filter_summary.find_dform cache.info
   let find_prec cache name = List.mem name cache.precs
   let resources cache      = cache.resources

   (************************************************************************
    * UPDATE                                                               *
    ************************************************************************)

   (*
    * Add a command to the summary.
    *)
   let add_command cache item =
      cache.info <- Filter_summary.add_command cache.info item
   
   (*
    * Add a precedence.
    *)
   let add_prec cache s =
      cache.precs <- s :: cache.precs

   (*
    * Add a resource.
    * Eliminate duplicates.
    *)
   let add_resource cache path rsrc =
      let { resource_name = name } = rsrc in
      let rec rsrc_member = function
         (path', rsrc')::t ->
            if path' = path & rsrc'.resource_name = name then
               true
            else
               rsrc_member t
       | [] ->
            false
      in
         if not (rsrc_member cache.resources) then
            cache.resources <- (path, rsrc) :: cache.resources

   (*
    * Inline a module into the current one.
    *)
   let rec inline_sig_components arg path self items =
      let cache, _, _ = arg in

      (* Get the opname for this path *)
      let opprefix = make_opname path in

      (* Get all the sub-summaries *)
      let inline_component (item, _) =
         match item with
            Module (n, _) ->
               (* The contained summaries become top level *)
               let base = cache.base in
               let info' = Base.sub_info base.lib self n in
                  base.sig_summaries <- info' :: base.sig_summaries

          | Opname { opname_name = str; opname_term = t } ->
               (* Hash this name to the full opname *)
               let opname = Opname.mk_opname str opprefix in
                  Hashtbl.add cache.optable str opname

          | Parent { parent_name = path } ->
               (* Recursive inline of all ancestors *)
               inline_sig_module arg path;
               ()

          | _ ->
               ()
      in
         List_util.rev_iter inline_component items

   and inline_str_components arg path self (items : (str_proof, str_ctyp, str_expr, str_item) summary_item_loc list) =
      let cache, _, _ = arg in

      (* Get the opname for this path *)
      let opprefix = make_opname path in

      (* Get all the sub-summaries *)
      let inline_component (item, _) =
         match item with
            Module (n, _) ->
               (* The contained summaries become top level *)
               let base = cache.base in
               let info' = Base.sub_info base.lib self n in
                  base.sig_summaries <- info' :: base.sig_summaries

          | Opname { opname_name = str; opname_term = t } ->
               (* Hash this name to the full opname *)
               let opname = Opname.mk_opname str opprefix in
                  Hashtbl.add cache.optable str opname

          | Parent { parent_name = path } ->
               (* Recursive inline of all ancestors *)
               inline_sig_module arg path;
               ()

          | _ ->
               ()
      in
         List_util.rev_iter inline_component items

   and inline_sig_module arg path =
      let cache, inline_hook, vals = arg in
         if debug_filter_cache then
            eprintf "FilterCache.inline_module': %s%t" (string_of_path path) eflush;
         try
            let info = find_summarized_sig_module cache path in
               if debug_filter_cache then
                  eprintf "FilterCache.inline_module': %s: already loaded%t" (string_of_path path) eflush;
               info
         with
            Not_found ->
               if debug_filter_cache then
                  eprintf "FilterCache.inline_module': finding: %s%t" (string_of_path path) eflush;
               let { base = { lib = base; sig_summaries = summaries } } = cache in
               let info = Base.find base path SigMarshal.select in
               let info' = SigMarshal.unmarshal (Base.info base info) in
                  (* This module gets listed in the inline stack *)
                  cache.base.sig_summaries <- info :: summaries;

                  (* Inline the subparts *)
                  inline_sig_components arg path info (info_items info');

                  (* Call the hook *)
                  if debug_resource then
                     begin
                        eprintf "Summary: %s%t" (string_of_path path) eflush;
                        eprint_info info'
                     end;
                  vals := inline_hook cache (path, info') !vals;

                  info

   let inline_module cache path inline_hook arg =
      let vals = ref arg in
      let info = inline_sig_module (cache, inline_hook, vals) path in
         SigMarshal.unmarshal (Base.info cache.base.lib info), !vals
   
   (*
    * To create, need:
    *    1. module_base
    *    2. Load path
    *)
   let create_cache base name self_select child_select =
      { opprefix = Opname.mk_opname (String.capitalize name) nil_opname;
        optable = create_optable ();
        precs = [];
        resources = [];
        info = new_module_info ();
        self = Base.create_info base.lib self_select "." name;
        name = name;
        base = base
      }
   
   (*
    * When a cache is loaded, we follow the steps to inline
    * the file into a new cache.
    *)
   let load base (name : module_name) (my_select : select) (child_select : select) (hook : 'a hook) (arg : 'a) =
      let vals = ref arg in
      let path = [name] in
      let info =
         try find_summarized_str_module base path with
            Not_found ->
               let info = Base.find base.lib path my_select in
                  base.str_summaries <- info :: base.str_summaries;
                  info
      in
      let info' = StrMarshal.unmarshal (Base.info base.lib info) in
      let cache =
         { opprefix = Opname.mk_opname (String.capitalize name) nil_opname;
           optable = create_optable ();
           precs = [];
           resources = [];
           info = info';
           self = info;
           name = name;
           base = base
         }
      in
         if debug_filter_cache then
            begin
               eprintf "Filter_cache.load: loaded %s%t" name eflush;
               eprint_info info'
            end;
         inline_str_components (cache, hook, vals) path info (info_items info');
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
   let sig_info cache alt_select =
      let { base = base; self = self; name = name } = cache in
      let { lib = lib; sig_summaries = summaries } = base in
      let info =
         try find_summarized_sig_module cache [name] with
            Not_found ->
               let info = Base.find_match lib self alt_select in
                  base.sig_summaries <- info :: summaries;
                  info
      in
         SigMarshal.unmarshal (Base.info lib info) 

   (*
    * Check the implementation with its interface.
    *)
   let check cache alt_select =
      let sig_info = sig_info cache alt_select in
      let id = find_id sig_info in
         add_command cache (Id id, (0, 0));
         check_implementation cache.info sig_info
   
   (*
    * Save the cache.
    *)
   let save cache =
      let { base = { lib = base }; self = self; info = info } = cache in
         if debug_filter_cache then
            begin
               eprintf "Filter_cache.save: begin%t" eflush;
               eprint_info info
            end;
         Base.set_info base self (StrMarshal.marshal info);
         Base.save base self;
         if debug_filter_cache then
            eprintf "Filter_cache.save: done%t" eflush
end
   
(*
 * $Log$
 * Revision 1.8  1998/04/21 19:53:30  jyh
 * Upgraded refiner for program extraction.
 *
 * Revision 1.7  1998/04/17 01:31:00  jyh
 * Editor is almost constructed.
 *
 * Revision 1.6  1998/04/15 22:28:57  jyh
 * Converting packages from summaries.
 *
 * Revision 1.5  1998/04/15 12:39:52  jyh
 * Updating editor packages to Filter_summarys.
 *
 * Revision 1.4  1998/04/09 18:25:49  jyh
 * Working compiler once again.
 *
 * Revision 1.3  1998/02/23 14:46:03  jyh
 * First implementation of binary file compilation.
 *
 * Revision 1.2  1998/02/21 20:57:42  jyh
 * Two phase parse/extract.
 *
 * Revision 1.1  1998/02/19 17:13:55  jyh
 * Splitting filter_parse.
 *
 * Revision 1.4  1998/02/18 18:46:12  jyh
 * Initial ocaml semantics.
 *
 * Revision 1.3  1997/09/12 17:21:35  jyh
 * Added MLast <-> term conversion.
 * Splitting filter_parse into two phases:
 *    1. Compile into Filter_summary
 *    2. Compile Filter_summary into code.
 *
 * Revision 1.2  1997/08/06 16:17:27  jyh
 * This is an ocaml version with subtyping, type inference,
 * d and eqcd tactics.  It is a basic system, but not debugged.
 *
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
