(*
 * This is the information about modules.
 * Its really derived from Filter_cache
 *)

open Printf

open Debug

open Theory

open Filter_summary_type
open Filter_summary
open Filter_summary_util
open Filter_cache
open Filter_debug
open Filter_util
open Filter_prog
open Infix

include Package_type

module Package : PackageSig =
struct
   (*
    * Our info contains the cache module,
    * plus the refiner and display forms.
    *)
   type package =
      { mutable pack_status  : status;
        pack_info : StrFilterCache.info
      }
   
   (*
    * Built from a filter cache.
    *)
   type t =
      { pack_cache : StrFilterCache.t;
        pack_dag : ImpDag.t;
        mutable pack_packages : package list
      }
   
   (*
    * Create the cache.
    *)
   let create path =
      { pack_cache = StrFilterCache.create path;
        pack_dag = ImpDag.create ();
        pack_packages = []
      }
   
   (*
    * See if a theory is already loaded.
    *)
   let is_theory_loaded name =
      let rec search = function
         { thy_name = name' } :: t ->
            if name = name' then
               true
            else
               search t
       | [] ->
            false
      in
         search (get_theories ())
   
   (*
    * Get a theory by name.
    *)
   let get_theory name =
      let rec search = function
         thy :: t ->
            if thy.thy_name = name then
               thy
            else
               search t
       | [] ->
            raise Not_found
      in
         search (get_theories ())
   
   (*
    * Get the refiner.
    *)
   let refiner { pack_info = info } =
      let name = StrFilterCache.name info in
         (get_theory name).thy_refiner

   (*
    * Get the list of display forms.
    *)
   let dforms { pack_info = info } =
      let name = StrFilterCache.name info in
         (get_theory name).thy_dformer
   
   (*
    * Get the name of the package.
    *)
   let name { pack_info = info } =
      StrFilterCache.name info
   
   (*
    * Get the filename for the package.
    *)
   let filename pack { pack_info = info } =
      StrFilterCache.filename pack.pack_cache info
   
   (*
    * Get the status of the package.
    *)
   let status { pack_status = status } =
      status
   
   (*
    * Set the status of the package.
    *)
   let set_status pack status =
      pack.pack_status <- status

   (*
    * Get the items in the module.
    *)
   let info { pack_info = info } =
      info_items (StrFilterCache.info info)
   
   (*
    * Add a signature package.
    * This does nothing if the package already exists,
    * otherwise it adds the package.
    *)
   let maybe_add_package pack info =
      let { pack_packages = packages } = pack in
      let parents = StrFilterCache.

   (*
    * When a module is inlined, add the resources and infixes.
    * The info is the _signature_.  Later we may want to replace
    * it with the implementation.
    *)
   let inline_hook pack root_path cache (path, info) (paths, resources) =
      (* Include all the resources *)
      if debug_resource then
         eprintf "Inline_hook: %s, %s%t" (string_of_path root_path) (string_of_path path) eflush;
      let add_resource rsrc =
         if debug_resource then
            eprintf "Adding resource: %s.%s%t" (string_of_path path) rsrc.resource_name eflush;
         StrFilterCache.add_resource cache path rsrc
      in
      let nresources, nresources' =
         (*
          * nresources: all the resources
          * nresources': just the new ones
          *)
         let rec collect nresources nresources' = function
            rsrc::tl ->
               if mem_resource rsrc nresources then
                  collect nresources nresources' tl
               else
                  collect (rsrc :: nresources) (rsrc :: nresources') tl
          | [] ->
               nresources, nresources'
         in
            collect resources [] (get_resources info)
      in
         List.iter add_resource nresources';
         
         (* Add all the infix words *)
         List.iter add_infix (get_infixes info);
         
         (* Add the path to the list of parents *)
         path :: paths, nresources

   (*
    * Add a theory to the list of loaded theories.
    *)
   let add_package pack info =
      let { pack_packages = packages } = pack in
         pack.pack_packages <- info :: packages
   
   (*
    * Get a loaded theory.
    *)
   let get { pack_packages = packages } name =
      let rec search = function
         pack :: t ->
            let { pack_info = info } = pack in
            let name' = StrFilterCache.name info in
               if name' = name then
                  pack
               else
                  search t
       | [] ->
            raise Not_found
      in
         search packages

   (*
    * Save a package.
    * This happens only if it is modified.
    *)
   let save pack info =
      let { pack_status = status; pack_info = info } = info in
         match status with
            ReadOnly ->
               raise (Failure "Package is read-only")
          | Unmodified ->
               ()
          | Modified ->
               StrFilterCache.save info

   (*
    * Load a package.
    * We search for the description, and load it.
    * If the ML file has already been loaded, retrieve
    * the refiner and display forms.  Else construct the code
    * to be evaluated, and return it.
    *)
   let load pack name =
      try
         let loc = 0, 0 in
         let { pack_cache = cache } = pack in
         let path = [name] in
         let info, _ =
            StrFilterCache.load cache name ImplementationType InterfaceType (inline_hook path) ([], [])
         in
         let mlexists =
            let filename = StrFilterCache.filename cache info in
            let filename = (Filename_util.root filename) ^ ".ml" in
               Sys.file_exists filename
         in
         let info' =
            { pack_status = if mlexists then ReadOnly else Unmodified;
              pack_info = info
            }
         in
            add_package pack info';
            if is_theory_loaded name then
               let unit = <:expr< () >> in
                  (<:str_item< $exp: unit$ >>)
            else
               (* Wrap the theory up in a module and evaluate it *)
               let items = extract_str (StrFilterCache.info info) (StrFilterCache.resources info) name in
               let mn = String.capitalize name in
               let me = (<:module_expr< struct $list:List.map fst items$ end >>) in
                  (<:str_item< module $mn$ = $me$ >>)
      with
         Sys_error _ ->
             raise (Failure ("Package_info.load: " ^ name ^ " not found"))
end

(*
 * $Log$
 * Revision 1.3  1998/04/15 22:28:49  jyh
 * Converting packages from summaries.
 *
 * Revision 1.2  1998/04/15 12:39:32  jyh
 * Updating editor packages to Filter_summarys.
 *
 * Revision 1.1  1997/08/06 16:17:16  jyh
 * This is an ocaml version with subtyping, type inference,
 * d and eqcd tactics.  It is a basic system, but not debugged.
 *
 * Revision 1.2  1996/10/23 15:17:50  jyh
 * First working version of dT tactic.
 *
 * Revision 1.1  1996/09/02 19:33:23  jyh
 * Semi-working package management.
 *
 * Revision 1.2  1996/05/21 02:25:29  jyh
 * This is a semi-working version before Wisconsin vacation.
 *
 * Revision 1.1  1996/05/20 16:59:55  jyh
 * This is an intermediate form of the editor with modules
 * before debugging.  Will be removing theoryGraph files next.
 *
 * -*-
 * Local Variables:
 * Caml-master: "editor.top"
 * End:
 * -*-
 *)
