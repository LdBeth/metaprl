(*
 * This is the information about modules.
 * Its really derived from Filter_cache
 *)

open Filter_summary_type
open Filter_summary
open Filter_cache

module Package : PackageSig =
sig
   (*
    * Built from a filter cache.
    *)
   type t = StrFilterCache.t
   
   (*
    * Our info contains the cache module,
    * plus the refiner and display forms.
    *)
   type info =
      { pack_status : status;
        pack_cache : StrFilterCache.info;
        pack_refiner : refiner;
        pack_dforms : dform_mode_base
      }
   
   (*
    * Create the cache.
    *)
   let create = StrFilterCache.create
   
   (*
    * When a module is inlined, add the resources and infixes.
    *)
   let inline_hook root_path cache (path, info) (paths, resources) =
      (* Include all the resources *)
      if debug_resource then
         eprintf "Inline_hook: %s, %s%t" (string_of_path root_path) (string_of_path path) eflush;
      let add_resource rsrc =
         if debug_resource then
            eprintf "Adding resource: %s.%s%t" (string_of_path path) rsrc.resource_name eflush;
         FilterCache.add_resource cache path rsrc
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
    * Load a package.
    * We search for the description, and load it.
    * If the ML file has already been loaded, retrieve
    * the refiner and display forms.  Else construct the code
    * to be evaluated, and return it.
    *)
   let load cache name =
      try
         let info, _ =
            StrFilterCache.load
            cache
            name
            ImplementationType
            InterfaceType
            (inline_hook name)
            ([], [])
         in
         let finish = (<:expr< $uid: "Package_info"$ . $lid: "finish_load"$ $lid: name$ >>) in
            if is_loaded name then
               finish
            else
               finish
end

(*
 * $Log$
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
