(*
 * This is the information about modules for the editor.
 *
 *)

open Util
open FileUtil
open FilterSummary
open FilterSummaryIO
open FilterCache

(************************************************************************
 * TYPES                                                                *
 ************************************************************************)

(*
 * For a cache, keep the path, or a loaded cache,
 * or failure.
 *
 * For CacheUnloaded, the arg is the id of the interface.
 *)
type cache_info =
   CacheUnloaded of int
 | CacheFile of string * int
 | CacheLoaded of moduleCache
 | CacheFailed

(*
 * This is the info for each open module.
 *)
type packageInfo =
   { mutable pack_status : package_status;
     
     (* Parts *)
     mutable pack_refiner : refiner;
     mutable pack_dformer : dform_mode_base;
     mutable pack_thms : thm_info list;
     mutable pack_cache : cache_info;
      
     (* Attributes *)
     mutable pack_name : string;
     mutable pack_file : string option
   }
   
(************************************************************************
 * GLOBALS                                                              *
 ************************************************************************)
   
(*
 * string -> path commands
 *)
let set_path doc var path =
   let path' = String_util.split ':' path in
      var := path'

let set_path_arg doc var =
   Arg.String (set_path doc var)

(*
 * This is a list of hosts to use for database lookup.
 *)
let include_path = Env_arg.general "include" ["."] "Include directories" set_path set_path_arg

(*
 * Global module base.
 *)
let module_base = new_module_base_io !include_path

(************************************************************************
 * IMPLEMENTATION                                                       *
 ************************************************************************)
   
(*
 * Read-only creation from a pre-existing theory.
 * We keep track of the name, so that we can load
 * the summary information when desired.
 *)
let package_of_theory
    { thy_name = name;
      thy_refiner = refiner;
      thy_dformer = dforms;
      thy_id = id
    } =
   { pack_status = PackageReadOnly;
     
     (* Info *)
     pack_refiner = refiner;
     pack_dformer = dforms;
     pack_cache = CacheUnloaded id;
     pack_thms = [];
     
     (* Attributes *)
     pack_name = name;
     pack_file = None
   }

(*
 * Create an interactive package.
 *)
let new_empty_package name =
   { pack_status = PackageUnmodified;
     
     (* Info *)
     pack_refiner = null_refiner;
     pack_dformer = null_mode_base;
     pack_cache = CacheLoaded (new_module_cache module_base name);
     pack_thms = [];
     
     (* Attributes *)
     pack_name = name;
     pack_file = Some name
   }

let make_package
    { thy_name = name;
      thy_refiner = refiner;
      thy_dformer = dforms;
      thy_id = id
    }
    thms
    path =
   { pack_status = PackageUnmodified;
     
     (* Info *)
     pack_refiner = refiner;
     pack_dformer = dforms;
     pack_cache = CacheFile (path, id);
     pack_thms = thms;
     
     (* Attributes *)
     pack_name = name;
     pack_file = Some path
   }

(*
 * Access.
 *)
let package_status pack = pack.pack_status

let package_refiner pack = pack.pack_refiner

let package_dforms pack = pack.pack_dformer

let package_thms pack = pack.pack_thms

let package_name pack = pack.pack_name

let package_file pack = pack.pack_file

(*
 * Cache access.
 *)
let package_cache pack =
   match pack.pack_cache with
      CacheUnloaded id ->
         let cache = new_module_cache module_base pack.pack_name in
            begin
               try load_module cache pack.pack_name id null_hook with
                  Not_found ->
                     pack.pack_cache <- CacheFailed;
                     raise Not_found
            end;
            pack.pack_cache <- CacheLoaded cache;
            cache
            
    | CacheFile (path, id) ->
         let cache =
            try get_module_cache module_base pack.pack_name path id null_hook with
               Not_found ->
                  pack.pack_cache <- CacheFailed;
                  raise Not_found
         in
            pack.pack_cache <- CacheLoaded cache;
            cache
         
    | CacheLoaded cache -> cache
                          
    | CacheFailed -> raise Not_found

(*
 * Modification.
 *)
let package_set_file pack path =
   let name = path_file path in
      pack.pack_name <- name;
      pack.pack_file <- Some path

(************************************************************************
 * INTERACTIVE ADDITION                                                 *
 ************************************************************************)

(*
 * Signal modification.
 *)
let package_update pack =
   if pack.pack_status = PackageReadOnly then
      raise (PackageReadError pack)
   else
      pack.pack_status <- PackageModified

(*
 * Add a parent package.
 *)
let package_add_parent pack new =
   let pcache = package_cache pack in
   let ncache = package_cache new in
   let rref = ref pack.pack_refiner in
   let dref = ref pack.pack_dformer in
      package_update pack;
      join_refiner rref new.pack_refiner;
      join_mode_base dref new.pack_dformer;
      pack.pack_refiner <- !rref;
      pack.pack_dformer <- !dref;
      inline_cache pcache ncache null_hook;
      ()

(*
 * Add a theorem.
 *)
let package_add_thm pack name ped =
   package_update pack;
   pack.pack_thms <- { thm_name = name; thm_ped = ped }::pack.pack_thms

(*
 * $Log$
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
