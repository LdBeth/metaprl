(*
 * Make a database of module_info.
 * The info in the database is named
 * with the module name, and the full pathname of
 * the module.
 *)

open Printf
open Debug

(*
 * Show the file loading.
 *)
let _ =
   if !debug_load then
      eprintf "Loading Filter_summary_base%t" eflush


(*
 * We keep lists of modules in a table.
 * Each module has a name, a path, and a summry.
 *)
type 'a module_table_entry =
   { mod_name : string;
     mod_fullname : module_path;
     mod_summary : 'a module_info
   }

(*
 * The database is just a list of modules.
 *)
and 'a module_table = 'a module_table_entry list

(*
 * For the official base, we also include the load path.
 *)
and 'a module_base = 'a module_table ref

(*
 * $Log$
 * Revision 1.3  1998/04/24 19:38:34  jyh
 * Updated debugging.
 *
 * Revision 1.2  1998/04/24 02:42:10  jyh
 * Added more extensive debugging capabilities.
 *
 * Revision 1.1  1997/08/06 16:17:33  jyh
 * This is an ocaml version with subtyping, type inference,
 * d and eqcd tactics.  It is a basic system, but not debugged.
 *
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
