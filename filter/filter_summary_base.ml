(*
 * Make a database of module_info.
 * The info in the database is named
 * with the module name, and the full pathname of
 * the module.
 *)

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
