(*
 * Utilities on summaries.
 *)

open Refiner.Refiner.Term

open Filter_summary

(*
 * Parameter lists.
 *)
val collect_cvars : param list -> string list
val collect_vars : param list -> string list
val collect_non_vars : param list -> term list
val split_params : param list -> string list * string list * term list
val name_params : param list -> string list * string list * string list * string list
val extract_params : string list -> string list -> term list -> param list

(*
 * Resources.
 *)
val mem_resource : 'ctyp resource_info -> 'ctyp resource_info list -> bool


(*
 * $Log$
 * Revision 1.7  1998/05/27 15:13:08  jyh
 * Functorized the refiner over the Term module.
 *
 * Revision 1.6  1998/02/21 20:58:00  jyh
 * Two phase parse/extract.
 *
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
