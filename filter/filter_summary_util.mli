(*
 * Utilities on summaries.
 *)

open Refiner.Refiner.Term

open Filter_summary

(*
 * Parameter lists.
 *)
val collect_cvars : term param list -> string list
val collect_vars : term param list -> string list
val collect_non_vars : term param list -> term list
val split_params : term param list -> string list * string list * term list
val name_params : term param list -> string list * string list * string list * string list
val extract_params : string list -> string list -> term list -> term param list

(*
 * Resources.
 *)
val mem_resource : 'ctyp resource_info -> 'ctyp resource_info list -> bool


(*
 * $Log$
 * Revision 1.8  1998/07/02 22:24:53  jyh
 * Created term_copy module to copy and normalize terms.
 *
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
