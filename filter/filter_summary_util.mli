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
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
