(*
 * We add a layer to filterSummry, to allow inlined modules
 * and cached info about opnames, axioms, and precedences.
 *
 *)

open Opname
open Term
open Filter_type
open Filter_summary
open Filter_summary_io

type module_cache

(*
 * This function gets called when a module is inlined.
 *)
type 'a module_inline_hook = module_cache -> (module_path * module_info) -> 'a

(* Need base, name, load_path *)
val new_module_cache :
       module_base_io ->
       string ->
       module_cache

val get_module_cache :
       module_base_io ->
       string ->
       string ->
       int ->
       'a module_inline_hook ->
       module_cache * 'a list

(* Access *)
val get_module_info : module_cache -> module_info
val get_optable : module_cache -> (string -> opname)
val get_sub_module_info : module_cache -> module_path -> module_info

(* Expand a partial path specification to a complte one *)
val expand_path : module_cache -> module_path -> module_path

(* Opname management *)
val find_opname : module_cache -> string -> opname
val get_opprefix : module_cache -> opname
val add_opname : module_cache -> string -> opname -> unit
val rm_opname : module_cache -> string -> unit

(* Inherited access *)
val find_axiom : module_cache -> string -> summary_item option
val find_rewrite : module_cache -> string -> summary_item option
val find_mlterm : module_cache -> term -> summary_item option
val find_condition : module_cache -> term -> summary_item option
val find_dform : module_cache -> term -> summary_item option
val find_id : module_cache -> int

val find_prec : module_cache -> string -> bool
val get_all_resources : module_cache -> (module_path * resource_info) list

(* Update *)
val add_command : module_cache -> summary_item -> summary_item
val add_resource : module_cache -> module_path -> resource_info -> unit
val add_prec : module_cache -> string -> unit

val inline_module : module_cache -> module_path -> int -> 'a module_inline_hook -> module_info * 'a list
val inline_cache : module_cache -> module_cache -> 'a module_inline_hook -> module_info * 'a list
val load_module : module_cache -> string -> int -> 'a module_inline_hook -> module_info * 'a list

(*
 * $Log$
 * Revision 1.1  1997/04/28 15:50:52  jyh
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
 * Revision 1.2  1996/10/23 15:17:55  jyh
 * First working version of dT tactic.
 *
 * Revision 1.1  1996/09/02 19:42:47  jyh
 * Semi working package management.
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
