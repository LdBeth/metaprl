(*
 * This module defines an interface for saving information about
 * modules.  We record information about each module interface,
 * to be used in the definition of the module and in submodules.
 *
 *)

open Term
open Term_util
open Filter_type

(************************************************************************
 * TYPES								*
 ************************************************************************)

(*
 * A module_base contains information about a collection of modules.
 * A module_info contains information about a specific module.
 *)
type module_base
type module_info
     
(*
 * The summary contains information about
 *     1. included modules
 *     2. new theorems
 *     3. new terms
 *)
type summary_item =
   Rewrite of rewrite_info
 | CondRewrite of cond_rewrite_info
 | Axiom of axiom_info
 | Rule of ruleInfo
 | Opname of opnameInfo
 | MLTerm of term
 | Condition of term
 | Parent of module_path
 | Module of string * module_info
 | DForm of term list * term
 | Prec of string
 | Id of int
 | Resource of resource_info
 | InheritedResource of resource_info
 | Infix of string
   
and axiom_info = { axiom_name : string; axiom_stmt : term }
and rewrite_info = { rw_name : string; rw_redex : term; rw_contractum : term }
and cond_rewrite_info =
   { crw_name : string;
     crw_params : param list;
     crw_args : term list;
     crw_redex : term;
     crw_contractum : term
   }
and ruleInfo =
   { rule_name : string;
     rule_params : param list;
     rule_stmt : meta_term
   }
and opnameInfo = { opname_name : string; opname_term : term }
                 
and resource_info =
   { resource_name : string;
     resource_extract_type : Ast.ctyp;
     resource_improve_type : Ast.ctyp;
     resource_data_type : Ast.ctyp
   }

and param =
   ContextParam of string
 | VarParam of string
 | TermParam of term

(************************************************************************
 * INTERFACE								*
 ************************************************************************)

(* Creation *)
val new_module_base : unit -> module_base
val ignore_id : int
val find_module :  module_base -> module_path -> int -> module_info option
val find_sub_module : module_info -> module_path -> module_info

val module_name : module_base -> module_info -> string
val module_fullname : module_base -> module_info -> module_path

val new_module_info : unit -> module_info
val info_items : module_info -> summary_item list
val normalize_info : module_info -> module_info

(* Access *)
val find_axiom : module_info -> string -> summary_item option
val find_rewrite : module_info -> string -> summary_item option
val find_mlterm : module_info -> term -> summary_item option
val find_condition : module_info -> term -> summary_item option
val find_dform : module_info -> term -> summary_item option
val find_prec : module_info -> string -> summary_item option
val find_id : module_info -> int

val get_resources : module_info -> resource_info list
val get_infixes : module_info -> string list

(* Update *)
val add_command : module_info -> summary_item -> module_info
val push_module : module_base -> string -> module_path -> module_info -> unit

(* Utilities *)
val collect_cvars : param list -> string array
val collect_vars : param list -> string array
val collect_non_vars : param list -> term list

(*
 * Interface checking.
 *)
val check_implementation : module_info -> module_info -> unit

(*
 * Module paths.
 *)
val string_of_path : module_path -> string
val output_path : out_channel -> module_path -> unit

(*
 * Debugging.
 *)
val eprint_info : module_info -> unit

(*
 * $Log$
 * Revision 1.1  1997/04/28 15:50:59  jyh
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
 * Revision 1.4  1996/11/13 22:58:09  jyh
 * Initial version of forward/backward chaining cache.
 *
 * Revision 1.3  1996/10/23 15:17:57  jyh
 * First working version of dT tactic.
 *
 * Revision 1.2  1996/09/25 22:51:59  jyh
 * Initial "tactical" commit.
 *
 * Revision 1.1  1996/09/02 19:43:15  jyh
 * Semi working package management.
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
