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
type 'a module_info
     
(*
 * The summary contains information about
 *     1. included modules
 *     2. new theorems
 *     3. new terms
 *)
type 'a summary_item =
   Rewrite of 'a rewrite_info
 | CondRewrite of 'a cond_rewrite_info
 | Axiom of 'a axiom_info
 | Rule of 'a rule_info
 | Opname of opname_info
 | MLTerm of term
 | Condition of term
 | Parent of module_path
 | Module of string * 'a module_info
 | DForm of dform_info
 | Prec of string
 | Id of int
 | Resource of resource_info
 | InheritedResource of resource_info
 | Infix of string
 | SummaryItem of term
   
and 'a rewrite_info =
   { rw_name : string;
     rw_redex : term;
     rw_contractum : term;
     rw_proof : 'a
   }
and 'a cond_rewrite_info =
   { crw_name : string;
     crw_params : param list;
     crw_args : term list;
     crw_redex : term;
     crw_contractum : term;
     crw_proof : 'a
   }
and 'a axiom_info =
   { axiom_name : string;
     axiom_stmt : term;
     axiom_proof : 'a
   }
and 'a rule_info =
   { rule_name : string;
     rule_params : param list;
     rule_stmt : meta_term;
     rule_proof : 'a
   }
and opname_info =
   { opname_name : string;
     opname_term : term
   }
and dform_info =
   { dform_options : term list;
     dform_redex : term;
     dform_def : term option
   }
and resource_info =
   { resource_name : string;
     resource_extract_type : MLast.ctyp;
     resource_improve_type : MLast.ctyp;
     resource_data_type : MLast.ctyp
   }
and param =
   ContextParam of string
 | VarParam of string
 | TermParam of term

(************************************************************************
 * INTERFACE								*
 ************************************************************************)

(* Creation *)
val find_sub_module : 'a module_info -> module_path -> 'a module_info

val new_module_info : unit -> 'a module_info
val info_items : 'a module_info -> 'a summary_item list
val normalize_info : 'a module_info -> ('a -> 'a) -> 'a module_info

(* Access *)
val find_axiom : 'a module_info -> string -> 'a summary_item option
val find_rewrite : 'a module_info -> string -> 'a summary_item option
val find_mlterm : 'a module_info -> term -> 'a summary_item option
val find_condition : 'a module_info -> term -> 'a summary_item option
val find_dform : 'a module_info -> term -> 'a summary_item option
val find_prec : 'a module_info -> string -> 'a summary_item option
val find_id : 'a module_info -> int

val get_resources : 'a module_info -> resource_info list
val get_infixes : 'a module_info -> string list

(* Update *)
val add_command : 'a module_info -> 'a summary_item -> 'a module_info * int
val set_commands : 'a module_info -> 'a summary_item list -> 'a module_info
val get_command : 'a module_info -> int -> 'a summary_item

(* Utilities *)
val collect_cvars : param list -> string array
val collect_vars : param list -> string array
val collect_non_vars : param list -> term list
val proof_map : (string -> 'a -> 'b) -> 'a module_info -> 'b module_info
val term_list : (string -> 'a -> term) -> 'a module_info -> term list
val of_term_list : (string -> term -> 'a) -> term list -> 'a module_info

(*
 * Interface checking.
 *)
val check_implementation : 'a module_info -> 'a module_info -> unit

(*
 * Module paths.
 *)
val string_of_path : module_path -> string
val output_path : out_channel -> module_path -> unit

(*
 * Debugging.
 *)
val eprint_command : 'a summary_item -> unit
val eprint_info : 'a module_info -> unit

(*
 * $Log$
 * Revision 1.4  1998/02/12 23:38:15  jyh
 * Added support for saving intermediate files to the library.
 *
 * Revision 1.3  1997/09/12 17:21:39  jyh
 * Added MLast <-> term conversion.
 * Splitting filter_parse into two phases:
 *    1. Compile into Filter_summary
 *    2. Compile Filter_summary into code.
 *
 * Revision 1.2  1997/08/06 16:17:33  jyh
 * This is an ocaml version with subtyping, type inference,
 * d and eqcd tactics.  It is a basic system, but not debugged.
 *
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
