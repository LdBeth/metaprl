(*
 * This module defines an interface for saving information about
 * modules.  We record information about each module interface,
 * to be used in the definition of the module and in submodules.
 *
 *)

open Refiner.Refiner.Term
open Refiner.Refiner.TermMeta
open Filter_type

(************************************************************************
 * TYPES								*
 ************************************************************************)

(*
 * A module_base contains information about a collection of modules.
 * A module_info contains information about a specific module.
 *
 * 'proof: type of proofs for rewrites, etc.
 * 'ctyp: type of type definitions for resources
 * 'expr: type of expressions used in ML definitions
 * 'item: type of summary items
 *)
type ('term, 'meta_term, 'proof, 'ctyp, 'expr, 'item) module_info

(*
 * The summary contains information about everything in the file.
 * We use the summary for both interfaces and implementations.
 * If the implementation requires a term for the definition,
 * we use a term option so that the term does not have to be
 * provided in the interface.
 *
 * A MagicBlock is a block of code that we use to compute
 * a magic number.  The magic number changes whenever the code changes.
 *)
type ('term, 'meta_term, 'proof, 'ctyp, 'expr, 'item) summary_item =
   Rewrite of ('term, 'proof) rewrite_info
 | CondRewrite of ('term, 'proof) cond_rewrite_info
 | Axiom of ('term, 'proof) axiom_info
 | Rule of ('term, 'meta_term, 'proof) rule_info
 | Opname of 'term opname_info
 | MLTerm of ('term, 'expr) mlterm_info
 | Condition of ('term, 'expr) mlterm_info
 | Parent of 'ctyp parent_info
 | Module of string * ('term, 'meta_term, 'proof, 'ctyp, 'expr, 'item) module_info
 | DForm of ('term, 'expr) dform_info
 | Prec of string
 | PrecRel of prec_rel_info
 | Id of int
 | Resource of 'ctyp resource_info
 | Infix of string
 | SummaryItem of 'item
 | MagicBlock of 'item magic_info

(*
 * Proof is type unit in interface.
 *)
and ('term, 'proof) rewrite_info =
   { rw_name : string;
     rw_redex : 'term;
     rw_contractum : 'term;
     rw_proof : 'proof
   }

and ('term, 'proof) cond_rewrite_info =
   { crw_name : string;
     crw_params : 'term param list;
     crw_args : 'term list;
     crw_redex : 'term;
     crw_contractum : 'term;
     crw_proof : 'proof
   }

and ('term, 'proof) axiom_info =
   { axiom_name : string;
     axiom_stmt : 'term;
     axiom_proof : 'proof
   }

and ('term, 'meta_term, 'proof) rule_info =
   { rule_name : string;
     rule_params : 'term param list;
     rule_stmt : 'meta_term;
     rule_proof : 'proof
   }

(*
 * A parent command lists all the modules that are recursively
 * opened, and it lists all the resources that were discovered.
 *)
and 'ctyp parent_info =
   { parent_name : module_path;
     parent_opens : module_path list;
     parent_resources : 'ctyp resource_info list
   }

(*
 * An mlterm is a term that has an ML procedure for its rewrite.
 * The definition is not required in the interface.
 *)
and ('term, 'expr) mlterm_info =
   { mlterm_term : 'term;
     mlterm_contracta : 'term list;
     mlterm_def : ('expr * 'expr) option
   }

and 'term opname_info =
   { opname_name : string;
     opname_term : 'term
   }

(*
 * Dform descriptions.
 * The definition is not required in the interface.
 *)
and ('term, 'expr) dform_info =
   { dform_name : string;
     dform_modes : string list;
     dform_options : dform_option list;
     dform_redex : 'term;
     dform_def : ('term, 'expr) dform_def
   }

and dform_option =
   DFormInheritPrec
 | DFormPrec of string
 | DFormParens

and ('term, 'expr) dform_def =
   NoDForm
 | TermDForm of 'term
 | MLDForm of ('term, 'expr) dform_ml_def

and ('term, 'expr) dform_ml_def =
   { dform_ml_printer : string;
     dform_ml_buffer : string;
     dform_ml_contracta : 'term list;
     dform_ml_code : 'expr
   }

(*
 * Define a precedence relation.
 *)
and prec_rel_info =
   { prec_rel : Precedence.relation;
     prec_left : string;
     prec_right : string
   }

(*
 * Resource descriptions.
 *)
and 'ctyp resource_info =
   { resource_name : string;
     resource_extract_type : 'ctyp;
     resource_improve_type : 'ctyp;
     resource_data_type : 'ctyp
   }

(*
 * Magic block needs a variable to bind the magic number to.
 *)
and 'item magic_info =
   { magic_name : string;
     magic_code : 'item list
   }

and 'term param =
   ContextParam of string
 | VarParam of string
 | TermParam of 'term

(*
 * Pair it with a location.
 *)
type ('term, 'meta_term, 'proof, 'ctyp, 'expr, 'item) summary_item_loc =
   ('term, 'meta_term, 'proof, 'ctyp, 'expr, 'item) summary_item * (int * int)

(*
 * Conversion functions.
 *)
type ('term1, 'meta_term1, 'proof1, 'ctyp1, 'expr1, 'item1,
      'term2, 'meta_term2, 'proof2, 'ctyp2, 'expr2, 'item2) convert =
   { term_f       : 'term1 -> 'term2;
     meta_term_f  : 'meta_term1 -> 'meta_term2;
     proof_f      : string -> 'proof1 -> 'proof2;
     ctyp_f       : 'ctyp1  -> 'ctyp2;
     expr_f       : 'expr1  -> 'expr2;
     item_f       : 'item1  -> 'item2
   }

(************************************************************************
 * Interface								*
 ************************************************************************)

(* Creation *)
val find_sub_module : ('term, 'meta_term, 'proof, 'ctyp, 'expr, 'item) module_info ->
   module_path ->
   ('term, 'meta_term, 'proof, 'ctyp, 'expr, 'item) module_info

val new_module_info : unit -> ('term, 'meta_term, 'proof, 'ctyp, 'expr, 'item) module_info

val info_items : ('term, 'meta_term, 'proof, 'ctyp, 'expr, 'item) module_info ->
   ('term, 'meta_term, 'proof, 'ctyp, 'expr, 'item) summary_item_loc list

(* Access *)
val find_axiom : ('term, 'meta_term, 'proof, 'ctyp, 'expr, 'item) module_info ->
   string ->
   ('term, 'meta_term, 'proof, 'ctyp, 'expr, 'item) summary_item_loc option

val find_rewrite : ('term, 'meta_term, 'proof, 'ctyp, 'expr, 'item) module_info ->
   string ->
   ('term, 'meta_term, 'proof, 'ctyp, 'expr, 'item) summary_item_loc option

val find_mlterm : (term, 'meta_term, 'proof, 'ctyp, 'expr, 'item) module_info ->
   term ->
   (term, 'meta_term, 'proof, 'ctyp, 'expr, 'item) summary_item_loc option

val find_condition : (term, 'meta_term, 'proof, 'ctyp, 'expr, 'item) module_info ->
   term ->
   (term, 'meta_term, 'proof, 'ctyp, 'expr, 'item) summary_item_loc option

val find_dform : (term, 'meta_term, 'proof, 'ctyp, 'expr, 'item) module_info ->
   term ->
   (term, 'meta_term, 'proof, 'ctyp, 'expr, 'item) summary_item_loc option

val find_prec : ('term, 'meta_term, 'proof, 'ctyp, 'expr, 'item) module_info ->
   string ->
   ('term, 'meta_term, 'proof, 'ctyp, 'expr, 'item) summary_item_loc option

val find_id : ('term, 'meta_term, 'proof, 'ctyp, 'expr, 'item) module_info -> int

val get_resources : ('term, 'meta_term, 'proof, 'ctyp, 'expr, 'item) module_info ->
   'ctyp resource_info list

val get_infixes : ('term, 'meta_term, 'proof, 'ctyp, 'expr, 'item) module_info ->
   string list

val get_proofs : ('term, 'meta_term, 'proof, 'ctyp, 'expr, 'item) module_info ->
   (string * 'proof) list

val find : (term, 'meta_term, 'proof, 'ctyp, 'expr, 'item) module_info ->
   string ->
   (term, 'meta_term, 'proof, 'ctyp, 'expr, 'item) summary_item_loc

val parents : ('term, 'meta_term, 'proof, 'ctyp, 'expr, 'item) module_info ->
   module_path list

(* Update *)
val add_command : ('term, 'meta_term, 'proof, 'ctyp, 'expr, 'item) module_info ->
   ('term, 'meta_term, 'proof, 'ctyp, 'expr, 'item) summary_item_loc ->
   ('term, 'meta_term, 'proof, 'ctyp, 'expr, 'item) module_info

val set_command : (term, 'meta_term, 'proof, 'ctyp, 'expr, 'item) module_info ->
   (term, 'meta_term, 'proof, 'ctyp, 'expr, 'item) summary_item_loc ->
   (term, 'meta_term, 'proof, 'ctyp, 'expr, 'item) module_info

(* Utilities *)
val summary_map :
   ('term1, 'meta_term1, 'proof1, 'ctyp1, 'expr1, 'item1,
    'term2, 'meta_term2, 'proof2, 'ctyp2, 'expr2, 'item2) convert ->
   ('term1, 'meta_term1, 'proof1, 'ctyp1, 'expr1, 'item1) module_info ->
   ('term2, 'meta_term2, 'proof2, 'ctyp2, 'expr2, 'item2) module_info

(*
 * Term conversion.
 *)
val term_of_rewrite :
   ('term, 'meta_term, 'proof, 'ctyp, 'expr, 'item,
    term, term, term, term, term, term) convert ->
   ('term, 'proof) rewrite_info ->
   term
val term_of_cond_rewrite :
   ('term, 'meta_term, 'proof, 'ctyp, 'expr, 'item,
    term, term, term, term, term, term) convert ->
   ('term, 'proof) cond_rewrite_info ->
   term
val term_of_axiom :
   ('term, 'meta_term, 'proof, 'ctyp, 'expr, 'item,
    term, term, term, term, term, term) convert ->
   ('term, 'proof) axiom_info ->
   term
val term_of_rule :
   ('term, 'meta_term, 'proof, 'ctyp, 'expr, 'item,
    term, term, term, term, term, term) convert ->
   ('term, 'meta_term, 'proof) rule_info ->
   term
val term_of_opname :
   ('term, 'meta_term, 'proof, 'ctyp, 'expr, 'item,
    term, term, term, term, term, term) convert ->
   'term opname_info -> term
val term_of_mlterm :
   ('term, 'meta_term, 'proof, 'ctyp, 'expr, 'item,
    term, term, term, term, term, term) convert ->
   ('term, 'expr) mlterm_info ->
   term
val term_of_condition :
   ('term, 'meta_term, 'proof, 'ctyp, 'expr, 'item,
    term, term, term, term, term, term) convert ->
   ('term, 'expr) mlterm_info ->
   term
val term_of_parent :
   ('term, 'meta_term, 'proof, 'ctyp, 'expr, 'item,
    term, term, term, term, term, term) convert ->
   'ctyp parent_info ->
   term
val term_of_dform :
   ('term, 'meta_term, 'proof, 'ctyp, 'expr, 'item,
    term, term, term, term, term, term) convert ->
   ('term, 'expr) dform_info ->
   term
val term_of_prec : string -> term
val term_of_prec_rel : prec_rel_info -> term
val term_of_id : int -> term
val term_of_resource :
   ('term, 'meta_term, 'proof, 'ctyp, 'expr, 'item,
    term, term, term, term, term, term) convert ->
   'ctyp resource_info ->
   term
val term_of_infix : string -> term
val term_of_summary_item :
   ('term, 'meta_term, 'proof, 'ctyp, 'expr, 'item,
    term, term, term, term, term, term) convert ->
   'item ->
   term
val term_of_magic_block :
   ('term, 'meta_term, 'proof, 'ctyp, 'expr, 'item,
    term, term, term, term, term, term) convert ->
   'item magic_info ->
   term
val term_list :
   ('term, 'meta_term, 'proof, 'ctyp, 'expr, 'item,
    term, term, term, term, term, term) convert ->
   ('term, 'meta_term, 'proof, 'ctyp, 'expr, 'item) module_info ->
   term list
val of_term_list :
   (term, term, term, term, term, term, 'term, 'meta_term, 'proof, 'ctyp, 'expr, 'item) convert ->
   term list ->
   ('term, 'meta_term, 'proof, 'ctyp, 'expr, 'item) module_info

(*
 * Interface checking implem/interf/exception.
 *)
val check_implementation :
   (term, meta_term, 'proof1, 'ctyp1, 'expr1, 'item1) module_info ->
   (term, meta_term, 'proof2, 'ctyp2, 'expr2, 'item2) module_info ->
   unit

(*
 * Debugging.
 *)
val eprint_command : (term, 'meta_term, 'proof, 'ctyp, 'expr, 'item) summary_item -> unit
val eprint_info : (term, 'meta_term, 'proof, 'ctyp, 'expr, 'item) module_info -> unit

val debug_summary : bool ref

(*
 * $Log$
 * Revision 1.18  1998/07/02 22:24:50  jyh
 * Created term_copy module to copy and normalize terms.
 *
 * Revision 1.17  1998/06/15 22:32:11  jyh
 * Added CZF.
 *
 * Revision 1.16  1998/06/12 13:46:38  jyh
 * D tactic works, added itt_bool.
 *
 * Revision 1.15  1998/05/29 14:53:02  jyh
 * Better Makefiles.
 *
 * Revision 1.14  1998/05/28 13:46:32  jyh
 * Updated the editor to use new Refiner structure.
 * ITT needs dform names.
 *
 * Revision 1.13  1998/05/27 15:13:03  jyh
 * Functorized the refiner over the Term module.
 *
 * Revision 1.12  1998/05/07 16:02:49  jyh
 * Adding interactive proofs.
 *
 * Revision 1.11  1998/04/29 20:53:29  jyh
 * Initial working display forms.
 *
 * Revision 1.10  1998/04/21 19:53:43  jyh
 * Upgraded refiner for program extraction.
 *
 * Revision 1.9  1998/04/17 20:48:31  jyh
 * Updating refiner for extraction.
 *
 * Revision 1.8  1998/04/15 22:29:01  jyh
 * Converting packages from summaries.
 *
 * Revision 1.7  1998/02/23 14:46:20  jyh
 * First implementation of binary file compilation.
 *
 * Revision 1.6  1998/02/21 20:57:54  jyh
 * Two phase parse/extract.
 *
 * Revision 1.5  1998/02/19 17:14:01  jyh
 * Splitting filter_parse.
 *
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
