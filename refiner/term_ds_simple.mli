(*
 * Simple functions for manipuating terms with simple shapes
 *
 * Note: many functions in this module (most dest_* and
 * is_* functions) have side-effect:
 * if the term is a Subst, they push substitutions one step down
 * and _replace_ the referenced term with the resulting Term term_single.
 *)

open Opname
open Term_ds

(*
 * Simple terms have no paramaters and
 * all subterms have no binding vars.
 *)
val mk_any_term : operator -> term list -> term
val mk_simple_term : opname -> term list -> term
val dest_simple_term : term -> (opname * term list)

(* Special cases *)
val is_dep0_term : opname -> term -> bool
val mk_dep0_term : opname -> term -> term
val dest_dep0_term : opname -> term -> term
val one_subterm : term -> term

val is_dep0_dep0_term : opname -> term -> bool
val mk_dep0_dep0_term : opname -> term -> term -> term
val dest_dep0_dep0_term : opname -> term -> term * term
val two_subterms : term -> term * term

val is_dep0_dep0_dep0_term : opname -> term -> bool
val mk_dep0_dep0_dep0_term : opname -> term -> term -> term -> term
val dest_dep0_dep0_dep0_term : opname -> term -> term * term * term
val three_subterms : term -> term * term * term
val four_subterms : term -> term * term * term * term
val five_subterms : term -> term * term * term * term * term

val is_dep1_term : opname -> term -> bool
val mk_dep1_term : opname -> string -> term -> term
val dest_dep1_term : opname -> term -> string * term

val is_dep0_dep1_term : opname -> term -> bool
val is_dep0_dep1_any_term : term -> bool
val mk_dep0_dep1_term : opname -> string -> term -> term -> term
val mk_dep0_dep1_any_term : operator -> string -> term -> term -> term
val dest_dep0_dep1_term : opname -> term -> string * term * term
val dest_dep0_dep1_any_term : term -> string * term * term

val is_dep0_dep2_term : opname -> term -> bool
val mk_dep0_dep2_term : opname -> string -> string -> term -> term -> term
val dest_dep0_dep2_term : opname -> term -> string * string * term * term

val is_dep2_dep0_term : opname -> term -> bool
val mk_dep2_dep0_term : opname -> string -> string -> term -> term -> term
val dest_dep2_dep0_term : opname -> term -> string * string * term * term

val is_dep0_dep0_dep1_term : opname -> term -> bool
val mk_dep0_dep0_dep1_term : opname -> term -> term -> string -> term -> term
val dest_dep0_dep0_dep1_term : opname -> term -> term * term * string * term

val is_dep0_dep0_dep1_any_term : term -> bool
val mk_dep0_dep0_dep1_any_term : operator -> term -> term -> string -> term -> term
val dest_dep0_dep0_dep1_any_term : term -> term * term * string * term

val is_dep0_dep1_dep1_term : opname -> term -> bool
val mk_dep0_dep1_dep1_term : opname -> term -> string -> term -> string -> term -> term
val dest_dep0_dep1_dep1_term : opname -> term -> term * string * term * string * term

val is_dep0_dep2_dep2_term : opname -> term -> bool
val mk_dep0_dep2_dep2_term : opname -> term -> string -> string -> term -> string -> string -> term -> term
val dest_dep0_dep2_dep2_term : opname -> term -> term * string * string * term * string * string * term

val is_dep0_dep2_dep0_dep2_term : opname -> term -> bool
val mk_dep0_dep2_dep0_dep2_term : opname -> term -> string -> string -> term -> term -> string -> string -> term -> term
val dest_dep0_dep2_dep0_dep2_term : opname -> term -> term * string * string * term * term * string * string * term

val is_dep0_dep0_dep3_term : opname -> term -> bool
val mk_dep0_dep0_dep3_term : opname -> term -> term -> string -> string -> string -> term -> term
val dest_dep0_dep0_dep3_term : opname -> term -> term * term * string * string * string * term

val is_string_term : opname -> term -> bool
val mk_string_term : opname -> string -> term
val dest_string_term : opname -> term -> string
val dest_string_param : term -> string

val is_string_dep0_term : opname -> term -> bool
val mk_string_dep0_term : opname -> string -> term -> term
val dest_string_dep0_term : opname -> term -> string * term

val is_string_string_dep0_term : opname -> term -> bool
val mk_string_string_dep0_term : opname -> string -> string -> term -> term
val dest_string_string_dep0_term : opname -> term -> string * string * term
val dest_string_string_dep0_any_term : term -> string * string * term

val is_number_number_dep0_term : opname -> term -> bool
val mk_number_number_dep0_term : opname -> Num.num -> Num.num -> term -> term
val dest_number_number_dep0_term : opname -> term -> Num.num * Num.num * term
val dest_number_number_dep0_any_term : term -> Num.num * Num.num * term

val is_string_string_dep0_dep0_term : opname -> term -> bool
val mk_string_string_dep0_dep0_term : opname -> string -> string -> term -> term -> term
val dest_string_string_dep0_dep0_term : opname -> term -> string * string * term * term
val dest_string_string_dep0_dep0_any_term : term -> string * string * term * term

val is_number_term : opname -> term -> bool
val mk_number_term : opname -> Num.num -> term
val dest_number_term : opname -> term -> Num.num
val dest_number_any_term : term -> Num.num

val is_univ_term : opname -> term -> bool
val mk_univ_term : opname -> level_exp -> term
val dest_univ_term : opname -> term -> level_exp

val is_token_term : opname -> term -> bool
val mk_token_term : opname -> string -> term
val dest_token_term : opname -> term -> string

val is_xrewrite_term : term -> bool
val mk_xrewrite_term : term -> term -> term
val dest_xrewrite : term -> term * term

(*
 * Primitive lists.
 *)
val is_xnil_term : term -> bool
val xnil_term : term

val is_xcons_term : term -> bool
val mk_xcons_term : term -> term -> term
val dest_xcons : term -> term * term

val is_xlist_term : term -> bool
val dest_xlist : term -> term list
val mk_xlist_term : term list -> term

(*
 * Primitive strings.
 *)
val is_xstring_term : term -> bool
val mk_xstring_term : string -> term
val dest_xstring : term -> string

(*
 * Primitive abstractions.
 *)
val mk_xlambda_term : string -> term -> term

(*
 * Sequents.
 * This should be visible only to sequents, but oh well.
 *)
val is_sequent_term : term -> bool

val is_hyp_term : term -> bool
val dest_hyp : term -> string * term * term
val mk_hyp_term : string -> term -> term -> term

val is_concl_term : term -> bool
val dest_concl : term -> term * term
val mk_concl_term : term -> term -> term
val null_concl : term

val is_sequent_term : term -> bool
val dest_sequent : term -> term list
val goal_of_sequent : term -> term
val mk_sequent_term : term list -> term

val nth_hyp : term -> int -> string * term
val nth_concl : term -> int -> term
val num_hyps : term -> int
val declared_vars : term -> string list
val declarations : term -> (string * term) list
val get_decl_number : term -> string -> int
val is_free_seq_var : int -> string -> term -> bool

val concl_addr : term -> int * int
val replace_concl : term -> term -> term
val replace_goal : term -> term -> term          (* One subgoal *)
