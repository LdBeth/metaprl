(*
 * The refiner works on proof trees, which are trees of sequents.
 * A basic refinement takes a sequent (a "goal") and produces a
 * list of sequents (the "subgoals"), and an extract term.  The type
 * "tactic" packages refinements, and elements of tactics are
 * always "correct" in the sense that they can be reduced to steps
 * of primitive inferences.
 *
 * The refiner also tracks rewrites, and just as for tactics,
 * elements of type Rewrite are always "correct".
 *
 *)

(************************************************************************
 * REFINER MODULE                                                       *
 ************************************************************************)

module type RefineSig =
sig
   type term
   type address
   type meta_term

   (*
    * A ML rewrite replaces a term with another,
    * no extract.
    *)
   type ml_rewrite = (string array * term list) -> term -> term

   (*
    * A condition relaces an goal with a list of subgoals,
    * and it provides a function to compute the extract.
    *)
   type ml_rule =
      { ml_rule_rewrite : (string array * term list) -> term -> term list;
        ml_rule_extract : (string array * term list) -> term list -> term * term list
      }

   (*
    * Refinements are on meta-sequents,
    * which are a restricted form of meta terms,
    * having only dependent functions format.
    *
    * Each hyp is labelled by its first argument.
    *)
   type msequent

   (************************************************************************
    * PROOFS AND VALIDATIONS                                               *
    ************************************************************************)

   (*
    * An extract is an abstract validation that is generated during
    * proof refinement using tactics.
    *)
   type extract

   (*
    * A checker is used to help make sure that the
    * refiner dependencies are respected.  However,
    * the real dependency checker checks the extract.
    *)
   type sentinal

   (************************************************************************
    * TACTICS                                                              *
    ************************************************************************)

   (*
    * Empty checker for just trying refinements.
    *)
   val any_sentinal : sentinal

   (*
    * A tactic is the reverse form of validation.
    * given a validation A -> B, that tactic would
    * prove B by asking for a subgoal A.
    *
    * Tactics operate on lists of terms.  These lists
    * represent meta-implications: the head term
    * is the goal, and the remainder are the assumptions.
    *)
   type tactic

   (* Tactic application *)
   val refine : sentinal -> tactic -> msequent -> msequent list * extract

   (* Compose extract tree *)
   val compose : extract -> extract list -> extract

   (*
    * The base case tactic proves a goal by assumption.
    *)
   val nth_hyp : int -> tactic

   (************************************************************************
    * REWRITES                                                             *
    ************************************************************************)

   (*
    * A normal rewrite can be applied to a term to rewrite it.
    *)
   type rw

   (*
    * Apply a rewrite to a subterm of the goal.
    *)
   val rwaddr : address -> rw -> rw

   (*
    * Convert a rewrite that likes to examine its argument.
    *)
   val rwtactic : rw -> tactic

   (*
    * Composition is supplied for efficiency.
    *)
   val andthenrw : rw -> rw -> rw
   val orelserw : rw -> rw -> rw

   (************************************************************************
    * CONDITIONAL REWRITE                                                  *
    ************************************************************************)

   (*
    * A conditional rewrite is a cross between a rewrite and
    * a tactic.  An application may generate subgoals that must
    * be proved.  A conditional rewrite is valid only for a sequent
    * calculus.
    *)
   type cond_rewrite

   (*
    * Inject a regular rewrite.
    *)
   val mk_cond_rewrite : rw -> cond_rewrite

   (*
    * Ask for the current sequent, and for the term be rewritten.
    *)
   val crwaddr : address -> cond_rewrite -> cond_rewrite

   (*
    * Application of a conditional rewrite.
    * In this application, the rewrite must be applied to
    * a sequent, and it returns the rewritten sequent
    * as the first subgoal.
    *)
   val crwtactic : cond_rewrite -> tactic

   (*
    * Composition is supplied for efficiency.
    *)
   val candthenrw : cond_rewrite -> cond_rewrite -> cond_rewrite
   val corelserw : cond_rewrite -> cond_rewrite -> cond_rewrite

   (************************************************************************
    * META SEQUENT                                                         *
    ************************************************************************)

   (*
    * Con/de-structors.
    *)
   val mk_msequent : term -> term list -> msequent
   val dest_msequent : msequent -> term * term list
   val dest_msequent_vars : msequent -> string list * term * term list

   (*
    * Alpha equality on sequent objects.
    * Tactic argument is ignored
    *)
   val msequent_alpha_equal : msequent -> msequent -> bool

   (*
    * Utils.
    *)
   val split_msequent_list : msequent list -> term list * term list list

   (************************************************************************
    * REFINER INTERFACE                                                    *
    ************************************************************************)

   (*
    * A refiner is basically just a collection of rules, tactics,
    * and rewrites.
    *)
   type refiner

   val null_refiner : refiner

   (*
    * These are the forms created at compile time with
    * extra arguments.
    *)
   type prim_tactic = address array * string array -> term list -> tactic
   type prim_rewrite = rw
   type prim_cond_rewrite = string array * term list -> cond_rewrite

   (*
    * Get the term corresponding to an extract.
    * This will fail if some of the rules are not justified
    * or if the extract is not complete.  The extract terms
    * for the arguments are included.
    *)
   val term_of_extract : refiner -> extract -> term list -> term

   (*
    * Get a checker from the refiner.
    *)
   val sentinal_of_refiner : refiner -> sentinal

   (*
    * An axiom is a term that is true.
    * This adds the theorem, and returns a tactic to prove a
    * goal that is the theorem.  This is used only in a sequent calculus.
    *
    * Once an axiom is defined, it can be justified as
    *    1. primitive (an term extract is given)
    *    2. derived (an extract from a proof is given)
    *    3. delayed (an extract can be computed on request)
    *)
   val create_axiom : refiner ref ->
      string ->                 (* name *)
      term ->                   (* statement *)
      prim_tactic
   val check_axiom : term -> bool
   val prim_axiom : refiner ref ->
      string ->                 (* name *)
      term ->                   (* extract *)
      unit
   val derived_axiom : refiner ref ->
      string ->                 (* name *)
      extract ->                (* derivation *)
      unit
   val delayed_axiom : refiner ref ->
      string ->                 (* name *)
      (unit -> extract) ->      (* derivation *)
      unit

   (*
    * A rule is an implication on terms (the conclusion
    * is true if all the antecedents are).
    *     Args: refiner, name, addrs, params, rule
    *)
   val create_rule : refiner ref ->
      string ->            (* name *)
      string array ->      (* addrs *)
      string array ->      (* vars *)
      term list ->         (* params *)
      meta_term ->         (* rule definition *)
      prim_tactic
   val create_ml_rule : refiner ref ->
      term ->                    (* term to be expanded *)
      ml_rule ->                 (* the rule definition *)
      prim_tactic
   val check_rule :
      string ->            (* name *)
      string array ->      (* addrs *)
      string array ->      (* vars *)
      term list ->         (* params *)
      meta_term ->         (* rule definition *)
      bool

   val prim_rule : refiner ref ->
      string ->                    (* name *)
      string array ->              (* vars *)
      term list ->                 (* params *)
      term list ->                 (* args (binding vars) *)
      term ->                      (* extract *)
      unit
   val derived_rule : refiner ref ->
      string ->                    (* name *)
      string array ->              (* vars *)
      term list ->                 (* params *)
      term list ->                 (* args (binding vars) *)
      extract ->                   (* derived justification *)
      unit
   val delayed_rule : refiner ref ->
      string ->                    (* name *)
      string array ->              (* vars *)
      term list ->                 (* params *)
      term list ->                 (* args (binding vars) *)
      (unit -> extract) ->         (* derived justification *)
      unit

   (*
    * Rewrites.
    *)
   val create_rewrite : refiner ref ->
      string ->            (* name *)
      term ->              (* redex *)
      term ->              (* contractum *)
      prim_rewrite
   val prim_rewrite : refiner ref ->
      string ->            (* name *)
      term ->              (* redex *)
      term ->              (* contractum *)
      unit
   val derived_rewrite : refiner ref ->
      string ->            (* name *)
      term ->              (* redex *)
      term ->              (* contractum *)
      extract ->           (* proof *)
      unit
   val delayed_rewrite : refiner ref ->
      string ->            (* name *)
      term ->              (* redex *)
      term ->              (* contractum *)
      (unit -> extract) -> (* proof *)
      unit

   val create_cond_rewrite : refiner ref ->
      string ->            (* name *)
      string array ->      (* vars *)
      term list ->         (* params *)
      term list ->         (* subgoals *)
      term ->              (* redex *)
      term ->              (* contractum *)
      prim_cond_rewrite
   val create_ml_rewrite : refiner ref -> string ->
      term list ->         (* subgoals *)
      ml_rewrite ->        (* rewriter *)
      prim_cond_rewrite
   val prim_cond_rewrite : refiner ref ->
      string ->            (* name *)
      string array ->      (* vars *)
      term list ->         (* params *)
      term list ->         (* subgoals *)
      term ->              (* redex *)
      term ->              (* contractum *)
      unit
   val derived_cond_rewrite : refiner ref ->
      string ->            (* name *)
      string array ->      (* vars *)
      term list ->         (* params *)
      term list ->         (* subgoals *)
      term ->              (* redex *)
      term ->              (* contractum *)
      extract ->           (* proof *)
      unit
   val delayed_cond_rewrite : refiner ref ->
      string ->            (* name *)
      string array ->      (* vars *)
      term list ->         (* params *)
      term list ->         (* subgoals *)
      term ->              (* redex *)
      term ->              (* contractum *)
      (unit -> extract) -> (* proof *)
      unit
   val check_rewrite :
      string ->            (* string *)
      string array ->      (* vars *)
      term list ->         (* params *)
      term list ->         (* subgoals *)
      term ->              (* redex *)
      term ->              (* contractum *)
      bool

   (*
    * Merge refiners.
    *)
   val label_refiner : refiner ref -> string -> unit
   val join_refiner : refiner ref -> refiner -> unit

   (************************************************************************
    * DESTRUCTION                                                          *
    ************************************************************************)

   type refiner_item =
      RIAxiom of ri_axiom
    | RIRule of ri_rule
    | RIPrimTheorem of ri_prim_theorem
    | RIMLRule of ri_ml_rule

    | RIRewrite of ri_rewrite
    | RICondRewrite of ri_cond_rewrite
    | RIPrimRewrite of ri_prim_rewrite
    | RIMLRewrite of ri_ml_rewrite

    | RIParent of refiner
    | RILabel of string

   and ri_axiom =
      { ri_axiom_name : string;
        ri_axiom_term : term
      }
   and ri_rule =
      { ri_rule_name : string;
        ri_rule_rule : msequent
      }
   and ri_ml_rule =
      { ri_ml_rule_arg : term }
   and ri_prim_theorem =
      { ri_pthm_axiom : refiner }

   and ri_rewrite =
      { ri_rw_name : string;
        ri_rw_redex : term;
        ri_rw_contractum : term
      }
   and ri_cond_rewrite =
      { ri_crw_name : string;
        ri_crw_conds : term list;
        ri_crw_redex : term;
        ri_crw_contractum : term
      }
   and ri_prim_rewrite =
      { ri_prw_rewrite : refiner }
   and ri_ml_rewrite =
      { ri_ml_rw_name : string }

   (*
    * Destructors.
    * dest_refiner raises (Invalid_argument "dest_refiner") if the refiner is empty
    *)
   val is_null_refiner : refiner -> bool
   val dest_refiner : refiner -> refiner_item * refiner
   val find_refiner : refiner -> string -> refiner
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner.run"
 * End:
 * -*-
 *)
