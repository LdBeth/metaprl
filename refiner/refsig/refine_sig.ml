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
   type rewrite_error

   (************************************************************************
    * ERRORS                                                               *
    ************************************************************************)

   exception FreeContextVars of string list

   (*
    * Unfortunately, we need to declare the general TacticException
    * type here, because the following combinators need to
    * collect exceptions of their subtactics.
    *)
   type refine_error =
      StringError of string
    | TermError of term
    | StringIntError of string * int
    | StringStringError of string * string
    | StringTermError of string * term
    | GoalError of string * refine_error
    | SecondError of string * refine_error
    | SubgoalError of string * int * refine_error
    | PairError of string * refine_error * refine_error
    | RewriteAddressError of string * address * refine_error
    | RewriteError of string * rewrite_error
    | NodeError of string * term * refine_error list

   exception RefineError of refine_error

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
   type msequent =
      { mseq_goal : term;
        mseq_hyps : term list
      }
   
   (************************************************************************
    * PROOFS AND VALIDATIONS                                               *
    ************************************************************************)

   (*
    * An extract is an abstract validation that is generated during
    * proof refinement using tactics.
    *)
   type extract

   (************************************************************************
    * TACTICS                                                              *
    ************************************************************************)

   (*
    * A tactic is the reverse form of validation.
    * given a validation A -> B, that tactic would
    * prove B by asking for a subgoal A.
    *
    * Tactics operate on lists of terms.  These lists
    * represent meta-implications: the head term
    * is the goal, and the remainder are the assumptions.
    *
    * safe_tactic is a subtype of (term -> term list)
    * where the inference is always correct.
    *)
   type tactic

   (* Tactic application *)
   val refine : tactic -> msequent -> msequent list * extract

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
    * UTILITIES                                                            *
    ************************************************************************)
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
   val join_refiner : refiner ref -> refiner -> unit
   val label_refiner : refiner ref -> string -> unit

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
end

(*
 * $Log$
 * Revision 1.3  1998/06/03 15:23:23  jyh
 * Generalized many the term_addr, term_man, and term_shape modules.
 *
 * Revision 1.2  1998/06/01 19:53:42  jyh
 * Working addition proof.  Removing polymorphism from refiner(?)
 *
 * Revision 1.1  1998/05/28 15:01:35  jyh
 * Partitioned refiner into subdirectories.
 *
 * Revision 1.6  1998/05/27 15:13:56  jyh
 * Functorized the refiner over the Term module.
 *
 * Revision 1.5  1998/04/28 18:30:45  jyh
 * ls() works, adding display.
 *
 * Revision 1.4  1998/04/21 19:54:00  jyh
 * Upgraded refiner for program extraction.
 *
 * Revision 1.3  1998/04/17 20:48:40  jyh
 * Updating refiner for extraction.
 *
 * Revision 1.2  1997/08/07 19:43:46  jyh
 * Updated and added Lori's term modifications.
 * Need to update all pattern matchings.
 *
 * Revision 1.1  1997/04/28 15:51:33  jyh
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
 * Revision 1.19  1996/11/13 22:58:09  jyh
 * Initial version of forward/backward chaining cache.
 *
 * Revision 1.18  1996/10/23 15:17:57  jyh
 * First working version of dT tactic.
 *
 * Revision 1.17  1996/09/25 22:52:00  jyh
 * Initial "tactical" commit.
 *
 * Revision 1.16  1996/05/21 02:14:05  jyh
 * This is a semi-working version before Wisconsin vacation.
 *
 * Revision 1.15  1996/03/25 20:50:45  jyh
 * Intermediate commit while modifying grammer.  Restricting
 * ML hooks to condition terms.
 *
 * Revision 1.14  1996/03/11 18:34:25  jyh
 * The filterModule module is untested, but it seems to work
 * correctly on most inputs, except for mlbegin ... mlend expressions.
 * That's the next task.
 *
 * Revision 1.13  1996/03/08 15:40:49  jyh
 * This version works for most constructs except for ML rewrites.
 * The next step will be to break apart the rewriter so that
 * redices and contracta can be compiled separately.
 *
 * Revision 1.12  1996/03/05 19:48:36  jyh
 * Preliminary version with logical framework.
 *
 * Revision 1.11  1996/02/19 18:46:58  jyh
 * Updating format.prl
 *
 * Revision 1.10  1996/02/18 23:32:32  jyh
 * Changin Format module to more Nuprl-like format.
 *
 * Revision 1.9  1996/02/14 03:51:51  jyh
 * This is a version common to Caml-Light and Caml-Special-Light.
 *
 * Revision 1.8  1996/02/13 21:32:31  jyh
 * This is an intermediate checkin while matching is being added to the rewriter.
 *
 * Revision 1.7  1996/02/10 20:19:56  jyh
 * Initial checkin of filter (prlcomp).
 *
 * Revision 1.6  1996/02/08 16:02:32  jyh
 * Adding type Theory.
 *
 * Revision 1.5  1996/02/07 23:41:17  jyh
 * First working version in CamlSpecialLight.
 *
 * Revision 1.4  1996/02/07 20:24:57  jyh
 * Partial checkin whil I change filenames to lowercase.
 *
 * Revision 1.3  1996/02/07 17:34:09  jyh
 * This is Version 0 of the refiner in Caml-Light.  At this point,
 * Caml-Light becomes a branch, and main development will be
 * in Caml-Special-Light.
 *
 * Revision 1.2  1996/02/05 18:14:56  jyh
 * Merge context rewrites onto the main branch.
 *
 * Revision 1.1.4.1  1996/02/05 06:09:53  jyh
 * This version has the rewriter with contexts, and Rule application
 * in Sequent.ml, but it is not fully debugged.
 *
 * Revision 1.1  1996/01/31 20:02:40  jyh
 * Generalizing rewriter to work on Sequents.
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner.run"
 * End:
 * -*-
 *)
