module TermSubstMm :
  functor
    (Term : sig
              type level_exp_var = Term_ds.TermType.level_exp_var
              and level_exp = Term_ds.TermType.level_exp
              and param = Term_ds.TermType.param
              and operator = Term_ds.TermType.operator
              and term = Term_ds.TermType.term
              and term_core = Term_ds.TermType.term_core
              and bound_term = Term_ds.TermType.bound_term
              and esequent = Term_ds.TermType.esequent
              and seq_hyps = Term_ds.TermType.seq_hyps
              and seq_goals = Term_ds.TermType.seq_goals
              and string_set = Term_ds.TermType.StringSet.t
              and hypothesis = Term_ds.TermType.hypothesis
              and level_exp_var' = Term_ds.TermType.level_exp_var'
              and level_exp' = Term_ds.TermType.level_exp'
              and object_id = Term_ds.TermType.object_id
              and param' = Term_ds.TermType.param'
              and operator' = Term_ds.TermType.operator'
              and term' = Term_ds.TermType.term'
              and bound_term' = Term_ds.TermType.bound_term'
              and term_subst = Term_ds.TermType.term_subst
              module SeqHyp :
                sig
                  type elt = hypothesis
                  and t = seq_hyps
                  and index = int
                  val empty : t
                  val singleton : elt -> t
                  val length : t -> int
                  val get : t -> index -> elt
                  val make : int -> elt -> t
                  val create : int -> elt -> t
                  val to_list : t -> elt list
                  val of_list : elt list -> t
                  val iter : (elt -> unit) -> t -> unit
                  val split : t -> index -> t * elt * t
                  val append : t -> elt -> t -> t
                  val append_list : t -> elt list -> t -> t
                  val lazy_apply : (elt -> elt) -> t -> t
                  val lazy_sub_map : (elt -> elt) -> t -> index -> index -> t
                  val mapi : (index -> elt -> elt) -> t -> t
                  val init : int -> (index -> elt) -> t
                  val collect : (elt, t) Array_util.array_part list -> t
                end
              module SeqGoal :
                sig
                  type elt = term
                  and t = seq_goals
                  and index = int
                  val empty : t
                  val singleton : elt -> t
                  val length : t -> int
                  val get : t -> index -> elt
                  val make : int -> elt -> t
                  val create : int -> elt -> t
                  val to_list : t -> elt list
                  val of_list : elt list -> t
                  val iter : (elt -> unit) -> t -> unit
                  val split : t -> index -> t * elt * t
                  val append : t -> elt -> t -> t
                  val append_list : t -> elt list -> t -> t
                  val lazy_apply : (elt -> elt) -> t -> t
                  val lazy_sub_map : (elt -> elt) -> t -> index -> index -> t
                  val mapi : (index -> elt -> elt) -> t -> t
                  val init : int -> (index -> elt) -> t
                  val collect : (elt, t) Array_util.array_part list -> t
                end
              val term_free_vars : term -> string_set
              val bterm_free_vars : bound_term -> string_set
              val do_term_subst : term_subst -> term -> term
              val do_bterm_subst : term_subst -> bound_term -> bound_term
              val get_core : term -> term_core
              val fail_core : string -> 'a
              val dest_term : term -> term'
              val make_term : term' -> term
              val mk_op : Opname.opname -> param list -> operator
              val mk_term : operator -> bound_term list -> term
              val mk_bterm : string list -> term -> bound_term
              val make_bterm : bound_term' -> bound_term
              val dest_bterm : bound_term -> bound_term'
              val mk_level : int -> level_exp_var list -> level_exp
              val mk_level_var : string -> int -> level_exp_var
              val mk_sequent_term : esequent -> term
              val no_bvars : bound_term list -> bool
              val mk_simple_bterm : term -> bound_term
              val dest_simple_bterm : bound_term -> term
              val opname_of_term : term -> Opname.opname
              val subterms_of_term : term -> term list
              val subterm_count : term -> int
              val subterm_arities : term -> int list
              val make_op : operator' -> operator
              val dest_op : operator -> operator'
              val make_param : param' -> param
              val dest_param : param -> param'
              val make_level : level_exp' -> level_exp
              val dest_level : level_exp -> level_exp'
              val make_level_var : level_exp_var' -> level_exp_var
              val dest_level_var : level_exp_var -> level_exp_var'
              val make_object_id : param list -> object_id
              val dest_object_id : object_id -> param list
              val var_opname : Opname.opname
              val context_opname : Opname.opname
              val xperv : Opname.opname
              val sequent_opname : Opname.opname
              val is_var_term : term -> bool
              val dest_var : term -> string
              val mk_var_term : string -> term
              val is_so_var_term : term -> bool
              val dest_so_var : term -> string * term list
              val mk_so_var_term : string -> term list -> term
              val is_context_term : term -> bool
              val dest_context : term -> string * term * term list
              val mk_context_term : string -> term -> term list -> term
              val mk_any_term : operator -> term list -> term
              val mk_simple_term : Opname.opname -> term list -> term
              val dest_simple_term : term -> Opname.opname * term list
              val is_simple_term_opname : Opname.opname -> term -> bool
              val dest_simple_term_opname :
                Opname.opname -> term -> term list
              val debug_print : out_channel -> term -> unit
              val print_term : out_channel -> term -> unit
              val print_term_list : out_channel -> term list -> unit
              val install_debug_printer :
                (out_channel -> term -> unit) -> unit
              val dest_descriptor :
                term -> term Weak_memo.TheWeakMemo.descriptor option
              val mk_descriptor_term :
                term Weak_memo.TheWeakMemo.descriptor -> term
            end) ->
    functor
      (RefineError : sig
                       type level_exp = Term_ds.TermType.level_exp
                       and param = Term_ds.TermType.param
                       and term = Term_ds.TermType.term
                       and bound_term = Term_ds.TermType.bound_term
                       and meta_term
                       and address
                       and seq_hyps
                       and seq_goals
                       and match_type =
                         | ParamMatch of param
                         | VarMatch of string
                         | TermMatch of term
                         | BTermMatch of bound_term
                         | HypMatch of seq_hyps
                         | GoalMatch of seq_goals
                       and refine_error =
                         | GenericError
                         | ToploopIgnoreError
                         | StringError of string
                         | IntError of int
                         | TermError of term
                         | StringIntError of string * int
                         | StringStringError of string * string
                         | StringTermError of string * term
                         | GoalError of string * refine_error
                         | SecondError of string * refine_error
                         | SubgoalError of int * string * refine_error
                         | PairError of string * refine_error * string *
                                          refine_error
                         | NodeError of string * term *
                                          (string * refine_error) list
                         | AddressError of address * term
                         | TermMatchError of term * string
                         | TermPairMatchError of term * term
                         | MetaTermMatchError of meta_term
                         | RewriteBoundSOVar of string
                         | RewriteFreeSOVar of string
                         | RewriteSOVarArity of string
                         | RewriteBoundParamVar of string
                         | RewriteFreeParamVar of string
                         | RewriteBadRedexParam of param
                         | RewriteNoRuleOperator
                         | RewriteBadMatch of match_type
                         | RewriteAllSOInstances of string
                         | RewriteMissingContextArg of string
                         | RewriteStringError of string
                         | RewriteAddressError of address * string *
                                                    refine_error
                         | RewriteFreeContextVars of string list
                       exception RefineError of string * refine_error
                       val generic_refiner_exn : exn
                     end) ->
      sig
        type term = Term_ds.TermType.term
        and term_subst = Term_ds.TermType.term_subst


(*  
*    MM-unification deals with the first order unification problems of the form
*               T1_i = T2_i  , i=1,2,...
*    for terms with bindings from Term_ds.TermType.term. The problems are members 
*    of eqnlist type. The unification is treated as a transformation of 
*    an arbitrary unification problem into an equivalent  problem in the 
*    "solved" form:
*           x1=F1(x2,...,xm)
*           x2=F2(x3,...,xm)
*           ...
*    If it is impossible then the exceptions Cycle or Clash are raised.
*    The conversion of a unification problem into its mgu always 
*    implies  the transformation into the "solved" form (N*log N) and
*    the calculation of the product of substitutions which may be more
*    expensive (m^2). The internal representation gives some speed-up
*    (5-10 times faster) but failes to reduce the order. 
*       Extract the mgu only when you need it!
*       Use unify_eqnl_eqnl for iterative calls.
*       The unifiable* functions are much faster!
*       In the negative case all the functions run in the same time!       
*       Use  eqnlist2ttlist if you need the unification problem as is.   
*)

        type  eqnlist = (term*term) list 
       
        val eqnlist_empty : eqnlist
        val eqnlist_append_eqns : eqnlist -> (term*term) list -> eqnlist
        val eqnlist2ttlist : eqnlist ->(term*term) list  
        
        val unifiable : Term.term -> Term.term -> string list -> bool 
        val unifiable_eqnl : eqnlist ->  string list -> bool

        val unify : Term.term -> Term.term -> string list -> term_subst 
        val unify_eqnl : eqnlist ->  string list -> term_subst
        val unify_eqnl_eqnl :  eqnlist ->  string list -> eqnlist 
  
      end









