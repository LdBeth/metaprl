(*
 * Marshal proofs to terms.
 *)

include Io_proof_type

open Printf
open Debug

open Opname
open Refiner.Refiner.TermType
open Refiner.Refiner.Term
open Refiner.Refiner.TermOp
open Refiner.Refiner.TermMan
open Rformat
open Dform

open Filter_ocaml

open Tactic_type
open Tacticals
open Io_proof_type

(*
 * Show the file loading.
 *)
let _ =
   if !debug_load then
      eprintf "Loading Filter_proof%t" eflush

(************************************************************************
 * PRINTING                                                             *
 ************************************************************************)

(*
 * Print the proof.
 * This is really for debugging.
 *)
let print_proof db proof =
   let buf = new_buffer () in
   let format_list format l =
      List.iter (fun x -> format_newline buf; format x) l
   in
   let rec format_proof { proof_status = status;
                          proof_step = node;
                          proof_children = children
       } =
      format_status status;
      format_newline buf;
      format_node node;
      format_list format_child children
   and format_status = function
      StatusBad ->
         format_string buf "Bad"
    | StatusPartial ->
         format_string buf "Partial"
    | StatusAsserted ->
         format_string buf "Asserted"
    | StatusComplete ->
         format_string buf "Complete"
   and format_node = function
      ProofStep step ->
         format_step step
    | ProofNode proof ->
         format_string buf "   ";
         format_pushm buf 0;
         format_proof proof;
         format_popm buf
   and format_child = function
      ChildGoal goal ->
         format_string buf "Goal: ";
         format_pushm buf 0;
         format_aterm goal;
         format_popm buf
    | ChildProof proof ->
         format_string buf "   ";
         format_pushm buf 0;
         format_proof proof;
         format_popm buf
   and format_step { step_goal = goal;
                     step_subgoals = subgoals;
                     step_text = text
       } =
      format_aterm goal;
      format_newline buf;
      format_string buf "BY ";
      format_string buf text;
      format_list format_aterm subgoals
   and format_aterm { aterm_goal = goal } =
      format_term db buf goal
   in
      format_proof proof;
      print_to_channel 120 buf stdout;
      flush stdout

(************************************************************************
 * TERM CONVERSION                                                      *
 ************************************************************************)

let summary_opname         = mk_opname "Summary" nil_opname

let interface_op           = mk_opname "interface"         summary_opname
let implementation_op      = mk_opname "implementation"    summary_opname

let interactive_op         = mk_opname "interactive"       summary_opname

let status_bad_op          = mk_opname "status_bad"        summary_opname
let status_partial_op      = mk_opname "status_partial"    summary_opname
let status_asserted_op     = mk_opname "status_asserted"   summary_opname
let status_complete_op     = mk_opname "status_complete"   summary_opname

let proof_step_op          = mk_opname "proof_step"        summary_opname
let proof_node_op          = mk_opname "proof_node"        summary_opname

let proof_child_goal_op    = mk_opname "proof_child_goal"  summary_opname
let proof_child_proof_op   = mk_opname "proof_child_proof" summary_opname

let proof_aterm_op         = mk_opname "proof_aterm"       summary_opname

let proof_term_arg_op      = mk_opname "proof_term_arg"    summary_opname
let proof_type_arg_op      = mk_opname "proof_type_arg"    summary_opname
let proof_int_arg_op       = mk_opname "proof_int_arg"     summary_opname
let proof_bool_arg_op      = mk_opname "proof_bool_arg"    summary_opname
let proof_subst_arg_op     = mk_opname "proof_subst_arg"   summary_opname
let proof_tactic_arg_op    = mk_opname "proof_tactic_arg"  summary_opname

(*
 * Make a term with a string parameter.
 *)
let mk_simple_string_term opname s terms =
   let param = make_param (String s) in
   let op = mk_op opname [param] in
   let bterms = List.map (fun t -> mk_bterm [] t) terms in
      mk_term op bterms

let mk_simple_int_term opname i terms =
   let param = make_param (Number (Num.Int i)) in
   let op = mk_op opname [param] in
   let bterms = List.map (fun t -> mk_bterm [] t) terms in
      mk_term op bterms

let mk_string_string_term opname s1 s2 =
   let param1 = make_param (String s1) in
   let param2 = make_param (String s2) in
   let op = mk_op opname [param1; param2] in
      mk_term op []

let mk_string_int_term opname s i =
   let param1 = make_param (String s) in
   let param2 = make_param (Number (Num.Int i)) in
   let op = mk_op opname [param1; param2] in
      mk_term op []

let dest_string_string_term t =
   let { term_op = op } = dest_term t in
   let { op_params = params } = dest_op op in
      match List.map dest_param params with
         [String s1; String s2] ->
            s1, s2
       | _ ->
            raise (Failure "dest_string_string_term")

let dest_string_int_term t =
   let { term_op = op } = dest_term t in
   let { op_params = params } = dest_op op in
      match List.map dest_param params with
         [String s; Number n] ->
            s, Num.int_of_num n
       | _ ->
            raise (Failure "dest_string_string_term")

let comment _ _ t = t

(************************************************************************
 * TO TERM                                                              *
 ************************************************************************)

(*
 * We define partially compiled types.
 *)
type 'term term_aterm =
   { term_aterm_goal  : 'term;         (* Goal of this step *)
     term_aterm_hyps  : 'term list;    (* Assumptions in this proof *)
     term_aterm_label : string;        (* Label describes the step in the proof *)
     term_aterm_args  : 'term          (* Proof annotations *)
   }

type 'term term_proof_step =
   { term_step_goal : 'term;
     term_step_subgoals : 'term list;
     term_step_ast : MLast.expr;       (* Parsed expression *)
     term_step_text : string           (* String representation of text *)
   }

(*
 * A proof is a recursive tree of steps.
 * The status is redundant, except for "asserted" proofs.
 * Asserted proofs are incomplete, but they are marked as complete.
 *)
type 'term term_proof =
   { term_proof_status : proof_status;
     term_proof_step : 'term;
     term_proof_children : 'term list;
     term_proof_extras : 'term list
   }

and 'term term_proof_node =
   TermProofStep of 'term term_proof_step
 | TermProofNode of 'term

and 'term term_proof_child =
   TermChildGoal of 'term
 | TermChildProof of 'term

(*
 * The proof contains a lot of sharing.
 * So use a memoizer to keep it.
 *)
type ('a, 'b) to_term =
   { term_f              : 'a -> 'b;
     term_of_proof       : (('a, 'b) to_term, 'a proof,       'b term_proof,       term) Memo.t;
     term_of_proof_node  : (('a, 'b) to_term, 'a proof_node,  'b term_proof_node,  term) Memo.t;
     term_of_proof_child : (('a, 'b) to_term, 'a proof_child, 'b term_proof_child, term) Memo.t;
     term_of_aterm       : (('a, 'b) to_term, 'a aterm,       'b term_aterm,       term) Memo.t;
     term_of_attributes  : (('a, 'b) to_term, 'a attributes,  'b attributes,       term) Memo.t
   }

(*
 * Compare that the elements on the lists are equal.
 *)
let rec list_mem_eq l1 l2 =
   match l1, l2 with
      h1::t1, h2::t2 ->
         h1 == h2 & list_mem_eq t1 t2
    | [], [] ->
         true
    | _ ->
         false

let compare_proof
    { term_proof_status = status1;
      term_proof_step = step1;
      term_proof_children = children1;
      term_proof_extras = extras1
    }
    { term_proof_status = status2;
      term_proof_step = step2;
      term_proof_children = children2;
      term_proof_extras = extras2
    } =
   (status1 = status2)
   & (step1 == step2)
   & (list_mem_eq children1 children2)
   & (list_mem_eq extras1 extras2)

let compare_proof_step
    { term_step_goal = goal1;
      term_step_subgoals = subgoals1;
      term_step_ast = ast1;
      term_step_text = text1
    }
    { term_step_goal = goal2;
      term_step_subgoals = subgoals2;
      term_step_ast = ast2;
      term_step_text = text2
    } =
   (goal1 == goal2)
   & (list_mem_eq subgoals1 subgoals2)
   & (ast1 = ast2)
   & (text1 = text2)

let compare_proof_node node1 node2 =
   match node1, node2 with
      TermProofStep step1, TermProofStep step2 ->
         compare_proof_step step1 step2
    | TermProofNode node1, TermProofNode node2 ->
         node1 == node2
    | _ ->
         false

let compare_proof_child child1 child2 =
   match child1, child2 with
      TermChildGoal goal1, TermChildGoal goal2 ->
         goal1 == goal2
    | TermChildProof proof1, TermChildProof proof2 ->
         proof1 == proof2
    | _ ->
         false

let compare_aterm
    { term_aterm_goal = goal1;
      term_aterm_hyps = hyps1;
      term_aterm_label = label1;
      term_aterm_args = args1
    }
    { term_aterm_goal = goal2;
      term_aterm_hyps = hyps2;
      term_aterm_label = label2;
      term_aterm_args = args2
    } =
   (goal1 == goal2)
   & (list_mem_eq hyps1 hyps2)
   & (label1 = label2)
   & (args1 == args2)

let compare_attributes list1 list2 =
   let compare (name1, att1) (name2, att2) =
      if name1 = name2 then
         match att1, att2 with
            TermArg t1, TermArg t2 ->
               t1 == t2
          | TypeArg t1, TypeArg t2 ->
               t1 == t2
          | IntArg i1, IntArg i2 ->
               i1 = i2
          | BoolArg b1, BoolArg b2 ->
               b1 = b2
          | SubstArg t1, SubstArg t2 ->
               t1 == t2
          | _ ->
               false
      else
         false
   in
      List_util.for_all2 compare list1 list2

(*
 * Copying functions perform lookup.
 *)
let make_proof info { proof_status = status;
                      proof_step = step;
                      proof_children = children;
                      proof_extras = extras
    } =
   { term_proof_status = status;
     term_proof_step = Memo.apply info.term_of_proof_node info step;
     term_proof_children = List.map (Memo.apply info.term_of_proof_child info) children;
     term_proof_extras = List.map (Memo.apply info.term_of_proof info) extras
   }

let make_proof_node info = function
   ProofStep { step_goal = goal;
               step_subgoals = subgoals;
               step_ast = ast;
               step_text = text
   } ->
      let step =
         { term_step_goal = Memo.apply info.term_of_aterm info goal;
           term_step_subgoals = List.map (Memo.apply info.term_of_aterm info) subgoals;
           term_step_ast = ast;
           term_step_text = text
         }
      in
         TermProofStep step
 | ProofNode proof ->
      TermProofNode (Memo.apply info.term_of_proof info proof)

let make_proof_step info { step_goal = goal;
                           step_subgoals = subgoals;
                           step_ast = ast;
                           step_text = text
    } =
   { term_step_goal = Memo.apply info.term_of_aterm info goal;
     term_step_subgoals = List.map (Memo.apply info.term_of_aterm info) subgoals;
     term_step_ast = ast;
     term_step_text = text
   }

let make_proof_child info = function
   ChildGoal goal ->
      TermChildGoal (Memo.apply info.term_of_aterm info goal)
 | ChildProof proof ->
      TermChildProof (Memo.apply info.term_of_proof info proof)

let make_aterm info { aterm_goal = goal;
                      aterm_hyps = hyps;
                      aterm_label = label;
                      aterm_args = args
    } =
   { term_aterm_goal = info.term_f goal;
     term_aterm_hyps = List.map info.term_f hyps;
     term_aterm_label = label;
     term_aterm_args = Memo.apply info.term_of_attributes info args
   }

(*
 * Attributes.
 *)
let rec make_attributes info = function
   [] ->
      []
 | (name, arg) :: tl ->
      let tl = make_attributes info tl in
         match arg with
            TermArg t ->
               (name, TermArg (info.term_f t)) :: tl
          | TypeArg t ->
               (name, TypeArg (info.term_f t)) :: tl
          | IntArg i ->
               (name, IntArg i) :: tl
          | BoolArg b ->
               (name, BoolArg b) :: tl
          | SubstArg t ->
               (name, SubstArg (info.term_f t)) :: tl
          | _ ->
               tl

(*
 * Convert a proof to a term.
 *)
let term_of_status status =
   let op =
      match status with
         StatusBad ->
            status_bad_op
       | StatusPartial ->
            status_partial_op
       | StatusAsserted ->
            status_asserted_op
       | StatusComplete ->
            status_complete_op
   in
      mk_simple_term op []

let term_of_proof _
    { term_proof_status = status;
      term_proof_step = step;
      term_proof_children = children;
      term_proof_extras = extras
    } =
   mk_simple_term interactive_op [term_of_status status;
                                  step;
                                  mk_xlist_term children;
                                  mk_xlist_term extras]

let term_of_proof_node _ = function
   TermProofStep { term_step_goal = goal;
                   term_step_subgoals = subgoals;
                   term_step_ast = ast;
                   term_step_text = text
   } ->
      mk_simple_term proof_step_op [goal;
                                    mk_xlist_term subgoals;
                                    term_of_expr [] comment ast;
                                    mk_simple_string_term proof_step_op text []]
 | TermProofNode proof ->
      mk_simple_term proof_node_op [proof]

let term_of_proof_child _ = function
   TermChildGoal goal ->
      mk_simple_term proof_child_goal_op [goal]
 | TermChildProof proof ->
      mk_simple_term proof_child_proof_op [proof]

let term_of_aterm _
    { term_aterm_goal = goal;
      term_aterm_hyps = hyps;
      term_aterm_label = label;
      term_aterm_args = args
    } =
   mk_simple_term proof_aterm_op [goal;
                                  mk_xlist_term hyps;
                                  args;
                                  mk_simple_string_term proof_aterm_op label []]

let term_of_attributes _ attributes =
   let rec collect = function
      [] ->
         []
    | (name, att) :: tl ->
         let tl = collect tl in
            match att with
               Tactic_type.TermArg t ->
                  (mk_simple_string_term proof_term_arg_op name [t]) :: tl
             | Tactic_type.TypeArg t ->
                  (mk_simple_string_term proof_type_arg_op name [t]) :: tl
             | Tactic_type.IntArg i ->
                  (mk_string_int_term proof_int_arg_op name i) :: tl
             | Tactic_type.BoolArg flag ->
                  let s =
                     if flag then
                        "true"
                     else
                        "false"
                  in
                     (mk_string_string_term proof_bool_arg_op name s) :: tl
             | Tactic_type.SubstArg t ->
                  (mk_simple_string_term proof_subst_arg_op name [t]) :: tl
             | Tactic_type.TacticArg _
             | Tactic_type.IntTacticArg _
             | Tactic_type.TypeinfArg _ ->
                  tl
   in
      mk_xlist_term (collect attributes)

let compare_attributes args1 args2 =
   args1 == args2

let term_of_proof term_f proof =
   let info =
      { term_f              = term_f;
        term_of_proof       = Memo.create make_proof term_of_proof compare_proof;
        term_of_proof_node  = Memo.create make_proof_node term_of_proof_node compare_proof_node;
        term_of_proof_child = Memo.create make_proof_child term_of_proof_child compare_proof_child;
        term_of_aterm       = Memo.create make_aterm term_of_aterm compare_aterm;
        term_of_attributes  = Memo.create make_attributes term_of_attributes compare_attributes
      }
   in
      Memo.apply info.term_of_proof info proof

(************************************************************************
 * FROM TERMS                                                           *
 ************************************************************************)

(*
 * The proof contains a lot of sharing.
 * So use a memoizer to keep it.
 *)
type 'a from_term =
   { term_f              : term -> 'a;
     global_args         : 'a attributes;
     proof_of_term       : ('a from_term, term, term term_proof,       'a proof)       Memo.t;
     proof_node_of_term  : ('a from_term, term, term term_proof_node,  'a proof_node)  Memo.t;
     proof_child_of_term : ('a from_term, term, term term_proof_child, 'a proof_child) Memo.t;
     aterm_of_term       : ('a from_term, term, term term_aterm,       'a aterm)       Memo.t;
     attributes_of_term  : ('a from_term, term, term,                  'a attributes)  Memo.t
   }

(*
 * Convert the term back into a proof.
 *)
let make_status t =
   let op = opname_of_term t in
      if op == status_bad_op then
         StatusBad
      else if op == status_partial_op then
         StatusPartial
      else if op == status_asserted_op then
         StatusAsserted
      else if op == status_complete_op then
         StatusComplete
      else
         raise (Failure "Filter_proof.status_of_term")

let make_proof info t =
   let status, step, children, extras = four_subterms t in
      { term_proof_status = make_status status;
        term_proof_step = step;
        term_proof_children = dest_xlist children;
        term_proof_extras = dest_xlist extras
      }

let make_proof_node info t =
   let op = opname_of_term t in
      if op == proof_step_op then
         let goal, subgoals, ast, text = four_subterms t in
         let step =
            { term_step_goal = goal;
              term_step_subgoals = dest_xlist subgoals;
              term_step_ast = expr_of_term ast;
              term_step_text = dest_string_param text
            }
         in
            TermProofStep step
      else if op == proof_node_op then
         TermProofNode (one_subterm t)
      else
         raise (Failure "Filter_proof.step_of_term")

let make_proof_child info t =
   let op = opname_of_term t in
      if op == proof_child_goal_op then
         TermChildGoal (one_subterm t)
      else if op == proof_child_proof_op then
         TermChildProof (one_subterm t)
      else
         raise (Failure "Filter_proof.child_of_term")

let make_aterm info t =
   let goal, hyps, args, label = four_subterms t in
      { term_aterm_goal = goal;
        term_aterm_hyps = dest_xlist hyps;
        term_aterm_label = dest_string_param label;
        term_aterm_args = args
      }

let make_attributes info t =
   t

let proof_of_term info
    { term_proof_status = status;
      term_proof_step = step;
      term_proof_children = children;
      term_proof_extras = extras
    } =
   { proof_status = status;
     proof_step = Memo.apply info.proof_node_of_term info step;
     proof_children = List.map (Memo.apply info.proof_child_of_term info) children;
     proof_extras = List.map (Memo.apply info.proof_of_term info) extras
   }

let proof_node_of_term info = function
   TermProofStep { term_step_goal = goal;
                   term_step_subgoals = subgoals;
                   term_step_ast = ast;
                   term_step_text = text
   } ->
      let step =
         { step_goal = Memo.apply info.aterm_of_term info goal;
           step_subgoals = List.map (Memo.apply info.aterm_of_term info) subgoals;
           step_ast = ast;
           step_text = text
         }
      in
         ProofStep step
 | TermProofNode proof ->
      ProofNode (Memo.apply info.proof_of_term info proof)

let proof_child_of_term info = function
   TermChildGoal goal ->
      ChildGoal (Memo.apply info.aterm_of_term info goal)
 | TermChildProof proof ->
      ChildProof (Memo.apply info.proof_of_term info proof)

let aterm_of_term info
      { term_aterm_goal = goal;
        term_aterm_hyps = hyps;
        term_aterm_label = label;
        term_aterm_args = args
      } =
   { aterm_goal = info.term_f goal;
     aterm_hyps = List.map info.term_f hyps;
     aterm_label = label;
     aterm_args = Memo.apply info.attributes_of_term info args
   }

let attributes_of_term info t =
   let rec collect = function
      [] ->
         info.global_args
    | t :: tl ->
         let op = opname_of_term t in
         let att =
            if op == proof_term_arg_op then
               let name, t = dest_string_dep0_term op t in
                  name, TermArg (info.term_f t)
            else if op == proof_type_arg_op then
               let name, t = dest_string_dep0_term op t in
                  name, TypeArg (info.term_f t)
            else if op == proof_int_arg_op then
               let name, i = dest_string_int_term t in
                  name, IntArg i
            else if op == proof_bool_arg_op then
               let name, flag = dest_string_string_term t in
                  name, BoolArg (flag <> "false")
            else if op == proof_subst_arg_op then
               let name, t = dest_string_dep0_term op t in
                  name, SubstArg (info.term_f t)
            else
               raise (Failure "Filter_proof.attribute_of_term")
         in
            att :: collect tl
   in
      collect (dest_xlist t)

let proof_of_term term_f global_args proof =
   let info =
      { term_f              = term_f;
        global_args         = global_args;
        proof_of_term       = Memo.create make_proof proof_of_term compare_proof;
        proof_node_of_term  = Memo.create make_proof_node proof_node_of_term compare_proof_node;
        proof_child_of_term = Memo.create make_proof_child proof_child_of_term compare_proof_child;
        aterm_of_term       = Memo.create make_aterm aterm_of_term compare_aterm;
        attributes_of_term  = Memo.create make_attributes attributes_of_term compare_attributes
      }
   in
      Memo.apply info.proof_of_term info proof

(************************************************************************
 * TACTIC HANDLING                                                      *
 ************************************************************************)

(*
 * Get tactic array.
 *)
let tactics_of_proof proof =
   let hash = Hashtbl.create 107 in
   let rec collect_proof
       { proof_step = step;
         proof_children = children;
         proof_extras = extras
       } =
      collect_proof_step step;
      List.iter collect_proof_child children;
      List.iter collect_proof extras

   and collect_proof_step = function
      ProofStep { step_ast = ast; step_text = text } ->
         Hashtbl.remove hash text;
         Hashtbl.add hash text ast
    | ProofNode proof ->
         collect_proof proof

   and collect_proof_child = function
      ChildGoal goal ->
         ()
    | ChildProof proof ->
         collect_proof proof
   in

   let loc = (0, 0) in
   let entries = ref [] in
   let collect name tac =
      let name = <:expr< $str: name$ >> in
      let pair = <:expr< ( $list: [ name; tac ]$ ) >> in
         entries := pair :: !entries
   in
   let _ = collect_proof proof in
   let _ = Hashtbl.iter collect hash in
      <:expr< [| $list: !entries$ |] >>

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
