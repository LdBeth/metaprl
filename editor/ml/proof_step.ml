(*
 * This is the basic step in an interactive proof.
 * It contains the goal, a list of subgoals, the tactic
 * used in the refinment, and the text corresponding to the tactic.
 *
 *)

open Term

include Tactic_type

(************************************************************************
 * TYPES                                                                *
 ************************************************************************)

(*
 * We keep all the info from the refiner,
 * plus the string representing the tactic.
 *)
type t =
   { step_goal : tactic_arg;
     step_subgoals : tactic_arg list;
     step_text : string;
     step_ast : MLast.expr;
     step_tactic : tactic
   }

(*
 * Compute a magic number from these types because we marshal
 * them to a file.
 *)
magic_block magic_number =
struct
   (*
    * An IO step does not save the tactic,
    * and saves only part of the goal.
    *)
   type aterm =
      { aterm_goal : term;
        aterm_label : string;
        aterm_args : refine_attribute list;
        aterm_cache : cache
      }

   type io_proof_step =
      { io_step_goal : aterm;
        io_step_subgoals : aterm list;
        io_step_ast : MLast.expr;
        io_step_text : string
      }

   (*
    * A handle is just a number used to lookup the
    * step from the database.
    *)
   type handle = int

   (*
    * The base stored in the file is an array of steps.
    *)
   type in_base = io_proof_step array
end

(*
 * The output base is used to collect steps to
 * save to a file.  The out base is a list,
 * and it is converted to an array for saving
 * in the file.
 *)
type out_base =
   { mutable out_steps : io_proof_step list;
     mutable out_count : int
   }

(************************************************************************
 * IMPLEMENTATION                                                       *
 ************************************************************************)

(*
 * Constructor.
 *)
let create goal subgoals text ast tac =
   { step_goal = goal;
     step_subgoals = subgoals;
     step_tactic = tac;
     step_ast = ast;
     step_text = text
   }

(*
 * Destructors.
 *)
let step_goal { step_goal = goal } = goal
let step_subgoals { step_subgoals = goals } = goals
let step_text { step_text = text } = text
let step_ast { step_ast = ast } = ast
let step_tactic { step_tactic = tac } = tac

(************************************************************************
 * BASE OPERATIONS                                                      *
 ************************************************************************)

(*
 * Throw away extra information from the goal.
 *)
let aterm_of_goal (t, { ref_label = label; ref_args = args; ref_fcache = fcache }) =
   { aterm_goal = t;
     aterm_label = label;
     aterm_args = args;
     aterm_cache = fcache
   }

let goal_of_aterm resources
    { aterm_goal = t;
      aterm_label = label;
      aterm_args = args;
      aterm_cache = fcache
    } =
   (t, { ref_label = label;
         ref_args = args;
         ref_fcache = fcache;
         ref_rsrc = resources
    })

(*
 * Throw away information.
 *)
let io_step_of_step 
    { step_goal = goal;
      step_subgoals = subgoals;
      step_tactic = tac;
      step_text = text;
      step_ast = ast
    } =
   { io_step_goal = aterm_of_goal goal;
     io_step_subgoals = List.map aterm_of_goal subgoals;
     io_step_text = text;
     io_step_ast = ast
   }

(*
 * Add the resource information.
 *)
let step_of_io_step resources tactic
    { io_step_goal = goal;
      io_step_subgoals = subgoals;
      io_step_text = text;
      io_step_ast = ast
    } =
   { step_goal = goal_of_aterm resources goal;
     step_subgoals = List.map (goal_of_aterm resources) subgoals;
     step_tactic = tactic;
     step_text = text;
     step_ast = ast
   }

(*
 * New output base.
 *)
let create_out_base () =
   { out_steps = [];
     out_count = 0
   }

(*
 * To save a step, add it to the list and
 * increment the counter.
 *)
let save_step base step =
   let { out_steps = steps; out_count = count } = base in
      base.out_steps <- (io_step_of_step step) :: steps;
      base.out_count <- count + 1;
      count

(*
 * In base is an array.
 *)
let in_base_of_out_base (l : out_base) : in_base =
   Array.of_list (List.rev l.out_steps)

(*
 * To save the base, marshal it with a magic number.
 *)
let save_base base out =
   output_binary_int out magic_number;
   output_value out (in_base_of_out_base base)

(*
 * Restore the marshaled base.
 *)
let restore_base inx =
   try 
      let magic' = input_binary_int inx in
         if magic' = magic_number then
            (input_value inx : in_base)
         else
            raise (Failure "Proof_step.restore_base: bad magic number")
   with
      End_of_file ->
         raise (Failure "Proof_step.restore_base: premature end of file")

(*
 * Extract the tactic expressions from the base.
 *)
let restore_tactics inx =
   let base = restore_base inx in
   let extract { io_step_ast = ast } = ast in
      Array.map extract base

(*
 * Restore a proof step, given the resources for this proof.
 *)
let restore_step base resources tacs hand =
   step_of_io_step resources tacs.(hand) base.(hand)

(*
 * $Log$
 * Revision 1.2  1998/04/09 19:07:27  jyh
 * Updating the editor.
 *
 * Revision 1.1  1997/08/06 16:17:24  jyh
 * This is an ocaml version with subtyping, type inference,
 * d and eqcd tactics.  It is a basic system, but not debugged.
 *
 * Revision 1.4  1996/09/02 19:33:37  jyh
 * Semi-working package management.
 *
 * Revision 1.3  1996/05/21 02:25:42  jyh
 * This is a semi-working version before Wisconsin vacation.
 *
 * Revision 1.2  1996/05/20 17:00:10  jyh
 * This is an intermediate form of the editor with modules
 * before debugging.  Will be removing theoryGraph files next.
 *
 * Revision 1.1  1996/05/01 15:04:25  jyh
 * This is the initial checkin of the NuprlLight editor.  This editor provides
 * an emacs interface, a library navigator, and a proof editor.
 *
 * -*-
 * Local Variables:
 * Caml-master: "editor.top"
 * End:
 * -*-
 *)
