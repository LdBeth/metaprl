(*
 * Print refine exceptions.
 *)

open Printf

open Debug

open Term
open Term_util
open Simple_print
open Rewrite
open Refine_sig

(*
 * Just print out a bunch of strings.
 *)
let rec print_string_list out = function
   [h] ->
      output_string out h
 | h::t ->
      fprintf out "%s " h;
      print_string_list out t
 | [] ->
      ()

(*
 * Match type in the rewriter.
 *)
let string_of_match_type = function
   ParamMatch p ->
      sprintf "ParamMatch: %s" (string_of_param p)
 | VarMatch s ->
      sprintf "VarMatch: %s" s
 | TermMatch t ->
      sprintf "TermMatch: %s" (string_of_term t)
 | BTermMatch bt ->
      sprintf "BTermMatch: %s" (string_of_bterm bt)

(*
 * Rewrite error.
 *)
let string_of_rewrite_error = function
   BoundSOVar s ->
      sprintf "BoundSoVar: %s" s
 | FreeSOVar s ->
      sprintf "FreeSOVar: %s" s
 | BoundParamVar s ->
      sprintf "BoundParamVar: %s" s
 | FreeParamVar s ->
      sprintf "FreeParamVar: %s" s
 | BadRedexParam p ->
      sprintf "BadRedexParam: %s" (string_of_param p)
 | NoRuleOperator ->
      "NoRuleOperator"
 | BadMatch t ->
      sprintf "BadMatch: %s" (string_of_match_type t)
 | AllSOInstances s ->
      sprintf "AllSoInstances: %s" s
 | MissingContextArg s ->
      sprintf "MissingContextArg: %s" s
 | StackError _ ->
      "StackError"
 | Rewrite.StringError s ->
      sprintf "StringError: %s" s

(*
 * Print a refinement error.
 *)
let string_of_refine_error error =
   let rec to_string = function
         StringError s ->
            s
       | TermError t ->
            string_of_term t
       | StringStringError (s1, s2) ->
            sprintf "%s %s" s1 s2
       | StringTermError (s, t) ->
            sprintf "%s %s" s (string_of_term t)
       | GoalError (s, e) ->
            sprintf "%s %s" s (to_string e)
       | SecondError (s, e) ->
            sprintf "%s %s" s (to_string e)
       | SubgoalError (s, i, e) ->
            sprintf "%s %d %s" s i (to_string e)
       | PairError (s, e1, e2) ->
            sprintf "%s %s %s" s (to_string e1) (to_string e2)
       | RewriteAddressError (s, a, e) ->
            sprintf "%s %s %s" s (string_of_address a) (to_string e)
       | RewriteError (s, e) ->
            sprintf "%s %s" s (string_of_rewrite_error e)
       | NodeError (s, t, el) ->
            sprintf "%s %s" s (string_of_term t)
   in
      to_string error

(*
 * Call a function on its argument,
 * handling exceptions.
 *)
let print_exn f x =
   try f x with
      exn ->
         let _ =
            match exn with
               RefineError msg ->
                  eprintf "Refine error: %s%t" (string_of_refine_error msg) eflush
             | FreeContextVars vars ->
                  eprintf "FreeContextVars: %a%t" print_string_list vars eflush
             | Rewrite.RewriteError msg ->
                  eprintf "Rewrite error: %s%t" (string_of_rewrite_error msg) eflush
             | Term.TermMatch (s1, t, s2) ->
                  eprintf "TermMatch: %s %s %s%t" s1 (string_of_term t) s2 eflush
             | IncorrectAddress (a, t) ->
                  eprintf "Incorrect address: %s: %s%t" (string_of_address a) (string_of_term t) eflush
             | BadAddressPrefix (a1, a2) ->
                  eprintf "Bad address prefix: %s %s%t" (string_of_address a1) (string_of_address a2) eflush
             | BadParamMatch (p1, p2) ->
                  eprintf "Parameters do not match: %s %s%t" (string_of_param p1) (string_of_param p2) eflush
             | Term.BadMatch (t1, t2) ->
                  eprintf "Terms do not match: %s %s%t" (string_of_term t1) (string_of_term t2) eflush
             | MetaTermMatch t ->
                  eprintf "Meta term does not match: %s%t" (string_of_mterm t) eflush
             | _ ->
                  ()
         in
            raise exn

(*
 * $Log$
 * Revision 1.1  1998/04/09 15:26:40  jyh
 * Added strip_mfunction.
 *
 * Revision 1.1  1998/04/08 14:57:33  jyh
 * ImpDag is in mllib.
 *
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
