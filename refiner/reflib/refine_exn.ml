(*
 * Print refine exceptions.
 *)

open Printf

open Debug

open Refiner.Refiner
open Refiner.Refiner.Term
open Refiner.Refiner.TermAddr
open Refiner.Refiner.TermMeta
open Refiner.Refiner.Rewrite
open Refiner.Refiner.Refine
open Rformat
open Simple_print
open Dform
open Dform_print

(*
 * Show the file loading.
 *)
let _ =
   if !debug_load then
      eprintf "Loading Refine_exn%t" eflush

(*
 * Default printer uses the Simple_print.
 *)
type printers =
   { format_term : dform_base -> buffer -> term -> unit;
     format_bterm : dform_base -> buffer -> bound_term -> unit;
     format_param : dform_base -> buffer -> param -> unit;
     format_mterm : dform_base -> buffer -> meta_term -> unit
   }

let simple_printers =
   { format_term = (fun _ t -> format_simple_term t);
     format_bterm = (fun _ t -> format_simple_bterm t);
     format_param = (fun _ t -> format_simple_param t);
     format_mterm = (fun _ t -> format_simple_mterm t);
   }

let dform_printers =
   { format_term = format_term;
     format_bterm = format_bterm;
     format_param = (fun _ p -> format_simple_param p);
     format_mterm = format_mterm
   }

(*
 * Print an address as a list of ints.
 *)
let format_address buf addr =
   format_string buf (string_of_address addr)

(*
 * Just print out a bunch of strings.
 *)
let rec format_strings buf = function
   [h] ->
      format_string buf h
 | h::t ->
      format_string buf h;
      format_space buf;
      format_strings buf t
 | [] ->
      ()

(*
 * Match type in the rewriter.
 *)
let format_match_type db buf printers = function
   ParamMatch p ->
      format_string buf "ParamMatch:";
      format_space buf;
      printers.format_param db buf p
 | VarMatch s ->
      format_string buf "VarMatch:";
      format_space buf;
      format_string buf s
 | TermMatch t ->
      format_string buf "TermMatch:";
      format_space buf;
      printers.format_term db buf t
 | BTermMatch bt ->
      format_string buf "BTermMatch:";
      format_space buf;
      printers.format_bterm db buf bt

(*
 * Rewrite error.
 *)
let format_rewrite_error db buf printers = function
   BoundSOVar s ->
      format_string buf "BoundSoVar:";
      format_space buf;
      format_string buf s
 | FreeSOVar s ->
      format_string buf "FreeSOVar:";
      format_space buf;
      format_string buf s
 | BoundParamVar s ->
      format_string buf "BoundParamVar:";
      format_space buf;
      format_string buf s
 | FreeParamVar s ->
      format_string buf "FreeParamVar:";
      format_space buf;
      format_string buf s
 | BadRedexParam p ->
      format_string buf "BadRedexParam:";
      format_space buf;
      printers.format_param db buf p
 | NoRuleOperator ->
      format_string buf "NoRuleOperator"
 | BadMatch t ->
      format_string buf "BadMatch:";
      format_space buf;
      format_match_type db buf printers t
 | AllSOInstances s ->
      format_string buf "AllSOInstances:";
      format_space buf;
      format_string buf s
 | MissingContextArg s ->
      format_string buf "MissingContextArg:";
      format_space buf;
      format_string buf s
 | StackError _ ->
      format_string buf "StackError"
 | Rewrite.StringError s ->
      format_string buf "StringError:";
      format_space buf;
      format_string buf s

(*
 * Print a refinement error.
 *)
let format_refine_error db buf printers error =
   let rec format = function
      StringError s ->
         format_string buf s
    | TermError t ->
         printers.format_term db buf t
    | StringIntError (s, i) ->
         format_string buf s;
         format_space buf;
         format_int buf i
    | StringStringError (s1, s2) ->
         format_string buf s1;
         format_space buf;
         format_string buf s2
    | StringTermError (s, t) ->
         format_string buf s;
         format_space buf;
         printers.format_term db buf t
    | GoalError (s, e) ->
         format_string buf s;
         format_space buf;
         format e
    | SecondError (s, e) ->
         format_string buf s;
         format_space buf;
         format e
    | SubgoalError (s, i, e) ->
         format_string buf s;
         format_space buf;
         format_int buf i;
         format_space buf;
         format e
    | PairError (s, e1, e2) ->
         format_string buf s;
         format_space buf;
         format e1;
         format_space buf;
         format e2
    | RewriteAddressError (s, a, e) ->
         format_string buf s;
         format_space buf;
         format_address buf a;
         format_space buf;
         format e
    | RewriteError (s, e) ->
         format_string buf s;
         format_space buf;
         format_rewrite_error db buf printers e
    | NodeError (s, t, el) ->
         format_string buf s;
         format_space buf;
         printers.format_term db buf t
   in
      format error

(*
 * Convert an exception to a string.
 *)
let format_exn db buf printers error =
   let format = function
      RefineError msg ->
         format_string buf "Refine error:";
         format_space buf;
         format_refine_error db buf printers msg
    | FreeContextVars vars ->
         format_string buf "FreeContextVars:";
         format_space buf;
         format_strings buf vars
    | Rewrite.RewriteError msg ->
         format_string buf "Rewrite error:";
         format_space buf;
         format_rewrite_error db buf printers msg
    | Term.TermMatch (s1, t, s2) ->
         format_string buf "TermMatch:";
         format_space buf;
         format_string buf s1;
         format_space buf;
         printers.format_term db buf t;
         format_space buf;
         format_string buf s2
    | IncorrectAddress (a, t) ->
         format_string buf "Incorrect address:";
         format_space buf;
         format_address buf a;
         format_space buf;
         printers.format_term db buf t
    | BadAddressPrefix (a1, a2) ->
         format_string buf "Bad address prefix:";
         format_space buf;
         format_address buf a1;
         format_space buf;
         format_address buf a2
    | Term.BadMatch (t1, t2) ->
         format_string buf "Terms do not match:";
         format_space buf;
         printers.format_term db buf t1;
         format_space buf;
         printers.format_term db buf t2
    | MetaTermMatch t ->
         format_string buf "Meta term does not match:";
         format_space buf;
         printers.format_mterm db buf t
    | exn ->
         format_string buf (Printexc.to_string exn)
   in
      format_pushm buf 4;
      format error;
      format_popm buf

(*
 * Formatting.
 *)
let format_rewrite_error db buf error =
   format_rewrite_error db buf dform_printers error

let format_refine_error db buf error =
   format_refine_error db buf dform_printers error

let format_exn db buf exn =
   format_exn db buf dform_printers exn

(*
 * Print to a channel.
 *)
let print db f x =
   try f x with
      exn ->
         let buf = new_buffer () in
            format_exn db buf exn;
            print_to_channel 80 buf stderr;
            raise exn

let print_exn db out s exn =
   let db = get_mode_base db "prl" in
   let buf = new_buffer () in
      format_szone buf;
      format_pushm buf 4;
      format_string buf s;
      format_space buf;
      format_exn db buf exn;
      format_popm buf;
      format_ezone buf;
      print_to_channel 80 buf out;
      flush out;
      raise exn

(*
 * $Log$
 * Revision 1.1  1998/05/28 15:01:04  jyh
 * Partitioned refiner into subdirectories.
 *
 * Revision 1.6  1998/05/27 15:13:54  jyh
 * Functorized the refiner over the Term module.
 *
 * Revision 1.5  1998/05/18 18:28:10  nogin
 * Removed standardize_apart function, compare_* functions
 *     and BadParamMatch exception
 *
 * Revision 1.4  1998/04/28 18:30:43  jyh
 * ls() works, adding display.
 *
 * Revision 1.3  1998/04/24 02:42:48  jyh
 * Added more extensive debugging capabilities.
 *
 * Revision 1.2  1998/04/21 19:53:59  jyh
 * Upgraded refiner for program extraction.
 *
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
