(*
 * Print refine exceptions.
 *
 * ----------------------------------------------------------------
 *
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/index.html for information on Nuprl,
 * OCaml, and more information about this system.
 *
 * Copyright (C) 1998 Jason Hickey, Cornell University
 * 
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 * 
 * Author: Jason Hickey
 * jyh@cs.cornell.edu
 *)

open Printf

open Mp_debug

open Refiner.Refiner
open Refiner.Refiner.TermType
open Refiner.Refiner.Term
open Refiner.Refiner.TermAddr
open Refiner.Refiner.TermMeta
open Refiner.Refiner.Rewrite
open Refiner.Refiner.RefineError
open Rformat
open Simple_print.SimplePrint
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
 * Format a hypothesis.
 *)
let format_hypothesis db buf printers = function
   Context (v, subterms) ->
      format_string buf "Context(";
      format_string buf v;
      List.iter (fun t ->
            format_string buf ", ";
            printers.format_term db buf t;
            format_newline buf) subterms;
      format_string buf ")"

 | Hypothesis (v, term) ->
      format_string buf "Hypothesis(";
      format_string buf v;
      format_string buf ", ";
      printers.format_term db buf term;
      format_string buf ")"

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
 | HypMatch hyps ->
      format_string buf "HypMatch:";
      format_newline buf;
      SeqHyp.iter (format_hypothesis db buf printers) hyps
 | GoalMatch goals ->
      format_string buf "GoalMatch:";
      SeqGoal.iter (fun t ->
            format_string buf ", ";
            printers.format_term db buf t) goals;
      format_string buf ")"

(*
 * Print a refinement error.
 *)
let format_refine_error db buf printers name error =
   let rec format indent name error =
      format_newline buf;
      for i = 0 to indent do
         format_char buf ' ';
      done;
      format_string buf name;
      format_string buf ": ";
      match error with
         GenericError ->
            format_string buf "Generic refiner error"
       | StringError s ->
            format_string buf s
       | IntError i ->
            format_int buf i
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
       | GoalError (name, e) ->
            format (indent + 3) name e
       | SecondError (name, e) ->
            format (indent + 3) name e
       | SubgoalError (i, name, e) ->
            format_int buf i;
            format_space buf;
            format (indent + 3) name e
       | PairError (name1, e1, name2, e2) ->
            format (indent + 3) name1 e1;
            format (indent + 3) name2 e2
       | NodeError (s, t, el) ->
            format_string buf s;
            format_space buf;
            printers.format_term db buf t
       | AddressError (addr, t) ->
            format_address buf addr;
            format_space buf;
            printers.format_term db buf t
       | TermMatchError (t, s2) ->
            format_string buf s2;
            format_newline buf;
            printers.format_term db buf t
       | TermPairMatchError (t1, t2) ->
            printers.format_term db buf t1;
            format_newline buf;
            printers.format_term db buf t2
       | MetaTermMatchError mt ->
            printers.format_mterm db buf mt
       | RewriteBoundSOVar s ->
            format_string buf "BoundSoVar:";
            format_space buf;
            format_string buf s
       | RewriteFreeSOVar s ->
            format_string buf "FreeSOVar:";
            format_space buf;
            format_string buf s
       | RewriteSOVarArity s ->
            format_string buf "SOVarArity:";
            format_space buf;
            format_string buf s
       | RewriteBoundParamVar s ->
            format_string buf "BoundParamVar:";
            format_space buf;
            format_string buf s
       | RewriteFreeParamVar s ->
            format_string buf "FreeParamVar:";
            format_space buf;
            format_string buf s
       | RewriteBadRedexParam p ->
            format_string buf "BadRedexParam:";
            format_space buf;
            printers.format_param db buf p
       | RewriteNoRuleOperator ->
            format_string buf "NoRuleOperator"
       | RewriteBadMatch t ->
            format_string buf "BadMatch:";
            format_space buf;
            format_match_type db buf printers t
       | RewriteAllSOInstances s ->
            format_string buf "AllSOInstances:";
            format_space buf;
            format_string buf s
       | RewriteMissingContextArg s ->
            format_string buf "MissingContextArg:";
            format_space buf;
            format_string buf s
       | RewriteStringError s ->
            format_string buf "StringError:";
            format_space buf;
            format_string buf s
       | RewriteAddressError (a, name, e) ->
            format_address buf a;
            format_space buf;
            format (indent + 3) name e
       | RewriteFreeContextVars vars ->
            format_string buf "FreeContextVars: ";
            format_strings buf vars
   in
      format 0 name error

(*
 * Convert an exception to a string.
 *)
let format_exn db buf printers exn =
   let format = function
      RefineError (name, msg) ->
         format_string buf "Refine error:";
         format_space buf;
         format_refine_error db buf printers name msg
    | exn ->
         format_string buf (Printexc.to_string exn)
   in
      format_pushm buf 4;
      format exn;
      format_popm buf

(*
 * Formatting.
 *)
let format_refine_error db buf name error =
   format_refine_error db buf dform_printers name error

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
            format_newline buf;
            print_to_channel 80 buf stderr;
            flush stderr;
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
      format_newline buf;
      print_to_channel 80 buf out;
      flush out;
      raise exn

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
