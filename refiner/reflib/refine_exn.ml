(*
 * Print refine exceptions.
 *
 * ----------------------------------------------------------------
 *
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/htmlman/default.html or visit http://metaprl.org/
 * for more information.
 *
 * Copyright (C) 1998-2004 MetaPRL Group
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
 * Author: Jason Hickey <jyh@cs.cornell.edu>
 * Modified by: Aleksey Nogin <nogin@cs.cornell.edu>
 *)
open Lm_debug
open Lm_symbol
open Lm_rprintf
open Lm_rformat

open Term_sig
open Refine_sig
open Opname
open Refiner.Refiner.Term
open Refiner.Refiner.TermAddr
open Refiner.Refiner.TermShape
open Refiner.Refiner.TermTy
open Refiner.Refiner.RefineError
open Simple_print.SimplePrint
open Dform

exception ToploopIgnoreExn of exn

(*
 * Show the file loading.
 *)
let _ =
   show_loading "Loading Refine_exn%t"

let backtrace =
   try String.contains (Sys.getenv "OCAMLRUNPARAM") 'b' with
      Not_found ->
         false

(*
 * Default printer uses the Simple_print.
 *)
type printers =
   { format_term  : dform_base -> buffer -> term -> unit;
     format_bterm : dform_base -> buffer -> bound_term -> unit;
     format_param : dform_base -> buffer -> param -> unit;
     format_mterm : dform_base -> buffer -> meta_term -> unit;
   }

let simple_printers =
   { format_term  = (fun _ t -> format_simple_term t);
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
(* unused
let rec format_strings buf = function
   [h] ->
      format_string buf h
 | h::t ->
      format_string buf h;
      format_space buf;
      format_strings buf t
 | [] ->
      ()
*)

(*
 * Format a hypothesis.
 *)
let format_hypothesis db buf printers = function
   Context (v, conts, subterms) ->
      format_string buf "Context(";
      format_string buf (string_of_symbol v);
      format_string buf ", <";
      format_string buf (String.concat ", " (List.map string_of_symbol conts));
      format_string buf ">";
      List.iter (fun t ->
            format_string buf ", ";
            printers.format_term db buf t;
            format_newline buf) subterms;
      format_string buf ")"

 | Hypothesis (v, term) ->
      format_string buf "Hypothesis(";
      if Lm_symbol.to_string v <> "" then begin
         format_string buf (string_of_symbol v);
         format_string buf ", ";
      end;
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
      format_string buf (string_of_symbol s)
 | TermMatch t ->
      format_string buf "TermMatch:";
      format_space buf;
      printers.format_term db buf t
 | TermMatch2 (t1, t2) ->
      format_string buf "TermMatch2:";
      format_space buf;
      printers.format_term db buf t1;
      format_space buf;
      format_string buf "==";
      format_space buf;
      printers.format_term db buf t2
 | BTermMatch bt ->
      format_string buf "BTermMatch:";
      format_space buf;
      printers.format_bterm db buf bt
 | HypMatch hyps ->
      format_string buf "HypMatch:";
      format_newline buf;
      SeqHyp.iter (format_hypothesis db buf printers) hyps

let format_explanation buf s =
   format_newline buf;
   format_szone buf;
   format_pushm buf 3;
   format_string buf "Explanation:";
   format_space buf;
   format_szone buf;
   let rec fmt = function
      [] -> ()
    | [s] -> format_string buf s
    | s :: ss -> format_string buf s; format_cbreak buf "" " "; fmt ss
   in
      fmt (String.split_on_char ' ' s);
   format_ezone buf;
   format_popm buf;
   format_ezone buf

(*
 * Print a refinement error.
 *)
let format_refine_error db buf printers name error =
   let rec format name error =
      format_newline buf;
      format_pushm buf 3;
      format_string buf name;
      format_string buf ":";
      format_space buf;
      format_szone buf;
      format_error error;
      format_ezone buf;
      format_popm buf

   and format_next_error error =
      format_space buf;
      format_string buf "- ";
      format_error error

   and format_error = function
      GenericError ->
         format_string buf "Generic refiner error"
    | ToploopIgnoreError ->
         raise (Invalid_argument "Print_exn.format_refine_error: got ToploopIgnoreError")
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
    | StringVarError (s, v) ->
         format_string buf s;
         format_space buf;
         format_string buf ("'" ^ string_of_symbol v)
    | StringTermError (s, t) ->
         format_string buf s;
         format_space buf;
         printers.format_term db buf t
    | StringWrapError (name, e) ->
         format name e
    | SubgoalError (i, name, e) ->
         format_int buf i;
         format_space buf;
         format name e
    | PairError (name1, e1, name2, e2) ->
         format name1 e1;
         format name2 e2
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
    | TermPairError (t1, t2) ->
         format_newline buf;
         format_string buf "Term 1: ";
         printers.format_term db buf t1;
         format_newline buf;
         format_string buf "Term 2: ";
         printers.format_term db buf t2
    | MetaTermMatchError mt ->
         printers.format_mterm db buf mt
    | RewriteFreeSOVar s ->
         format_szone buf;
         format_string buf "FreeSOVar:";
         format_space buf;
         format_string buf ("'" ^ string_of_symbol s);
         format_ezone buf;
         format_explanation buf "The meta-variable is a second-order, context, or first-order variable that occurs free.  For rules, each meta-variable must appear in the goal or as an argument to the rule.  For rewrites, each meta-variable must occur in the redex (left-hand side of the rewrite), or as an argument to the rewrite."
    | RewriteSOVarArity s ->
         format_string buf "SOVarArity:";
         format_space buf;
         format_string buf ("'" ^ string_of_symbol s)
    | RewriteBoundParamVar s ->
         format_string buf "BoundParamVar:";
         format_space buf;
         format_string buf ("'" ^ string_of_symbol s)
    | RewriteFreeParamVar s ->
         format_string buf "FreeParamVar:";
         format_space buf;
         format_string buf ("'" ^ string_of_symbol s)
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
         format_string buf (string_of_symbol s);
         format_explanation buf "Second-order variables that take arguments need to occur in such a position that the exact \"address\" of the arguments can be deduced. In oder words, each second variable that occurs in a rule/rewrite need to occur in either arguments of the rule conclusion (left hand side of the rewrite) in such a way that all its arguments are distinct bound variables and not being itself inside an argument to a meta-variable."
    | RewriteMissingContextArg s ->
         format_string buf "MissingContextArg:";
         format_space buf;
         format_string buf (string_of_symbol s)
    | RewriteStringError s ->
         format_string buf "StringError:";
         format_space buf;
         format_string buf s
    | RewriteStringOpnameOpnameError (s, opname1, opname2) ->
         format_string buf "StringOpnameOpnameError:";
         format_space buf;
         format_string buf s;
         format_space buf;
         format_string buf (string_of_opname opname1);
         format_space buf;
         format_string buf (string_of_opname opname2)
    | RewriteAddressError (a, name, e) ->
         format_address buf a;
         format_space buf;
         format name e
    | RewriteFreeContextVar (v1, v2) ->
         format_string buf "FreeContextVar: ";
         format_string buf (string_of_symbol v1);
         format_string buf " (in context or variable: ";
         format_string buf (string_of_symbol v2);
         format_string buf ")"
    | VarError v ->
         format_string buf ("'" ^ string_of_symbol v)
    | OpnameError opname ->
         format_string buf (string_of_opname opname)
    | Opname2Error (opname1, opname2) ->
         format_string buf (string_of_opname opname1);
         format_string buf ",";
         format_space buf;
         format_string buf (string_of_opname opname2)
    | ParamError param ->
         printers.format_param db buf param
    | Param2Error (param1, param2) ->
         printers.format_param db buf param1;
         format_string buf ",";
         format_space buf;
         printers.format_param db buf param2
    | ParamTyParamError (param, ty_param) ->
         printers.format_param db buf param;
         format_string buf ",";
         format_space buf;
         format_string buf (string_of_ty_param ty_param)
    | ShapeError shape ->
         format_string buf (string_of_shape shape)
    | Shape2Error (shape1, shape2) ->
         format_string buf (string_of_shape shape1);
         format_string buf ",";
         format_space buf;
         format_string buf (string_of_shape shape2)
    | Term2Error (t1, t2) ->
         format_newline buf;
         format_string buf "Term 1: ";
         printers.format_term db buf t1;
         format_newline buf;
         format_string buf "Term 2: ";
         printers.format_term db buf t2
    | VarTermError (v, t) ->
         format_string buf (string_of_symbol v);
         format_string buf ":";
         format_space buf;
         printers.format_term db buf t
    | IntTermError (i, t) ->
         format_int buf i;
         format_string buf ":";
         format_space buf;
         printers.format_term db buf t

      (* Wrapped errors *)
    | StringErrorError (s, err) ->
         format_string buf s;
         format_next_error err

    | VarErrorError (v, err) ->
         format_string buf ("'" ^ string_of_symbol v);
         format_next_error err

    | IntErrorError (i, err) ->
         format_int buf i;
         format_next_error err

    | TermErrorError (t, err) ->
         printers.format_term db buf t;
         format_next_error err

    | OpnameErrorError (opname, err) ->
         format_string buf (string_of_opname opname);
         format_next_error err

    | ShapeErrorError (shape, err) ->
         format_string buf (string_of_shape shape);
         format_next_error err

    | ParamErrorError (param, err) ->
         printers.format_param db buf param;
         format_next_error err

    | MetaTermErrorError (mt, err) ->
         printers.format_mterm db buf mt;
         format_next_error err
   in
      format name error

(*
 * Print a generic exception.
 *)
let format_exn_aux db printers exn =
   let buf = new_buffer () in
      format_pushm buf 4;
      (match exn with
          RefineError (name, msg) ->
             format_string buf "Refine error:";
             format_refine_error db buf printers name msg
        | RefineForceError (debug, name, msg) ->
             format_string buf "Forced refine error: ";
             format_string buf debug;
             format_refine_error db buf printers name msg
        | Incomplete opname ->
             format_string buf ("Incomplete proof: /" ^ (String.concat "/" (List.rev (dest_opname opname))))
        | Invalid_argument s ->
             format_string buf ("Invalid Argument:\n" ^ s)
        | Failure s ->
             format_string buf ("Failure:\n" ^ s)
        | Lm_parser.ParseError (loc, s) ->
             format_string buf (Lm_location.string_of_location loc);
             format_space buf;
             format_string buf s
        | exn ->
             format_string buf (Printexc.to_string exn));
      format_popm buf;
      buf

(*
 * Convert an exception to a string.
 *)
let format_exn db buf printers exn =
   let buf' =
      try format_exn_aux db printers exn with
         _ ->
            try format_exn_aux db simple_printers exn with
               exn ->
                  let buf = new_buffer () in
                     format_hzone buf;
                     format_string buf "!!! Refine_exn.format_exn: unrecoverable error during exception printing !!!";
                     format_space buf;
                     format_string buf (Printexc.to_string exn);
                     format_ezone buf;
                     buf
   in
      format_buffer buf buf'

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
let print_exn db f x =
   try f x with
      exn ->
         let buf = new_buffer () in
            format_exn db buf exn;
            format_newline buf;
            output_rbuffer stderr buf;
            flush stderr;
            if backtrace then
               raise exn
            else begin
               match exn with
                  RefineError(s, _) ->
                     raise(RefineError(s, ToploopIgnoreError))
                | _ ->
                     raise (ToploopIgnoreExn exn)
            end

let stderr_exn exn =
   let buf = new_buffer () in
      format_szone buf;
      format_pushm buf 3;
      format_exn Dform.null_base buf exn;
      format_popm buf;
      format_ezone buf;
      format_newline buf;
      output_rbuffer stderr buf;
      flush stderr;
      if backtrace then
         raise exn
      else begin
         match exn with
            RefineError(s, _) ->
               raise(RefineError(s, ToploopIgnoreError))
          | _ ->
               raise (ToploopIgnoreExn exn)
      end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
