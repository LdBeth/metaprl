(*
 * Utilities for the filter_summary module.
 *)

open Printf

open Term
open Opname
open Simple_print

open Filter_ast
open Filter_summary

(*
 * Distinguish between context var parameters, var names,
 * and other parameters.
 *)
let extract_params cvars bvars =
   let aux h =
      if is_var_term h then
         let v = dest_var h in
            if List.mem v cvars then
               ContextParam v
            else if List.mem v bvars then
               VarParam v
            else
               TermParam h
      else
         TermParam h
   in
      List.map aux

(*
 * Param expression.
 *)
let param_expr loc = function
   ContextParam s ->
      <:expr< $uid:"Filter_summary"$ . $uid:"ContextParam"$ $str:s$ >>
 | VarParam v ->
      <:expr< $uid:"Filter_summary"$ . $uid:"VarParam"$ $str:v$ >>
 | TermParam t ->
      let t' = build_ml_term loc t in
         <:expr< $uid:"Filter_summary"$ . $uid:"TermParam"$ $t'$ >>
                                           
(*
 * Create function type.
 *)
let params_ctyp loc ctyp params =
   let rec convert = function
      [] -> ctyp
    | h::t ->
         let ctyp' = convert t in
         let arg_type =
            match h with
               ContextParam _ ->
                  <:ctyp< $lid:"int"$ >>
             | VarParam _ ->
                  <:ctyp< $lid:"string"$ >>
             | TermParam _ ->
                  <:ctyp< $uid:"Term"$ . $lid:"term"$ >>
         in
            <:ctyp< $arg_type$ -> $ctyp'$ >>
   in
      convert params

(************************************************************************
 * OPNAMES                                                              *
 ************************************************************************)

(*
 * $Log$
 * Revision 1.5  1998/02/19 17:14:04  jyh
 * Splitting filter_parse.
 *
 * Revision 1.2  1997/08/06 16:17:35  jyh
 * This is an ocaml version with subtyping, type inference,
 * d and eqcd tactics.  It is a basic system, but not debugged.
 *
 * Revision 1.1  1997/04/28 15:51:00  jyh
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
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
