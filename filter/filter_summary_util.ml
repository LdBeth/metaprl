(*
 * Utilities for the filter_summary module.
 *)

open Printf

open Debug

open Refiner.Refiner.Term
open Resource

open Filter_summary

(*
 * Show the file loading.
 *)
let _ =
   if !debug_load then
      eprintf "Loading Filter_summary_util%t" eflush


(*
 * Extract the context var arguments.
 *)
let rec collect_cvars = function
   ContextParam v::t ->
      v :: collect_cvars t
 | _::t ->
      collect_cvars t
 | [] ->
      []

let rec collect_vars = function
   VarParam v::t ->
      v :: collect_vars t
 | _::t ->
      collect_vars t
 | [] ->
      []

let rec collect_non_vars = function
   TermParam x :: t ->
      x :: collect_non_vars t
 | _ :: t ->
      collect_non_vars t
 | [] ->
      []

(*
 * Split parameters into the three types.
 *)
let rec split_params = function
   h::t ->
      let cvars, tvars, tparams = split_params t in
         begin
            match h with
               ContextParam v ->
                  v :: cvars, tvars, tparams
             | VarParam v ->
                  cvars, v :: tvars, tparams
             | TermParam t ->
                  cvars, tvars, t :: tparams
         end
 | [] ->
      [], [], []

(*
 * Give names to all the parameters.
 *)
let name_params =
   let rec loop i = function
      h::t ->
         let aids, cids, tids, xids = loop (i + 1) t in
         let name = "id_" ^ (string_of_int i) in
            begin
               match h with
                  ContextParam _ ->
                     name :: aids, name :: cids, tids, xids
                | VarParam _ ->
                     name :: aids, cids, name :: tids, xids
                | TermParam _ ->
                     name :: aids, cids, tids, name :: xids
            end
    | [] ->
         [], [], [], []
   in
      loop 0

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
 * Membership in a resource list.
 *)
let mem_resource { resource_name = name } resources =
   let rec search = function
      { resource_name = name' } :: t ->
         if !debug_resource then
            eprintf "Resource: %s%t" name' eflush;
         if name = name' then
            true
         else
            search t
    | [] ->
         false
   in
      search resources

(*
 * $Log$
 * Revision 1.10  1998/06/12 13:46:40  jyh
 * D tactic works, added itt_bool.
 *
 * Revision 1.9  1998/05/27 15:13:07  jyh
 * Functorized the refiner over the Term module.
 *
 * Revision 1.8  1998/04/24 19:38:36  jyh
 * Updated debugging.
 *
 * Revision 1.7  1998/04/24 02:42:13  jyh
 * Added more extensive debugging capabilities.
 *
 * Revision 1.6  1998/02/21 20:57:57  jyh
 * Two phase parse/extract.
 *
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
