(*
 * Utilities for the filter_summary module.
 *)

open Printf

open Nl_debug

open Refiner.Refiner.Term
open Nl_resource

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
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
