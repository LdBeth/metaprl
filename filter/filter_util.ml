(*
 * Common utilities for filtering modules.
 *
 *)

open Printf
open Debug
open Opname
open Refiner.Refiner
open Refiner.Refiner.Term
open Refiner.Refiner.TermType
open Refiner.Refiner.TermMeta
open Ml_file
open Simple_print
open Filter_ast
open Filter_type

(*
 * Show the file loading.
 *)
let _ =
   if !debug_load then
      eprintf "Loading Filter_util%t" eflush


(************************************************************************
 * UTILS								*
 ************************************************************************)

(*
 * Get the context vars from a list.
 *)
let rec context_vars_list = function
   h::t ->
      List_util.union (TermSubst.context_vars h) (context_vars_list t)
 | [] ->
      []

let rec binding_vars_list = function
   h::t ->
      List_util.union (TermSubst.binding_vars h) (binding_vars_list t)
 | [] ->
      []

(*
 * Collect the arguments in a rewrite.
 *)
let unzip_rewrite name =
   let rec aux = function
      MetaImplies (MetaTheorem a, b) ->
         let args, redex, contractum = aux b in
            a::args, redex, contractum
    | MetaIff (MetaTheorem redex, MetaTheorem contractum) ->
         [], redex, contractum
    | _ ->
         raise (BadCommand (name ^ ": illegal specification"))
   in
      aux

(*
 * Split the function into var names and a simple mterm.
 *)
let split_mfunction mterm =
   let subgoals, goal = unzip_mfunction mterm in
   let collect pair (i, vars, terms) =
      match pair with
         Some v, t ->
            i, v :: vars, t :: terms
       | None, t ->
            i + 1, (mk_var_term ("_" ^ (string_of_int i))) :: vars, t :: terms
   in
   let _, vars, terms = List.fold_right collect subgoals (0, [], [goal]) in
      vars, zip_mimplies terms

(************************************************************************
 * OPNAMES                                                              *
 ************************************************************************)

(*
 * Opname printing.
 *)
let string_of_opname_list =
   let rec print_path path' = function
      h::t ->
         if path' = "" then
            print_path h t
         else
            print_path (path' ^ "!" ^ h) t
    | [] ->
         path'
   in
      print_path ""

let print_opname ofile name =
   output_string ofile (string_of_opname_list name)

(*
 * Get a string from an opname.
 *)
let translate_opname opname =
   let l = dest_opname opname in
   let rec aux = function
      [h] -> h
    | h::t -> h ^ "_" ^ (aux t)
    | [] -> ""   in
      aux l

(************************************************************************
 * MODULE PATHS                                                         *
 ************************************************************************)

let rec string_of_path = function
   [] ->
      (* This should never happen because paths always point to something *)
      raise (EmptyModulePath "string_of_path")
 | [h] -> h
 | h::t -> h ^ "/" ^ (string_of_path t)

(*
 * Output a path to an ml file.
 *)
let output_path oport =
   let rec aux = function
      [] -> raise (EmptyModulePath "output_path")
    | [h] -> output_string oport (String.capitalize h)
    | h::t ->
         output_string oport h;
         output_string oport ".";
         aux t
   in
      aux

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
