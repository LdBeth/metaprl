(*
 * Common utilities for filtering modules.
 *
 *)

open Printf
open Opname
open Term
open Term_util
open Ml_file
open Simple_print
open Filter_ast
open Filter_type

(************************************************************************
 * UTILS								*
 ************************************************************************)

(*
 * Get the context vars from a list.
 *)
let rec context_vars_list = function
   h::t -> List_util.union (Term.context_vars h) (context_vars_list t)
 | [] -> []

let rec binding_vars_list = function
   h::t -> List_util.union (Term.binding_vars h) (binding_vars_list t)
 | [] -> []

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

(*
 * $Log$
 * Revision 1.1  1997/04/28 15:51:02  jyh
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
 * Revision 1.1  1996/09/02 19:43:07  jyh
 * Semi working package management.
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
