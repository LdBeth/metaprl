(*
 * Display all the elements in a particular theory.
 *)

include Base_theory

include Io_proof
include Package_info

open Printf
open Debug

open Opname
open Refiner.Refiner.Term
open Refiner.Refiner.TermMan
open Rformat
open Dform_print
open Dform

open Filter_summary
open Filter_ocaml
open Filter_cache

open Io_proof
open Package_info
open Package

(*
 * Show that the file is loading.
 *)
let _ =
   if !debug_load then
      eprintf "Loading Package_df%t" eflush

(*
 * Our display parameters.
 *)
let tabstop = 3
let min_screen_width = 80

(*
 * Unit term used for interfaces.
 *)
let null_term = mk_simple_term nil_opname []
let null_proof = Primitive null_term

(*
 * When a StrFilterCache or SigFilterCache is
 * saved, comments are not saved.
 *)
let identity x = x
let comment loc t = t
let term_of_expr = term_of_expr [] comment
let term_of_type = term_of_type comment
let term_of_sig_item = term_of_sig_item comment
let term_of_str_item = term_of_str_item comment

let convert_intf =
      { term_f  = identity;
        meta_term_f = term_of_meta_term;
        proof_f = (fun _ _ -> null_term);
        ctyp_f  = term_of_type;
        expr_f  = term_of_expr;
        item_f  = term_of_sig_item
      }

let convert_impl =
   let convert_proof _ = function
      Primitive t ->
         t
    | Derived expr ->
         term_of_expr expr
    | Incomplete
    | Interactive _ ->
         null_term
   in
      { term_f  = identity;
        meta_term_f = term_of_meta_term;
        proof_f = convert_proof;
        ctyp_f  = term_of_type;
        expr_f  = term_of_expr;
        item_f  = term_of_str_item
      }

(*
 * Display the entire package.
 *)
let format_interface mode buf pack =
   let dbase = dforms pack in
   let db = get_mode_base dbase mode in
   let tl = term_list convert_intf (Package.sig_info pack) in
   let t = mk_simple_term interface_op [mk_xlist_term tl] in
      format_term db buf t;
      format_newline buf

(*
 * Display the entire package.
 *)
let format_implementation mode buf pack =
   let dbase = dforms pack in
   let db = get_mode_base dbase mode in
   let tl = term_list convert_impl (Package.info pack) in
   let t = mk_simple_term implementation_op [mk_xlist_term tl] in
      format_term db buf t;
      format_newline buf

(*
 * Display the package DAG, using indentition.
 * Each package is listed with its children indented.
 *)
let format_packages buf pack =
   let compare pack1 pack2 =
      Package.name pack1 <= Package.name pack2
   in
   let packs = Sort.list compare (Package.packages pack) in
   let format_package pack' =
      let children = Sort.list compare (Package.children pack pack') in
      let format_child child =
         format_newline buf;
         format_string buf (Package.name child)
      in
         format_pushm buf tabstop;
         format_string buf (Package.name pack');
         List.iter format_child children;
         format_popm buf;
         format_newline buf
   in
      List.iter format_package packs

(*
 * -*-
 * Local Variables:
 * Caml-master: "editor.top"
 * End:
 * -*-
 *)
