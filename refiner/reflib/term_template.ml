(*
 * This module computes term templates for use by hashtables.
 *)

open Printf
open Debug

open Opname
open Refiner.Refiner
open Refiner.Refiner.Term

open Simple_print

(*
 * Show the file loading.
 *)
let _ =
   if !debug_load then
      eprintf "Loading Term_template%t" eflush

(************************************************************************
 * TYPES                                                                *
 ************************************************************************)

(*
 * Terms are reduced to these templates for indexing
 * purposes.  Each templet just contains information
 * about the opname, the order and types of params,
 * and the arties of the subterms.
 *)
type template =
   { template_opname : opname;
     template_params : param_template array;
     template_arities : int array
   }

and param_template =
   Number
 | String
 | Token
 | Level
 | Var
 | ObId

type t = int

(************************************************************************
 * IMPLEMENTATION                                                       *
 ************************************************************************)

(*
 * Printing.
 *)
let string_of_param = function
    Number ->
       ":n"
  | String ->
       ":s"
  | Token ->
       ":t"
  | Level ->
       ":l"
  | Var ->
       ":v"
  | ObId ->
       ":o"

let print_params out params =
   let print param =
      output_string out (string_of_param param)
   in
      Array.iter print params

let print_arities out arities =
   let print_arity arity =
      output_string out (string_of_int arity)
   in
   let length = Array.length arities in
   let rec print i =
      if i <> length then
         begin
            print_arity arities.(i);
            if i <> length - 1 then
               begin
                  output_string out ",";
                  print (i + 1)
               end
         end
   in
      print 0

(*
 * Compute the template for a particular term.
 *)
let template_of_term t =
   let { term_op = op; term_terms = bterms } = dest_term t in
   let { op_name = opname; op_params = params } = dest_op op in
   let compute_arity bterm =
      let { bvars = bvars } = dest_bterm bterm in
         List.length bvars
   in
   let rec compute_param l = function
      [] ->
         l
    | param::t ->
         match dest_param param with
            Term.Number _
          | Term.MNumber _
          | Term.MSum _
          | Term.MDiff _
          | Term.MProduct _
          | Term.MQuotient _
          | Term.MRem _ ->
               compute_param (Number :: l) t

          | Term.String _
          | Term.MString _ ->
               compute_param (String :: l) t

          | Term.Token _
          | Term.MToken _
          | Term.MNotEqual _
          | Term.MLessThan _
          | Term.MEqual _ ->
               compute_param (Token :: l) t

          | Term.Level _
          | Term.MLevel _ ->
               compute_param (Level :: l) t

          | Term.Var _
          | Term.MVar _ ->
               compute_param (Var :: l) t

          | Term.ObId _ ->
               compute_param (ObId :: l) t

          | Term.ParamList l' ->
               compute_param (compute_param l l') t
   in
      { template_opname = opname;
        template_params =
           if params = [] then
              [||]
           else
              Array.of_list (compute_param [] params);
        template_arities = Array.of_list (List.map compute_arity bterms)
      }

(*
 * Integer index from template.
 *)
let of_term t =
   let t = template_of_term t in
      if !debug_dform then
         begin
            let { template_opname = opname;
                  template_params = params;
                  template_arities = arities
                } = t
            in
               eprintf "Term_template.compute_template: %s[%a]{%a}%t" (**)
                  (string_of_opname opname)
                  print_params params
                  print_arities arities
                  eflush
         end;
      Hashtbl.hash_param 25 250 t

let of_term_list tl =
   let tl = List.map template_of_term tl in
      if !debug_dform then
         begin
            let print_template
                { template_opname = opname;
                  template_params = params;
                  template_arities = arities
                } =
               eprintf "Term_template.compute_template: %s[%a]{%a}%t" (**)
                  (string_of_opname opname)
                  print_params params
                  print_arities arities
                  eflush
            in
               List.iter print_template tl
         end;
      Hashtbl.hash_param 25 250 tl

(*
 * Trivial conversion to int.
 *)
let to_int t = t

(*
 * $Log$
 * Revision 1.2  1998/06/01 13:55:09  jyh
 * Proving twice one is two.
 *
 * Revision 1.1  1998/05/28 15:01:26  jyh
 * Partitioned refiner into subdirectories.
 *
 * Revision 1.9  1998/05/27 15:15:04  jyh
 * Functorized the refiner over the Term module.
 *
 * Revision 1.8  1998/05/01 18:43:34  jyh
 * Added raw display.
 *
 * Revision 1.7  1998/04/29 20:53:40  jyh
 * Initial working display forms.
 *
 * Revision 1.6  1998/04/29 14:48:34  jyh
 * Added ocaml_sos.
 *
 * Revision 1.5  1998/04/28 21:38:13  jyh
 * Adjusted uppercasing.
 *
 * Revision 1.4  1998/04/28 18:30:50  jyh
 * ls() works, adding display.
 *
 * Revision 1.3  1998/04/24 02:43:05  jyh
 * Added more extensive debugging capabilities.
 *
 * Revision 1.2  1997/09/12 17:21:47  jyh
 * Added MLast <-> term conversion.
 * Splitting filter_parse into two phases:
 *    1. Compile into Filter_summary
 *    2. Compile Filter_summary into code.
 *
 * Revision 1.1  1997/04/28 15:51:48  jyh
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
 * Revision 1.1  1996/11/13 22:59:16  jyh
 * Initial version of forward/backward chaining cache.
 *
 * -*-
 * Local Variables:
 * Caml-master: "editor.run"
 * End:
 * -*-
 *)
