(*
 * Conversion from module_info to program text.
 *)

open Printf

open Debug

open Term
open Term_util
open Opname
open Precedence
open Rewrite

open Free_vars
open Filter_util
open Filter_ast
open Filter_cache
open Filter_summary_type
open Filter_summary_util
open Filter_summary

(*
 * Show the file loading.
 *)
let _ =
   if !debug_load then
      eprintf "Loading Filter_html%t" eflush


(*************************************************************************
 * BUFFERS                                                               
 *************************************************************************)

(*
 * A buffer contains a hashtable of items printed.
 *)
type buffer =
   { buf_file : out_channel;
     buf_opnames : (string list, string) Hashtbl.t;
     buf_terms : (term, string) Hashtbl.t
   }

(*
 * Print to the buffer.
 *)
let bprintf buf =
   fprintf buf.buf_file

let rec print_strings out = function
   [] ->
      ()
 | [h] ->
      output_string out h
 | h::t ->
      output_string out h;
      output_string out " ";
      print_strings out t

(*
 * Symbol generator.
 *)
let gensym =
   let index = ref 0 in
   let gensym () =
      index := !index + 1;
      "v" ^ (string_of_int !index)
   in
      gensym

(*
 * Print an opname.
 * Hashcons it.
 *)
let print_opname buf opname =
   let opname = dest_opname opname in
   let { buf_opnames = opnames } = buf in
   let rec search opname =
      try Hashtbl.find opnames opname with
         Not_found ->
            let name = gensym () in
            let _ =
               Hashtbl.add opnames opname name;
               match opname with
                  [] ->
                     bprintf buf "(opname %s)\n" name
                | [h] ->
                     bprintf buf "(opname %s %s)\n" name h
                | h::t ->
                     let tname = search t in
                        bprintf buf "(opname %s %s %s)\n" name h tname
            in
               name
   in
      search opname

(*
 * Print a parameter.
 *)
let print_param buf name =
   gensym ()

(*
 * Print a term. hash-cons it.
 *)
let rec print_term buf term =
   gensym ()

let rec print_mterm buf mterm =
   gensym ()

(*
 * Display form options.
 *)
let dform_opt = function
   DFormInheritPrec ->
      "DFormInheritPrec"
 | DFormPrec name ->
      sprintf "(DFormPrec %s)" name
 | DFormParens ->
      "DFormParens"

(*
 * Print an expression.
 *)
let print_expr buf expr =
   gensym ()

let print_expr_opt buf = function
   Some expr ->
      print_expr buf expr
 | None ->
      "none"

(*
 * Print a type.
 *)
let print_ctyp buf ctyp =
   gensym ()

(*
 * Signature item.
 *)
let print_sig_item buf item =
   gensym ()

(*
 * Structure item.
 *)
let print_str_item buf item =
   gensym ()

(************************************************************************
 * SIGNATURES                                                           *
 ************************************************************************)

(*
 * Rewrites.
 *)
let declare_rewrite buf
    { rw_name = name;
      rw_redex = redex;
      rw_contractum = contractum
    } =
   let rname = print_term buf redex in
   let cname = print_term buf contractum in
      bprintf buf "(rewrite %s %s %s)\n" name rname cname;
      name
   
let declare_cond_rewrite buf
    { crw_name = name;
      crw_params = params;
      crw_args = args;
      crw_redex = redex;
      crw_contractum = contractum
    } =
   let pnames = List.map (print_param buf) params in
   let anames = List.map (print_term buf) args in
   let rname = print_term buf redex in
   let cname = print_term buf contractum in
      bprintf buf "(cond-rewrite %s (%a) (%a) %s %s)\n" name print_strings pnames print_strings anames rname cname;
      name

(*
 * Rules.
 *)
let declare_axiom buf
    { axiom_name = name;
      axiom_stmt = stmt
    } =
   let sname = print_term buf stmt in
      bprintf buf "(axiom %s %s)" name sname;
      name

let declare_rule buf
    { rule_name = name;
      rule_params = params;
      rule_stmt = stmt
    } =
   let pnames = List.map (print_param buf) params in
   let sname = print_mterm buf stmt in
      bprintf buf "(rule %s (%a) %s)\n" name print_strings pnames sname;
      name

(*
 * Opname.
 *)
let declare_opname buf
    { opname_name = name;
      opname_term = term
    } =
   let tname = print_term buf term in
      bprintf buf "(opname %s %s)\n" name tname;
      name

(*
 * MLterm.
 *)
let declare_mlterm buf
    { mlterm_term = term;
      mlterm_contracta = contracta;
      mlterm_def = def
    } =
   let tname = print_term buf term in
   let cnames = List.map (print_term buf) contracta in
   let enames = print_expr_opt buf def in
      bprintf buf "(mlterm %s (%a) %s)\n" tname print_strings cnames enames;
      tname

(*
 * When a parent is declared, we need to open all the ancestors.
 *)
let declare_parent buf { parent_name = name } =
   bprintf buf "(parent %a)\n" print_strings name;
   ""

(*
 * Display form.
 *)
let print_dform_def buf = function
   NoDForm ->
      "NoDForm"
 | TermDForm t ->
      print_term buf t
 | MLDForm d ->
      "MLDForm"

let declare_dform buf
    { dform_modes = modes;
      dform_options = options;
      dform_redex = redex;
      dform_def = def
    } =
   let onames = List.map dform_opt options in
   let rname = print_term buf redex in
   let dname = print_dform_def buf def in
      bprintf buf "(dform (%a) (%a) %s %s)\n" print_strings modes print_strings onames rname dname;
      ""

(*
 * Precedence.
 *)
let declare_prec buf name =
   bprintf buf "(prec %s)\n" name;
   name

let declare_prec_rel buf
    { prec_rel = rel;
      prec_left = left;
      prec_right = right
    } =
   let rel =
      match rel with
         NoRelation ->
            "!="
       | LTRelation ->
            "<"
       | EQRelation ->
            "="
       | GTRelation ->
            ">"
   in
      bprintf buf "(prec_rel %s %s %s)\n" left rel right;
      ""

(*
 * Resource.
 *)
let declare_resource buf
    { resource_name = name;
      resource_extract_type = extract_type;
      resource_improve_type = improve_type;
      resource_data_type = data_type
    } =
   let ename = print_ctyp buf extract_type in
   let iname = print_ctyp buf improve_type in
   let dname = print_ctyp buf data_type in
      bprintf buf "(resource %s %s %s %s)\n" name ename iname dname;
      name

(*
 * Infix info.
 *)
let declare_infix buf name =
   bprintf buf "(infix %s)\n" name;
   name

(*
 * Standard summary item.
 *)
let declare_sig_item buf item =
   let name = print_sig_item buf item in
      bprintf buf "(sig_item %s)\n" name;
      name

(*
 * Standard summary item.
 *)
let declare_str_item buf item =
   let name = print_str_item buf item in
      bprintf buf "(str_item %s)\n" name;
      name

(*
 * Magic block is a block of items.
 *)
let declare_magic_block buf declare_item { magic_code = items } =
   let names = List.map (declare_item buf) items in
      bprintf buf "(magic_block %a)\n" print_strings names;
      ""

(*
 * Extract a signature item.
 *)
let print_item buf declare_item (item, _) =
   let _ =
      match item with
         Rewrite ({ rw_name = name } as rw) ->
            if !debug_filter_prog then
               eprintf "Filter_html.extract_sig_item: rewrite: %s%t" name eflush;
            declare_rewrite buf rw
       | CondRewrite ({ crw_name = name } as crw) ->
            if !debug_filter_prog then
               eprintf "Filter_html.extract_sig_item: cond rewrite: %s%t" name eflush;
            declare_cond_rewrite buf crw
       | Axiom ({ axiom_name = name } as ax) ->
            if !debug_filter_prog then
               eprintf "Filter_html.extract_sig_item: axiom: %s%t" name eflush;
            declare_axiom buf ax
       | Rule ({ rule_name = name } as rule) ->
            if !debug_filter_prog then
               eprintf "Filter_html.extract_sig_item: rule: %s%t" name eflush;
            declare_rule buf rule
       | Prec name ->
            if !debug_filter_prog then
               eprintf "Filter_html.extract_sig_item: prec: %s%t" name eflush;
            declare_prec buf name
       | Resource ({ resource_name = name } as rsrc) ->
            if !debug_filter_prog then
               eprintf "Filter_html.extract_sig_item: resource: %s%t" name eflush;
            declare_resource buf rsrc
       | Parent ({ parent_name = name } as parent) ->
            if !debug_filter_prog then
               eprintf "Filter_html.extract_sig_item: parent: %s%t" (string_of_path name) eflush;
            declare_parent buf parent
       | SummaryItem item ->
            if !debug_filter_prog then
               eprintf "Filter_html.extract_sig_item: summary_item%t" eflush;
            declare_item buf item
       | MagicBlock block ->
            if !debug_filter_prog then
               eprintf "Filter_html.extract_sig_item: magic block%t" eflush;
            declare_magic_block buf declare_item block
       | Opname name ->
            if !debug_filter_prog then
               eprintf "Filter_html.extract_sig_item: opname%t" eflush;
            declare_opname buf name
       | MLTerm term ->
            if !debug_filter_prog then
               eprintf "Filter_html.extract_sig_item: mlterm%t" eflush;
            declare_mlterm buf term
       | Condition term ->
            if !debug_filter_prog then
               eprintf "Filter_html.extract_sig_item: condition%t" eflush;
            declare_mlterm buf term
       | DForm df ->
            if !debug_filter_prog then
               eprintf "Filter_html.extract_sig_item: dform%t" eflush;
            declare_dform buf df
       | PrecRel rel ->
            if !debug_filter_prog then
               eprintf "Filter_html.extract_sig_item: prec rel%t" eflush;
            declare_prec_rel buf rel
       | Id id ->
            if !debug_filter_prog then
               eprintf "Filter_html.extract_sig_item: id: 0x%08x%t" id eflush;
            ""
       | Infix name ->
            if !debug_filter_prog then
               eprintf "Filter_html.extract_sig_item: infix: %s%t" name eflush;
            declare_infix buf name
       | Module (name, _) ->
            if !debug_filter_prog then
               eprintf "Filter_html.extract_sig_item: module: %s%t" name eflush;
            raise (Failure "Filter_sig.extract_sig_item: nested modules are not implemented")
   in
      ()

(*
 * Extract a signature.
 *)
let print_sig out info =
   let _ =
      if !debug_filter_prog then
         eprintf "Filter_html.extract_sig: begin%t" eflush
   in
   let buf =
      { buf_file = out;
        buf_opnames = Hashtbl.create 19;
        buf_terms = Hashtbl.create 19
      }
   in
      List.iter (print_item buf declare_sig_item) (info_items info)

(*
 * Extract a signature.
 *)
let print_str out info =
   let _ =
      if !debug_filter_prog then
         eprintf "Filter_html.extract_sig: begin%t" eflush
   in
   let buf =
      { buf_file = out;
        buf_opnames = Hashtbl.create 19;
        buf_terms = Hashtbl.create 19
      }
   in
      List.iter (print_item buf declare_str_item) (info_items info)

(*
 * $Log$
 * Revision 1.3  1998/04/24 19:38:21  jyh
 * Updated debugging.
 *
 * Revision 1.2  1998/04/24 02:41:53  jyh
 * Added more extensive debugging capabilities.
 *
 * Revision 1.1  1998/03/12 00:27:04  jyh
 * Added filter_html, but its not finished yet.
 *
 * Revision 1.2  1998/02/23 14:46:14  jyh
 * First implementation of binary file compilation.
 *
 * Revision 1.1  1998/02/21 20:57:47  jyh
 * Two phase parse/extract.
 *
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
