(*
 * Display form handler.
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
 * Author: Jason Hickey <jyh@cs.cornell.edu>
 * Modified by: Aleksey Nogin <nogin@cs.cornell.edu>
 *)

open Printf
open Mp_debug

open Precedence
open Rformat
open Opname
open Refiner.Refiner.Term
open Refiner.Refiner.TermType
open Refiner.Refiner.TermMan
open Refiner.Refiner.TermMeta
open Refiner.Refiner.Rewrite
open Term_match_table
open Simple_print.SimplePrint

(*
 * Show loading of the file.
 *)
let _ =
   show_loading "Loading Dform%t"

let debug_dform =
   create_debug (**)
      { debug_name = "dform";
        debug_description = "show display form formatting";
        debug_value = false
      }

(************************************************************************
 * TYPES                                                                *
 ************************************************************************)

(*
 * Print to term tagged buffers.
 *)
type buffer = term Rformat.buffer

(*
 * A display form printer knows about this term, and
 * a printer for subterms.  The subterm printer takes
 * an extra argument that specifies parenthesization.
 *)
type parens =
   NOParens
 | LTParens
 | LEParens

type dform_printer_info =
   { dform_term : term;
     dform_items : rewrite_item list;

     dform_printer : buffer -> parens -> term -> unit;
     dform_buffer : buffer
   }

type dform_printer =
   DFormExpansion of term
 | DFormPrinter of (dform_printer_info -> unit)

(*
 * Options on a dform.
 *)
type dform_option =
   DFormInheritPrec
 | DFormPrec of precedence
 | DFormParens
 | DFormInternal

(*
 * This is the info needed for each display form.
 *)
type dform_info =
   { dform_name : string;
     dform_pattern : term;
     dform_options : dform_option list;
     dform_print : dform_printer
   }

(*
 * The display database is just a table matching terms
 * with their precedence and printer.
 *)
type df_printer =
   DFExpansion of rewrite_redex * rewrite_contractum
 | DFPrinter of (dform_printer_info -> unit)

type dform_item =
   { df_name : string;
     df_precedence : precedence;
     df_printer : df_printer;
     df_external : bool
   }

type dform_base = (dform_item, dform_item) term_table

(*
 * Destruct a base.
 *)
type dform_entry =
   DFormEntry of dform_info
 | DFormBase of dform_base

(************************************************************************
 * IMPLEMENTATION                                                       *
 ************************************************************************)

(*
 * "slot" term has special meaning.
 *)
let slot_opname = mk_opname "slot" nil_opname

(*
 * We use a special precedence to specify that a form should
 * inherit the precedence of its parent.
 *)
let inherit_prec = new_prec ()

(*
 * Display form installation.
 *)
let rec process_options precedence parens internal = function
   [] ->
      if parens then
         precedence, internal
      else
         max_prec, internal
 | h::t ->
      match h with
         DFormInheritPrec ->
            process_options inherit_prec true internal t
       | DFormPrec p ->
            process_options p true internal t
       | DFormParens ->
            process_options precedence true internal t
       | DFormInternal ->
            process_options precedence parens true t

let add_dform base { dform_name = name;
                     dform_pattern = t;
                     dform_options = options;
                     dform_print = printer
                   } =
   if (!debug_dform) then
      eprintf "Adding DForm %s%t" name eflush;
   let precedence, internal = process_options min_prec false false options in
   let printer' =
      match printer with
         DFormExpansion e ->
            let redex, _ = compile_redex Relaxed [||] t in
            let contractum = compile_contractum Relaxed redex e in
               DFExpansion (redex, contractum)
       | DFormPrinter f ->
            DFPrinter f
   in
      insert base t { df_name = name;
                      df_precedence = precedence;
                      df_printer = printer';
                      df_external = not internal
      }

(*
 * Join two bases.
 *)
let join_dforms = join_tables

(*
 * Destruct a base.
 *)
let equal_dfbases = equal_tables

(*
 * Commands in initial base.
 *)
let lzone { dform_buffer = buf } =
   format_lzone buf

let hzone { dform_buffer = buf } =
   format_hzone buf

let szone { dform_buffer = buf } =
   format_szone buf

let izone { dform_buffer = buf } =
   format_izone buf

let ezone { dform_buffer = buf } =
   format_ezone buf

let hbreak = function
   { dform_items = [RewriteString yes; RewriteString no]; dform_buffer = buf } ->
      format_hbreak buf yes no
 | _ -> raise (Invalid_argument "Dform.hbreak")

let sbreak = function
   { dform_items = [RewriteString yes; RewriteString no]; dform_buffer = buf } ->
      format_sbreak buf yes no
 | _ -> raise (Invalid_argument "Dform.sbreak")

let cbreak = function
   { dform_items = [RewriteString yes; RewriteString no]; dform_buffer = buf } ->
      format_cbreak buf yes no
 | _ -> raise (Invalid_argument "Dform.sbreak")

let space { dform_buffer = buf } =
   format_space buf

let hspace { dform_buffer = buf } =
   format_hspace buf

let newline { dform_buffer = buf } =
   format_newline buf

let pushm = function
   { dform_items = [RewriteNum n]; dform_buffer = buf } ->
      format_pushm buf (Mp_num.int_of_num n)
 | { dform_items = [RewriteString s]; dform_buffer = buf } ->
      format_pushm_str buf s
 | { dform_items = []; dform_buffer = buf } ->
      format_pushm buf 0
 | _ -> raise (Invalid_argument "Dform.pushm")

let popm { dform_buffer = buf } =
   format_popm buf

let pushfont _ =
   ()

let popfont _ =
   ()

(************************************************************************
 * FORMATTERS                                                           *
 ************************************************************************)

(* List of params *)
let rec format_paramlist buf = function
   [] ->
      ()
 | [h] ->
      format_simple_param buf h
 | h::t ->
      format_simple_param buf h;
      format_char buf ',';
      format_hspace buf;
      format_paramlist buf t

(* Optional empty params *)
let format_params buf = function
   [] ->
      ()
 | params ->
      format_hbreak buf "" "";
      format_char buf '[';
      format_pushm buf 0;
      format_paramlist buf params;
      format_char buf ']';
      format_popm buf

(* Print a single bterm *)
let rec format_bterm' buf printer bterm =
   match dest_bterm bterm with
      { bvars = []; bterm = term } ->
         printer term
    | { bvars = vars; bterm = term } ->
         let rec format_bvars = function
            [] -> ()
          | [h] -> format_quoted_string buf h
          | h::t ->
               format_quoted_string buf h;
               format_string buf ", ";
               format_bvars t
         in
            format_bvars vars;
            format_string buf ". ";
            printer term

(* Nonempty list *)
and format_btermlist buf printer = function
   [] ->
      ()
 | [h] ->
      format_bterm' buf printer h
 | h::t ->
      format_bterm' buf printer h;
      format_char buf ';';
      format_hspace buf;
      format_btermlist buf printer t

(* Optional empty bterm list *)
and format_bterms buf printer = function
   [] ->
      ()
 | bterms ->
      format_hbreak buf "" "";
      format_char buf '{';
      format_pushm buf 0;
      format_btermlist buf printer bterms;
      format_char buf '}';
      format_popm buf

(*
 * A special format for sequents.
 *)
and format_sequent buf format_term term =
   let rec format_goal goals i len =
      if i <> len then
         begin
            format_string buf (if i = 0 then ">-" else ";");
            format_space buf;
            format_term (SeqGoal.get goals i);
            format_goal goals (i + 1) len
         end
   in
   let rec format_hyp hyps i len =
      if i <> len then
         let _ =
            if i <> 0 then
               format_string buf ";"
         in
         let _ =
            match SeqHyp.get hyps i with
               Hypothesis (v, a) ->
                  format_space buf;
                  format_string buf v;
                  format_string buf ". ";
                  format_term a
             | Context (v, values) ->
                  format_space buf;
                  format_term (mk_so_var_term v values)
         in
            format_hyp hyps (i + 1) len
   in
      let { sequent_args = args;
            sequent_hyps = hyps;
            sequent_goals = goals
          } = explode_sequent term
      in
         format_szone buf;
         format_pushm buf 0;
         format_string buf "sequent {";
         format_hyp hyps 0 (SeqHyp.length hyps);
         format_goal goals 0 (SeqGoal.length goals);
         format_string buf " }";
         format_popm buf;
         format_ezone buf

(*
 * This is the default top level print function.
 * Check for variables.
 *)
and format_term buf shortener printer term =
   if is_so_var_term term then
      format_simple_term buf term
   else if is_sequent_term term then
      format_sequent buf printer term
   else
      (* Standard term *)
      let { term_op = op; term_terms = bterms } = dest_term term in
      let { op_name = name; op_params = params } = dest_op op in
         format_szone buf;
         format_pushm buf 4;
         begin
            match dest_opname name with
               h::_ ->
                  if Opname.eq (shortener h) name then
                     format_string buf h
                  else
                     format_quoted_string buf (string_of_opname name)
             | [] ->
                  format_string buf "$"
                  (* raise (Invalid_argument "DForm.format_term") *)
         end;
         format_params buf params;
         format_bterms buf printer bterms;
         format_popm buf;
         format_ezone buf

(************************************************************************
 * PRINTING                                                             *
 ************************************************************************)

(*
 * Sequents are handled specially.
 *)
let sequent_term =
   let opname = mk_opname "sequent" nil_opname in
      mk_simple_term opname [mk_var_term "ext"; mk_var_term "hyps"]

(*
 * Print a term to a buffer.
 *)
let format_short_term base shortener =
   (* Print a single term, ignoring lookup errors *)
   let rec print_term' pprec buf eq t =
      (* Check for a display form entry *)
      let items, { df_name = name;
                   df_precedence = pr';
                   df_printer = printer;
                   df_external = is_external
          } =
         let t =
            if is_sequent_term t then
               sequent_term
            else
               t
         in
            (* HACK! There used to be an identity instead of List.rev here, but I changed it
               because for some reason (I have no idea why) the entries were going in the wrong
               order *)
            lookup "format_short_term" base List.rev t
      in
      let pr, parenflag =
         if pr' = inherit_prec then
            begin
               if !debug_dform then
                  eprintf "Dform %s: inherit_prec%t" name eflush;
               pprec, false
            end
         else
            pr', (if eq = NOParens or get_prec pr' max_prec = EQRelation then
                     begin
                        if !debug_dform then
                           eprintf "Dform %s: NOParens%t" name eflush;
                        false
                     end
                  else
                     match get_prec pprec pr' with
                        NoRelation ->
                           if !debug_dform then
                              eprintf "Dform %s: NoRelation%t" name eflush;
                           true
                      | LTRelation ->
                           if !debug_dform then
                              eprintf "Dform %s: LtRelation%t" name eflush;
                           false
                      | EQRelation ->
                           if !debug_dform then
                              eprintf "Dform %s: EqRelation%t" name eflush;
                           eq = LEParens
                      | GTRelation ->
                           if !debug_dform then
                              eprintf "Dform %s: GTRelation%t" name eflush;
                           true)
      in
         if parenflag then
            format_string buf "(";
         if is_external then
            format_tzone buf t;
         begin
            match printer with
               DFPrinter f ->
                  let entry =
                     { dform_term = t;
                       dform_items = items;
                       dform_printer = print_term pr;
                       dform_buffer = buf
                     }
                  in
                     if !debug_dform then
                        eprintf "Dform fun %s: %s%t" name (short_string_of_term t) eflush;
                     f entry
             | DFExpansion (r, c) ->
                  let stack = apply_redex r [||] t [] in
                  let t = make_contractum c stack in
                     if !debug_dform then
                        eprintf "Dform %s%t" name eflush;
                     print_entry pr buf eq t
         end;
         if is_external then
            format_ezone buf;
         if parenflag then
            format_string buf ")";

   (* If there is no template, use the standard printer *)
   and print_term pprec buf eq t =
      if !debug_dform then
         eprintf "Dform: %s%t" (short_string_of_term t) eflush;
      try print_term' pprec buf eq t with
         Not_found ->
            if !debug_dform then
               let rec format t =
                  format_term buf shortener format t
               in
                  eprintf "Default display form: %s%t" (short_string_of_term t) eflush;
                  format t
            else
               format_term buf shortener (print_term max_prec buf NOParens) t

   (* Print an entry in the list of terms being displayed *)
   and print_entry pprec buf eq =
      let rec aux t =
         if is_xcons_term t then
            let hd, tl = dest_xcons t in
               if is_xstring_term hd then
                  let s = dest_xstring hd in
                     if !debug_dform then
                        eprintf "Dform string: %s%t" s eflush;
                     format_string buf s
               else
                  print_term pprec buf eq hd;
               aux tl
         else if is_xnil_term t then
            ()
         else
            print_term pprec buf eq t
      in
         aux

   (* Print a list of terms *)
   and print_termlist pprec buf eq l =
      List.iter (print_entry pprec buf eq) l

   in
   let print buf = print_term max_prec buf NOParens in
      print

(************************************************************************
 * BASE                                                                 *
 ************************************************************************)

(* Terms *)
let null_shortener _ = nil_opname

(*
 * The "slot" term is special because it has a subterm.
 *)
let slot { dform_items = items; dform_printer = printer; dform_buffer = buf } =
   match items with
      [RewriteString parens; RewriteTerm body] ->
         begin
            match parens with
               "le" ->
                  printer buf LEParens body
             | "lt" ->
                  printer buf LTParens body
             | "raw" ->
                  let rec format t =
                     format_term buf null_shortener format t
                  in
                     format body
             | _ ->
                  printer buf NOParens body
         end
    | [RewriteTerm body] ->
         if !debug_dform then
            eprintf "Dform.slot: %s%t" (short_string_of_term body) eflush;
         printer buf LTParens body
    | [RewriteString s] ->
         if !debug_dform then
            eprintf "Dform.slot: %s%t" s eflush;
         format_string buf s
    | [RewriteString "raw"; RewriteString s] ->
         if !debug_dform then
            eprintf "Dform.slot: %s%t" s eflush;
         format_raw_string buf s
    | [RewriteNum n] ->
         let s = Mp_num.string_of_num n in
         if !debug_dform then
             eprintf "Dform.slot: %s%t" s eflush;
         format_string buf s
    | [RewriteLevel l] ->
         if !debug_dform then
            eprintf "Dform.slot: level%t" eflush;
         format_simple_level_exp buf l
    | _ ->
         raise (Invalid_argument "slot")

(*
 * Install initial commands.
 *)
let init_list =
   ["sbreak", [MString "y"; MString "n"], sbreak;
    "cbreak", [MString "y"; MString "n"], cbreak;
    "hbreak", [MString "y"; MString "n"], hbreak;
    "space", [], space;
    "hspace", [], hspace;
    "newline", [], newline;
    "lzone", [], lzone;
    "szone", [], szone;
    "hzone", [], hzone;
    "izone", [], izone;
    "ezone", [], ezone;
    "pushm", [MNumber "i"], pushm;
    "pushm", [MString "s"], pushm;
    "pushm", [], pushm;
    "popm", [], popm;
    "pushfont", [MString "plain"], pushfont;
    "popfont", [], popfont;
    "slot", [String "raw"; MString "s"], slot;
    "slot", [MString "s"], slot;
    "slot", [MLevel (mk_var_level_exp "l")], slot;
    "slot", [MToken "t"], slot;
    "slot", [MNumber "n"], slot;
    "slot", [MVar "v"], slot;
   ]

let null_base =
   let rec aux = function
      (name, params, f)::t ->
         let term = mk_term (mk_op (make_opname [name]) (List.map make_param params)) [] in
         let entry =
            { dform_name = name;
              dform_pattern = term;
              dform_options = [DFormInheritPrec; DFormInternal];
              dform_print = DFormPrinter f
            }
         in
            add_dform (aux t) entry
    | [] ->
         new_table ()
   in
   let base = aux init_list in
   let slot_entry1 =
      { dform_name = "slot_entry1";
        dform_pattern =
           mk_term (mk_op slot_opname [make_param (MString "eq")])
              [mk_bterm [] (mk_var_term "v")];
        dform_options = [DFormInheritPrec; DFormInternal];
        dform_print = DFormPrinter slot
      }
   in
   let slot_entry2 =
      { dform_name = "slot_entry2";
        dform_pattern =
           mk_term (mk_op slot_opname [])
              [mk_bterm [] (mk_var_term "v")];
        dform_options = [DFormInheritPrec; DFormInternal];
        dform_print = DFormPrinter slot
      }
   in
   let base = add_dform base slot_entry1 in
   let base = add_dform base slot_entry2 in
      base

let is_null_dfbase = equal_tables null_base

(************************************************************************
 * SIMPLIFIED PRINTERS                                                  *
 ************************************************************************)

let format_quoted_term base buf t =
   format_term buf null_shortener
   (fun t -> format_short_term base null_shortener buf t) t

let format_term base = format_short_term base null_shortener

let print_term_fp base out term =
   let buf = new_buffer () in
      format_term base buf term;
      print_to_channel 80 buf out

let print_short_term_fp base shortener out term =
   let buf = new_buffer() in
      format_short_term base shortener buf term;
      print_to_channel 80 buf out

let print_term base = print_term_fp base stdout

let prerr_term base = print_term_fp base stderr

let string_of_term base term =
   let buf = new_buffer () in
      format_term base buf term;
      print_to_string 80 buf

(* Terms *)
let format_bterm base buf =
   format_bterm' buf (format_term base buf)

let print_bterm_fp base out term =
   let buf = new_buffer () in
      format_bterm base buf term;
      print_to_channel 80 buf out

let print_bterm base = print_bterm_fp base stdout

let prerr_bterm base = print_bterm_fp base stderr

let string_of_bterm base term =
   let buf = new_buffer () in
      format_bterm base buf term;
      print_to_string 80 buf

(*
 * MetaTerms.
 *)
let rec format_mterm base buf = function
   MetaTheorem t ->
      format_term base buf t
 | MetaImplies (a, b) ->
      format_szone buf;
      format_pushm buf 0;
      format_mterm base buf a;
      format_string buf " -->";
      format_hspace buf;
      format_mterm base buf b;
      format_popm buf;
      format_ezone buf
 | MetaFunction (v, a, b) ->
      format_szone buf;
      format_pushm buf 0;
      format_term base buf v;
      format_string buf " : ";
      format_mterm base buf a;
      format_string buf " -->";
      format_hspace buf;
      format_mterm base buf b;
      format_popm buf;
      format_ezone buf
 | MetaIff (a, b) ->
      format_szone buf;
      format_pushm buf 0;
      format_mterm base buf a;
      format_string buf " <-->";
      format_hspace buf;
      format_mterm base buf b;
      format_popm buf;
      format_ezone buf
 | MetaLabeled (l, t) ->
      format_szone buf;
      format_pushm buf 0;
      format_string buf ( " ("^l^")" );
      format_hspace buf;
      format_mterm base buf t;
      format_popm buf;
      format_ezone buf

let print_mterm_fp base out mterm =
   let buf = new_buffer () in
      format_mterm base buf mterm;
      print_to_channel 80 buf out

let print_mterm base = print_mterm_fp base stdout

let prerr_mterm base = print_mterm_fp base stderr

let string_of_mterm base mterm =
   let buf = new_buffer () in
      format_mterm base buf mterm;
      print_to_string 80 buf

let debug_base = ref null_base

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
