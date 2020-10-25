(*
 * Display form handler.
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
 * Copyright (C) 1998-2004, MetaPRL Group
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
open Lm_printf
open Lm_printf_rbuffer
open Lm_rformat
open Lm_rformat_text
open Lm_string_set

open Precedence
open Opname
open Term_sig
open Rewrite_sig
open Refiner.Refiner.Term
open Refiner.Refiner.TermType
open Refiner.Refiner.TermOp
open Refiner.Refiner.TermMan
open Refiner.Refiner.TermMeta
open Refiner.Refiner.TermShape
open Refiner.Refiner.Rewrite
open Term_match_table
open Simple_print
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

let debug_dform_depth =
   create_debug (**)
      { debug_name = "dform_depth";
        debug_description = "check zone depth to catch unbalanced buffers";
        debug_value = false
      }

let debug_rewrite = load_debug "rewrite"

(************************************************************************
 * TYPES                                                                *
 ************************************************************************)

(*
 * Print to term tagged buffers.
 *)
type buffer = Lm_rformat.buffer

type dform_mode = string

type dform_modes =
   Modes of dform_mode list       (* include these modes *)
 | ExceptModes of dform_mode list (* exclude these modes *)
 | AllModes
 | PrimitiveModes

(*
 * Subterm printers take an extra argument that specifies parenthesization.
 *)
type parens =
   NOParens
 | LTParens
 | LEParens

(*
 * Some functions define a "shortener:" a function to produce
 * a string from a description of a term.
 *)
type shortener = opname -> op_kind -> param list -> bound_term list -> string

type dform_printer_info =
   { dform_term : term;
     dform_items : rewrite_item list;
     dform_printer : buffer -> parens -> term -> unit;
     dform_buffer : buffer;
     dform_state : dform_base
   }

(*
 * The display database is just a table matching terms
 * with their precedence and printer.
 *)
and df_printer =
   DFExpansion of rewrite_rule
 | DFPrinter of (dform_printer_info -> unit)

and dform_item =
   { df_modes : dform_modes;
     df_parens : bool;
     df_precedence : precedence;
     df_printer : df_printer;
     df_name : string;
   }

and dform_table = dform_item term_table

(*
 * Dforms base contains the basic information of the display used and the current state ---
 * such as terms collectes as they are displayed
 *)

and dform_base = {
   df_mode : dform_mode;
   df_short : shortener;
   mutable df_table : delayed_dform_table;
   mutable df_terms : term StringTable.t option
}

and delayed_dform_table =
   DfBk of Mp_resource.bookmark
 | DfTable of dform_table

(*
 * A display form printer knows about this term, and
 * a printer for subterms.
 *)
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

(*
 * This is the info needed for each display form.
 *)
type dform_info =
   { dform_modes : dform_modes;
     dform_pattern : term;
     dform_options : dform_option list;
     dform_print : dform_printer;
     dform_name : string;
   }

(************************************************************************
 * IMPLEMENTATION                                                       *
 ************************************************************************)

let change_mode base mode = { base with df_mode = mode }

(*
 * We use a special precedence to specify that a form should
 * inherit the precedence of its parent.
 *)
let inherit_prec = new_prec ()

(*
 * Display form installation.
 *)
let rec process_options precedence parens options =
   match options with
      [] ->
         parens, precedence
    | h :: t ->
         match h with
            DFormInheritPrec ->
               process_options inherit_prec true t
          | DFormPrec p ->
               process_options p parens t
          | DFormParens ->
               process_options precedence true t

(*
 * Add the display form to the table.
 *)
let add_dform tbl df =
   (*
    * DEBUG: the raw mode can never be mentioned explicitly.
    *)
   let () =
      match df.dform_modes with
         AllModes
       | PrimitiveModes ->
            ()
       | ExceptModes l
       | Modes l ->
            if List.mem "raw" l then
               raise (Invalid_argument("Dform.add_dform: raw mode is built-in and can not be mentioned"))
   in
   let parens, precedence = process_options min_prec false df.dform_options in
   let printer' =
      match df.dform_print with
         DFormExpansion e ->
            DFExpansion (term_rewrite Relaxed empty_args_spec [df.dform_pattern] [e])
       | DFormPrinter f ->
            DFPrinter f
   in
      add_item tbl df.dform_pattern (**)
         { df_modes = df.dform_modes;
           df_name = df.dform_name;
           df_parens = parens;
           df_precedence = precedence;
           df_printer = printer';
         }

(*
 * Commands in initial base.
 *)
let lzone df = format_lzone df.dform_buffer
let hzone df = format_hzone df.dform_buffer
let szone df = format_szone df.dform_buffer
let izone df = format_izone df.dform_buffer
let azone df = format_azone df.dform_buffer
let ezone df = format_ezone df.dform_buffer

let string_of_param = function
   RewriteParam s -> s
 | RewriteMetaParam v -> dstring_of_var v

let tzone = function
   { dform_items = [RewriteString tag]; dform_buffer = buf; _ } ->
      format_tzone buf (string_of_param tag)
 | _ ->
      raise (Invalid_argument "Dform.sbreak")

let hbreak = function
   { dform_items = [RewriteString yes; RewriteString no]; dform_buffer = buf; _ } ->
      format_hbreak buf (string_of_param yes) (string_of_param no)
 | _ ->
      raise (Invalid_argument "Dform.hbreak")

let sbreak = function
   { dform_items = [RewriteString yes; RewriteString no]; dform_buffer = buf; _ } ->
      format_sbreak buf (string_of_param yes) (string_of_param no)
 | _ ->
      raise (Invalid_argument "Dform.sbreak")

let cbreak = function
   { dform_items = [RewriteString yes; RewriteString no]; dform_buffer = buf; _ } ->
      format_cbreak buf (string_of_param yes) (string_of_param no)
 | _ ->
      raise (Invalid_argument "Dform.sbreak")

let space df = format_space df.dform_buffer
let hspace df = format_hspace df.dform_buffer
let newline df = format_newline df.dform_buffer

let pushm = function
   { dform_items = [RewriteNum (RewriteParam n)]; dform_buffer = buf; _ } ->
      format_pushm buf (Lm_num.int_of_num n)
 | { dform_items = [RewriteNum (RewriteMetaParam v)]; dform_buffer = buf; _ } ->
      format_pushm_str buf (dstring_of_var v)
 | { dform_items = [RewriteString s]; dform_buffer = buf; _ } ->
      format_pushm_str buf (string_of_param s)
 | { dform_items = []; dform_buffer = buf; _ } ->
      format_pushm buf 0
 | _ ->
      raise (Invalid_argument "Dform.pushm")

let popm df = format_popm df.dform_buffer

let translate_font s =
   match s with
      "bf" -> Lm_terminfo.enter_bold_mode
    | "it" | "em" -> Lm_terminfo.enter_italics_mode
    | "rm" -> Lm_terminfo.enter_normal_quality
    | "sub" -> Lm_terminfo.enter_subscript_mode
    | "sup" -> Lm_terminfo.enter_superscript_mode
    | "ul" -> Lm_terminfo.enter_underline_mode
    | _ -> Lm_terminfo.exit_attribute_mode

let changefont buf s =
   match Lm_terminfo.tgetstr (translate_font s) with
      Some s' ->
         format_izone buf;
         format_raw_string buf s';
         format_ezone buf
    | None ->
         ()

let pushfont df =
   let font, buf =
      match df with
         { dform_items = [RewriteString font]; dform_buffer = buf; _ } ->
            string_of_param font, buf
       | { dform_buffer = buf; _ } ->
            "rm", buf
   in
      changefont buf font

let popfont { dform_buffer = buf; _ } =
   changefont buf "exit"

(************************************************************************
 * FORMATTERS                                                           *
 ************************************************************************)

(*
 * Sequents and variables are handled specially.
 *)
let base_opname   = mk_opname "Base_dform"     nil_opname
let dsovar_opname = mk_opname "df_so_var"      base_opname
let dvar_opname   = mk_opname "df_var"         base_opname
let dfvar_opname  = mk_opname "df_free_fo_var" base_opname
let dcont_opname =  mk_opname "df_context"     base_opname
let dcontv_opname = mk_opname "df_context_var" base_opname

let make_cont v = mk_term (mk_op dcontv_opname [make_param (String (dstring_of_var v))]) []

(* List of params *)
(*
 * XXX: BUG: Nogin: format_simple_param will not do the right thing for printing
 * shape parameters in the "src" mode
 *)
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
          | [h] -> format_quoted_string buf (dstring_of_var h)
          | h::t ->
               format_quoted_string buf (dstring_of_var h);
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
and format_sequent buf printer term =
   let rec format_hyp hyps i len =
      if i <> len then
         let _ =
            if i <> 0 then
               format_string buf ";"
         in
         let hyp =
            match SeqHyp.get hyps i with
               Hypothesis (v, t) when
                  Opname.eq (opname_of_term t) dcont_opname && Lm_symbol.to_string v = "" ->
                     let v, conts, ts = dest_so_var (one_subterm t) in
                        Context (v, conts, ts)
             | hyp -> hyp
         in
         let () =
            match hyp with
               Hypothesis (v, a) ->
                  format_space buf;
                  if Lm_symbol.to_string v <> "" then begin
                     format_string buf (dstring_of_var v);
                     format_string buf ": ";
                  end;
                  printer a
             | Context (v, conts, values) ->
                  format_space buf;
                  format_string buf "<";
                  printer (mk_so_var_term v conts values);
                  format_string buf ">"
         in
            format_hyp hyps (succ i) len
   in
   let { sequent_args = arg;
         sequent_hyps = hyps;
         sequent_concl = concl
       } = explode_sequent term
   in
      format_szone buf;
      format_pushm buf 3;
      format_string buf "sequent";
      format_space buf;
      format_string buf "[";
      printer arg;
      format_string buf "]";
      format_space buf;
      format_string buf "{";
      format_hyp hyps 0 (SeqHyp.length hyps);
      format_string buf " >-";
      format_space buf;
      printer concl;
      format_popm buf;
      format_space buf;
      format_string buf "}";
      format_ezone buf

(*
 * This is the default top level print function.
 * Check for variables.
 *)
and default_format_term buf (shortener : shortener) printer term =
   if is_var_term term then
      format_string buf ("'" ^ dstring_of_var (dest_var term))
   else if is_so_var_term term || is_context_term term then
      format_simple_term buf term
   else if is_sequent_term term then
      format_sequent buf printer term
   else
      (* Standard term *)
      let { term_op = op; term_terms = bterms } = dest_term term in
      let { op_name = name; op_params = params } = dest_op op in
      let opname = shortener name NormalKind params bterms in
         format_szone buf;
         format_pushm buf 4;
         format_quoted_string buf opname;
         format_params buf params;
         format_bterms buf printer bterms;
         format_popm buf;
         format_ezone buf

(************************************************************************
 * BASE                                                                 *
 ************************************************************************)

(* Terms *)
let null_shortener opname _ _ _ =
   Opname.string_of_opname opname

(*
 * Saving slot terms.
 *)
let save_slot_terms base =
   { base with df_terms = Some StringTable.empty }

let get_slot_terms base =
   match base.df_terms with
      Some table ->
         table
    | None ->
         raise (Invalid_argument "Dform.get_slot_terms")

(*
 * Tag all terms printed in slots (if requested).
 *)
let format_tag base buf t =
   match base.df_terms with
      Some table ->
         let index = StringTable.cardinal table in
         let id = sprintf "slot%d" index in
         let table = StringTable.add table id t in
            format_tzone buf id;
            base.df_terms <- Some table
    | None ->
         ()

let format_etag base buf =
   match base.df_terms with
      Some _ ->
         format_ezone buf
    | None ->
         ()

(*
 * The "slot" term is special because it has a subterm.
 *
 * BUG nogin: We should add a form that only accepts RewriteParam's and invokes
 * some fall-back mechanism on receiving RewriteMetaParam's
 *)
let slot { dform_state = state; dform_items = items; dform_printer = printer; dform_buffer = buf; _ } =
   match items with
      [RewriteString parens; RewriteTerm body] ->
         let parens = string_of_param parens in
            if !debug_dform then
               eprintf "Dform.slot: %s: %s%t" parens (string_of_term body) eflush;
            (* Tag the term *)
            format_tag state buf body;
            (match parens with
                "le" ->
                   printer buf LEParens body
              | "lt" ->
                   printer buf LTParens body
              | "raw" ->
                   let rec format t =
                      default_format_term buf null_shortener format t
                   in
                      format body
              | _ ->
                   printer buf NOParens body);
            format_etag state buf
    | [RewriteTerm body] ->
         if !debug_dform then
            eprintf "Dform.slot: term: %s%t" (string_of_term body) eflush;
         format_tag state buf body;
         printer buf LTParens body;
         format_etag state buf
    | [RewriteString s]
    | [RewriteNum ((RewriteMetaParam _) as s)]
    | [RewriteToken ((RewriteMetaParam _) as s)]
    | [RewriteShape ((RewriteMetaParam _) as s)]
    | [RewriteOperator ((RewriteMetaParam _) as s)] ->
         let s = string_of_param s in
         if !debug_dform then
            eprintf "Dform.slot: str: %s%t" s eflush;
         format_string buf s
    | [RewriteString (RewriteParam "raw"); RewriteString s] ->
         let s = string_of_param s in
         if !debug_dform then
            eprintf "Dform.slot: raw str: %s%t" s eflush;
         format_raw_string buf s
    | [RewriteString (RewriteParam "esc"); RewriteString s] ->
         let s = string_of_param s in
         if !debug_dform then
            eprintf "Dform.slot: escaped str: %s%t" s eflush;
         format_string buf (String.escaped s)
    | [RewriteString (RewriteParam "cesc"); RewriteString s] ->
         let s = string_of_param s in
         if !debug_dform then
            eprintf "Dform.slot: C escaped str: %s%t" s eflush;
         format_string buf (Lm_string_util.c_escaped s)
    | [RewriteToken (RewriteParam opname)] ->
         if !debug_dform then
            eprintf "Dform.slot: token: %s%t" (string_of_opname opname) eflush;
         format_string buf (state.df_short opname TokenKind [] [])
    | [RewriteShape (RewriteParam sh)] ->
         if !debug_dform then
             eprintf "Dform.slot: shape: %s%t" (string_of_shape sh) eflush;
         printer buf NOParens (canonical_term_of_shape sh)
    | [RewriteOperator (RewriteParam op)] ->
         if !debug_dform then
             eprintf "Dform.slot: opparam: %s%t" (string_of_opparam op) eflush;
         printer buf NOParens (canonical_term_of_opparam op)
    | [RewriteNum (RewriteParam n)] ->
         let s = Lm_num.string_of_num n in
            if !debug_dform then
               eprintf "Dform.slot: num: %s%t" s eflush;
            format_string buf s
    | [RewriteLevel l] ->
         if !debug_dform then
            eprintf "Dform.slot: level%t" eflush;
         format_simple_level_exp buf l
    | _ ->
         raise(Failure "Dform.slot: unknown stack type")

(*
 * Wrap the slot display with a debugging check to test
 * the the contents of the slot are balanced.
 *)
let slot df =
   if !debug_dform_depth then
      let depth = zone_depth df.dform_buffer in
         begin
            slot df;
            if depth != zone_depth df.dform_buffer then
               let str =
                  line_format default_width (**)
                     (fun buf ->
                           let rec format t =
                              default_format_term buf null_shortener format t
                           in
                              slot { df with dform_printer = (fun _ _ -> format); dform_buffer = buf })
               in
                  eprintf "Unbalanced display form: %s%t" str eflush
         end
   else
      slot df

(*
 * Install initial commands.
 *)
let slot_opname = mk_opname "slot" xperv

let y_sym = Lm_symbol.add "y"
let n_sym = Lm_symbol.add "n"
let t_sym = Lm_symbol.add "t"
let s_sym = Lm_symbol.add "s"
let i_sym = Lm_symbol.add "i"
let l_sym = Lm_symbol.add "l"
let v_sym = Lm_symbol.add "v"
let raw_sym   = Lm_symbol.add "raw"
let plain_sym = Lm_symbol.add "plain"
let eq_sym    = Lm_symbol.add "eq"

let init_list =
   ["sbreak",   [MString y_sym; MString n_sym], sbreak;
    "cbreak",   [MString y_sym; MString n_sym], cbreak;
    "hbreak",   [MString y_sym; MString n_sym], hbreak;
    "space",    [], space;
    "hspace",   [], hspace;
    "newline",  [], newline;
    "lzone",    [], lzone;
    "szone",    [], szone;
    "hzone",    [], hzone;
    "izone",    [], izone;
    "azone",    [], azone;
    "ezone",    [], ezone;
    "tzone",    [MString t_sym], tzone;
    "pushm",    [MNumber i_sym], pushm;
    "pushm",    [MString s_sym], pushm;
    "pushm",    [], pushm;
    "popm",     [], popm;
    "pushfont", [MString plain_sym], pushfont;
    "popfont",  [], popfont;
    "slot",     [MString raw_sym; MString s_sym], slot;
    "slot",     [MString s_sym], slot;
    "slot",     [MShape s_sym], slot;
    "slot",     [MOperator v_sym], slot;
    "slot",     [MLevel (mk_var_level_exp l_sym)], slot;
    "slot",     [MToken t_sym], slot;
    "slot",     [MNumber n_sym], slot;
    "slot",     [Var v_sym], slot]

let null_list =
   let v_bterms = [mk_bterm [] (mk_so_var_term v_sym [] [])] in
   let aux (name, params, f) =
      let term = mk_term (mk_op (mk_opname name xperv) (List.map make_param params)) [] in
         { dform_modes = PrimitiveModes;
           dform_name = name;
           dform_pattern = term;
           dform_options = [DFormInheritPrec];
           dform_print = DFormPrinter f
         }
   in
   let slot_entry1 =
      { dform_name = "slot_entry1";
        dform_pattern =
           mk_term (mk_op slot_opname [make_param (MString eq_sym)]) v_bterms;
        dform_options = [DFormInheritPrec];
        dform_print = DFormPrinter slot;
        dform_modes = PrimitiveModes;
      }
   in
   let slot_entry2 =
      { dform_name = "slot_entry2";
        dform_pattern = mk_term (mk_op slot_opname []) v_bterms;
        dform_options = [DFormInheritPrec];
        dform_print = DFormPrinter slot;
        dform_modes = PrimitiveModes;
      }
   in
      slot_entry1 :: slot_entry2 :: (List.map aux init_list)

let null_table =
   List.fold_left add_dform empty_table null_list

let null_base =
   { df_mode = "src"; df_table = DfTable null_table; df_short = null_shortener; df_terms = None }

(*
 * The display form tables for different theories are managed as a resource.
 * The resource interface is not fully type safe (normally filter ensures the
 * type safety by inserting appropriate checks), so we have to be very careful.
 *
 * XXX: TODO: The reason we are creating the resource manually (as opposed
 * to putting in into a PRL file and having filter do it) is that we do not want
 * people to be able to use "let resource +=" on it. One the private resources
 * are implemented (see Bug 168), this code should be moved into a PRL file.
 *)
let dform_resource =
   Mp_resource.create_resource "dform" (
      Mp_resource.Functional {
         Mp_resource.fp_empty = null_table;
         Mp_resource.fp_add = add_dform;
         Mp_resource.fp_retr = ((fun tbl -> tbl) : (dform_table -> dform_table));
      })

let find_dftable bk =
   let bk =
      (* XXX: HACK: use "summary" dforms in the "comment" module *)
      if fst bk = "comment" then
         Mp_resource.theory_bookmark "summary"
      else
         bk
   in
      dform_resource (Mp_resource.find bk)

let add_dform df =
   Mp_resource.improve Mp_resource.Public "dform" (Obj.repr (df : dform_info))

let get_table base =
   match base.df_table with
      DfTable t -> t
    | DfBk bk ->
         let t =
             try find_dftable bk with
               Not_found ->
                  raise (Failure("Dform.get_mode_base: can not find display forms for " ^ (String.capitalize (fst bk)) ^ "." ^ (snd bk)))
         in
            base.df_table <- DfTable t;
            t

let get_mode_base dfbase dfmode dfshort =
   { df_mode = dfmode; df_table = DfBk dfbase; df_short = dfshort; df_terms = None }

(************************************************************************
 * PRINTING                                                             *
 ************************************************************************)

(*
 * Mode "src" is treated specially.  It is only allowed if explicitly mentioned.
 *
 * BUG JYH: we should have some generic way to specify special modes.
 * A special mode is defined as: the display form is included *only if*
 * the mode is mentioned explicitly in the display form.
 *)
let special_modes = ["src"]

let mode_selector mode df =
   match df with
      { df_modes = ExceptModes l; _ } -> not (List.mem mode l) && not (List.mem mode special_modes)
    | { df_modes = Modes l; _ } -> List.mem mode l
    | { df_modes = AllModes; _ } -> not (List.mem mode special_modes)
    | { df_modes = PrimitiveModes; _ } -> true

(*
 * Print a term to a buffer.
 *)
let format_term base =
   (* Print a single term, ignoring lookup errors *)
   let lookup =
      if base.df_mode = "raw" then
         fun _ -> raise Not_found
      else
         lookup_rwi (get_table base) (mode_selector base.df_mode)
   in
   let shortener = if base.df_mode = "raw" then null_shortener else base.df_short in
   let rec print_term' pprec buf eq t =
      (* Convert a variable into a display_var *)
      let t =
         if is_encoded_free_var t then
            let v = decode_free_var t in
               mk_term (mk_op dfvar_opname [make_param (Var v)]) []
         else if is_var_term t then
            let v = dest_var t in
               mk_term (mk_op dvar_opname [make_param (Var v)]) []
         else if is_so_var_term t then
            let v, conts, terms = dest_so_var t in
               mk_term (mk_op dsovar_opname [make_param (Var v)]) (**)
                  [mk_simple_bterm (mk_xlist_term (List.map make_cont conts)); mk_simple_bterm (mk_xlist_term terms)]
         else if is_context_term t then
            let v, t, conts, terms = dest_context t in
               mk_term (mk_op dcont_opname [make_param (Var v)]) (**)
                  [mk_simple_bterm t;
                   mk_simple_bterm (mk_xlist_term (List.map make_cont conts));
                   mk_simple_bterm (mk_xlist_term terms)]
         else
            t
      in

      (* Get the display form *)
      let items, df =
         match lookup t with
            Some idf -> idf
          | None -> raise Not_found
      in
      let name = df.df_name in

      (* Get precedence of this display form, and whether it should be parenthesized *)
      let cprec = df.df_precedence in
      let cprec, parenflag =
         if cprec = inherit_prec then
            begin
               if !debug_dform then
                  eprintf "Dform %s: inherit_prec%t" name eflush;
               pprec, false
            end
         else if eq = NOParens then
            begin
               if !debug_dform then
                  eprintf "Dform %s: NOParens%t" name eflush;
               cprec, false
            end
         else if df.df_parens then
            let parens =
               match get_prec pprec df.df_precedence with
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
                     true
            in
               cprec, parens
         else
            cprec, false
      in
         if parenflag then
            format_string buf "(";

         begin
            try
               match df.df_printer with
                  DFPrinter f ->
                     let entry =
                        { dform_term = t;
                          dform_items = items;
                          dform_printer = print_term cprec;
                          dform_buffer = buf;
                          dform_state = base
                        }
                     in
                        if !debug_dform then
                           eprintf "Dform fun %s: %s%t" name (short_string_of_term t) eflush;
                        f entry
                | DFExpansion r ->
                     begin
                        match apply_rewrite r empty_args t [] with
                           [t] ->
                              if !debug_dform then
                                 eprintf "Dform %s%t" name eflush;
                              print_entry cprec buf eq t
                         | _ ->
                              raise (Invalid_argument ("Dform.format_term"))
                     end
            with
               exn when (match exn with Invalid_argument _ -> false | _ -> true) ->
                  raise (Invalid_argument ("Dform " ^ name ^ " raised an exception when trying to print "
                                           ^ short_string_of_term t ^ ": " ^ Printexc.to_string exn))
         end;

         if parenflag then
            format_string buf ")"

   (* If there is no template, use the standard printer *)
   and print_term pprec buf eq t =
      if !debug_dform then
         eprintf "Dform: %s%t" (short_string_of_term t) eflush;
      try print_term' pprec buf eq t with
         Not_found ->
            if !debug_dform then
               eprintf "Default display form: %s%t" (short_string_of_term t) eflush;
            default_format_term buf shortener (print_term max_prec buf NOParens) t

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
         else if not (is_xnil_term t) then
            print_term pprec buf eq t
      in
         aux

   in
   fun buf t ->
      let save_debug = !debug_rewrite in
         if save_debug then debug_rewrite := false;
         try
            print_term max_prec buf NOParens t;
            if save_debug then debug_rewrite := true
         with
            exn ->
               if save_debug then debug_rewrite := true;
               raise exn

(************************************************************************
 * SIMPLIFIED PRINTERS                                                  *
 ************************************************************************)

let format_quoted_term base buf t =
   let t = display_term_of_term t in
      default_format_term buf null_shortener (fun t ->
            format_term base buf t) t

let format_term base buf t =
   format_term base buf (display_term_of_term t)

let print_term_fp base out term =
   let buf = new_buffer () in
      format_term base buf term;
      output_rbuffer out buf

let print_term base = print_term_fp base stdout

let prerr_term base = print_term_fp base stderr

let string_of_term base term =
   let buf = new_buffer () in
      format_term base buf term;
      print_text_string default_width buf

(* Terms *)
let format_bterm base buf =
   format_bterm' buf (format_term base buf)

let print_bterm_fp base out term =
   let buf = new_buffer () in
      format_bterm base buf term;
      output_rbuffer out buf

let print_bterm base = print_bterm_fp base stdout

let prerr_bterm base = print_bterm_fp base stderr

let string_of_bterm base term =
   let buf = new_buffer () in
      format_bterm base buf term;
      print_text_string default_width buf

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
      format_string buf ( " [\"" ^ l ^ "\"]" );
      format_hspace buf;
      format_mterm base buf t;
      format_popm buf;
      format_ezone buf

let print_mterm_fp base out mterm =
   let buf = new_buffer () in
      format_mterm base buf mterm;
      output_rbuffer out buf

let print_mterm base = print_mterm_fp base stdout

let prerr_mterm base = print_mterm_fp base stderr

let string_of_mterm base mterm =
   let buf = new_buffer () in
      format_mterm base buf mterm;
      print_text_string 80 buf

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
