(*
 * Display form handler.
 *
 *)

open Printf
open Debug

open Precedence
open Rformat
open Opname
open Refiner.Refiner.Term
open Refiner.Refiner.TermType
open Refiner.Refiner.TermMan
open Refiner.Refiner.TermMeta
open Refiner.Refiner.Rewrite
open Term_table
open Simple_print

(*
 * Show loading of the file.
 *)
let _ =
   if !debug_load then
      eprintf "Loading Dform%t" eflush

(************************************************************************
 * TYPES                                                                *
 ************************************************************************)

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
     dform_stack : rewrite_stack;
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
   (* Parens and precedences *)
   DFormInheritPrec
 | DFormPrec of precedence
 | DFormParens

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
   DFExpansion of rewrite_contractum
 | DFPrinter of (dform_printer_info -> unit)

type dform_item =
   { df_name : string;
     df_precedence : precedence;
     df_printer : df_printer
   }

type dform_base = dform_item term_table

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
let add_dform base { dform_name = name;
                     dform_pattern = t;
                     dform_options = options;
                     dform_print = printer
                   } =
   (* The options all have to do with precedence right now *)
   let rec process_options (prec, parens) = function
      [] ->
         if parens then
            prec
         else
            max_prec
      | h::t ->
          match h with
              DFormInheritPrec ->
                 inherit_prec
            | DFormPrec p ->
                 process_options (p, true) t
            | DFormParens ->
                 process_options (prec, true) t
   in
   let prec = process_options (min_prec, false) options in
   let printer' =
      match printer with
         DFormExpansion e ->
            let redex = compile_redex [||] t in
            let contractum = compile_contractum redex e in
               DFExpansion contractum
       | DFormPrinter f ->
            DFPrinter f
   in
      insert base t { df_name = name; df_precedence = prec; df_printer = printer' }

(*
 * Join two bases.
 *)
let join_dforms = join_tables

(*
 * Destruct a base.
 *)
let equal_dfbases = equal_tables

let dest_dfbase base =
   let info, base' = dest_table base in
   let info' =
      match info with
         TableEntry (t, { df_name = name; df_precedence = pr; df_printer = p }) ->
            let options =
               if pr = max_prec then
                  []
               else
                  [DFormParens; DFormPrec pr]
            in
            let printer =
               match p with
                  DFExpansion e ->
                     (* BUG: this is not right, but hard to construct the right term *)
                     DFormExpansion t

                | DFPrinter p ->
                     DFormPrinter p
            in
               DFormEntry { dform_name = name;
                            dform_pattern = t;
                            dform_options = options;
                            dform_print = printer
               }

       | TableTable t ->
            DFormBase t
   in
      info', base'

(*
 * Commands in initial base.
 *)
let lzone { dform_buffer = buf } =
   format_lzone buf

let hzone { dform_buffer = buf } =
   format_hzone buf

let szone { dform_buffer = buf } =
   format_szone buf

let ezone { dform_buffer = buf } =
   format_ezone buf

let break = function
   { dform_items = [RewriteString yes; RewriteString no]; dform_buffer = buf } ->
      format_break buf yes no
 | _ -> raise (Invalid_argument "Dform.break")

let sbreak = function
   { dform_items = [RewriteString yes; RewriteString no]; dform_buffer = buf } ->
      format_sbreak buf yes no
 | _ -> raise (Invalid_argument "Dform.sbreak")

let space { dform_buffer = buf } =
   format_space buf

let hspace { dform_buffer = buf } =
   format_hspace buf

let newline { dform_buffer = buf } =
   format_newline buf

let pushm = function
   { dform_items = [RewriteInt i]; dform_buffer = buf } ->
      format_pushm buf i
 | { dform_items = []; dform_buffer = buf } ->
      format_pushm buf 0
 | _ -> raise (Invalid_argument "Dform.pushm")

let popm { dform_buffer = buf } =
   format_popm buf

let pushfont _ =
   ()

let popfont _ =
   ()

(*
 * Install the commands.
 *)
let init_list =
   ["sbreak", [MString "y"; MString "n"], sbreak;
    "break", [MString "y"; MString "n"], break;
    "space", [], space;
    "hspace", [], hspace;
    "newline", [], newline;
    "lzone", [], lzone;
    "szone", [], szone;
    "hzone", [], hzone;
    "ezone", [], ezone;
    "pushm", [MNumber "i"], pushm;
    "pushm", [], pushm;
    "popm", [], popm;
    "pushfont", [MString "plain"], pushfont;
    "popfont", [], popfont]

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
      format_break buf "" "";
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
      format_break buf "" "";
      format_char buf '{';
      format_pushm buf 0;
      format_btermlist buf printer bterms;
      format_char buf '}';
      format_popm buf

(*
 * This is the default top level print function.
 * Check for variables.
 *)
and format_term buf shortener printer term =
   if is_so_var_term term then
      format_simple_term buf term
   else
      (* Standard term *)
      let { term_op = op; term_terms = bterms } = dest_term term in
      let { op_name = name; op_params = params } = dest_op op in
         format_szone buf;
         format_pushm buf 4;
         begin
            match dest_opname name with
               h::_ ->
                  if shortener h == name then
                     format_string buf h
                  else
                     format_quoted_string buf (string_of_opname name)
             | [] ->
                  raise (Invalid_argument "DForm.format_term")
         end;
         format_params buf params;
         format_bterms buf printer bterms;
         format_popm buf;
         format_ezone buf

(************************************************************************
 * PRINTING                                                             *
 ************************************************************************)

(*
 * Print a term to a buffer.
 *)
let format_short_term base shortener =
   (* Print a single term, ignoring lookup errors *)
   let rec print_term' pprec buf eq t =
      (* Check for a display form entry *)
      let stack, items, { df_name = name; df_precedence = pr'; df_printer = printer } = lookup "format_short_term" base t in
      let pr, parenflag =
         if pr' = inherit_prec then
            begin
               if !debug_dform then
                  eprintf "Dform %s: inherit_prec%t" name eflush;
               pprec, false
            end
         else
            pr', (if eq = NOParens then
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
         begin
            match printer with
               DFPrinter f ->
                  let entry =
                     { dform_term = t;
                       dform_stack = stack;
                       dform_items = items;
                       dform_printer = print_term pr;
                       dform_buffer = buf
                     }
                  in
                     if !debug_dform then
                        eprintf "Dform fun %s: %s%t" name (string_of_term t) eflush;
                     f entry
             | DFExpansion c ->
                  let t = make_contractum c stack in
                     if !debug_dform then
                        eprintf "Dform %s%t" name eflush;
                     print_entry pr buf eq t
         end;
         if parenflag then
            format_string buf ")"

   (* If there is no template, use the standard printer *)
   and print_term pprec buf eq t =
      if !debug_dform then
         eprintf "Dform: %s%t" (string_of_term t) eflush;
      try print_term' pprec buf eq t with
         Not_found ->
            if !debug_dform then
               let rec format t =
                  format_term buf shortener format t
               in
                  eprintf "Default display form: %s%t" (string_of_term t) eflush;
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
            eprintf "Dform.slot: %s%t" (string_of_term body) eflush;
         printer buf LTParens body
    | [RewriteString s] ->
         if !debug_dform then
            eprintf "Dform.slot: %s%t" s eflush;
         format_string buf s
    | _ ->
         raise (Invalid_argument "slot")

(*
 * Install initial commands.
 *)
let null_base =
   let rec aux = function
      (name, params, f)::t ->
         let term = mk_term (mk_op (make_opname [name]) (List.map make_param params)) [] in
         let entry =
            { dform_name = name;
              dform_pattern = term;
              dform_options = [DFormInheritPrec];
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
        dform_options = [DFormInheritPrec];
        dform_print = DFormPrinter slot
      }
   in
   let slot_entry2 =
      { dform_name = "slot_entry2";
        dform_pattern =
           mk_term (mk_op slot_opname [])
              [mk_bterm [] (mk_var_term "v")];
        dform_options = [DFormInheritPrec];
        dform_print = DFormPrinter slot
      }
   in
   let slot_entry3 =
      { dform_name = "slot_entry3";
        dform_pattern = mk_term (mk_op slot_opname [make_param (MString "eq")]) [];
        dform_options = [];
        dform_print = DFormPrinter slot
      }
   in
   let base = add_dform base slot_entry1 in
   let base = add_dform base slot_entry2 in
   let base = add_dform base slot_entry3 in
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
let format_mterm base buf =
   let rec aux = function
      MetaTheorem t ->
         format_term base buf t
    | MetaImplies (a, b) ->
         format_szone buf;
         format_pushm buf 0;
         aux a;
         format_string buf " -->";
         format_hspace buf;
         aux b;
         format_popm buf;
         format_ezone buf
    | MetaFunction (v, a, b) ->
         format_szone buf;
         format_pushm buf 0;
         format_term base buf v;
         format_string buf " : ";
         aux a;
         format_string buf " -->";
         format_hspace buf;
         aux b;
         format_popm buf;
         format_ezone buf
    | MetaIff (a, b) ->
         format_szone buf;
         format_pushm buf 0;
         aux a;
         format_string buf " <-->";
         format_hspace buf;
         aux b;
         format_popm buf;
         format_ezone buf
   in
      aux

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

(*
 * $Log$
 * Revision 1.5  1998/07/02 18:35:38  jyh
 * Refiner modules now raise RefineError exceptions directly.
 * Modules in this revision have two versions: one that raises
 * verbose exceptions, and another that uses a generic exception.
 *
 * Revision 1.4  1998/06/15 22:32:32  jyh
 * Added CZF.
 *
 * Revision 1.3  1998/06/12 18:36:32  jyh
 * Working factorial proof.
 *
 * Revision 1.2  1998/06/01 13:54:49  jyh
 * Proving twice one is two.
 *
 * Revision 1.1  1998/05/28 15:00:42  jyh
 * Partitioned refiner into subdirectories.
 *
 * Revision 1.12  1998/05/27 15:13:42  jyh
 * Functorized the refiner over the Term module.
 *
 * Revision 1.11  1998/05/04 13:01:16  jyh
 * Ocaml display without let rec.
 *
 * Revision 1.10  1998/05/01 18:43:31  jyh
 * Added raw display.
 *
 * Revision 1.9  1998/05/01 14:59:33  jyh
 * Updating display forms.
 *
 * Revision 1.8  1998/04/30 14:20:23  jyh
 * Updating term_table.
 *
 * Revision 1.7  1998/04/29 20:53:32  jyh
 * Initial working display forms.
 *
 * Revision 1.6  1998/04/29 14:48:14  jyh
 * Added ocaml_sos.
 *
 * Revision 1.5  1998/04/28 18:30:35  jyh
 * ls() works, adding display.
 *
 * Revision 1.4  1998/04/24 02:42:35  jyh
 * Added more extensive debugging capabilities.
 *
 * Revision 1.3  1998/02/21 20:58:16  jyh
 * Two phase parse/extract.
 *
 * Revision 1.2  1998/02/18 18:46:52  jyh
 * Initial ocaml semantics.
 *
 * Revision 1.1  1997/04/28 15:51:16  jyh
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
 * Revision 1.4  1996/09/02 19:42:41  jyh
 * Semi working package management.
 *
 * Revision 1.3  1996/05/21 02:13:36  jyh
 * This is a semi-working version before Wisconsin vacation.
 *
 * Revision 1.2  1996/04/11 13:29:16  jyh
 * This is the final version with the old syntax for terms.
 *
 * Revision 1.1  1996/04/07 18:27:03  jyh
 * Intermediate checking while updating dform commands.
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
