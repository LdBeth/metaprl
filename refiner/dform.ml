(*
 * Display form handler.
 *
 *)

open Precedence
open Rformat
open Opname
open Term
open Term_util
open Term_table
open Rewrite
open Simple_print

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
   { dform_pattern : term;
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
   { df_precedence : precedence;
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
 * We use a special precedence to specify that a form should
 * inherit the precedence of its parent.
 *)
let inherit_prec = new_prec ()

(*
 * Display form installation.
 *)
let add_dform base { dform_pattern = t;
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
      insert base t { df_precedence = prec; df_printer = printer' }

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
         TableEntry (t, { df_precedence = pr; df_printer = p }) ->
            let options =
               if pr = max_prec then
                  []
               else
                  [DFormParens; DFormPrec pr]
            in
            let printer =
               match p with
                  DFExpansion e ->
                     DFormExpansion t
                     
                | DFPrinter p -> 
                     DFormPrinter p
            in
               DFormEntry { dform_pattern = t;
                 dform_options = options;
                 dform_print = printer
               }

       | TableTable t -> DFormBase t
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

(*
 * The "slot" term is special because it has a subterm.
 *)
let slot = function
   { dform_items = [RewriteString parens; RewriteTerm body];
     dform_printer = printer;
     dform_buffer = buf
   } ->
      let eq =
         match parens with
            "le" -> LEParens
          | "lt" -> LTParens
          | _ -> NOParens
      in
         printer buf eq body
 | { dform_items = [RewriteTerm body];
     dform_printer = printer;
     dform_buffer = buf
   } ->
      printer buf NOParens body
 | _ -> raise (Invalid_argument "slot")

(*
 * Install initial commands.
 *)
let null_base =
   let rec aux = function
      (name, params, f)::t ->
         let term = mk_term (mk_op (make_opname [name]) (List.map make_param params)) [] in
         let entry =
            { dform_pattern = term;
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
      { dform_pattern =
           mk_term (mk_op (make_opname ["slot"]) [make_param (MString "eq")])
              [mk_bterm [] (mk_var_term "v")];
        dform_options = [DFormInheritPrec];
        dform_print = DFormPrinter slot
      }
   in
   let slot_entry2 =
      { dform_pattern =
           mk_term (mk_op (make_opname ["slot"]) [])
              [mk_bterm [] (mk_var_term "v")];
        dform_options = [DFormInheritPrec];
        dform_print = DFormPrinter slot
      }
   in
   let base' = add_dform base slot_entry1 in
      add_dform base' slot_entry2

let is_null_dfbase = equal_tables null_base

(************************************************************************
 * FORMATTERS                                                           *
 ************************************************************************)

(* List of params *)
let rec format_paramlist buf = function
   [] -> ()
 | h::[] ->
      format_simple_param buf h
 | h::t ->
      format_simple_param buf h;
      format_char buf ',';
      format_space buf;
      format_paramlist buf t

(* Optional empty params *)
let format_params buf = function
   [] -> ()
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
   [] -> ()
 | [h] ->
      format_bterm' buf printer h
 | h::t ->
      format_bterm' buf printer h;
      format_char buf ';';
      format_space buf;
      format_btermlist buf printer t

(* Optional empty bterm list *)
and format_bterms buf printer = function
   [] -> ()
 | _::_ as bterms ->
      format_break buf "" "";
      format_char buf '{';
      format_pushm buf 0;
      format_btermlist buf printer bterms;
      format_char buf '}';
      format_popm buf

(*
 * Top level print function.
 *)
and format_term buf shortener printer term =
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
      let stack, items, { df_precedence = pr'; df_printer = printer } = lookup base t in
      let pr, parenflag =
         if pr' = inherit_prec then
            pprec, false
         else
            pr', (if eq = NOParens then
                     false
                  else
                     match get_prec pprec pr' with
                        NoRelation ->
                           true
                      | LTRelation ->
                           false
                      | EQRelation ->
                           eq = LEParens
                      | GTRelation ->
                           true)
      in
      let entry = { dform_term = t;
                    dform_stack = stack;
                    dform_items = items;
                    dform_printer = print_entry pr;
                    dform_buffer = buf
                  }
      in
      let printer' =
         match printer with
            DFPrinter f ->
               (function () -> f entry)
          | DFExpansion c ->
               (function () -> print_entry pr buf eq (make_contractum c stack))
      in
         if parenflag then
            begin
               format_string buf "(";
               printer' ();
               format_string buf ")"
            end
         else
            printer' ()

   (* If there is no template, use the standard printer *)
   and print_term pprec buf eq t =
      try print_term' pprec buf eq t with
         Not_found ->
            format_term buf shortener (print_term max_prec buf NOParens) t

   (* Print an entry in the list of terms being displayed *)
   and print_entry pprec buf eq =
      let rec aux t =
         if is_xcons_term t then
            let hd, tl = dest_xcons t in
               if is_xstring_term hd then
                  format_string buf (dest_xstring hd)
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
   function buf -> print_term max_prec buf NOParens

(************************************************************************
 * SIMPLIFIED PRINTERS                                                  *
 ************************************************************************)

                                                        (* Terms *)
let null_shortener _ = nil_opname

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
         format_string buf v;
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
