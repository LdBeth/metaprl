(*
 * Pretty printer for terms.
 *
 *)

open Printf
open Debug

open Rformat
open Opname
open Refiner.Refiner.Term
open Refiner.Refiner.TermType
open Refiner.Refiner.TermAddr
open Refiner.Refiner.TermMeta

(*
 * Show the file loading.
 *)
let _ =
   if !debug_load then
      eprintf "Loading Simple_print%t" eflush

let max_column = 120

(************************************************************************
 * PRINTERS                                                             *
 ************************************************************************)

(* Level expression *)
let format_level_exp buf l =
   if !debug_simple_print then
      eprintf "Simple_print.format_level_exp%t" eflush;
   let rec format_quotes = function
      0 -> ()
    | i -> format_char buf '\''; format_quotes (i - 1)
   in
   let format_var lv =
      match dest_level_var lv with
	 { le_var = v; le_offset = o } ->
	    if o < 3 then begin
	       format_quoted_string buf v;
	       format_quotes o
	    end
	    else begin
	       format_quoted_string buf v;
	       format_char buf ' ';
	       format_int buf o
	    end
   in
      match dest_level l with
	 { le_const = 0; le_vars = [v] } ->
	    (match dest_level_var v with
		{ le_var = v; le_offset = o } ->
		   if o < 3 then begin
		      format_quoted_string buf v;
		      format_quotes o
		   end
		   else begin
		      format_string buf "{";
		      format_quoted_string buf v;
		      format_int buf o;
		      format_string buf "}"
		   end)

       | { le_const = c; le_vars = vars } ->
	    let rec maxaux = function
	       [] -> 0
	     | h::t ->
		  match dest_level_var h with
		     { le_var = _; le_offset = i } -> max i (maxaux t)
	    in
	    let maxoff = maxaux vars in
	    let rec format_vars = function
	       [] -> ()
	     | [h] -> format_var h
	     | h::t ->
		  format_var h;
		  format_string buf " | ";
		  format_vars t
	    in
	       format_string buf "{";
	       if maxoff < c then begin
		  format_int buf c;
		  format_string buf " | "
	       end;
	       format_vars vars;
	       format_string buf "}"

(*
 * Operator name.
 *)
let string_of_opname opname =
   if !debug_simple_print then
      eprintf "Simple_print.string_of_opname%t" eflush;
   let rec aux v = function
      [] -> v
    | str::opname' ->
         let str' =
            if v = "" then
               str
            else
               str ^ "!" ^ v
         in
            aux str' opname'
   in
      aux "" (dest_opname opname)

(* General parameter *)
let rec format_param buf p =
   if !debug_simple_print then
      eprintf "Simple_print.format_param%t" eflush;
   match dest_param p with
      Number n -> format_num buf n; format_string buf ":n"
    | String s -> format_quoted_string buf s; format_string buf ":s"
    | Token t -> format_quoted_string buf t; format_string buf ":t"
    | Level l -> format_level_exp buf l; format_string buf ":l"
    | Var v -> format_quoted_string buf v; format_string buf ":v"
    | MNumber v -> format_char buf '@'; format_quoted_string buf v; format_string buf ":n"
    | MString v -> format_char buf '@'; format_quoted_string buf v; format_string buf ":s"
    | MToken v -> format_char buf '@'; format_quoted_string buf v; format_string buf ":t"
    | MLevel v -> format_char buf '@'; format_quoted_string buf v; format_string buf ":l"
    | MVar v -> format_char buf '@'; format_quoted_string buf v; format_string buf ":v"
    | MSum (a, b) -> format_param buf a; format_string buf " + "; format_param buf b; format_string buf ":n"
    | MDiff (a, b) -> format_param buf a; format_string buf " - "; format_param buf b; format_string buf ":n"
    | MProduct (a, b) -> format_param buf a; format_string buf " * "; format_param buf b; format_string buf ":n"
    | MQuotient(a, b) -> format_param buf a; format_string buf " / "; format_param buf b; format_string buf ":n"
    | MRem (a, b) -> format_param buf a; format_string buf " % "; format_param buf b; format_string buf ":n"
    | MLessThan (a, b) -> format_param buf a; format_string buf " < "; format_param buf b; format_string buf ":n"
    | MEqual (a, b) -> format_param buf a; format_string buf " = "; format_param buf b; format_string buf ":n"
    | MNotEqual (a, b) -> format_param buf a; format_string buf " <> "; format_param buf b; format_string buf ":n"
    | ObId a -> format_string buf "<object-id>"
    | ParamList l ->
         let rec format = function
            [h] ->
               format_param buf h
          | h::t ->
               format_param buf h;
               format_string buf "; ";
               format t
          | [] ->
               ()
         in
            format_string buf "[";
            format l;
            format_string buf "]"

(* List of params *)
let rec format_paramlist buf = function
   [] -> ()
 | h::[] ->
      format_param buf h
 | h::t ->
      format_param buf h;
      format_char buf ',';
      format_space buf;
      format_paramlist buf t

(* Optional empty params *)
let format_params buf = function
   [] -> ()
 | _::_ as params ->
      (* format_space buf; *)
      format_char buf '[';
      format_pushm buf 1;
      format_paramlist buf params;
      format_char buf ']';
      format_popm buf

(* Print a single bterm *)
let rec format_bterm buf bterm =
   if !debug_simple_print then
      eprintf "Simple_print.format_bterm%t" eflush;
   match dest_bterm bterm with
      { bvars = []; bterm = term } ->
         format_term buf term
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
            format_term buf term

(* Nonempty list *)
and format_btermlist buf = function
   [] -> ()
 | [h] ->
      format_bterm buf h
 | h::t ->
      format_bterm buf h;
      format_char buf ';';
      format_space buf;
      format_btermlist buf t

(* Optional empty bterm list *)
and format_bterms buf = function
   [] -> ()
 | _::_ as bterms ->
      (* format_space buf; *)
      format_char buf '{';
                        format_pushm buf 1;
                        format_btermlist buf bterms;
                        format_char buf '}';
      format_popm buf

(*
 * Top level print function.
 *)
and format_term buf term =
   if !debug_simple_print then
      eprintf "Simple_print.format_term%t" eflush;
   if is_so_var_term term then
      let _ =
         if !debug_simple_print then
            eprintf "Simple_print.format_term: got a variable%t" eflush
      in
      let v, subterms = dest_so_var term in
      let rec format_termlist = function
         [] -> ()
       | [h] -> format_term buf h
       | h::t ->
            format_term buf h;
            format_char buf ';';
            format_space buf;
            format_termlist t
      in
      let format_terms = function
         [] -> ()
       | _::_ as subterms ->
            format_string buf "[";
            format_pushm buf 0;
            format_termlist subterms;
            format_string buf "]";
            format_popm buf
      in
         if !debug_simple_print then
            eprintf "Simple_print.format_term: var: %s%t" v eflush;
         if subterms = [] then
            format_char buf '\'';
         format_quoted_string buf v;
         if !debug_simple_print then
            eprintf "Simple_print.format_terms%t" eflush;
         format_terms subterms

   else
      (* Standard term *)
      let _ =
         if !debug_simple_print then
            eprintf "Simple_print.format_term: regular term%t" eflush
      in
      let { term_op = op; term_terms = bterms } = dest_term term in
      let { op_name = name; op_params = params } = dest_op op in
         if !debug_simple_print then
            eprintf "Simple_print.format_term: destructed term%t" eflush;
         format_pushm buf 4;
         format_quoted_string buf (string_of_opname name);
         format_params buf params;
         format_bterms buf bterms;
         format_popm buf

(*
 * List of terms.
 *)
and format_termlist buf l =
   let rec aux = function
      [] -> ()
    | [h] -> format_term buf h
    | h::t ->
         format_term buf h;
         format_string buf ";";
         format_space buf;
         aux t
   in
      format_pushm buf 1;
      format_string buf "[";
      aux l;
      format_string buf "]";
      format_popm buf

(*
 * MetaTerms.
 *)
let format_mterm buf =
   let rec aux = function
      MetaTheorem t ->
         format_term buf t
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
         format_term buf v;
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

(************************************************************************
 * INTERFACE                                                            *
 ************************************************************************)

(* Level_Exps *)
let format_simple_level_exp = format_level_exp

let print_simple_level_exp_fp out p =
   let buf = new_buffer () in
      format_level_exp buf p;
      print_to_channel max_column buf out

let print_simple_level_exp = print_simple_level_exp_fp stdout

let prerr_simple_level_exp = print_simple_level_exp_fp stderr

let string_of_level_exp p =
   let buf = new_buffer () in
      format_level_exp buf p;
      print_to_string max_column buf

(* Params *)
let format_simple_param = format_param

let print_simple_param_fp out p =
   let buf = new_buffer () in
      format_param buf p;
      print_to_channel max_column buf out

let print_simple_param = print_simple_param_fp stdout

let prerr_simple_param = print_simple_param_fp stderr

let string_of_param p =
   let buf = new_buffer () in
      format_param buf p;
      print_to_string max_column buf

(* Terms *)
let format_simple_term = format_term

let print_simple_term_fp out term =
   let buf = new_buffer () in
      format_term buf term;
      print_to_channel max_column buf out

let print_simple_term = print_simple_term_fp stdout

let prerr_simple_term = print_simple_term_fp stderr

let string_of_term term =
   let buf = new_buffer () in
      format_term buf term;
      print_to_string max_column buf

(* Terms *)
let format_simple_bterm buf = format_bterm buf

let print_simple_bterm_fp out term =
   let buf = new_buffer () in
      format_bterm buf term;
      print_to_channel max_column buf out

let print_simple_bterm = print_simple_bterm_fp stdout

let prerr_simple_bterm = print_simple_bterm_fp stderr

let string_of_bterm term =
   let buf = new_buffer () in
      format_bterm buf term;
      print_to_string max_column buf

(*
 * MetaTerms.
 *)
let format_simple_mterm = format_mterm

let print_simple_mterm_fp out mterm =
   let buf = new_buffer () in
      format_mterm buf mterm;
      print_to_channel max_column buf out

let print_simple_mterm = print_simple_mterm_fp stdout

let prerr_simple_mterm = print_simple_mterm_fp stderr

let string_of_mterm mterm =
   let buf = new_buffer () in
      format_mterm buf mterm;
      print_to_string max_column buf

(*
 * Addresses.
 *)
let print_simple_address_fp out address =
   output_string out (string_of_address address)

let print_simple_address = print_simple_address_fp stdout
let prerr_simple_address = print_simple_address_fp stderr

(*
 * Install simple printer as default printer.
 *)
let _ = install_debug_printer print_simple_term_fp

(*
 * $Log$
 * Revision 1.3  1998/07/02 18:35:42  jyh
 * Refiner modules now raise RefineError exceptions directly.
 * Modules in this revision have two versions: one that raises
 * verbose exceptions, and another that uses a generic exception.
 *
 * Revision 1.2  1998/06/22 19:45:42  jyh
 * Rewriting in contexts.  This required a change in addressing,
 * and the body of the context is the _last_ subterm, not the first.
 *
 * Revision 1.1  1998/05/28 15:01:13  jyh
 * Partitioned refiner into subdirectories.
 *
 * Revision 1.7  1998/05/27 15:14:12  jyh
 * Functorized the refiner over the Term module.
 *
 * Revision 1.6  1998/04/24 02:42:56  jyh
 * Added more extensive debugging capabilities.
 *
 * Revision 1.5  1998/04/09 18:26:00  jyh
 * Working compiler once again.
 *
 * Revision 1.4  1998/03/20 22:16:20  eli
 * Eli: Changed integer parameters to Num.num's.
 *
 * Revision 1.3  1998/02/21 20:58:19  jyh
 * Two phase parse/extract.
 *
 * Revision 1.2  1997/09/12 17:21:44  jyh
 * Added MLast <-> term conversion.
 * Splitting filter_parse into two phases:
 *    1. Compile into Filter_summary
 *    2. Compile Filter_summary into code.
 *
 * Revision 1.1  1997/04/28 15:51:38  jyh
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
 * Revision 1.14  1996/05/21 02:14:17  jyh
 * This is a semi-working version before Wisconsin vacation.
 *
 * Revision 1.13  1996/04/07 18:24:54  jyh
 * This is an intermediate commit while adjusting the dforms.
 * We intend that dform printers just return a list of terms.
 *
 * Revision 1.12  1996/03/30 01:37:55  jyh
 * Initial version of ITT.
 *
 * Revision 1.11  1996/03/25 20:50:59  jyh
 * Intermediate commit while modifying grammer.  Restricting
 * ML hooks to condition terms.
 *
 * Revision 1.10  1996/03/08 15:40:57  jyh
 * This version works for most constructs except for ML rewrites.
 * The next step will be to break apart the rewriter so that
 * redices and contracta can be compiled separately.
 *
 * Revision 1.9  1996/02/25 15:16:22  jyh
 * This is a partial checkin as filterModule is being developed.
 * After the meta-logical framework is developed, sequent.* will go away.
 *
 * Revision 1.8  1996/02/19 18:47:09  jyh
 * Updating format.prl
 *
 * Revision 1.7  1996/02/18 23:32:37  jyh
 * Changin Format module to more Nuprl-like format.
 *
 * Revision 1.6  1996/02/14 03:51:56  jyh
 * This is a version common to Caml-Light and Caml-Special-Light.
 *
 * Revision 1.5  1996/02/13 21:33:09  jyh
 * This is an intermediate checkin while matching is being added to the rewriter.
 *
 * Revision 1.4  1996/02/07 23:41:49  jyh
 * First working version in CamlSpecialLight.
 *
 * Revision 1.3  1996/01/31 20:02:57  jyh
 * Generalizing rewriter to work on Sequents.
 *
 * Revision 1.2  1996/01/26 20:15:13  jyh
 * This version has a complete rewriter using the simple term structure.
 * Next implement sequents and refinement.
 *
 * Revision 1.1  1995/12/06 16:43:19  jyh
 * This is an ML version of a term rewriting system.
 * This checkin is partial, and provides a rewriter on
 * regular terms.
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner.run"
 * End:
 * -*-
 *)

