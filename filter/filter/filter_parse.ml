(*
 * Filter an ML file.
 *
 * The grammar of OCaml is extended to include MetaPRL commands.
 * This file contains all of the extensions.
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
open Pcaml

open Mp_debug
open Precedence
open Simple_print.SimplePrint
open Mp_resource
open File_base_type
open Term_shape_sig

open Refiner_io
open Refiner.Refiner
open Term
open TermOp
open TermType
open TermMan
open TermMeta
open Rewrite
open RefineError

open Infix
open Free_vars
open Term_grammar
open Filter_grammar
open Filter_type
open Filter_util
open Filter_ast
open Filter_summary
open Filter_summary_type
open Filter_summary_util
open Filter_cache
open Filter_prog
open Filter_magic

(*
 * Show loading of the file.
 *)
let _ =
   show_loading "Loading Filter_parse%t"

let debug_filter_parse =
   create_debug (**)
      { debug_name = "filter_parse";
        debug_description = "display compiling operations";
        debug_value = false
      }

let debug_dform =
   create_debug (**)
      { debug_name = "dform";
        debug_description = "show display form formatting";
        debug_value = false
      }

let debug_spell =
   create_debug (**)
      { debug_name = "spell";
        debug_description = "check spelling";
        debug_value = false
      }

(************************************************************************
 * PATHS                                                                *
 ************************************************************************)

(*
 * Save the include path.
 *)
let include_path = ref ["."]

let set_include_path path =
   include_path := path

(*
 * Spelling.
 *)
let misspelled = ref []

let init () =
   if !debug_spell then
      Filter_spell.init ()

let close () =
   let rec print col word = function
      h :: t ->
         if h = word then
            print col word t
         else
            let len = String.length h in
            let col =
               if col + len >= 80 then
                  begin
                     eprintf "\n\t%s" h;
                     len + 9
                  end
               else
                  begin
                     eprintf " %s" h;
                     col + len + 1
                  end
            in
               print col h t
    | [] ->
         ()
   in
   let l = Sort.list (<) !misspelled in
      misspelled := [];
      if l <> [] then
         begin
            eprintf "The following words may be misspelled:";
            print 80 "" l;
            eflush stderr;
            raise (Failure "spelling")
         end

(************************************************************************
 * TERM GRAMMAR                                                         *
 ************************************************************************)

(*
 * This is a hack!
 * We have to get around the type system somehow.
 * The FilterCache modules have different type for
 * signatures and implementations, but their mk_opname
 * functions will have the same type.  However, at the
 * time that the TermGrammar is defined, we don't know if this
 * is a signature or implementation.  So instead we leave a
 * reference to the mk_opname.  The refence is set by the get_proc
 * method, below, which knows if this is an implementation or
 * interface.
 *)
let mk_opname_ref =
   ref ((fun _ _ _ -> raise (Failure "Filter_parse.mk_opname is unititialized"))
      : opname_fun)

let mk_opname loc l p a =
   try !mk_opname_ref l p a with
      exn ->
         Stdpp.raise_with_loc loc exn

(*
 * Base term grammar.
 *)
module TermGrammarBefore : TermGrammarSig =
struct
   let mk_opname = mk_opname

   (*
    * Term grammar.
    *)
   let gram = Pcaml.gram
   let term_eoi = Grammar.Entry.create gram "term"
   let term = Grammar.Entry.create gram "term"
   let quote_term = Grammar.Entry.create gram "quote_term"
   let mterm = Grammar.Entry.create gram "mterm"
   let bmterm = Grammar.Entry.create gram "mterm"
   let singleterm = Grammar.Entry.create gram "singleterm"
   let bound_term = Grammar.Entry.create gram "bound_term"
   let xdform = Grammar.Entry.create gram "xdform"
end

(*
 * Extended term grammar.
 *)
module TermGrammar = MakeTermGrammar (TermGrammarBefore)
open TermGrammar

(************************************************************************
 * QUOTATIONS                                                           *
 ************************************************************************)

(*
 * String -> string translator.
 *)
let parse_term s =
   let cs = Stream.of_string s in
      Grammar.Entry.parse TermGrammar.term_eoi cs

let term_exp s =
   let cs = Stream.of_string s in
   let t = Grammar.Entry.parse TermGrammar.term_eoi cs in
      expr_of_term (0, 0) t

let term_patt s =
   raise (Failure "Filter_parse.term_patt: not implemented yet")

(*
 * Contractum terms.
 * This saves the term for future use in building a contractum,
 * and just returns a function to build the contractum.
 *
 * Contracta are kept in a global list, and are collected
 * only when the mode is on.
 *)
let contracta = ref []
let contract_flag = ref false

let start_rewrite () =
   contract_flag := true

let end_rewrite () =
   let cons = List.rev !contracta in
      contract_flag := false;
      contracta := [];
      cons

let stack_id = "rewrite_stack"

let contractum_exp s =
   if !contract_flag then
      let cs = Stream.of_string s in
      let t = Grammar.Entry.parse TermGrammar.term_eoi cs in
      let index = List.length !contracta in
         contracta := t :: !contracta;
         expr_of_contractum (0, 0) index
   else
      Stdpp.raise_with_loc (0, String.length s) (Failure "not in a rewrite block")

let contractum_patt s =
   raise (Failure "Filter_parse.term_patt: not implemented yet")

(*
 * Documentation strings are converted to identifable ocaml code.
 *)
let string_exp s =
   let loc = 0, 0 in
      <:expr< $str:s$ >>

let string_patt s =
   let loc = 0, 0 in
      <:patt< $str: s$ >>

let _ = Quotation.add "term" (Quotation.ExAst (term_exp, term_patt))
let _ = Quotation.add "con" (Quotation.ExAst (contractum_exp, contractum_patt))
let _ = Quotation.add "string" (Quotation.ExAst (string_exp, string_patt))
let _ = Quotation.default := "term"

(************************************************************************
 * TERM HACKING                                                         *
 ************************************************************************)

(*
 * Collect the argument names, or make up wildcards.
 *)
let collect_anames args =
   let rec avar i = function
      [] -> []
    | h::t ->
         match h with
            { aname = Some v } ->
               v :: avar i t
          | { aname = None } ->
               mk_var_term (sprintf "_%d" i) :: avar (i + 1) t
   in
      avar 0 args

(*
 * There should be only one param, of [M]String type.
 * Get it.
 *)
let get_string_param loc t =
   let { term_op = op } = dest_term t in
      match dest_op op with
         { op_params = [param] } ->
            begin
               match dest_param param with
                  String s | MString s -> s
                | _ -> Stdpp.raise_with_loc loc (RefineError ("Filter_parse.get_string_param", TermMatchError (t, "param type")))
            end
       | { op_params = [] } ->
            Stdpp.raise_with_loc loc (RefineError ("Filter_parse.get_string_param", TermMatchError (t, "no params")))
       | _ ->
            Stdpp.raise_with_loc loc (RefineError ("Filter_parse.get_string_param", TermMatchError (t, "too many params")))

(*
 * Make a term with a string parameter.
 *)
let mk_string_param_term opname s terms =
   let param = make_param (String s) in
   let op = mk_op opname [param] in
   let bterms = List.map (fun t -> mk_bterm [] t) terms in
      mk_term op bterms

(*
 * Terms for representing comments.
 *)
let mk_comment_opname =
   let op = Opname.mk_opname "Comment" Opname.nil_opname in
      fun s -> Opname.mk_opname s op

let comment_white_op = mk_comment_opname "comment_white"
let comment_string_op = mk_comment_opname "comment_string"
let comment_block_op = mk_comment_opname "comment_block"
let comment_term_op = mk_comment_opname "comment_term"

(*
 * Parse a comment string.
 *)
type spelling =
   SpellOff
 | SpellOn
 | SpellAdd

let fake_arities =
   List.map ( fun _ -> 0)

let string_params =
   List.map ( fun _ -> ShapeString)

let parse_comment loc t =
   (*
    * Convert the result of the Comment_parse.
    *)
   let rec build_comment_term spelling = function
      Comment_parse.White ->
         mk_simple_term comment_white_op []
    | Comment_parse.String s ->
         if !debug_spell then
            begin
               match spelling with
                  SpellOff ->
                     ()
                | SpellAdd ->
                     Filter_spell.add s
                | SpellOn ->
                     if not (Filter_spell.check s) then
                        misspelled := s :: !misspelled
            end;
         mk_string_param_term comment_string_op s []
    | Comment_parse.Term (opname, params, args) ->
         let spelling =
            if !debug_spell then
               match opname with
                  ["spelling"] ->
                     SpellAdd
                | ["misspelled"]
                | ["math_misspelled"] ->
                     SpellOff
                | _ ->
                     spelling
            else
               spelling
         in
         let opname =
            mk_opname loc opname (string_params params) (fake_arities args)
         in let params = List.map (fun s -> make_param (String s)) params in
         let args = List.map (fun t -> mk_simple_bterm (build_term spelling t)) args in
         let op = mk_op opname params in
         let t = mk_term op args in
            mk_simple_term comment_term_op [t]
    | Comment_parse.Block items ->
         mk_simple_term comment_block_op [build_term spelling items]
    | Comment_parse.Quote (tag, s) ->
         if tag = "" then
            mk_simple_term comment_term_op [parse_term s]
         else
            mk_string_param_term comment_string_op s []

   and build_term spelling tl =
      mk_xlist_term (List.map (build_comment_term spelling) tl)
   in
   let s = dest_string_param t in
   let loc1, loc2 = loc in
   let items =
      try Comment_parse.parse s with
         Comment_parse.Parse_error (s, loc1', loc2') ->
            Stdpp.raise_with_loc (loc1 + loc1', loc1 + loc2') (ParseError s)
   in
      mk_simple_term comment_term_op [build_term SpellOn items]

(*
 * Wrap a code block with a binding variable.
 *)
let wrap_code loc v body =
   let v =
      match v with
         Some v -> dest_var v
       | None -> "_$goal"
   in
   let p = <:patt< $lid:v$ >> in
      <:expr< fun [ $list: [p, None, body]$ ] >>

(************************************************************************
 * GENERIC CONSTRUCTION                                                 *
 ************************************************************************)

(*
 * We may be able to do better sometime, but for now
 * we print the terms using the default display forms.
 *)
let print_exn f s (start, stop) =
   if !debug_filter_parse then
      eprintf "Filter_parse.%s (%d, %d)%t" s start stop eflush;
   Filter_exn.print Dform.null_base f ()

(*
 * Need some info about types and extraction.
 *)
module type FilterInfoSig =
sig
   type proof
   type expr
   type ctyp
   type item
   type sig_info
   type resource

   (*
    * This is only really used for interactive proofs.
    *)
   val copy_proof : proof -> proof -> proof

   (*
    * Extract the str_items.
    *)
   val extract :
      sig_info ->
      (term, meta_term, proof, resource, ctyp, expr, item) module_info ->
      (module_path * string * ctyp resource_sig) list ->
      string -> (item * (int * int)) list
end

(*
 * Make the filter.
 *)
module MakeFilter (**)
   (Info : FilterInfoSig)
   (FilterCache : SummaryCacheSig
    with type sig_ctyp  = Info.ctyp
    with type str_proof = Info.proof
    with type str_expr  = Info.expr
    with type str_ctyp  = Info.ctyp
    with type str_item  = Info.item
    with type str_resource = Info.resource
    with type select    = select_type
    with type arg       = unit) =
struct
   (*
    * Processors include both the cache and the name of the module.
    *)
   type t =
      { cache : FilterCache.info;
        select : FilterCache.select;
        name : string
      }

   (*
    * When a module is inlined, add the resources and infixes.
    *)
   let inline_hook root_path cache (path, info) paths =
      (* Add all the infix words *)
      List.iter add_infix (get_infixes info);

      (* Add the path to the list of parents *)
      path :: paths

   (*
    * Include a parent.
    * This performs the following tasks:
    *    1. incorporates the parents:
    *       a. adds the resources
    *       b. adds the infix directives.
    *)
   let declare_parent proc loc path =
      (* Lots of errors can occur here *)
      let _, opens = FilterCache.inline_module proc.cache () path (inline_hook path) [] in
      let resources = FilterCache.sig_resources proc.cache path in
      begin if !debug_resource then
         let print_resources out resources =
            List.iter (fprintf out " %s") (List.map fst resources)
         in
            eprintf "Filter_parse.declare_parent: %s:%a%t" (string_of_path path) print_resources resources eflush
      end;
      let info =
         { parent_name = path;
           parent_opens = opens;
           parent_resources = resources
         }
      in
         FilterCache.add_command proc.cache (Parent info, loc)

   (*
    * Declare a term.
    * This defines a new opname,
    * stores the term in the cache,
    * and returns the term that was created.
    *
    * This command is used both for signature items
    * as well as structure items.
    *)
   let declare_term proc loc (s, params, bterms) =
      let opname' = Opname.mk_opname s (FilterCache.op_prefix proc.cache) in
      let t = mk_term (mk_op opname' params) bterms in
         FilterCache.update_opname proc.cache s t;
         FilterCache.add_command proc.cache (Opname { opname_name = s; opname_term = t }, loc);
         t

   (*
    * Define a rewrite in an interface.
    * Rewrites are somewhat redundant, since they can be defined as
    * axioms, but we use this special form to specifically indicate
    * a rewrite to create an efficient evaluator.  The format is:
    *    rewrite name [params...] : [cond1 -> ... -> condn] -> (redex <--> contractum)
    * The params are supplied terms, and the conditions are terms that
    * must be provable _in the current context_.  In a sequent calculus,
    * the current context would be the assumption list.
    *)
   let simple_rewrite proc name redex contractum pf res =
      (* Check that rewrite will succeed *)
      Refine.check_rewrite name [||] [] [] redex contractum;

      (* Construct the command *)
      Rewrite { rw_name = name;
                rw_redex = redex;
                rw_contractum = contractum;
                rw_proof = pf;
                rw_resources = res
      }

   let simple_input_form proc name redex contractum pf res =
      (* Check that rewrite will succeed *)
      Refine.check_rewrite name [||] [] [] redex contractum;

      (* Construct the command *)
      InputForm { rw_name = name;
                  rw_redex = redex;
                  rw_contractum = contractum;
                  rw_proof = pf;
                  rw_resources = res
      }

   let cond_rewrite proc name params args pf res =
      (* Print the type to the .mli file *)
      let cvars = context_vars args in
      let bvars = binding_vars args in
      let params' = extract_params cvars bvars params in
      let args', redex, contractum = unzip_rewrite name args in
         (* Check the rewrite *)
         Refine.check_rewrite (**)
            name
            (Array.of_list (collect_vars params'))
            (collect_non_vars params')
            args' redex contractum;

         (* Construct the command *)
         CondRewrite { crw_name = name;
                       crw_params = params';
                       crw_args = args';
                       crw_redex = redex;
                       crw_contractum = contractum;
                       crw_proof = pf;
                       crw_resources = res
         }

   (*
    * Compile the rewrite.
    *)
   let rewrite_command proc name params args pf res =
      match params, args with
         [], MetaIff (MetaTheorem redex, MetaTheorem contractum) ->
            (* This is a simple rewrite *)
            simple_rewrite proc name redex contractum pf res
       | _ ->
            (* Conditional rewrite *)
            cond_rewrite proc name params args pf res

   let input_form_command proc name params args pf res =
      match params, args with
         [], MetaIff (MetaTheorem redex, MetaTheorem contractum) ->
            (* This is a simple rewrite *)
            simple_input_form proc name redex contractum pf res
       | _ ->
            (* Conditional rewrite *)
            raise (RefineError ("input_form_command", StringError "conditional input forms are not allowed"))

   (*
    * Add the command and return the declaration.
    *)
   let declare_rewrite proc loc name params args pf res =
      try
         let cmd = rewrite_command proc name params args pf res in
            FilterCache.add_command proc.cache (cmd, loc)
      with exn ->
         Stdpp.raise_with_loc loc exn

   let declare_input_form proc loc name params args pf res =
      try
         let cmd = input_form_command proc name params args pf res in
            FilterCache.add_command proc.cache (cmd, loc)
      with exn ->
         Stdpp.raise_with_loc loc exn

   (*
    * Declare a term, and define a rewrite in one step.
    *)
   let define_term proc loc name redex contractum pf res =
      let redex' = declare_term proc loc redex in
         declare_rewrite proc loc name [] (MetaIff (MetaTheorem redex', MetaTheorem contractum)) pf res

   (*
    * Declare an axiom in an interface.  This has a similar flavor
    * as rewrites, but context args have to be extracted from the args.
    *)
   let simple_axiom proc name arg pf res =
      (* Check it *)
      Refine.check_axiom arg;

      (* Save it in the transcript *)
      Axiom { axiom_name = name; axiom_stmt = arg; axiom_proof = pf; axiom_resources = res }

   let rec print_terms out = function
      h::t ->
         eprintf "\t%s\n" (string_of_term h);
         print_terms out t
    | [] ->
         flush stderr

   let rec print_vterms out = function
      (labels, Some v, h)::t ->
         eprintf "\t%a %s. %s\n" print_string_list labels (string_of_term v) (string_of_term h);
         print_vterms out t
    | (labels, None, h)::t ->
         eprintf "\t%a %s\n" print_string_list labels (string_of_term h);
         print_vterms out t
    | [] ->
         flush stderr

   let print_non_vars out params =
      print_terms out (collect_non_vars params)

   let cond_axiom proc name params t pf res =
      (* Extract context names *)
      let _ =
         if !debug_grammar then
            eprintf "Conditional rule: %s%t" name eflush
      in
      let cvars = context_vars t in
      let bvars = binding_vars t in
      let params' = extract_params cvars bvars params in
         (* Do some checking on the rule *)
         if !debug_grammar then
            begin
               let t, result = unzip_mfunction t in
                  eprintf "Checking rule: %s\n" name;
                  eprintf "Non vars:\n%a" print_non_vars params';
                  eprintf "Args:\n%a --> %s\n" print_vterms t (string_of_term result)
            end;
         Refine.check_rule (**)
            name
            (Array.of_list (collect_cvars params'))
            (Array.of_list (collect_vars params'))
            (collect_non_vars params')
            (strip_mfunction t);
         if !debug_grammar then
            eprintf "Checked rule: %s%t" name eflush;

         (* If checking completes, add the rule *)
         Rule { rule_name = name;
                rule_params = params';
                rule_stmt = t;
                rule_proof = pf;
                rule_resources = res
         }

   let axiom_command proc name params t pf res =
      match params, t with
         [], MetaTheorem a ->
            simple_axiom proc name a pf res
       | _ ->
            cond_axiom proc name params t pf res

   let declare_axiom proc loc name args t pf res =
      try
         let cmd = axiom_command proc name args t pf res in
            FilterCache.add_command proc.cache (cmd, loc)
      with exn ->
         Stdpp.raise_with_loc loc exn


   (*
    * Infix directive.
    *)
   let declare_infix proc loc s =
      FilterCache.add_command proc.cache (Infix s, loc);
      add_infix s

   (*
    * Declare an ML term rewrite.
    * There is no definition.
    *)
   let declare_mlrewrite proc loc mlname args t def resources =
      let cvars = context_vars (MetaTheorem t) in
      let bvars = binding_vars (MetaTheorem t) in
      let params = extract_params cvars bvars args in
         FilterCache.add_command proc.cache (MLRewrite { mlterm_name = mlname;
                                                         mlterm_params = params;
                                                         mlterm_term = t;
                                                         mlterm_contracta = end_rewrite ();
                                                         mlterm_def = def;
                                                         mlterm_resources = resources
                                             }, loc)

   let declare_mlaxiom proc loc mlname args t def resources =
      let cvars = context_vars (MetaTheorem t) in
      let bvars = binding_vars (MetaTheorem t) in
      let params = extract_params cvars bvars args in
         FilterCache.add_command proc.cache (MLAxiom { mlterm_name = mlname;
                                                       mlterm_params = params;
                                                       mlterm_term = t;
                                                       mlterm_contracta = end_rewrite ();
                                                       mlterm_def = def;
                                                       mlterm_resources = resources
                                             }, loc)

   (*
    * Record a resource.
    *
    * type resource_name
    *)
   let declare_resource proc loc name r =
      FilterCache.add_resource proc.cache name r

   let define_resource proc loc name r =
      FilterCache.add_command proc.cache (Resource (name, r), loc)

   let improve_resource proc loc i =
      FilterCache.add_command proc.cache (Improve i, loc)

   (*
    * Extract the options and return the mode paired with
    * the list of string defining the forms.
    *)
   let get_dform_options proc loc options =
      let rec compile_options = function
         hd::tl ->
            begin
               let modes, except_modes, options = compile_options tl in
                  match Opname.dest_opname (opname_of_term hd) with
                     "parens" :: _
                   | "inherit" :: _ ->
                        modes, except_modes, DFormParens :: options
                   | "prec" :: _ ->
                        modes, except_modes, (DFormPrec (get_string_param loc hd)) :: options
                   | "internal" :: _ ->
                        modes, except_modes, DFormInternal :: options
                   | "mode" :: _ ->
                        (get_string_param loc hd)::modes, except_modes, options
                   | "except_mode" :: _ ->
                        modes, (get_string_param loc hd)::except_modes, options
                   | _ ->
                        eprintf "warning: unknown display form option %s%t" (string_of_term hd) eflush;
                        modes, except_modes, options
            end
       | [] ->
            [], [], []
      in
      match compile_options options with
         [], [], options -> AllModes, List.rev options
       | modes, [], options -> Modes modes, List.rev options
       | [], except_modes, options -> ExceptModes except_modes, List.rev options
       | _ -> Stdpp.raise_with_loc loc (Failure "Both \"mode\" and \"except_mode\" flags on the same display form")

   (*
    * Dform declaration.
    *)
   let declare_dform proc loc name options t =
      let modes, options' = get_dform_options proc loc options in
      let df =
         DForm { dform_name = name;
                 dform_modes = modes;
                 dform_options = options';
                 dform_redex = t;
                 dform_def = NoDForm
         }
      in
         FilterCache.add_command proc.cache (df, loc)

   (*
    * Define a display form expansion.
    *
    * create_dform dformer [modes]
    *    { dform_pattern = t;
    *      dform_options = [options];
    *      dform_print = DFormExpansion expansion
    *    }
    *)
   let define_dform proc loc name options t expansion =
      let modes, options' = get_dform_options proc loc options in
         if (!debug_dform) && (modes=AllModes) then
            eprintf "Warning: display form %s - no modes specified%t" name eflush;
         begin try
            let redex, _ = compile_redex Relaxed [||] t in
            ignore (compile_contractum Relaxed redex expansion)
         with
            exn ->
               Stdpp.raise_with_loc loc exn
         end;
         FilterCache.add_command proc.cache (DForm { dform_name = name;
                                                     dform_modes = modes;
                                                     dform_options = options';
                                                     dform_redex = t;
                                                     dform_def = TermDForm expansion
                                             }, loc)

   (*
    * An ml dterm is a display form that is computed in ML.
    *
    * Within the body, terms may expand to contracta.
    *)
   let define_ml_dform proc loc name options t printer buffer code =
      let modes, options' = get_dform_options proc loc options in
      if (!debug_dform) && (modes=AllModes) then eprintf "Warning: ML display form %s - no modes specified%t" name eflush;
      let ml_def =
         { dform_ml_printer = printer;
           dform_ml_buffer = buffer;
           dform_ml_contracta = end_rewrite ();
           dform_ml_code = code
         }
      in
      let info =
         { dform_name = name;
           dform_modes = modes;
           dform_options = options';
           dform_redex = t;
           dform_def = MLDForm ml_def
         }
      in
         FilterCache.add_command proc.cache (DForm info, loc)

   (*
    * Precedence declaration.
    *)
   let declare_prec proc loc s =
(*
      if FilterCache.find_prec proc.cache s then
         Stdpp.raise_with_loc loc (Failure (sprintf "prec '%s' already declared" s));
*)
      FilterCache.add_command proc.cache (Prec s, loc);
      FilterCache.add_prec proc.cache s

   (*
    * Precedence definition relation.
    *)
   let define_prec_rel proc loc s s' rel =
      if not (FilterCache.find_prec proc.cache s) then
         Stdpp.raise_with_loc loc (Failure (sprintf "prec '%s' not defined" s));
      if not (FilterCache.find_prec proc.cache s') then
         Stdpp.raise_with_loc loc (Failure (sprintf "prec '%s' not defined" s'));
      FilterCache.add_command proc.cache (PrecRel { prec_rel = rel;
                                                    prec_left = s;
                                                    prec_right = s'
                                          }, loc)

   (*
    * A toplevel declaration.
    *)
   let declare_topval proc loc item =
      FilterCache.add_command proc.cache (ToploopItem item, loc)

   (*
    * A toplevel structured comment is converted to a term.
    *)
   let declare_comment proc loc s =
      FilterCache.add_command proc.cache (Comment (mk_string_param_term comment_string_op s []), loc)

   (*
    * A magic block computes a hash value from the definitions
    * in the block.
    *)
   let define_magic_block proc loc name stmts =
      FilterCache.add_command proc.cache (MagicBlock { magic_name = name;
                                                       magic_code = stmts
                                          }, loc)

   (*
    * Processor.
    *)
   let proc_ref = ref None

   let get_proc loc =
      match !proc_ref with
         Some proc ->
            proc
       | None ->
            let select, module_name =
               let name = !Pcaml.input_file in
                  if Filename.check_suffix name ".ml" then
                     ImplementationType, Filename.chop_suffix name ".ml"
                  else if Filename.check_suffix name ".mli" then
                     InterfaceType, Filename.chop_suffix name ".mli"
                  else
                     Stdpp.raise_with_loc loc (Failure "Input is not a .ml or .mli file")
            in
            let cache = FilterCache.create !include_path in
            let info = FilterCache.create_cache cache module_name select InterfaceType in
            let proc = { cache = info;
                         select = select;
                         name = module_name
                       }
            in
               mk_opname_ref := FilterCache.mk_opname info;
               proc_ref := Some proc;
               proc

   (*
    * Our version of add_command.
    *)
   let add_command proc cmd =
      FilterCache.add_command proc.cache cmd

   (*
    * Save the summary.
    *)
   let save proc suffix =
      FilterCache.save proc.cache () suffix

   (*
    * Extract an item list.
    *)
   let extract sig_info proc =
      (Info.extract sig_info (FilterCache.info proc.cache) (**)
          (FilterCache.resources proc.cache)) (Filename.basename proc.name)

   (*
    * Check the implementation with its interface.
    *)
   let check proc alt_select =
      (* Check that implementation matches interface *)
      let sig_info = FilterCache.check proc.cache alt_select in
      let _ =
         (* Read the comments *)
         FilterCache.parse_comments proc.cache parse_comment;

         (* Also copy the proofs if they exist *)
         if proc.select = ImplementationType then
            let name = proc.name in
               if file_interactive (name ^ ".prlb") || Sys.file_exists (name ^ ".prla") then
                  begin
                     FilterCache.set_mode proc.cache InteractiveSummary;
                     FilterCache.copy_proofs proc.cache () Info.copy_proof
                  end
      in
         sig_info
end

(*
 * Use proof conversion module.
 *)
module Convert = Proof_convert.Convert

(*
 * Extractors.
 *)
module Extract = MakeExtract (Convert)

(*
 * Caches.
 *)
module Cache = MakeCaches (Convert)

(*
 * The two caches.
 *)
module SigFilterInfo =
struct
   type proof = unit
   type expr  = MLast.expr
   type ctyp  = MLast.ctyp
   type item  = MLast.sig_item
   type resource = MLast.ctyp resource_sig
   type sig_info = unit

   let copy_proof proof1 proof2 = proof1
   let extract = Extract.extract_sig
end

module StrFilterInfo =
struct
   type proof = Convert.cooked proof_type
   type expr  = MLast.expr
   type ctyp  = MLast.ctyp
   type item  = MLast.str_item
   type resource = MLast.expr
   type sig_info = (term, meta_term, unit, MLast.ctyp resource_sig, MLast.ctyp, MLast.expr, MLast.sig_item) module_info
   (*
    * Proof copying.
    *)
   let copy_proof proof1 proof2 =
      match proof1, proof2 with
         (Incomplete | Interactive _), Interactive _ ->
            proof2
       | _ ->
            proof1

   let extract = Extract.extract_str ()
end

module SigFilter = MakeFilter (SigFilterInfo) (Cache.SigFilterCache)
module StrFilter = MakeFilter (StrFilterInfo) (Cache.StrFilterCache)

(************************************************************************
 * DEFINITION COMMANDS                                                  *
 ************************************************************************)

(*
 * A primitive rule specifies the extract.
 *)
let define_rule proc loc name
    (params : term list)
    (mterm : meta_term)
    (extract : Convert.cooked proof_type)
    (res : MLast.expr resource_def) =
   try
      let cmd = StrFilter.axiom_command proc name params mterm extract res in
      StrFilter.add_command proc (cmd, loc)
   with exn ->
      Stdpp.raise_with_loc loc exn

let define_prim proc loc name params mterm extract =
   define_rule proc loc name params mterm (Primitive extract)

let define_thm proc loc name params mterm tac =
   define_rule proc loc name params mterm (Derived tac)

let define_int_thm proc loc name params mterm =
   define_rule proc loc name params mterm Incomplete

(************************************************************************
 * GRAMMAR EXTENSION                                                    *
 ************************************************************************)

(*
 * Add the infixes.
 *)
module Unit =
struct
end

module FGrammar = MakeFilterGrammar (Unit)

(*
 * Empty items.
 *)
let empty_sig_item loc =
   <:sig_item< declare $list:[]$ end >>

let empty_str_item loc =
   <:str_item< declare $list:[]$ end >>

(*
 * Extend the programming language.
 *)
let _ =
   Grammar.Unsafe.clear_entry interf;
   Grammar.Unsafe.clear_entry implem

let operator = Pa_o.operator_rparen

EXTEND
   GLOBAL: interf implem sig_item str_item expr;

   interf:
      [[ interf_opening; st = LIST0 interf_item; EOI ->
          let f () =
             let proc = SigFilter.get_proc loc in
             let id = Hashtbl.hash proc in
                SigFilter.add_command proc (Id id, (0, 0));
                SigFilter.save proc AnySuffix;
                SigFilter.extract () proc
          in
             print_exn f "interf" loc, false
       ]];

   interf_opening:
      [[ OPT "PRL_interface" ->
          let f () =
             SigFilter.get_proc loc
          in
             print_exn f "interf_opening" loc
       ]];

   interf_item:
      [[ s = sig_item; OPT ";;" ->
          let f () =
             if !debug_filter_parse then
                eprintf "Filter_parse.interf_item: adding item%t" eflush;
             begin
                match s with
                   <:sig_item< declare $list: []$ end >> ->
                   ()
                 | _ ->
                      SigFilter.add_command (SigFilter.get_proc loc) (SummaryItem s, loc)
             end;
             s, loc
           in
              print_exn f "interf_item" loc
       ]];

   implem:
      [[ implem_opening; st = LIST0 implem_item; EOI ->
          let f () =
             let proc = StrFilter.get_proc loc in
             let interf = StrFilter.check proc () InterfaceType in
                StrFilter.save proc AnySuffix;
                StrFilter.extract interf proc
          in
             print_exn f "implem" loc, false
       ]];

   implem_opening:
      [[ OPT "PRL_implementation" ->
         let f () =
            StrFilter.get_proc loc
         in
            print_exn f "implem_opening" loc
       ]];

   implem_item:
      [[ s = str_item; OPT ";;" ->
          let f () =
             begin
                match s with
                   <:str_item< declare $list: []$ end >> ->
                      ()
                 | _ ->
                      StrFilter.add_command (StrFilter.get_proc loc) (SummaryItem s, loc)
             end;
             s, loc
          in
             print_exn f "implem_item" loc
       ]];

   sig_item:
      [[ "extends"; path = mod_ident ->
          let f () =
             SigFilter.declare_parent (SigFilter.get_proc loc) loc path
          in
             print_exn f "extends" loc;
             empty_sig_item loc
        | "derive"; path = mod_ident ->
          let f () =
             SigFilter.declare_parent (SigFilter.get_proc loc) loc path
          in
             print_exn f "derive" loc;
             empty_sig_item loc
        | "declare"; t = quote_term ->
          let f () =
             ignore (SigFilter.declare_term (SigFilter.get_proc loc) loc t)
          in
             print_exn f "declare" loc;
             empty_sig_item loc
        | "define"; name = LIDENT; ":"; t = quote_term; "<-->"; def = term ->
           let f () =
             SigFilter.define_term (SigFilter.get_proc loc) loc name t def () []
           in
             print_exn f "define" loc;
             empty_sig_item loc
        | "rewrite"; name = LIDENT; args = optarglist; ":"; t = mterm ->
           let f () =
             SigFilter.declare_rewrite (SigFilter.get_proc loc) loc name args t () []
           in
             print_exn f "rewrite" loc;
             empty_sig_item loc
        | "ml_rw"; name = LIDENT; args = optarglist; ":"; t = term ->
           let f () =
             SigFilter.declare_mlrewrite (SigFilter.get_proc loc) loc name args t None []
           in
             print_exn f "ml_rw" loc;
             empty_sig_item loc
        | rule_keyword; name = LIDENT; args = optarglist; ":"; t = mterm ->
           let f () =
             SigFilter.declare_axiom (SigFilter.get_proc loc) loc name args t () []
           in
              print_exn f "rule" loc;
              empty_sig_item loc
        | mlrule_keyword; name = LIDENT; args = optarglist; ":"; t = term ->
           let f () =
              SigFilter.declare_mlaxiom (SigFilter.get_proc loc) loc name args t None []
           in
              print_exn f "mlrule_keyword" loc;
             empty_sig_item loc
        | "resource"; "("; input = ctyp; ","; output = ctyp; ")"; name = LIDENT ->
           let f () =
              let r = {
                 resource_input = input;
                 resource_output = output
              } in let proc = SigFilter.get_proc loc in
                 SigFilter.declare_resource proc loc name r;
                 SigFilter.define_resource proc loc name r
           in
              print_exn f "resource" loc;
              empty_sig_item loc
        | "dform"; name = LIDENT; ":"; options = df_options ->
           let f () =
              let options', t = options in
                 SigFilter.declare_dform (SigFilter.get_proc loc) loc name options' t;
           in
              print_exn f "dform" loc;
              empty_sig_item loc
        | "infix"; name = ident ->
           let f () =
              SigFilter.declare_infix (SigFilter.get_proc loc) loc name
           in
              print_exn f "infix" loc;
             empty_sig_item loc
        | "prec"; name = LIDENT ->
           let f () =
              SigFilter.declare_prec (SigFilter.get_proc loc) loc name
           in
              print_exn f "prec" loc;
              empty_sig_item loc
        | "topval"; name = LIDENT; ":"; t = ctyp ->
           let f () =
              SigFilter.declare_topval (SigFilter.get_proc loc) loc <:sig_item< value $name$ : $t$ >>
           in
              print_exn f "topval" loc;
              empty_sig_item loc
        | "topval"; "("; name = operator; ")"; ":"; t = ctyp ->
           let f () =
              SigFilter.declare_topval (SigFilter.get_proc loc) loc <:sig_item< value $name$ : $t$ >>
           in
              print_exn f "topval" loc;
              empty_sig_item loc
        | com = COMMENT ->
           let f () =
              StrFilter.declare_comment (StrFilter.get_proc loc) loc com
           in
              print_exn f "comment" loc;
              empty_sig_item loc
       ]];

   str_item:
      [[ "extends"; path = mod_ident ->
          let f () =
             StrFilter.declare_parent (StrFilter.get_proc loc) loc path
          in
             print_exn f "extends" loc;
             empty_str_item loc
        | "derive"; path = mod_ident ->
          let f () =
             StrFilter.declare_parent (StrFilter.get_proc loc) loc path
          in
             print_exn f "derive" loc;
             empty_str_item loc
        | "declare"; t = quote_term ->
          let f () =
             ignore (StrFilter.declare_term (StrFilter.get_proc loc) loc t)
          in
             print_exn f "declare" loc;
             empty_str_item loc
        | "define"; name = LIDENT; res = optresources; ":"; t = quote_term; "<-->"; def = term ->
           let f () =
             StrFilter.define_term (StrFilter.get_proc loc) loc name t def (Primitive xnil_term) res
           in
             print_exn f "define" loc;
             empty_str_item loc
        | "prim_rw"; name = LIDENT; res = optresources; args = optarglist; ":"; t = mterm ->
           let f () =
              StrFilter.declare_rewrite (StrFilter.get_proc loc) loc name args t (Primitive xnil_term) res
           in
              print_exn f "prim_rw" loc;
              empty_str_item loc
        | "iform"; name = LIDENT; res = optresources; args = optarglist; ":"; t = mterm ->
           let f () =
              StrFilter.declare_input_form (StrFilter.get_proc loc) loc name args t (Primitive xnil_term) res
           in
              print_exn f "iform" loc;
              empty_str_item loc
        | "interactive_rw"; name = LIDENT; res = optresources; args = optarglist; ":"; t = mterm ->
           let f () =
              StrFilter.declare_rewrite (StrFilter.get_proc loc) loc name args t Incomplete res
           in
              print_exn f "interactive_rw" loc;
              empty_str_item loc
        | "derived_rw"; name = LIDENT; res = optresources; args = optarglist; ":"; t = mterm ->
           let f () =
              StrFilter.declare_rewrite (StrFilter.get_proc loc) loc name args t Incomplete res
           in
              print_exn f "interactive_rw" loc;
              empty_str_item loc
        | "thm_rw"; name = LIDENT; res = optresources; args = optarglist; ":"; t = mterm; "="; body = expr ->
           let f () =
              StrFilter.declare_rewrite (StrFilter.get_proc loc) loc name args t (Derived body) res
           in
              print_exn f "thm_rw" loc;
             empty_str_item loc
        | "ml_rw"; name = LIDENT; res = optresources; args = optarglist; ":"; t = bound_term; "="; code = expr ->
           let f () =
              StrFilter.declare_mlrewrite (StrFilter.get_proc loc) loc name args t.aterm (Some (wrap_code loc t.aname code)) res
           in
              print_exn f "ml_rw" loc;
              empty_str_item loc
        | "prim"; name = LIDENT; res = optresources; params = optarglist; ":"; mt = bmterm; "="; extract = term ->
           let f () =
              define_prim (StrFilter.get_proc loc) loc name params mt extract res
           in
              print_exn f "prim" loc;
              empty_str_item loc
        | "thm"; name = LIDENT; res = optresources; params = optarglist; ":"; mt = bmterm; "="; tac = expr ->
           let f () =
              define_thm (StrFilter.get_proc loc) loc name params mt tac res
           in
              print_exn f "thm" loc;
              empty_str_item loc
        | "interactive"; name = LIDENT; res = optresources; params = optarglist; ":"; mt = bmterm ->
           let f () =
              define_int_thm (StrFilter.get_proc loc) loc name params mt res
           in
              print_exn f "interactive" loc;
              empty_str_item loc
        | "derived"; name = LIDENT; res = optresources; params = optarglist; ":"; mt = bmterm ->
           let f () =
              define_int_thm (StrFilter.get_proc loc) loc name params mt res
           in
              print_exn f "derived" loc;
              empty_str_item loc
        | mlrule_keyword; name = LIDENT; res = optresources; args = optarglist; ":"; t = bound_term; "="; code = expr ->
           let f () =
              StrFilter.declare_mlaxiom (StrFilter.get_proc loc) loc name args t.aterm (Some (wrap_code loc t.aname code)) res
           in
              print_exn f "mlrule" loc;
              empty_str_item loc
        |  "let"; "resource"; name = LIDENT; "="; code = expr ->
           let f () =
              StrFilter.define_resource (StrFilter.get_proc loc) loc name code
           in
              print_exn f "resource" loc;
              empty_str_item loc
        | "let"; "resource"; name = LIDENT; "+=" ; code = expr ->
           let f () =
              StrFilter.improve_resource (StrFilter.get_proc loc) loc {
                 improve_name = name;
                 improve_expr = code
              }
           in
              print_exn f "improve_resource" loc;
              empty_str_item loc
        | "dform"; name = LIDENT; ":"; options = df_options; "="; form = xdform ->
           let f () =
              let options', t = options in
                 StrFilter.define_dform (StrFilter.get_proc loc) loc name options' t form
           in
              print_exn f "dform" loc;
              empty_str_item loc
        | "ml_dform"; name = LIDENT; ":"; options = df_options; buf = LIDENT; format = LIDENT; "="; code = expr ->
           let f () =
              let options', t = options in
                 StrFilter.define_ml_dform (StrFilter.get_proc loc) loc name options' t buf format code
           in
              print_exn f "ml_dform" loc;
              empty_str_item loc
        | "infix"; name = ident ->
           let f () =
              StrFilter.declare_infix (StrFilter.get_proc loc) loc name
           in
              print_exn f "infix" loc;
              empty_str_item loc
        | "prec"; name = LIDENT ->
           let f () =
              StrFilter.declare_prec (StrFilter.get_proc loc) loc name
           in
              print_exn f "prec" loc;
              empty_str_item loc
        | "prec"; name1 = LIDENT; "<"; name2 = LIDENT ->
           let f () =
              StrFilter.define_prec_rel (StrFilter.get_proc loc) loc name1 name2 LTRelation
           in
              print_exn f "prec" loc;
              empty_str_item loc
        | "prec"; name1 = LIDENT; "="; name2 = LIDENT ->
           let f () =
              StrFilter.define_prec_rel (StrFilter.get_proc loc) loc name1 name2 EQRelation
           in
              print_exn f "prec" loc;
              empty_str_item loc
        | "prec"; name1 = LIDENT; ">"; name2 = LIDENT ->
           let f () =
              StrFilter.define_prec_rel (StrFilter.get_proc loc) loc name1 name2 GTRelation
           in
              print_exn f "prec" loc;
              empty_str_item loc
        | "magic_block"; name = LIDENT; "=";
          "struct"; st = LIST0 [ s = str_item; OPT ";;" -> s ]; "end" ->
           let f () =
              StrFilter.define_magic_block (StrFilter.get_proc loc) loc name st
           in
              print_exn f "magic_block" loc;
              empty_str_item loc
        | com = COMMENT ->
           let f () =
              StrFilter.declare_comment (StrFilter.get_proc loc) loc com
           in
              print_exn f "comment" loc;
              empty_str_item loc
       ]];

   mod_ident:
      [ RIGHTA
        [ i = UIDENT ->
           [i]
         | i = LIDENT ->
           [i]
         | m = UIDENT; "."; i = mod_ident ->
           m :: i
        ]
      ];

   (* Arglist is a list of terms *)
   optarglist:
      [[ args = LIST0 singleterm ->
          List.map (function t -> t.aterm) args
       ]];

   (* The optional list of resources to update *)
   optresources:
      [[ ores = OPT updresources ->
          match ores with
             Some ores -> ores
           | None -> []
      ]];

   updresources:
      [[ "{|"; e = expr; "|}" ->
           let f () =
                let rec split_application loc tl expr =
                   match expr with
                      <:expr< $e1$ $e2$ >> ->
                         split_application loc (e2 :: tl) e1
                    | <:expr< $lid:name$ >> ->
                         loc, name, tl
                    | _ ->
                         Stdpp.raise_with_loc (MLast.loc_of_expr expr) (Failure "resource is not a sequence")
                in
                   match e with
                      <:expr< do { $list:el$ } >> ->
                         List.map (fun expr -> split_application (MLast.loc_of_expr expr) [] expr) el
                    | _ ->
                         [split_application (MLast.loc_of_expr e) [] e]
           in
              print_exn f "updresources" loc
      ]];

   rule_keyword:
      [[ "rule" -> ()
       | "axiom" -> ()
      ]];

   mlrule_keyword:
      [[ "ml_rule" ->
         start_rewrite ()
       | "ml_axiom" ->
         start_rewrite ()
      ]];

   (*
    * DISPLAY FORMS.
    *)
   df_options:
      [[ l = LIST1 singleterm SEP "::" ->
          List_util.split_last (List.map (function { aterm = t } -> t) l)
       ]];

   (*
    * Upper or lowercase identifier.
    *)
   ident:
      [[ name = UIDENT ->
          name
        | name = LIDENT ->
          name
       ]];

   (*
    * Add the ML parts of the terms.
    *
   exterm:
      [[ "ml_expr"; e = expr ->
          !interp (term_of_expr e)
        | "ml_patt"; p = patt ->
          !interp (term_of_patt p)
        | "ml_type"; t = ctyp ->
          !interp (term_of_type t)
        | "ml_sig_item"; si = sig_item ->
          !interp (term_of_sig_item si)
        | "ml_str_item"; si = str_item ->
          !interp (term_of_str_item si)
        | "ml_module_type"; mt = module_type ->
          !interp (term_of_module_type mt)
        | "ml_module_expr"; me = module_expr ->
          !interp (term_of_module_expr me)
       ]];
    *)
END

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
