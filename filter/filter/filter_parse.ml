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
 * Modified By: Aleksey Nogin <nogin@cs.cornell.edu>
 *)
open Printf
open Pcaml

open Lm_symbol
open Lm_debug
open Precedence
open Simple_print.SimplePrint
open Mp_resource
open File_base_type
open Term_shape_sig

open Refiner.Refiner
open Term
open TermOp
open TermType
open TermMan
open TermMeta
open Rewrite
open RefineError

open Term_grammar
open Filter_type
open Filter_util
open Filter_summary
open Filter_summary_type
open Filter_summary_util
open Filter_prog
open Filter_magic
open Proof_convert

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

let debug_grammar =
   create_debug (**)
      { debug_name = "grammar";
        debug_description = "display term parsing operations";
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

(************************************************************************
 * TERM GRAMMAR                                                         *
 ************************************************************************)

(*
 * XXX This is a HACK!
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
 * Base term grammar
 *)
module TermGrammarBefore : TermGrammarSig =
struct
   let mk_opname = mk_opname
   let mk_var_contexts _ v _ = None

   (*
    * Term grammar.
    *)
   let gram = Pcaml.gram
   let term_eoi = Grammar.Entry.create gram "term_eoi"
   let term = Grammar.Entry.create gram "term"
   let parsed_term = Grammar.Entry.create gram "parsed_term"
   let quote_term = Grammar.Entry.create gram "quote_term"
   let mterm = Grammar.Entry.create gram "mterm"
   let bmterm = Grammar.Entry.create gram "mterm"
   let singleterm = Grammar.Entry.create gram "singleterm"
   let applytermlist = Grammar.Entry.create gram "applytermlist"
   let parsed_bound_term = Grammar.Entry.create gram "bound_term"
   let xdform = Grammar.Entry.create gram "xdform"
   let term_con_eoi = Grammar.Entry.create gram "term_con_eoi"
end

(*
 * Extended term grammar.
 *)
module TermGrammar = MakeTermGrammar (TermGrammarBefore)
open TermGrammarBefore
open TermGrammar

(************************************************************************
 * QUOTATIONS                                                           *
 ************************************************************************)

let term_exp s =
   let cs = Stream.of_string s in
   let t = Grammar.Entry.parse term_eoi cs in
      add_binding (BindTerm t)

let term_patt s =
   let cs = Stream.of_string s in
   let t = Grammar.Entry.parse term_eoi cs in
      try
         Filter_patt.build_term_patt t
      with exn ->
         eprintf "Can not build a patten out of a term:\n\t%a%t" (Filter_exn.print_exn Dform.null_base) exn eflush;
         raise exn

let _ = Quotation.add "term" (Quotation.ExAst (term_exp, term_patt))
let _ = Quotation.default := "term"

let rec mk_list_expr loc = function
   [] -> <:expr< [] >>
 | hd :: tl -> <:expr< $uid:"::"$ $hd$ $mk_list_expr loc tl$ >>

let expr_of_bvar_con loc = function
   ConBVarConst s -> <:expr< $str: s$ >>
 | ConBVarExpr e -> e

let expr_of_pcon loc = function
   ConPStr s, shape ->
      let shape =
         match shape with
            ShapeString -> "String"
          | ShapeToken -> "Token"
          | _ -> Stdpp.raise_with_loc loc (Invalid_argument "\"con\" quotation: string constant parameter must
 be of string or token kind")
      in
         <:expr< Refiner.Refiner.Term.make_param (Refiner.Refiner.TermType.$uid:shape$ $str:s$) >>
 | ConPMeta l, ShapeLevel ->
      <:expr<
         Refiner.Refiner.Term.make_param
            (Refiner.Refiner.TermType.MLevel
               (Refiner.Refiner.TermMan.mk_var_level_exp $str:string_of_symbol l$))
      >>
 | ConPMeta s, shape ->
      let shape =
         match shape with
            ShapeString -> "MString"
          | ShapeNumber -> "MNumber"
          | ShapeToken -> "MToken"
          | _ -> Stdpp.raise_with_loc loc (Invalid_argument "\"con\" quotation: unsupported meta-parameter")
      in
         <:expr< Refiner.Refiner.Term.make_param (Refiner.Refiner.TermType.$uid:shape$ $str:string_of_symbol s$) >>
  | ConPExpr e, shape ->
      let shape =
         match shape with
            ShapeString -> "String"
          | ShapeToken -> "Token"
          | ShapeNumber -> "Number"
          | ShapeLevel -> "Level"
          | ShapeVar -> "Var"
      in
         <:expr< Refiner.Refiner.Term.make_param (Refiner.Refiner.TermType.$uid:shape$ $e$) >>
  | ConPNum n, ShapeNumber ->
      <:expr< Refiner.Refiner.Term.make_param (Refiner.Refiner.TermType.Number $add_binding (BindNum n)$) >>
  | ConPInt e, ShapeNumber ->
      <:expr< Refiner.Refiner.Term.make_param (Refiner.Refiner.TermType.Number (Lm_num.num_of_int $e$)) >>
  | (ConPNum _|ConPInt _), _ ->
      Stdpp.raise_with_loc loc (Invalid_argument "\"con\" quotation: numeric parameter of non-numeric kind?")

let is_simp_bterm = function
   [], _ -> true
 | _ -> false

let rec expr_of_term_con loc = function
   ConTerm t ->
      add_binding (BindTerm t)
 | ConExpr e ->
      e
 | ConVar e ->
      let loc = (0, 0) in
         <:expr< Refiner.Refiner.Term.mk_var_term $e$ >>
 | ConConstruct (op, params, bterms) ->
      let op = add_binding (BindOpname op) in
         if params = [] && List.for_all is_simp_bterm bterms then
            let bterms = mk_list_expr loc (List.map (fun (_, bt) -> expr_of_term_con loc bt) bterms) in
               <:expr< Refiner.Refiner.Term.mk_simple_term $op$ $bterms$ >>
         else
            let bterms = mk_list_expr loc (List.map (expr_of_bterm_con loc) bterms) in
            let params = mk_list_expr loc (List.map (expr_of_pcon loc) params) in
               <:expr< Refiner.Refiner.Term.mk_term (Refiner.Refiner.Term.mk_op $op$ $params$) $bterms$ >>
 | ConSequent (arg, hyps, goals) ->
      let arg = expr_of_term_con loc arg in
      let hyps = expr_of_hyps_con loc hyps in
      let goals = mk_list_expr loc (List.map (expr_of_term_con loc) goals) in
         <:expr< Refiner.Refiner.TermMan.mk_sequent_term
                    { Refiner.Refiner.TermType.sequent_args = $arg$;
                      Refiner.Refiner.TermType.sequent_hyps = Refiner.Refiner.Term.SeqHyp.of_list $hyps$;
                      Refiner.Refiner.TermType.sequent_goals = Refiner.Refiner.Term.SeqGoal.of_list $goals$
         } >>

and expr_of_bterm_con loc (bvars, bt) =
   let bt = expr_of_term_con loc bt in
   if bvars = [] then
      <:expr< Refiner.Refiner.Term.mk_simple_bterm $bt$ >>
   else
      let bvars = mk_list_expr loc (List.map (expr_of_bvar_con loc) bvars) in
         <:expr< Refiner.Refiner.Term.mk_bterm $bvars$ $bt$ >>

and expr_of_hyps_con loc hyps =
   match hyps with
      [] ->
         <:expr< [] >>
    | hyp :: hyps ->
         let hyps = expr_of_hyps_con loc hyps in
            match hyp with
               ConContext (v, args) ->
                  let args = mk_list_expr loc (List.map (expr_of_term_con loc) args) in
                  let e = <:expr< Refiner.Refiner.TermType.Context (Lm_symbol.add $v$, $args$) >> in
                     <:expr< [ $e$ :: $hyps$ ] >>
             | ConHypList l ->
                  <:expr< $l$ @ $hyps$ >>
             | ConHypothesis t ->
                  let e = <:expr< Refiner.Refiner.TermType.Hypothesis $expr_of_term_con loc t$ >> in
                     <:expr< [ $e$ :: $hyps$ ] >>
             | ConHypBinding (v, t) ->
                  let e = <:expr< Refiner.Refiner.TermType.HypBinding (Lm_symbol.add $v$, $expr_of_term_con loc t$) >> in
                     <:expr< [ $e$ :: $hyps$ ] >>

let con_exp s =
   let cs = Stream.of_string s in
   let con = Grammar.Entry.parse term_con_eoi cs in
      expr_of_term_con (0,0) con

let con_patt _ =
   raise(Invalid_argument "<:con< >> quotation can not be used where pattern is expected")

let _ = Quotation.add "con" (Quotation.ExAst (con_exp, con_patt))

let bind_item i =
   { item_item = i;
     item_bindings = get_bindings ();
   }

(* Convert contexts in meta-terms, terms args and resource term bindings *)
let parse_mtlr mt tl rs =
   let mt, tl, f = mterms_of_parsed_mterms mt tl in
   let conv = function
      v, BindTerm t -> v, BindTerm(f t)
    | bnd -> bnd
   in
      mt, tl, { rs with item_bindings = List.map conv rs.item_bindings }

(* Same as parse_mtlr, but with extract term as well *)
let parse_mtlre mt tl rs extract =
   let mt, tl, f = mterms_of_parsed_mterms mt tl in
   let conv = function
      v, BindTerm t -> v, BindTerm(f t)
    | bnd -> bnd
   in
   let extract =
      (*
       * XXX HACK: we assume that extracts from sequents must be sequents of the same shape
       * And whenever users specify a non-sequent extract, they must be meaning to specify a
       * conclusion of a sequent.
       * There is a complimentary HACK in Term_grammar.create_meta_function
       *)
      let _, goal = unzip_mfunction mt in
         if is_sequent_term goal && not (is_sequent_term extract) then replace_goal goal extract else extract
   in
      mt, tl, { rs with item_bindings = List.map conv rs.item_bindings }, f extract

(************************************************************************
 * TERM HACKING                                                         *
 ************************************************************************)

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
                  String s -> s
                | MString s -> string_of_symbol s
                | _ -> Stdpp.raise_with_loc loc (RefineError ("Filter_parse.get_string_param", TermMatchError (t, "param type")))
            end
       | { op_params = [] } ->
            Stdpp.raise_with_loc loc (RefineError ("Filter_parse.get_string_param", TermMatchError (t, "no params")))
       | _ ->
            Stdpp.raise_with_loc loc (RefineError ("Filter_parse.get_string_param", TermMatchError (t, "too many params")))

(*
 * Wrap a code block with a binding variable.
 *)
let wrap_code loc v body =
   let v =
      match v with
         Some v -> string_of_symbol (dest_var v)
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
    * Include a parent.
    * This performs the following tasks:
    *    1. incorporates the parents:
    *       a. adds the resources
    *       b. adds the infix directives.
    *)
   let infix_set = ref Infix.Set.empty
   let declare_parent proc loc path =
      (* Lots of errors can occur here *)
      ignore(FilterCache.inline_module proc.cache () path);
      let infixes = FilterCache.sig_infixes proc.cache path in
      Infix.Set.iter Infix.add (Infix.Set.diff infixes !infix_set);
      infix_set := Infix.Set.union infixes !infix_set;
      let resources = FilterCache.sig_resources proc.cache path in
      begin if !debug_resource then
         let print_resources out resources =
            List.iter (fprintf out " %s") (List.map fst resources)
         in
            eprintf "Filter_parse.declare_parent: %s:%a%t" (string_of_path path) print_resources resources eflush
      end;
      let info =
         { parent_name = path;
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
   let declare_term_opname proc loc (s, params, bterms) =
      let opname' = Opname.mk_opname s (FilterCache.op_prefix proc.cache) in
      let t = term_of_parsed_term (mk_term (mk_op opname' params) bterms) in
         FilterCache.update_opname proc.cache s t;
         t

   let declare_term proc loc ((s, _, _) as term_spec) =
      let t = declare_term_opname proc loc term_spec in
         FilterCache.add_command proc.cache (Opname { opname_name = s; opname_term = t }, loc)

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
      Refine.check_rewrite name [] [] redex contractum;

      (* Construct the command *)
      Rewrite { rw_name = name;
                rw_redex = redex;
                rw_contractum = contractum;
                rw_proof = pf;
                rw_resources = res
      }

   let simple_input_form proc name redex contractum pf res =
      (* Check that rewrite will succeed *)
      Refine.check_rewrite name [] [] redex contractum;

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
            (collect_terms params')
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
   let define_term proc loc name ((s,_,_) as term_spec) contractum res =
      let t = declare_term_opname proc loc term_spec in
         Refine.check_definition name t contractum;
         FilterCache.add_command proc.cache (Definition {
            opdef_name = name;
            opdef_opname = s;
            opdef_term = t;
            opdef_definition = contractum;
            opdef_resources = res;
         }, loc)

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
      print_terms out (collect_terms params)

   let rule_command proc name params t pf res =
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
         Refine.check_rule name (Array.of_list (collect_cvars params')) (collect_terms params') (strip_mfunction t);
         if !debug_grammar then
            eprintf "Checked rule: %s%t" name eflush;

         (* If checking completes, add the rule *)
         Rule { rule_name = name;
                rule_params = params';
                rule_stmt = t;
                rule_proof = pf;
                rule_resources = res
         }

   let declare_rule proc loc name args t pf res =
      try
         let cmd = rule_command proc name args t pf res in
            FilterCache.add_command proc.cache (cmd, loc)
      with exn ->
         Stdpp.raise_with_loc loc exn

   (*
    * Infix directive.
    *)
   let declare_gupd proc loc upd =
      FilterCache.add_command proc.cache (GramUpd upd, loc);
      Infix.add upd

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
            ignore(term_rewrite Relaxed empty_args_spec [t] [expansion])
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
   let declare_comment proc loc t =
      FilterCache.add_command proc.cache (Comment t, loc)

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
         FilterCache.parse_comments proc.cache convert_comment;

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
   let extract = extract_sig
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

   let extract = extract_str ()
end

module SigFilter = MakeFilter (SigFilterInfo) (ProofCaches.SigFilterCache)
module StrFilter = MakeFilter (StrFilterInfo) (ProofCaches.StrFilterCache)

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
    (res : ((MLast.expr, term) resource_def)) =
   try
      let cmd = StrFilter.rule_command proc name params mterm extract res in
      begin match cmd, extract with
         Rule r, Primitive extract ->
            let _, ext_args, _ = split_mfunction mterm in
            let addrs = Array.of_list (collect_cvars r.rule_params) in
               Refine.check_prim_rule name addrs (collect_terms r.rule_params) (strip_mfunction mterm) ext_args extract
       | _ -> ()
      end;
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
             if get_bindings () <> [] then
                Stdpp.raise_with_loc loc (Invalid_argument "Filter_parse.interf_item: sig item has bindings");
             begin
                match s with
                   <:sig_item< declare $list: []$ end >> ->
                      ()
                 | _ ->
                      let item = SummaryItem { item_bindings = []; item_item = s } in
                         SigFilter.add_command (SigFilter.get_proc loc) (item, loc)
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
                      if get_bindings () <> [] then
                         Stdpp.raise_with_loc loc (Invalid_argument "Filter_parse.implem_item: empty str item has bindings")
                 | _ ->
                      StrFilter.add_command (StrFilter.get_proc loc) (SummaryItem (bind_item s), loc)
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
             SigFilter.declare_term (SigFilter.get_proc loc) loc t
          in
             print_exn f "declare" loc;
             empty_sig_item loc
        | "define"; name = LIDENT; ":"; t = quote_term; "<-->"; def = parsed_term ->
           let f () =
             SigFilter.define_term (SigFilter.get_proc loc) loc name t def no_resources
           in
             print_exn f "define" loc;
             empty_sig_item loc
        | "rewrite"; name = LIDENT; args = optarglist; ":"; t = mterm ->
           let f () =
             let t, args, _ = mterms_of_parsed_mterms t args in
             SigFilter.declare_rewrite (SigFilter.get_proc loc) loc name args t () no_resources
           in
             print_exn f "rewrite" loc;
             empty_sig_item loc
        | "ml_rw"; name = LIDENT; args = optarglist; ":"; t = parsed_term ->
           let f () =
             let args = List.map term_of_parsed_term args in
             SigFilter.declare_mlrewrite (SigFilter.get_proc loc) loc name args t None no_resources
           in
             print_exn f "ml_rw" loc;
             empty_sig_item loc
        | rule_keyword; name = LIDENT; args = optarglist; ":"; t = mterm ->
           let f () =
             let t, args, _ = mterms_of_parsed_mterms t args in
             SigFilter.declare_rule (SigFilter.get_proc loc) loc name args t () no_resources
           in
              print_exn f "rule" loc;
              empty_sig_item loc
        | mlrule_keyword; name = LIDENT; args = optarglist; ":"; t = parsed_term ->
           let f () =
              let args = List.map term_of_parsed_term args in
              SigFilter.declare_mlaxiom (SigFilter.get_proc loc) loc name args t None no_resources
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
              SigFilter.declare_gupd (SigFilter.get_proc loc) loc (Infix name)
           in
              print_exn f "infix" loc;
              empty_sig_item loc
        | "suffix"; name = ident ->
           let f () =
              SigFilter.declare_gupd (SigFilter.get_proc loc) loc (Suffix name)
           in
              print_exn f "suffix" loc;
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
        | "doc"; doc_sig ->
           empty_sig_item loc
       ]];

   doc_sig:
      [[ q = QUOTATION ->
           let f () =
               match dest_quot q with
                  "doc", com ->
                     SigFilter.declare_comment (SigFilter.get_proc loc) loc (mk_string_term comment_string_op com)
                | q ->
                     SigFilter.declare_comment (SigFilter.get_proc loc) loc (parse_quotation loc "doc" q)
           in
              print_exn f "comment" loc;
        | tl = applytermlist ->
           SigFilter.declare_comment (SigFilter.get_proc loc) loc (mk_comment_term (List.map term_of_parsed_term tl))
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
             StrFilter.declare_term (StrFilter.get_proc loc) loc t
          in
             print_exn f "declare" loc;
             empty_str_item loc
        | "define"; name = LIDENT; res = optresources; ":"; t = quote_term; "<-->"; def = parsed_term ->
           let f () =
             StrFilter.define_term (StrFilter.get_proc loc) loc name t def res
           in
             print_exn f "define" loc;
             empty_str_item loc
        | "prim_rw"; name = LIDENT; res = optresources; args = optarglist; ":"; t = mterm ->
           let f () =
              let t, args, res = parse_mtlr t args res in
              StrFilter.declare_rewrite (StrFilter.get_proc loc) loc name args t (Primitive xnil_term) res
           in
              print_exn f "prim_rw" loc;
              empty_str_item loc
        | "iform"; name = LIDENT; res = optresources; args = optarglist; ":"; t = mterm ->
           let f () =
              let t, args, res = parse_mtlr t args res in
              StrFilter.declare_input_form (StrFilter.get_proc loc) loc name args t (Primitive xnil_term) res
           in
              print_exn f "iform" loc;
              empty_str_item loc
        | "interactive_rw"; name = LIDENT; res = optresources; args = optarglist; ":"; t = mterm ->
           let f () =
              let t, args, res = parse_mtlr t args res in
              StrFilter.declare_rewrite (StrFilter.get_proc loc) loc name args t Incomplete res
           in
              print_exn f "interactive_rw" loc;
              empty_str_item loc
        | "derived_rw"; name = LIDENT; res = optresources; args = optarglist; ":"; t = mterm ->
           let f () =
              let t, args, res = parse_mtlr t args res in
              StrFilter.declare_rewrite (StrFilter.get_proc loc) loc name args t Incomplete res
           in
              print_exn f "derived_rw" loc;
              empty_str_item loc
        | "thm_rw"; name = LIDENT; res = optresources; args = optarglist; ":"; t = mterm; "="; body = expr ->
           let f () =
              let t, args, res = parse_mtlr t args res in
              StrFilter.declare_rewrite (StrFilter.get_proc loc) loc name args t (Derived body) res
           in
              print_exn f "thm_rw" loc;
             empty_str_item loc
        | "ml_rw"; name = LIDENT; res = optresources; args = optarglist; ":"; t = parsed_bound_term; "="; code = expr ->
           let f () =
              let args = List.map term_of_parsed_term args in
              let code = bind_item (wrap_code loc t.aname code) in
                 StrFilter.declare_mlrewrite (StrFilter.get_proc loc) loc name args t.aterm (Some code) res
           in
              print_exn f "ml_rw" loc;
              empty_str_item loc
        | "prim"; name = LIDENT; res = optresources; params = optarglist; ":"; mt = bmterm; "="; extract = term ->
           let f () =
              let mt, params, res, extract = parse_mtlre mt params res extract in
              define_prim (StrFilter.get_proc loc) loc name params mt extract res
           in
              print_exn f "prim" loc;
              empty_str_item loc
        | "thm"; name = LIDENT; res = optresources; params = optarglist; ":"; mt = bmterm; "="; tac = expr ->
           let f () =
              let mt, params, res = parse_mtlr mt params res in
              define_thm (StrFilter.get_proc loc) loc name params mt tac res
           in
              print_exn f "thm" loc;
              empty_str_item loc
        | "interactive"; name = LIDENT; res = optresources; params = optarglist; ":"; mt = bmterm ->
           let f () =
              let mt, params, res = parse_mtlr mt params res in
              define_int_thm (StrFilter.get_proc loc) loc name params mt res
           in
              print_exn f "interactive" loc;
              empty_str_item loc
        | "derived"; name = LIDENT; res = optresources; params = optarglist; ":"; mt = bmterm ->
           let f () =
              let mt, params, res = parse_mtlr mt params res in
              define_int_thm (StrFilter.get_proc loc) loc name params mt res
           in
              print_exn f "derived" loc;
              empty_str_item loc
        | mlrule_keyword; name = LIDENT; res = optresources; args = optarglist; ":"; t = parsed_bound_term; "="; code = expr ->
           let f () =
              let args = List.map term_of_parsed_term args in
              let code = bind_item (wrap_code loc t.aname code) in
                 StrFilter.declare_mlaxiom (StrFilter.get_proc loc) loc name args t.aterm (Some code) res
           in
              print_exn f "mlrule" loc;
              empty_str_item loc
        | "let"; "resource"; name = LIDENT; "="; code = expr ->
           let f () =
              StrFilter.define_resource (StrFilter.get_proc loc) loc name code
           in
              print_exn f "resource" loc;
              empty_str_item loc
        | "let"; "resource"; name = LIDENT; "+=" ; code = expr ->
           let f () =
              StrFilter.improve_resource (StrFilter.get_proc loc) loc {
                 improve_name = name;
                 improve_expr = (bind_item code);
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
                 StrFilter.define_ml_dform (StrFilter.get_proc loc) loc name options' t buf format (bind_item code)
           in
              print_exn f "ml_dform" loc;
              empty_str_item loc
        | "infix"; name = ident ->
           let f () =
              StrFilter.declare_gupd (StrFilter.get_proc loc) loc (Infix name)
           in
              print_exn f "infix" loc;
              empty_str_item loc
        | "suffix"; name = ident ->
           let f () =
              StrFilter.declare_gupd (StrFilter.get_proc loc) loc (Suffix name)
           in
              print_exn f "suffix" loc;
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
        | "doc"; doc_str ->
           empty_str_item loc
       ]];

    doc_str:
       [[ q = QUOTATION ->
           let f () =
               match dest_quot q with
                  (* XXX HACK: this makes sure parsing of top-level "doc" quotations is lazy *)
                  "doc", com ->
                     StrFilter.declare_comment (StrFilter.get_proc loc) loc (mk_string_term comment_string_op com)
                | q ->
                     StrFilter.declare_comment (StrFilter.get_proc loc) loc (parse_quotation loc "doc" q)
           in
              print_exn f "comment" loc
        | tl = applytermlist ->
           StrFilter.declare_comment (StrFilter.get_proc loc) loc (mk_comment_term (List.map term_of_parsed_term tl))
       ]];

   mod_ident:
      [ RIGHTA
        [ i = UIDENT ->
           [String.uncapitalize i]
         | i = LIDENT ->
           [i]
         | m = UIDENT; "."; i = mod_ident ->
           (String.uncapitalize m) :: i
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
           | None -> no_resources
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
                let e =
                   match e with
                      <:expr< do { $list:el$ } >> ->
                         List.map (fun expr -> split_application (MLast.loc_of_expr expr) [] expr) el
                    | _ ->
                         [split_application (MLast.loc_of_expr e) [] e]
                in
                  (* context in resource term quotations will get converted by parse_mtlr *)
                  { item_item = e; item_bindings = get_unparsed_bindings (); }
           in
              print_exn f "updresources" loc
      ]];

   rule_keyword:
      [[ "rule" -> () ]];

   mlrule_keyword:
      [[ "ml_rule" | "ml_axiom" -> () ]];

   (*
    * DISPLAY FORMS.
    *)
   df_options:
      [[ l = LIST1 singleterm SEP "::" ->
          Lm_list_util.split_last (List.map (function { aterm = t } -> term_of_parsed_term t) l)
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
