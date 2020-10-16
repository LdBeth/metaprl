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
 * See the file doc/htmlman/default.html or visit http://metaprl.org/
 * for more information.
 *
 * Copyright (C) 1998-2007 MetaPRL Group, Cornell University and California
 * Institute of Technology
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
open Lm_debug
open Lm_symbol
open Lm_printf
open Lm_string_set

open Pcaml

open Opname
open Precedence
open File_base_type
open Term_ty_sig
open Term_sig

open Term_ty_infer

open Refiner.Refiner
open Refiner.Refiner.Term
open Refiner.Refiner.TermOp
open Refiner.Refiner.TermMan
open Refiner.Refiner.TermMeta
open Refiner.Refiner.TermShape
open Refiner.Refiner.TermTy
open Refiner.Refiner.Rewrite
open Refiner.Refiner.RefineError
open Refiner.Refiner.Refine
open Mp_resource

open Term_grammar
open Filter_base_type
open Filter_type
open Filter_util
open Filter_shape
open Filter_summary
open Filter_summary_type
open Filter_summary_util
open Filter_prog
open Filter_magic
open Proof_boot
open Proof_convert
open Simple_print

(************************************************************************
 * TYPES                                                                *
 ************************************************************************)

(*
 * Temporary type for precedence directives.
 *)
type prec_info =
   PrecEqual of term
 | PrecLessThan of term
 | PrecGreaterThan of term
 | PrecNone

(************************************************************************
 * DEBUGGING                                                            *
 ************************************************************************)

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

(* unused
let rec print_terms out = function
   h::t ->
      eprintf "\t%s\n" (SimplePrint.string_of_term h);
      print_terms out t
 | [] ->
      flush stderr

let rec print_vterms out = function
   (labels, Some v, h)::t ->
      eprintf "\t%a %s. %s\n" print_string_list labels (SimplePrint.string_of_term v) (SimplePrint.string_of_term h);
      print_vterms out t
 | (labels, None, h)::t ->
      eprintf "\t%a %s\n" print_string_list labels (SimplePrint.string_of_term h);
      print_vterms out t
 | [] ->
      flush stderr

let print_non_vars out params =
   print_terms out (collect_terms params)
*)

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
 * TERM HACKING                                                         *
 ************************************************************************)

(*
 * There should be only one param, of [M]String type.
 * Get it.
 *)
let get_string_param loc t =
   match dest_params (dest_op (dest_term t).term_op).op_params with
      [ String s ] ->
         s
    | [ MString s ] ->
         string_of_symbol s
    | _ ->
         Ploc.raise loc (RefineError ("Filter_parse.get_string_param", StringTermError ("not a string param", t)))

(*
 * Wrap a code block with a binding variable.
 *)
let wrap_code _loc v body =
   let v =
      match v with
         Some v -> string_of_symbol (dest_var v)
       | None -> "_$goal"
   in
   let p = <:patt< $lid:v$ >> in
      <:expr< fun [ $list: [p, Ploc.VaVal None, body]$ ] >>

(************************************************************************
 * TERM GRAMMAR                                                         *
 ************************************************************************)

let parsing_state = ref None

(*
 * Base term grammar
 *)
module TermGrammarBefore : TermGrammarSig =
struct
   let parsing_state loc =
      match !parsing_state with
         Some st -> st
       | None -> Ploc.raise loc (Failure "Filter_parse.parsing_state is uninitialized")

   (*
    * Term grammar.
    *)
   let gram              = Pcaml.gram
   let opname            = Grammar.Entry.create gram "opname"
   let opname_name       = Grammar.Entry.create gram "opname_name"
   let term_eoi          = Grammar.Entry.create gram "term_eoi"
   let term              = Grammar.Entry.create gram "term"
   let parsed_term       = Grammar.Entry.create gram "parsed_term"
   let quote_term        = Grammar.Entry.create gram "quote_term"
   let mterm             = Grammar.Entry.create gram "mterm"
   let bmterm            = Grammar.Entry.create gram "mterm"
   let singleterm        = Grammar.Entry.create gram "singleterm"
   let parsed_bound_term = Grammar.Entry.create gram "bound_term"
   let xdform            = Grammar.Entry.create gram "xdform"
   let term_con_eoi      = Grammar.Entry.create gram "term_con_eoi"
end

(*
 * Extended term grammar.
 *)
module TermGrammar = MakeTermGrammar (TermGrammarBefore)

(*
 * The bindings contain raw, parsed terms.
 * Wrap the binding functions.
 *)
let add_parsed_binding bind =
   let bind =
      match bind with
         BindTerm t ->
            BindTerm (TermGrammar.raw_term_of_parsed_term t)
       | BindOpname _
       | BindNum _ as bind ->
            bind
   in
      add_binding bind

let get_unchecked_bindings = get_bindings

let get_checked_bindings loc =
   List.map (fun (v, bind) ->
         let bind =
            match bind with
               BindTerm t ->
                  BindTerm (TermGrammar.parse_term loc (TermGrammar.mk_parsed_term t))
             | BindOpname _
             | BindNum _ as bind ->
                  bind
         in
            v, bind) (get_bindings ())

(* Just to make sure we don't use them accidentally *)
(* unused
let get_bindings () = ()
let add_binding () = ()
*)

(************************************************************************
 * GENERIC CONSTRUCTION                                                 *
 ************************************************************************)

let theory_group, theory_groupdsc = make_groupdsc_opts ()

(*
 * We may be able to do better sometime, but for now
 * we print the terms using the default display forms.
 *)
let handle_exn f s loc =
   Filter_exn.handle_exn Dform.null_base (Some (sprintf "While processing %s:\n" s)) loc f

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
      string -> string -> string -> (item * MLast.loc) list
end

module StrLSet = Lm_set.LmMake (struct
   type t = string list
   let compare = Pervasives.compare
end)

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
    with type arg       = unit) =
struct
   (*
    * Processors include both the cache and the name of the module.
    *
    * Filename, must be globally unique
    * Gensym prefix must be globally unique,
    *    deterministically generated, and different
    *    for .ml and .mli files ()
    *)
   type t =
      { cache           : FilterCache.info;
        select          : select_type;
        name            : string;
        gensym          : string;
        group           : string; (* e.g. "itt" *)
        groupdsc        : string; (* e.g. "Constructive Type Theory" *)
        mutable names   : StringSet.t;
        mutable parents : StrLSet.t;
        mutable infixes : Infix.Set.t;
      }

   (*
    * Processor.
    *)
   let proc_ref = ref None

   (*
    * Parse an input expression.
    * This comes before get_proc because
    * the get_proc function needs to set the start symbols.
    *)
   let mk_parse_state loc id =
      { parse_quotation =
           (fun name s ->
                 TermGrammar.raw_term_of_parsed_term (TermGrammar.parse_quotation loc id name s));
        parse_opname = TermGrammar.mk_opname_kind loc;
        parse_shape  = TermGrammar.find_shape_class loc;
        parse_param  = TermGrammar.dest_xparam loc
      }

   let input_exp shape id s =
      match !proc_ref with
         Some proc ->
            let loc = Ploc.make_loc proc.name 1 0 (0, 0) "" in
            let state = mk_parse_state dummy_loc id in
            let t = FilterCache.parse state proc.cache loc shape s in
            let t = TermGrammar.mk_parsed_term t in
               add_parsed_binding (BindTerm t)
       | None ->
            raise (Invalid_argument "Input grammar is not initialized")

   let input_patt shape id s =
      match !proc_ref with
         Some proc ->
            let loc = Ploc.make_loc proc.name 1 0 (0, 0) "" in
            let state = mk_parse_state dummy_loc id in
            let t = FilterCache.parse state proc.cache loc shape s in
            let t = TermGrammar.unchecked_term_of_parsed_term dummy_loc (TermGrammar.mk_parsed_term t) in
               Filter_exn.print_exn Dform.null_base (Some "Can not build a pattern out of a term:\n") Filter_patt.build_term_patt t
       | None ->
            raise (Invalid_argument "Input grammar is not initialized")

   let add_start name shape =
      Quotation.add name (Quotation.ExAst (input_exp shape name, input_patt shape name))

   let add_starts starts =
      StringTable.iter add_start starts

   (*
    * Our version of add_command - make sure there are no name clashes.
    *
    * XXX: We have a huge number of similarly named display forms, so the
    * display form names are not being checked (after all they are only used in
    * debug messages when debug_dform is enabled).
    *)
   let add_command proc cmd =
      begin
         match fst cmd with
            Rewrite { rw_name = name; _ }
          | InputForm { iform_name = name; _ }
          | CondRewrite { crw_name = name; _ }
          | Rule { rule_name = name; _ }
          | MLRewrite { mlterm_name = name; _ }
          | MLAxiom { mlterm_name = name; _ }
          | MLGramUpd (Infix name)
          | MLGramUpd (Suffix name)
          | DefineTerm (_, _, { term_def_name = name; _ }) ->
               if StringSet.mem proc.names name then
                  raise (Invalid_argument ("Filter_parse.add_command: duplicate name " ^ name));
               proc.names <- StringSet.add proc.names name
          | DForm _
          | SummaryItem _
          | ToploopItem _
          | DeclareTypeClass _
          | DeclareType _
          | DeclareTerm _
          | DeclareTypeRewrite _
          | Parent _
          | Module _
          | Prec _
          | PrecRel _
          | Resource _
          | Improve _
          | Id _
          | MagicBlock _
          | Comment _
          | PRLGrammar _ ->
               ()
      end;
      FilterCache.add_command proc.cache cmd

   (*
    * Include a parent.
    * This performs the following tasks:
    *    1. incorporates the parents:
    *       a. adds the resources
    *       b. adds the infix directives.
    *)
(* unused
   let rec pp_print_string_list buf sl =
      match sl with
         [s] ->
            pp_print_string buf s
       | s :: sl ->
            pp_print_string buf s;
            pp_print_char buf '.';
            pp_print_string_list buf sl
       | [] ->
            ()
*)

   let declare_parent proc loc path =
      (* Prevent multiple inclusion *)
      if StrLSet.mem proc.parents path then
         Ploc.raise loc (Invalid_argument "Same theory extended twice");
      proc.parents <- StrLSet.add proc.parents path;

      (* Lots of errors can occur here *)
      let () =
         try FilterCache.inline_module proc.cache () path with
            exn ->
               Ploc.raise loc exn
      in

      (* Add infixes *)
      let () =
         let infixes = FilterCache.sig_infixes proc.cache path in
            Infix.Set.iter Infix.add (Infix.Set.diff infixes proc.infixes);
            proc.infixes <- Infix.Set.union infixes proc.infixes
      in

      (* Add resources and grammar start symbols *)
      let info =
         { parent_name = path;
           parent_resources = FilterCache.sig_resources proc.cache path;
         }
      in
         add_starts (FilterCache.get_start proc.cache);
         add_command proc (Parent info, loc)

   (*
    * Declarations.
    *)
   let declare_typeclass proc loc shapeclass kind_opname typeclass_type typeclass_parent =
      FilterCache.declare_typeclass proc.cache shapeclass kind_opname typeclass_type typeclass_parent;
      add_command proc (DeclareTypeClass (shapeclass, kind_opname, typeclass_type, typeclass_parent), loc)

   let declare_type proc loc shapeclass ty_term ty_parent =
      FilterCache.declare_type proc.cache shapeclass ty_term ty_parent;
      add_command proc (DeclareType (shapeclass, ty_term, ty_parent), loc)

   let declare_term proc loc shapeclass ty_term =
      FilterCache.declare_term proc.cache shapeclass ty_term;
      add_command proc (DeclareTerm (shapeclass, ty_term), loc)

   let declare_type_cases proc loc shapeclass ty_term ty_parent cases =
      let ty_type = term_of_ty ty_term in
         FilterCache.declare_type proc.cache shapeclass ty_term ty_parent;
         add_command proc (DeclareType (shapeclass, ty_term, ty_parent), loc);
         List.iter (fun ty_term ->
               let ty_term = { ty_term with ty_type = ty_type } in
                  FilterCache.declare_term proc.cache shapeclass ty_term;
                  add_command proc (DeclareTerm (shapeclass, ty_term), loc)) cases

   let declare_define_term proc shapeclass ty_term =
      FilterCache.declare_term proc.cache shapeclass ty_term

   let declare_type_rewrite proc loc redex contractum =
      FilterCache.declare_type_rewrite proc.cache redex contractum;
      add_command proc (DeclareTypeRewrite (redex, contractum), loc)

   (*
    * Define a rewrite in an interface.
    * Rewrites are somewhat redundant, since they can be defined as
    * axioms, but we use this special form to specifically indicate
    * a rewrite to create an efficient evaluator.  The format is:
    *    rewrite name [params...] : [cond1 -> ... -> condn] -> (redex <--> contractum)
    * The params are supplied terms, and the conditions are terms that
    * must be provable _in the current context_.  In a sequent calculus,
    * the current context would be the assumption list.
    *
    * Note that Refine.check_rewrite has already been called at
    * this point.
    *)
   let simple_rewrite proc name redex contractum pf res =
      Rewrite { rw_name       = name;
                rw_redex      = redex;
                rw_contractum = contractum;
                rw_proof      = pf;
                rw_resources  = res
      }

   let cond_rewrite proc name params args pf res =
      let cvars = context_vars args in
      let params = extract_params cvars params in
      let args, redex, contractum = unzip_rewrite name args in
         CondRewrite { crw_name       = name;
                       crw_params     = params;
                       crw_assums       = args;
                       crw_redex      = redex;
                       crw_contractum = contractum;
                       crw_proof      = pf;
                       crw_resources  = res
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

   let input_form_command proc name = function
      MetaIff (MetaTheorem redex, MetaTheorem contractum) ->
         redex, contractum,
         InputForm { iform_name       = name;
                     iform_redex      = redex;
                     iform_contractum = contractum
         }
    | _ ->
         (* Conditional rewrite *)
         raise (RefineError ("input_form_command", StringError "input form must be a simple rewrite"))

   (*
    * Add the command and return the declaration.
    *)
   let declare_rewrite proc loc name params args pf res =
      try
         let cmd = rewrite_command proc name params args pf res in
            add_command proc (cmd, loc)
      with exn ->
         Ploc.raise loc exn

   let declare_input_form proc loc name mt =
      try
         let redex, contractum, cmd = input_form_command proc name mt in
            add_command proc (cmd, loc);
            redex, contractum
      with exn ->
         Ploc.raise loc exn

   (* Note that Refine.check_rule has already been issued *)
   let rule_command proc name params mt pf res =
      (* Extract context names *)
      let cvars = context_vars mt in
      let params = extract_params cvars params in
         Rule { rule_name      = name;
                rule_params    = params;
                rule_stmt      = mt;
                rule_proof     = pf;
                rule_resources = res
         }

   let declare_rule proc loc name args t pf res =
      try
         add_command proc (rule_command proc name args t pf res, loc)
      with
         exn ->
            Ploc.raise loc exn

   (*
    * Infix directive.
    *)
   let declare_gupd proc loc upd =
      add_command proc (MLGramUpd upd, loc);
      Infix.add upd

   (*
    * Declare an ML term rewrite.
    * There is no definition.
    *)
   let declare_mlrewrite proc loc mlname args t def resources =
      let cvars = context_vars (MetaTheorem t) in
      let params = extract_params cvars args in
         add_command proc (MLRewrite { mlterm_name = mlname;
                                       mlterm_params = params;
                                       mlterm_term = t;
                                       mlterm_def = def;
                                       mlterm_resources = resources
                                     }, loc)

   let declare_mlaxiom proc loc mlname args t def resources =
      let cvars = context_vars (MetaTheorem t) in
      let params = extract_params cvars args in
         add_command proc (MLAxiom { mlterm_name = mlname;
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
      add_command proc (Resource (name, r), loc)

   let improve_resource proc loc i =
      add_command proc (Improve i, loc)

   (*
    * Extract the options and return the mode paired with
    * the list of string defining the forms.
    *)
   let get_dfmode loc t =
      let mode = get_string_param loc t in
         if mode = "raw" then
            Ploc.raise loc (Invalid_argument "Attempts to refer to the built-in \"raw\" mode");
         mode

   let get_dform_options proc loc options =
      let rec compile_options = function
         hd :: tl ->
            begin
               let modes, except_modes, options = compile_options tl in
                  match Opname.dest_opname (opname_of_term hd) with
                     "parens" :: _ ->
                        modes, except_modes, DFormParens :: options
                   | "prec" :: _ ->
                        modes, except_modes, (DFormPrec (get_string_param loc hd)) :: options
                   | "mode" :: _ ->
                        get_dfmode loc hd :: modes, except_modes, options
                   | "except_mode" :: _ ->
                        modes, get_dfmode loc hd :: except_modes, options
                   | _ ->
                        Ploc.raise loc (Failure("warning: unknown display form option " ^ (SimplePrint.string_of_term hd)))
            end
       | [] ->
            [], [], []
      in
      match compile_options options with
         [], [], options -> Dform.AllModes, List.rev options
       | modes, [], options -> Dform.Modes modes, List.rev options
       | [], except_modes, options -> Dform.ExceptModes except_modes, List.rev options
       | _ -> Ploc.raise loc (Failure "Both \"mode\" and \"except_mode\" flags on the same display form")

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
         add_command proc (df, loc)

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
         if !debug_dform && modes = Dform.AllModes then
            eprintf "Warning: display form %s - no modes specified%t" name eflush;
         begin
            try
               ignore (term_rewrite Rewrite_sig.Relaxed empty_args_spec [t] [expansion])
            with
               exn ->
                  Ploc.raise loc exn
         end;
         add_command proc (DForm { dform_name = name;
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
      if (!debug_dform) && (modes=Dform.AllModes) then eprintf "Warning: ML display form %s - no modes specified%t" name eflush;
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
         add_command proc (DForm info, loc)

   (*
    * Precedence declaration.
    *)
   let declare_prec proc loc s =
(*
      if FilterCache.find_prec proc.cache s then
         Ploc.raise loc (Failure (sprintf "prec '%s' already declared" s));
*)
      add_command proc (Prec s, loc);
      FilterCache.add_prec proc.cache s

   (*
    * Precedence definition relation.
    *)
   let define_prec_rel proc loc s s' rel =
      if not (FilterCache.find_prec proc.cache s) then
         Ploc.raise loc (Failure (sprintf "prec '%s' not defined" s));
      if not (FilterCache.find_prec proc.cache s') then
         Ploc.raise loc (Failure (sprintf "prec '%s' not defined" s'));
      add_command proc (PrecRel { prec_rel = rel;
                                  prec_left = s;
                                  prec_right = s'
                                }, loc)

   (*
    * A toplevel declaration.
    *)
   let declare_topval proc loc item =
      add_command proc (ToploopItem item, loc)

   (*
    * A toplevel structured comment is converted to a term.
    *)
   let declare_comment proc loc t =
      add_command proc (Comment t, loc)

   (*
    * A magic block computes a hash value from the definitions
    * in the block.
    *)
   let define_magic_block proc loc name stmts =
      add_command proc (MagicBlock { magic_name = name; magic_code = stmts }, loc)

   (*
    * Input processor.
    *)
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
                     Ploc.raise loc (Failure "Input is not a .ml or .mli file")
            in
            let cache = FilterCache.create !include_path in
            let info = FilterCache.create_cache cache module_name select in
            (* Important: proc.name should be globlly unique *)
            let gensym_name =
               let suffix =
                  match select with
                     ImplementationType ->
                        "_ml"
                   | InterfaceType ->
                        "_mli"
               in
                  module_name ^ suffix
            in
            let proc =
               { cache    = info;
                 select   = select;
                 name     = module_name;
                 gensym   = gensym_name;
                 group    = theory_group ();
                 groupdsc = theory_groupdsc ();
                 names    = StringSet.empty;
                 parents  = StrLSet.empty;
                 infixes  = Infix.Set.empty;
               }
            in
               if select = ImplementationType then
                  FilterCache.load_sig_grammar info () InterfaceType;
               add_starts (FilterCache.get_start info);
               parsing_state := Some (FilterCache.get_parsing_state info);
               proc_ref := Some proc;
               proc

   let hash proc =
      FilterCache.hash proc.cache

   (*
    * Save the summary.
    *)
   let save proc suffix =
      FilterCache.save proc.cache () suffix

   let set_mode proc mode =
      FilterCache.set_mode proc.cache mode

   (*
    * Extract an item list.
    *)
   let extract sig_info proc =
      (Info.extract sig_info (FilterCache.info proc.cache) (**)
          (FilterCache.resources proc.cache)) (Filename.basename proc.name) proc.group proc.groupdsc

   (*
    * Check the implementation with its interface.
    *)
   let check proc alt_select =
      (*
       * Check that implementation matches interface.
       * This will also copy part of the interface into the implementation.
       *)
      let sig_info = FilterCache.check proc.cache alt_select in
      let _ =
         (* Read the comments *)
         FilterCache.parse_comments proc.cache TermGrammar.convert_comment;

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

   (************************************************************************
    * Grammar interface.
    *)

   (*
    * This function must guarantee global uniqueness, even across separate compilations
    * of separate (distinct) files.
    *)
   let gensym proc =
      Lm_symbol.new_symbol_string proc.gensym

   let add_token proc loc lexer_id s t =
      FilterCache.add_token proc.cache lexer_id (gensym proc) s t

   let add_token_pair proc loc lexer_id s1 s2 t =
      FilterCache.add_token_pair proc.cache lexer_id (gensym proc) s1 s2 t

   let add_production proc loc args opt_prec t =
      FilterCache.add_production proc.cache (gensym proc) args opt_prec t

   let input_prec proc loc assoc tl rel =
      let info = proc.cache in
      let pre =
         match rel with
            PrecEqual t2 ->
               FilterCache.find_input_prec info t2
          | PrecLessThan t2 ->
               FilterCache.input_prec_lt info t2 assoc
          | PrecGreaterThan t2 ->
               FilterCache.input_prec_gt info t2 assoc
          | PrecNone ->
               FilterCache.input_prec_new info assoc
      in
         List.iter (FilterCache.add_input_prec info pre) tl

   let add_parser proc loc t lexer_id =
      let shape = shape_of_term t in
      let name = fst (dst_opname (opname_of_term t)) in
         FilterCache.add_start proc.cache name t lexer_id;
         add_start name shape

   let add_iform proc loc redex contractum =
      FilterCache.add_iform proc.cache (gensym proc) redex contractum

   let compile_parser proc loc =
      FilterCache.compile_parser proc.cache

   let define_term proc loc shapeclass name ty_term contractum res opaque =
      let redex = term_of_ty ty_term in
      let term_def =
         { term_def_name = name;
           term_def_value = contractum;
           term_def_resources = res;
           term_def_opaque = opaque;
         }
      in
         if is_shape_iform shapeclass then
            add_iform proc loc redex contractum;
         add_command proc (DefineTerm (shapeclass, ty_term, term_def), loc)
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
   type resource = (ctyp, expr) resource_str
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
      let () =
         match cmd, extract with
            Rule r, Primitive extract ->
               let _, ext_args, _ = split_mfunction mterm in
               let addrs = collect_cvars r.rule_params in
                  Refine.check_prim_rule name addrs (collect_terms r.rule_params) (strip_mfunction mterm) ext_args extract
          | _ ->
               ()
      in
         StrFilter.add_command proc (cmd, loc)
   with
      exn ->
         Ploc.raise loc exn

let define_prim proc loc name params mterm extract res =
   define_rule proc loc name params mterm (Primitive extract) res

let define_thm proc loc name params mterm s res =
   let assums, goal = unzip_mfunction mterm in
   let assums = List.map (fun (_, _, assum) -> assum) assums in
   let mseq = mk_msequent goal assums in
   let proof = Proof.create_io_rulebox mseq s in
   let proof = Convert.of_raw () s proof in
      define_rule proc loc name params mterm (Interactive proof) res

let define_int_thm proc loc name params mterm res =
   define_rule proc loc name params mterm Incomplete res

(************************************************************************
 * QUOTATIONS                                                           *
 ************************************************************************)

open TermGrammar

(* Parse a term *)
let term_exp s =
   let cs = Stream.of_string s in
   let t = grammar_parse term_eoi cs in
      add_parsed_binding (BindTerm t)

let term_patt s =
   let cs = Stream.of_string s in
   let t = grammar_parse term_eoi cs in
   let t = TermGrammar.parse_term dummy_loc t in
      Filter_exn.print_exn Dform.null_base (Some "Can not build a pattern out of a term:\n") Filter_patt.build_term_patt t

let _ = Quotation.add "term" (Quotation.ExAst (term_exp, term_patt))
let _ = Quotation.default := "term"

(* Allow dforms too *)
let add_quot name check =
   let quot_exp s =
      let t = parse_quotation dummy_loc "term" name s in
         add_parsed_binding (BindTerm t)
   in
   let quot_patt s =
      let t = parse_quotation dummy_loc "term" name s in
      let t = unchecked_term_of_parsed_term dummy_loc t in
         Filter_exn.print_exn Dform.null_base (Some "Can not build a pattern out of a term:\n") Filter_patt.build_term_patt t
   in
      ignore (Quotation.add name (Quotation.ExAst (quot_exp, quot_patt)))

let () = add_quot "dform" true
let () = add_quot "doc" false
let () = add_quot "topdoc" false

let rec mk_list_expr _loc = function
   [] -> <:expr< [] >>
 | hd :: tl -> <:expr< $uid:"::"$ $hd$ $mk_list_expr _loc tl$ >>

let expr_of_bvar_con _loc = function
   ConBVarConst s -> <:expr< $str: s$ >>
 | ConBVarExpr e -> e

let expr_of_pcon _loc = function
   ConPStr s, shape ->
      let shape =
         match shape with
            ShapeString -> "String"
          | _ ->
               Ploc.raise _loc (Invalid_argument "\"con\" quotation: string constant parameter must be of string kind")
      in
         <:expr< Refiner.Refiner.Term.make_param (Term_sig.$uid:shape$ $str:s$) >>
 | ConPToken opname, shape ->
      let strings = Opname.dest_opname opname in
      let strings = List.map (fun s -> <:expr< $str: s$ >>) strings in
          <:expr< Refiner.Refiner.Term.make_param (Term_sig.Token (Opname.make_opname $mk_list_expr _loc strings$)) >>
 | ConPMeta l, ShapeLevel ->
      <:expr<
         Refiner.Refiner.Term.make_param
            (Term_sig.MLevel
               (Refiner.Refiner.TermMan.mk_var_level_exp $str:string_of_symbol l$))
      >>
 | ConPMeta s, shape ->
      let shape =
         match shape with
            ShapeString -> "MString"
          | ShapeNumber -> "MNumber"
          | ShapeToken -> "MToken"
          | ShapeShape -> "MShape"
          | ShapeOperator -> "MOperator"
          | _ -> Ploc.raise _loc (Invalid_argument "\"con\" quotation: unsupported meta-parameter")
      in
         <:expr< Refiner.Refiner.Term.make_param (Term_sig.$uid:shape$ $str:string_of_symbol s$) >>
 | ConPExpr e, shape ->
      let shape =
         match shape with
            ShapeString -> "String"
          | ShapeToken -> "Token"
          | ShapeNumber -> "Number"
          | ShapeLevel -> "Level"
          | ShapeVar -> "Var"
          | ShapeQuote -> "Quote"
          | ShapeShape -> "Shape"
          | ShapeOperator -> "Operator"
      in
         <:expr< Refiner.Refiner.Term.make_param (Term_sig.$uid:shape$ $e$) >>
 | ConPNum n, ShapeNumber ->
      <:expr< Refiner.Refiner.Term.make_param (Term_sig.Number $add_parsed_binding (BindNum n)$) >>
 | ConPInt e, ShapeNumber ->
      <:expr< Refiner.Refiner.Term.make_param (Term_sig.Number (Lm_num.num_of_int $e$)) >>
 | ConPNum _, _
 | ConPInt _, _ ->
      Ploc.raise _loc (Invalid_argument "\"con\" quotation: numeric parameter of non-numeric kind?")

let is_simp_bterm = function
   [], _ -> true
 | _ -> false

let rec expr_of_term_con _loc = function
   ConTerm t ->
      add_parsed_binding (BindTerm t)
 | ConExpr e ->
      e
 | ConVar v ->
      <:expr< Refiner.Refiner.Term.mk_var_term $v$ >>
 | ConConstruct (op, params, bterms) ->
      let op = add_parsed_binding (BindOpname op) in
         if params = [] && List.for_all is_simp_bterm bterms then
            let bterms = mk_list_expr _loc (List.map (fun (_, bt) -> expr_of_term_con _loc bt) bterms) in
               <:expr< Refiner.Refiner.Term.mk_simple_term $op$ $bterms$ >>
         else
            let bterms = mk_list_expr _loc (List.map (expr_of_bterm_con _loc) bterms) in
            let params = mk_list_expr _loc (List.map (expr_of_pcon _loc) params) in
               <:expr< Refiner.Refiner.Term.mk_term (Refiner.Refiner.Term.mk_op $op$ $params$) $bterms$ >>
 | ConSequent (arg, hyps, concl) ->
      let arg = expr_of_term_con _loc arg in
      let hyps = expr_of_hyps_con _loc hyps in
      let concl = expr_of_term_con _loc concl in
         <:expr< Refiner.Refiner.TermMan.mk_sequent_term (**)
                    { Refiner.Refiner.TermType.sequent_args = $arg$;
                      Refiner.Refiner.TermType.sequent_hyps = Refiner.Refiner.Term.SeqHyp.of_list $hyps$;
                      Refiner.Refiner.TermType.sequent_concl = $concl$
         } >>

and expr_of_bterm_con _loc (bvars, bt) =
   let bt = expr_of_term_con _loc bt in
      if bvars = [] then
         <:expr< Refiner.Refiner.Term.mk_simple_bterm $bt$ >>
      else
         let bvars = mk_list_expr _loc (List.map (expr_of_bvar_con _loc) bvars) in
            <:expr< Refiner.Refiner.Term.mk_bterm $bvars$ $bt$ >>

and expr_of_hyps_con _loc hyps =
   match hyps with
      [] ->
         <:expr< [] >>
    | hyp :: hyps ->
         let hyps = expr_of_hyps_con _loc hyps in
            match hyp with
               ConContext (v, args) ->
                  let args = mk_list_expr _loc (List.map (expr_of_term_con _loc) args) in
                  let e = <:expr< Context (Lm_symbol.add $v$, $args$) >> in
                     <:expr< [ $e$ :: $hyps$ ] >>
             | ConHypList l ->
                  <:expr< $l$ @ $hyps$ >>
             | ConHypothesis (v, t) ->
                  let e = <:expr< Hypothesis ($v$, $expr_of_term_con _loc t$) >> in
                     <:expr< [ $e$ :: $hyps$ ] >>

let con_exp s =
   let cs = Stream.of_string s in
   let con = grammar_parse term_con_eoi cs in
      expr_of_term_con dummy_loc con

let con_patt _ =
   raise (Invalid_argument "<:con< >> quotation can not be used where pattern is expected")

let _ = Quotation.add "con" (Quotation.ExAst (con_exp, con_patt))

(*
 * <:action< s >> is like <:con< s >>, but it is wrapped in
 * a (fun argv -> ...)
 *)
(* unused
let action_exp s =
   let e = con_exp s in
   let _loc = dummy_loc in
      <:expr< fun argv -> $e$ >>

let action_patt _ =
   raise (Invalid_argument "<:action< >> quotation can not be used where pattern is expected")

let _ = Quotation.add "action" (Quotation.ExAst (action_exp, action_patt))
*)

(*
 * Parsed the terms from the bindings.
 *)
let bind_item loc i =
   { item_item = i;
     item_bindings = get_checked_bindings loc
   }

(*
 * Replace a term with another.
 *)
(* unused
let subst_term term1 term2 t =
   let v = new_symbol_string "v" in
   let t = var_subst t term1 v in
      subst1 t v term2
*)

(* Convert the quotation *)
let parse_quote loc quote token_type bvar_type subterm_type term_type =
   let { ty_params = params;
         ty_bterms = bterms;
         ty_type   = ty;
         _
       } = quote
   in
   let parse_param param =
      match param with
         TyToken t ->
            TyToken (parse_quoted_term loc token_type bvar_type term_type t)
       | TyNumber
       | TyString
       | TyShape
       | TyOperator
       | TyLevel
       | TyVar
       | TyQuote as param ->
            param
   in
   let parse_bterm { ty_bvars = bvars; ty_bterm = term } =
      { ty_bvars = List.map (parse_quoted_term loc token_type bvar_type term_type) bvars;
        ty_bterm = parse_quoted_term loc token_type bvar_type subterm_type term
      }
   in
      { quote with ty_params = List.map parse_param params;
                   ty_bterms = List.map parse_bterm bterms;
                   ty_type   = parse_quoted_term loc token_type bvar_type term_type ty
      }

(* For term definitions, the default type is Term *)
let parse_declare_term loc quote =
   let quote = parse_quote loc quote term_type term_type term_type term_type in
      { quote with ty_term = quoted_term_of_parsed_term loc quote.ty_term }

(* For type definitions, the default type is Ty *)
let parse_declare_type loc quote =
   let quote = parse_quote loc quote term_type term_type term_type type_type in
      { quote with ty_term = quoted_term_of_parsed_term loc quote.ty_term }

(* For term definitions, the default type is Term *)
let parse_define_quote loc quote =
   parse_quote loc quote term_type term_type term_type term_type

let parse_define_redex loc quote =
   { quote with ty_term = quoted_term_of_parsed_term loc quote.ty_term }

let parse_define_term loc name shape_class quote contractum =
   let redex, contractum =
      if is_shape_iform shape_class then
         let mt = parse_iform loc name (MetaIff (MetaTheorem quote.ty_term, MetaTheorem contractum)) in
         let _, redex, contractum = unzip_rewrite name mt in
            redex, contractum
      else
         parse_define loc name quote.ty_term contractum
   in
      { quote with ty_term = redex }, contractum

(* Parse and check a rewrite definition *)
let parse_rewrite loc name mt tl rs =
   let mt, tl, f = TermGrammar.parse_rewrite loc name mt tl in
   let conv = function
      v, BindTerm t ->
         v, BindTerm (f t)
    | bnd ->
         bnd
   in
      mt, tl, { rs with item_bindings = List.map conv rs.item_bindings }

let parse_type_rewrite loc redex contractum =
   TermGrammar.parse_type_rewrite loc redex contractum

(* Don't require the two sides to have the same type *)
let parse_iform = TermGrammar.parse_iform

(* Convert contexts in meta-terms, terms args and resource term bindings *)
let parse_rule loc name mt tl rs =
   let cvars, mt, tl, f = TermGrammar.parse_rule loc name mt tl in
   let conv = function
      v, BindTerm t ->
         v, BindTerm (f t)
    | bnd ->
         bnd
   in
      cvars, mt, tl, { rs with item_bindings = List.map conv rs.item_bindings }

(* Same as parse_rule, but with extract term as well *)
let parse_rule_with_extract loc name mt tl rs extract =
   let _, mt, tl, f = TermGrammar.parse_rule loc name mt tl in
   let conv = function
      v, BindTerm t ->
         v, BindTerm (f t)
    | bnd ->
         bnd
   in
   let extract =
      (*
       * XXX HACK: we assume that extracts from sequents must be sequents of the same shape
       * And whenever users specify a non-sequent extract, they must be meaning to specify a
       * conclusion of a sequent.
       * There is a complimentary HACK in Term_grammar.create_meta_function
       *)
      let _, goal = unzip_mfunction mt in
         if is_sequent_term goal && not (is_sequent_term extract) then
            replace_concl goal extract
         else
            extract
   in
      mt, tl, { rs with item_bindings = List.map conv rs.item_bindings }, f extract

(* Display forms *)
let parse_dform loc options redex contractum =
   let options = List.map (unparsed_term_of_parsed_term loc) options in
   let redex, contractum = TermGrammar.parse_dform loc redex contractum in
      options, redex, contractum

let str_keyword kw loc =
   Ploc.raise loc (Invalid_argument
      ("Implementation keyword encountered where an interface one was expected: \"" ^ kw ^ "\""))

let sig_keyword kw loc =
   Ploc.raise loc (Invalid_argument
      ("Interface keyword encountered where an implementation one was expected: \"" ^ kw ^ "\""))

(************************************************************************
 * GRAMMAR EXTENSION                                                    *
 ************************************************************************)

(*
 * Empty items.
 *)
let empty_sig_item _loc =
   <:sig_item< declare $list:[]$ end >>

let empty_str_item _loc =
   <:str_item< declare $list:[]$ end >>

(*
 * Extend the programming language.
 *)
let _ =
   Grammar.Unsafe.clear_entry interf;
   Grammar.Unsafe.clear_entry implem


let is_operator =
   let ht = Hashtbl.create 73 in
   let ct = Hashtbl.create 73 in
   List.iter (function x -> Hashtbl.add ht x true)
      ["asr"; "land"; "lor"; "lsl"; "lsr"; "lxor"; "mod"; "or"];
   List.iter (function x -> Hashtbl.add ct x true)
      ['!'; '&'; '*'; '+'; '-'; '/'; ':'; '<'; '='; '>'; '@'; '^'; '|'; '~';
       '?'; '%'; '.'; '$'];
   function x ->
      try Hashtbl.find ht x with
      Not_found -> try Hashtbl.find ct x.[0] with _ -> false

let operator =
   Grammar.Entry.of_parser gram "operator"
      (function strm ->
         match Stream.npeek 2 strm with
            [("", s); ("", ")")] when is_operator s ->
               (Stream.junk strm;
                Stream.junk strm;
                s)
          | _ -> raise Stream.Failure)

EXTEND
   GLOBAL: interf implem sig_item str_item expr;

   interf:
      [[ interf_opening; st = LIST0 interf_item; EOI ->
          let f () =
             let proc = SigFilter.get_proc _loc in
             let id = SigFilter.hash proc in
                SigFilter.add_command proc (Id id, _loc);
                SigFilter.save proc AnySuffix;
                SigFilter.extract () proc
          in
             handle_exn f "interf" _loc, Some _loc
       ]];

   interf_opening:
      [[ OPT "PRL_interface" ->
          let f () =
             SigFilter.get_proc _loc
          in
             handle_exn f "interf_opening" _loc
       ]];

   interf_item:
      [[ s = sig_item; OPT ";;" ->
          let f () =
             let proc = SigFilter.get_proc _loc in
             let () =
                if !debug_filter_parse then
                   eprintf "Filter_parse.interf_item: adding item%t" eflush;
                if get_checked_bindings _loc <> [] then
                   Ploc.raise _loc (Invalid_argument "Filter_parse.interf_item: sig item has bindings");
                match s with
                   <:sig_item< declare $list: []$ end >> ->
                      ()
                 | _ ->
                      let item = SummaryItem { item_bindings = []; item_item = s } in
                         SigFilter.add_command proc (item, _loc)
             in
                s, _loc
           in
              handle_exn f "interf_item" _loc
       ]];

   implem:
      [[ implem_opening; st = LIST0 implem_item; EOI ->
          let f () =
             let proc = StrFilter.get_proc _loc in
             let interf = StrFilter.check proc () InterfaceType in
                StrFilter.set_mode proc InteractiveSummary;
                StrFilter.save proc (OnlySuffixes ["cmoz"]);
                StrFilter.extract interf proc
          in
             handle_exn f "implem" _loc, Some _loc
       ]];

   implem_opening:
      [[ OPT "PRL_implementation" ->
         let f () =
            StrFilter.get_proc _loc
         in
            handle_exn f "implem_opening" _loc
       ]];

   implem_item:
      [[ s = str_item; OPT ";;" ->
          let f () =
             let proc = StrFilter.get_proc _loc in
             let () =
                match s with
                   <:str_item< declare $list: []$ end >> ->
                      if get_checked_bindings _loc <> [] then
                         Ploc.raise _loc (Invalid_argument "Filter_parse.implem_item: empty str item has bindings")
                 | _ ->
                      StrFilter.add_command proc (SummaryItem (bind_item _loc s), _loc)
             in
                s, _loc
          in
             handle_exn f "implem_item" _loc
       ]];

   sig_item:
      [[ "extends"; path = mod_ident ->
          let f () =
             SigFilter.declare_parent (SigFilter.get_proc _loc) _loc path
          in
             handle_exn f "extends" _loc;
             empty_sig_item _loc
        | "derive"; path = mod_ident ->
          let f () =
             SigFilter.declare_parent (SigFilter.get_proc _loc) _loc path
          in
             handle_exn f "derive" _loc;
             empty_sig_item _loc

          (************************************************************************
           * Opname classes.
           *)
        | "declare"; "typeclass"; sc = shapeclasses; name = opname_name; typeclass_type = opt_typeclass_type; typeclass_parent = opt_typeclass_parent ->
          let f () =
             SigFilter.declare_typeclass (SigFilter.get_proc _loc) _loc sc name typeclass_type typeclass_parent
          in
             handle_exn f "declare-typeclass" _loc;
             empty_sig_item _loc
        | "declare"; "type"; sc = shapeclasses; quote = quote_term; ty_parent = opt_type_parent ->
          let f () =
             let quote = parse_declare_type _loc quote in
                SigFilter.declare_type (SigFilter.get_proc _loc) _loc sc quote ty_parent
          in
             handle_exn f "declare-type" _loc;
             empty_sig_item _loc
        | "declare"; "type"; sc = shapeclasses; quote = quote_term; ty_parent = opt_type_parent; "="; cases = declare_cases ->
          let f () =
             let quote = parse_declare_type _loc quote in
             let cases = List.map (parse_declare_term _loc) cases in
                SigFilter.declare_type_cases (SigFilter.get_proc _loc) _loc sc quote ty_parent cases
          in
             handle_exn f "declare-type-cases" _loc;
             empty_sig_item _loc
        | "declare"; sc = shapeclasses; quote = quote_term ->
          let f () =
             let quote = parse_declare_term _loc quote in
                SigFilter.declare_term (SigFilter.get_proc _loc) _loc sc quote
          in
             handle_exn f "declare" _loc;
             empty_sig_item _loc
        | "define"; sc = shapeclasses; name = LIDENT; res = optresources; ":"; quote = quote_term; "<-->"; def = term ->
          let f () =
             let proc = SigFilter.get_proc _loc in
             let quote = parse_define_quote _loc quote in
             let () = SigFilter.declare_define_term proc sc (parse_define_redex _loc quote) in
             let quote, def = parse_define_term _loc name sc quote def in
                SigFilter.define_term proc _loc sc name quote def res false
           in
              handle_exn f ("define " ^ name) _loc;
              empty_sig_item _loc
        | "declare"; "rewrite"; redex = term; "<-->"; contractum = term ->
          let f () =
             let redex, contractum = parse_type_rewrite _loc redex contractum in
                SigFilter.declare_type_rewrite (SigFilter.get_proc _loc) _loc redex contractum
          in
             handle_exn f "declare-rewrite" _loc;
             empty_sig_item _loc

          (************************************************************************
           * Standard forms.
           *)
        | "rewrite"; name = LIDENT; args = optarglist; ":"; t = mterm ->
           let f () =
              let proc = SigFilter.get_proc _loc in
              let t, args, _ = TermGrammar.parse_rewrite _loc name t args in
                 SigFilter.declare_rewrite proc _loc name args t () no_resources
           in
             handle_exn f ("rewrite " ^ name) _loc;
             empty_sig_item _loc
        | "iform"; name = LIDENT; ":"; t = mterm ->
           let f () =
              let proc = SigFilter.get_proc _loc in
              let t = parse_iform _loc name t in
              let redex, contractum = SigFilter.declare_input_form proc _loc name t in
                 SigFilter.add_iform proc _loc redex contractum
           in
              handle_exn f ("iform " ^ name) _loc;
              empty_sig_item _loc
        | "ml_rw"; name = LIDENT; args = optarglist; ":"; t = parsed_term ->
           let f () =
              let proc = SigFilter.get_proc _loc in
              let args = List.map (parse_term _loc) args in
                SigFilter.declare_mlrewrite proc _loc name args t None no_resources
           in
             handle_exn f ("ml_rw " ^ name) _loc;
             empty_sig_item _loc
        | "rule"; name = LIDENT; args = optarglist; ":"; mt = mterm ->
           let f () =
              let proc = SigFilter.get_proc _loc in
              let _, t, args, _ = TermGrammar.parse_rule _loc name mt args in
                 SigFilter.declare_rule proc _loc name args t () no_resources
           in
              handle_exn f ("rule " ^ name) _loc;
              empty_sig_item _loc
        | mlrule_keyword; name = LIDENT; args = optarglist; ":"; t = parsed_term ->
           let f () =
              let proc = SigFilter.get_proc _loc in
              let args = List.map (parse_term _loc) args in
                 SigFilter.declare_mlaxiom proc _loc name args t None no_resources
           in
              handle_exn f ("mlrule " ^ name) _loc;
              empty_sig_item _loc
        | "resource"; "("; input = ctyp; ","; output = ctyp; ")"; name = LIDENT ->
           let f () =
              let r =
                 { resource_input = input;
                   resource_output = output
                 }
              in
              let proc = SigFilter.get_proc _loc in
                 SigFilter.declare_resource proc _loc name r;
                 SigFilter.define_resource proc _loc name r
           in
              handle_exn f ("resource " ^ name) _loc;
              empty_sig_item _loc
        | "dform"; name = LIDENT; ":"; options = parsed_df_options ->
           let f () =
              let options', t = options in
                 SigFilter.declare_dform (SigFilter.get_proc _loc) _loc name options' t;
           in
              handle_exn f ("dform " ^ name) _loc;
              empty_sig_item _loc
        | "infix"; name = ident ->
           let f () =
              SigFilter.declare_gupd (SigFilter.get_proc _loc) _loc (Infix name)
           in
              handle_exn f ("infix " ^ name) _loc;
              empty_sig_item _loc
        | "suffix"; name = ident ->
           let f () =
              SigFilter.declare_gupd (SigFilter.get_proc _loc) _loc (Suffix name)
           in
              handle_exn f ("suffix " ^ name) _loc;
              empty_sig_item _loc
        | "prec"; name = LIDENT ->
           let f () =
              SigFilter.declare_prec (SigFilter.get_proc _loc) _loc name
           in
              handle_exn f ("prec " ^ name) _loc;
              empty_sig_item _loc
        | "topval"; name = LIDENT; ":"; t = ctyp ->
           let f () =
              SigFilter.declare_topval (SigFilter.get_proc _loc) _loc <:sig_item< value $name$ : $t$ >>
           in
              handle_exn f ("topval " ^ name) _loc;
              empty_sig_item _loc
        | "topval"; "("; name = operator; ")"; ":"; t = ctyp ->
           let f () =
              SigFilter.declare_topval (SigFilter.get_proc _loc) _loc <:sig_item< value $name$ : $t$ >>
           in
              handle_exn f ("topval " ^ name) _loc;
              empty_sig_item _loc
        | "doc"; doc_sig ->
          empty_sig_item _loc

          (* Grammar *)
        | "lex_token"; id = singleterm; ":"; regex = STRING; t = OPT token_expansion ->
          let f () =
             SigFilter.add_token (SigFilter.get_proc _loc) _loc (opname_of_term (parse_term _loc id.aterm)) regex t;
          in
             handle_exn f "lex_token" _loc;
             empty_sig_item _loc

        | "lex_token"; id = singleterm; ":"; regex1 = STRING; regex2 = STRING; t = OPT token_expansion ->
          let f () =
             SigFilter.add_token_pair (SigFilter.get_proc _loc) _loc (opname_of_term (parse_term _loc id.aterm)) regex1 regex2 t;
          in
             handle_exn f "lex_token pair" _loc;
             empty_sig_item _loc

        | "production"; t = term; opt_prec = OPT prec_term; "<--"; args = LIST0 term SEP ";" ->
          let f () =
             let args, t = parse_production _loc args t in
                SigFilter.add_production (SigFilter.get_proc _loc) _loc args opt_prec t
          in
             handle_exn f "production" _loc;
             empty_sig_item _loc

        | "lex_prec"; assoc = prec_declare; "["; args = LIST1 parsed_term SEP ";"; "]"; rel = prec_relation ->
          let f () =
             SigFilter.input_prec (SigFilter.get_proc _loc) _loc assoc args rel;
          in
             handle_exn f "lex_prec" _loc;
             empty_sig_item _loc

        | "parser"; t = term; ":"; id = parsed_term ->
          let f () =
             let t = unchecked_term_of_parsed_term _loc t in
                SigFilter.add_parser (SigFilter.get_proc _loc) _loc t (opname_of_term id);
          in
             handle_exn f "parser" _loc;
             empty_sig_item _loc

        | "GENGRAMMAR" ->
          let f () =
             SigFilter.compile_parser (SigFilter.get_proc _loc) _loc;
          in
             handle_exn f "GENGRAMMAR" _loc;
             empty_sig_item _loc

        | "prim" -> str_keyword "prim" _loc
        | "interactive" -> str_keyword "interactive" _loc
        | "prim_rw" -> str_keyword "prim_rw" _loc
        | "interactive_rw" -> str_keyword "interactive_rw" _loc
       ]];

   doc_sig:
      [[ q = QUOTATION ->
           let f () =
               match dest_quot q with
                  "doc", com ->
                     SigFilter.declare_comment (SigFilter.get_proc _loc) _loc (mk_string_term comment_string_op com)
                | (name, q) ->
                     let q = unchecked_term_of_parsed_term _loc (parse_quotation _loc "doc" name q) in
                        SigFilter.declare_comment (SigFilter.get_proc _loc) _loc q
           in
              handle_exn f "comment" _loc;
        | t = parsed_term ->
           SigFilter.declare_comment (SigFilter.get_proc _loc) _loc t
       ]];

   str_item:
      [[ "extends"; path = mod_ident ->
          let f () =
             StrFilter.declare_parent (StrFilter.get_proc _loc) _loc path
          in
             handle_exn f "extends" _loc;
             empty_str_item _loc
        | "derive"; path = mod_ident ->
          let f () =
             StrFilter.declare_parent (StrFilter.get_proc _loc) _loc path
          in
             handle_exn f "derive" _loc;
             empty_str_item _loc

          (************************************************************************
           * Opname classes.
           *)
        | "declare"; "typeclass"; sc = shapeclasses; name = opname_name; typeclass_type = opt_typeclass_type; typeclass_parent = opt_typeclass_parent ->
          let f () =
             StrFilter.declare_typeclass (StrFilter.get_proc _loc) _loc sc name typeclass_type typeclass_parent
          in
             handle_exn f "declare-typeclass" _loc;
             empty_str_item _loc
        | "declare"; "type"; sc = shapeclasses; quote = quote_term; ty_parent = opt_type_parent ->
          let f () =
             let quote = parse_declare_type _loc quote in
                StrFilter.declare_type (StrFilter.get_proc _loc) _loc sc quote ty_parent
          in
             handle_exn f "declare-type" _loc;
             empty_str_item _loc
        | "declare"; "type"; sc = shapeclasses; quote = quote_term; ty_parent = opt_type_parent; "="; cases = declare_cases ->
          let f () =
             let quote = parse_declare_type _loc quote in
             let cases = List.map (parse_declare_term _loc) cases in
                StrFilter.declare_type_cases (StrFilter.get_proc _loc) _loc sc quote ty_parent cases
          in
             handle_exn f "declare-type-cases" _loc;
             empty_str_item _loc
        | "declare"; sc = shapeclasses; quote = quote_term ->
          let f () =
             let quote = parse_declare_term _loc quote in
                StrFilter.declare_term (StrFilter.get_proc _loc) _loc sc quote
          in
             handle_exn f "declare" _loc;
             empty_str_item _loc
        | "define"; opaque = opaque_flag; sc = shapeclasses; name = LIDENT; res = optresources; ":"; quote = quote_term; "<-->"; def = term ->
          let f () =
             let proc = StrFilter.get_proc _loc in
             let quote = parse_define_quote _loc quote in
             let () = StrFilter.declare_define_term proc sc (parse_define_redex _loc quote) in
             let quote, def = parse_define_term _loc name sc quote def in
                StrFilter.define_term proc _loc sc name quote def res opaque
           in
              handle_exn f ("define " ^ name) _loc;
              empty_str_item _loc
        | "declare"; "rewrite"; redex = term; "<-->"; contractum = term ->
          let f () =
             let redex, contractum = parse_type_rewrite _loc redex contractum in
                StrFilter.declare_type_rewrite (StrFilter.get_proc _loc) _loc redex contractum
          in
             handle_exn f "declare-rewrite" _loc;
             empty_str_item _loc

          (************************************************************************
           * Standard grammar.
           *)
        | "prim_rw"; name = LIDENT; res = optresources; args = optarglist; ":"; t = mterm ->
           let f () =
              let proc = StrFilter.get_proc _loc in
              let t, args, res = parse_rewrite _loc name t args res in
                 StrFilter.declare_rewrite proc _loc name args t (Primitive xnil_term) res
           in
              handle_exn f ("prim_rw " ^ name) _loc;
              empty_str_item _loc
        | "iform"; name = LIDENT; ":"; t = mterm ->
          let f () =
              let proc = StrFilter.get_proc _loc in
              let t = parse_iform _loc name t in
              let redex, contractum =
                 StrFilter.declare_input_form (StrFilter.get_proc _loc) _loc name t
              in
                 StrFilter.add_iform proc _loc redex contractum
           in
              handle_exn f ("iform " ^ name) _loc;
              empty_str_item _loc
        | "interactive_rw"; name = LIDENT; res = optresources; args = optarglist; ":"; t = mterm ->
           let f () =
              let proc = StrFilter.get_proc _loc in
              let t, args, res = parse_rewrite _loc name t args res in
                 StrFilter.declare_rewrite proc _loc name args t Incomplete res
           in
              handle_exn f ("interactive_rw " ^ name) _loc;
              empty_str_item _loc
        | "derived_rw"; name = LIDENT; res = optresources; args = optarglist; ":"; t = mterm ->
           let f () =
              let proc = StrFilter.get_proc _loc in
              let t, args, res = parse_rewrite _loc name t args res in
                 StrFilter.declare_rewrite proc _loc name args t Incomplete res
           in
              handle_exn f ("derived_rw " ^ name) _loc;
              empty_str_item _loc
        | "thm_rw"; name = LIDENT; res = optresources; args = optarglist; ":"; t = mterm; "="; body = expr ->
           let f () =
              let proc = StrFilter.get_proc _loc in
              let t, args, res = parse_rewrite _loc name t args res in
                 StrFilter.declare_rewrite proc _loc name args t (Derived body) res
           in
              handle_exn f ("thm_rw " ^ name) _loc;
             empty_str_item _loc
        | "ml_rw"; name = LIDENT; res = optresources; args = optarglist; ":"; t = parsed_bound_term; "="; code = expr ->
           let f () =
              let proc = StrFilter.get_proc _loc in
              let args = List.map (parse_term _loc) args in
              let code = bind_item _loc (wrap_code _loc t.aname code) in
                 StrFilter.declare_mlrewrite proc _loc name args t.aterm (Some code) res
           in
              handle_exn f ("ml_rw " ^ name) _loc;
              empty_str_item _loc
        | "prim"; name = LIDENT; res = optresources; params = optarglist; ":"; body = rule_body ->
           let f () =
              let proc = StrFilter.get_proc _loc in
              let mt, extract = body in
              let mt, params, res, extract = parse_rule_with_extract _loc name mt params res extract in
                 define_prim proc _loc name params mt extract res
           in
              handle_exn f ("prim " ^ name) _loc;
              empty_str_item _loc
        | "thm"; name = LIDENT; res = optresources; params = optarglist; ":"; mt = bmterm; "="; tac = STRING ->
           let f () =
              let proc = StrFilter.get_proc _loc in
              let _, mt, params, res = parse_rule _loc name mt params res in
                 define_thm proc _loc name params mt tac res
           in
              handle_exn f ("thm " ^ name) _loc;
              empty_str_item _loc
        | "interactive"; name = LIDENT; res = optresources; params = optarglist; ":"; mt = bmterm ->
           let f () =
              let proc = StrFilter.get_proc _loc in
              let _, mt, params, res = parse_rule _loc name mt params res in
                 define_int_thm proc _loc name params mt res
           in
              handle_exn f ("interactive " ^ name) _loc;
              empty_str_item _loc
        | "derived"; name = LIDENT; res = optresources; params = optarglist; ":"; mt = bmterm ->
           let f () =
              let proc = StrFilter.get_proc _loc in
              let _, mt, params, res = parse_rule _loc name mt params res in
                 define_int_thm proc _loc name params mt res
           in
              handle_exn f ("derived " ^ name) _loc;
              empty_str_item _loc
        | mlrule_keyword; name = LIDENT; res = optresources; args = optarglist; ":"; t = parsed_bound_term; "="; code = expr ->
           let f () =
              let proc = StrFilter.get_proc _loc in
              let args = List.map (parse_term _loc) args in
              let code = bind_item _loc code in
                 StrFilter.declare_mlaxiom proc _loc name args t.aterm (Some code) res
           in
              handle_exn f ("mlrule " ^ name) _loc;
              empty_str_item _loc
        | "let"; "resource"; "("; inp = ctyp; ","; outp = ctyp; ")"; name = LIDENT; "="; code = expr ->
           let f () =
              StrFilter.define_resource (StrFilter.get_proc _loc) _loc name (**)
                 { res_input = inp; res_output = outp; res_body = code }
           in
              handle_exn f ("resource " ^ name) _loc;
              empty_str_item _loc
        | "let"; "resource"; flag = opt_pvt_flag; name = LIDENT; "+=" ; code = expr ->
           let f () =
              let proc = StrFilter.get_proc _loc in
                 StrFilter.improve_resource proc _loc (**)
                    { improve_name = name;
                      improve_flag = flag;
                      improve_expr = bind_item _loc code;
                    }
           in
              handle_exn f (name ^ "resource improvement") _loc;
              empty_str_item _loc
        | "dform"; name = LIDENT; ":"; options = df_options; "="; form = xdform ->
           let f () =
              let options, t = options in
              let options, redex, contractum = parse_dform _loc options t form in
                 StrFilter.define_dform (StrFilter.get_proc _loc) _loc name options redex contractum
           in
              handle_exn f ("dform " ^ name) _loc;
              empty_str_item _loc
        | "ml_dform"; name = LIDENT; ":"; options = parsed_df_options; format = LIDENT; buf = LIDENT; "="; code = expr ->
           let f () =
              let options', t = options in
              let proc = StrFilter.get_proc _loc in
                 StrFilter.define_ml_dform proc _loc name options' t format buf (bind_item _loc code)
           in
              handle_exn f ("ml_dform " ^ name) _loc;
              empty_str_item _loc
        | "infix"; name = ident ->
           let f () =
              StrFilter.declare_gupd (StrFilter.get_proc _loc) _loc (Infix name)
           in
              handle_exn f ("infix " ^ name) _loc;
              empty_str_item _loc
        | "suffix"; name = ident ->
           let f () =
              StrFilter.declare_gupd (StrFilter.get_proc _loc) _loc (Suffix name)
           in
              handle_exn f ("suffix " ^ name) _loc;
              empty_str_item _loc
        | "prec"; name = LIDENT ->
           let f () =
              StrFilter.declare_prec (StrFilter.get_proc _loc) _loc name
           in
              handle_exn f ("prec " ^ name) _loc;
              empty_str_item _loc
        | "prec"; name1 = LIDENT; "<"; name2 = LIDENT ->
           let f () =
              StrFilter.define_prec_rel (StrFilter.get_proc _loc) _loc name1 name2 LTRelation
           in
              handle_exn f "prec" _loc;
              empty_str_item _loc
        | "prec"; name1 = LIDENT; "="; name2 = LIDENT ->
           let f () =
              StrFilter.define_prec_rel (StrFilter.get_proc _loc) _loc name1 name2 EQRelation
           in
              handle_exn f "prec" _loc;
              empty_str_item _loc
        | "prec"; name1 = LIDENT; ">"; name2 = LIDENT ->
           let f () =
              StrFilter.define_prec_rel (StrFilter.get_proc _loc) _loc name1 name2 GTRelation
           in
              handle_exn f "prec" _loc;
              empty_str_item _loc
        | "magic_block"; name = LIDENT; "=";
          "struct"; st = LIST0 [ s = str_item; OPT ";;" -> s ]; "end" ->
           let f () =
              StrFilter.define_magic_block (StrFilter.get_proc _loc) _loc name st
           in
              handle_exn f "magic_block" _loc;
              empty_str_item _loc
        | "doc"; doc_str ->
           empty_str_item _loc

          (* Grammar *)
        | "lex_token"; id = singleterm; ":"; regex = STRING; t = OPT token_expansion ->
          let f () =
             StrFilter.add_token (StrFilter.get_proc _loc) _loc (opname_of_term (parse_term _loc id.aterm)) regex t;
          in
             handle_exn f "lex_token" _loc;
             empty_str_item _loc

        | "lex_token"; id = singleterm; ":"; regex1 = STRING; regex2 = STRING; t = OPT token_expansion ->
          let f () =
             StrFilter.add_token_pair (StrFilter.get_proc _loc) _loc (opname_of_term (parse_term _loc id.aterm)) regex1 regex2 t;
          in
             handle_exn f "lex_token pair" _loc;
             empty_str_item _loc

        | "production"; t = term; opt_prec = OPT prec_term; "<--"; args = LIST0 term SEP ";" ->
          let f () =
             let args, t = parse_production _loc args t in
                StrFilter.add_production (StrFilter.get_proc _loc) _loc args opt_prec t
          in
             handle_exn f "production" _loc;
             empty_str_item _loc

        | "lex_prec"; assoc = prec_declare; "["; args = LIST1 parsed_term SEP ";"; "]"; rel = prec_relation ->
          let f () =
             StrFilter.input_prec (StrFilter.get_proc _loc) _loc assoc args rel;
          in
             handle_exn f "lex_prec" _loc;
             empty_str_item _loc

        | "parser"; t = term; ":"; id = parsed_term ->
          let f () =
             let t = unchecked_term_of_parsed_term _loc t in
                StrFilter.add_parser (StrFilter.get_proc _loc) _loc t (opname_of_term id);
          in
             handle_exn f "parser" _loc;
             empty_str_item _loc

        | "GENGRAMMAR" ->
          let f () =
             StrFilter.compile_parser (StrFilter.get_proc _loc) _loc;
          in
             handle_exn f "GENGRAMMAR" _loc;
             empty_str_item _loc

        | "rule" -> sig_keyword "rule" _loc
        | "rewrite" -> sig_keyword "rewrite" _loc
        | "topval" -> sig_keyword "topval" _loc
      ]];

    opt_pvt_flag:
       [[ flag = OPT pvt_flag ->
             match flag with Some flag -> flag | None -> Public
       ]];

    pvt_flag:
       [[ "public" -> Public | "private" -> Private ]];

    declare_cases:
      [[ cases = LIST1 quote_term SEP "|" ->
          cases
       ]];

    opt_type_parent:
      [[ ty_parent = OPT type_parent ->
          match ty_parent with
             Some opname ->
                opname
           | None ->
                term_opname
       ]];

    type_parent:
      [[ "->"; opname = opname ->
          opname
       ]];

    opt_typeclass_type:
      [[ opname = OPT typeclass_type ->
          match opname with
             Some opname ->
                opname
           | None ->
                type_opname
       ]];

    typeclass_type:
      [[ ":"; opname = opname ->
          opname
       ]];

    opt_typeclass_parent:
      [[ t = OPT typeclass_parent ->
          match t with
             Some kind ->
                kind
           | None ->
                ParentNone
       ]];

    typeclass_parent:
      [[ "->"; t = singleterm ->
          ParentExtends (opname_of_term (parse_term _loc t.aterm))
        | "<-"; t = singleterm ->
          ParentInclude (opname_of_term (parse_term _loc t.aterm))
       ]];

    doc_str:
       [[ q = QUOTATION ->
           let f () =
               match dest_quot q with
                  (* XXX HACK: this makes sure parsing of top-level "doc" quotations is lazy *)
                  "doc", com ->
                     StrFilter.declare_comment (StrFilter.get_proc _loc) _loc (mk_string_term comment_string_op com)
                | (name, q) ->
                     let q = unchecked_term_of_parsed_term _loc (parse_quotation _loc "doc" name q) in
                        StrFilter.declare_comment (StrFilter.get_proc _loc) _loc q
           in
              handle_exn f "comment" _loc
        | t = parsed_term ->
           StrFilter.declare_comment (StrFilter.get_proc _loc) _loc t
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

   updresource:
      [[ flag = opt_pvt_flag; e = expr LEVEL "expr1" ->
            let rec split_application tl expr =
               match expr with
                  <:expr< $e1$ $e2$ >> ->
                     split_application (e2 :: tl) e1
                | <:expr< $lid:name$ >> ->
                     { res_loc = _loc; res_name = name; res_flag = flag; res_args = tl }
                | _ ->
                     Ploc.raise (MLast.loc_of_expr expr) (Failure "resource is not an application")
            in
               split_application [] e
      ]];

   updresources:
      [[ "{|"; res = LIST1 updresource SEP ";" ; "|}" ->
         { item_item = res; item_bindings = get_unchecked_bindings () }
      ]];

   mlrule_keyword:
      [[ "ml_rule" | "ml_axiom" -> () ]];

   rule_body:
      [[ mt = bmterm; "="; extract = term ->
            mt, raw_input_term_of_parsed_term extract
       | mt = bmterm ->
            try
               mt, mk_simple_term ((TermGrammarBefore.parsing_state _loc).mk_opname_kind _loc NormalKind ["default_extract"] [] []) []
            with
               Ploc.Exc (_, Failure _) ->
                  Ploc.raise _loc (Failure "No computational witness (\"extract\") specified for a prim rule and the default_extract{} opname is not declared")
      ]];

   (*
    * Precedence option.
    *)
   token_expansion:
      [[ "-->"; t = parsed_term -> t ]];

   prec_term:
      [[ "%"; "prec"; t = parsed_term -> t ]];

   prec_declare:
      [[ LIDENT "left" -> Filter_grammar.LeftAssoc
       | LIDENT "right" -> Filter_grammar.RightAssoc
       | LIDENT "nonassoc" -> Filter_grammar.NonAssoc
      ]];

   prec_relation:
      [[ "<"; t = parsed_term -> PrecLessThan t
       | "="; t = parsed_term -> PrecEqual t
       | ">"; t = parsed_term -> PrecGreaterThan t
       | -> PrecNone
      ]];

   (*
    * DISPLAY FORMS.
    *)
   parsed_df_options:
      [[ l = LIST1 singleterm SEP "::" ->
          Lm_list_util.split_last (List.map (function { aterm = t; _ } -> parse_term _loc t) l)
       ]];

   df_options:
      [[ l = LIST1 singleterm SEP "::" ->
          Lm_list_util.split_last (List.map (function { aterm = t; _ } -> t) l)
       ]];

   (*
    * Shapeclass.
    *)
   shapeclasses:
      [[ sc = LIST0 shapeclass ->
            List.fold_left shape_combine shape_normal sc
      ]];

   shapeclass:
      [[ "iform" -> shape_iform
       | LIDENT "const" -> shape_const
      ]];

   opaque_flag:
      [[ opaque = OPT "opaque" ->
            match opaque with Some _ -> true | None -> false
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
    * Add a location identifier.
    *)
   expr:
      [[ "LOCATION" ->
            expr_of_loc _loc
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
 * End:
 * -*-
 *)
