(*
 * Filter an ML file.
 *
 * The grammar of OCaml is extended to include Nuprl-Light commands.
 * This file contains all of the extensions.
 *
 * Bogus note: whenever a Nuprl-Light term is read, a placeholder
 * is returned to the parser for the item that was read.  We keep track
 * of a list of Nuprl commands in Filter-Cache.  After the entire file
 * is read, we postprocess the parse tree, and remove these Nuprl
 * annotations.
 *
 * The reason for this is that we also want to capture all the normal
 * OCaml syntax as Nuprl commands, but Camlp4 does not let us do this
 * easily.
 *)
open Printf
open Pcaml

open Debug
open Term
open Term_util
open Rewrite
open Refine
open Precedence
open Simple_print

open Infix
open Free_vars
open Term_grammar
open Filter_type
open Filter_util
open Filter_ast
open Filter_summary
open Filter_summary_type
open Filter_summary_util
open Filter_cache
open Filter_prog

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
   ref ((fun _ -> raise (Failure "Filter_parse.mk_opname is unititialized"))
        : string list -> Opname.opname)

(*
 * Base term grammar.
 *)
module TermGrammarBefore : TermGrammarSig =
struct
   let mk_opname loc l =
      try !mk_opname_ref l with
         exn ->
            Stdpp.raise_with_loc loc exn
   
   (*
    * Term grammar.
    *)
   let gram = Pcaml.gram
   let term_eoi = Grammar.Entry.create gram "term"
   let term = Grammar.Entry.create gram "term"
   let quote_term = Grammar.Entry.create gram "quote_term"
   let mterm = Grammar.Entry.create gram "mterm"
   let singleterm = Grammar.Entry.create gram "singleterm"
   let bound_term = Grammar.Entry.create gram "bound_term"
   let xdform = Grammar.Entry.create gram "xdform"
end

(*
 * Extended term grammar.
 *)
module TermGrammar = MakeTermGrammar (TermGrammarBefore)
open TermGrammar

(*
 * Show the file loading.
 *)
let _ =
   if !debug_load then
      eprintf "Loading Filter_parse%t" eflush


(************************************************************************
 * QUOTATIONS                                                           *
 ************************************************************************)

(*
 * String -> string translator.
 *)
let term_exp s =
   let cs = Stream.of_string s in
   let t = Grammar.Entry.parse TermGrammar.term_eoi cs in
      build_ml_term (0, 0) t

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
   
let contractum_exp patt s =
   if !contract_flag then
      let cs = Stream.of_string s in
      let t = Grammar.Entry.parse TermGrammar.term_eoi cs in
      let index = List.length !contracta in
         contracta := t :: !contracta;
         sprintf "(Rewrite.make_contractum contractum_%d %s)" index stack_id
   else
      Stdpp.raise_with_loc (0, String.length s) (Failure "not in a rewrite block")

let _ = Quotation.add "term" (Quotation.ExAst (term_exp, term_patt))
let _ = Quotation.add "con" (Quotation.ExStr contractum_exp)
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
 * There should be only one param, of String type.
 * Get it. 
 *)
let get_string_param loc t =
   let { term_op = op } = dest_term t in
      match dest_op op with 
         { op_params = [param] } ->
            begin
               match dest_param param with
                  String s ->
                     s
                | _ ->
                     Stdpp.raise_with_loc loc (Term.TermMatch ("get_string_param", t, "param type"))
            end
       | { op_params = [] } ->
            Stdpp.raise_with_loc loc (Term.TermMatch ("get_string_param", t, "no params"))
       | _ ->
            Stdpp.raise_with_loc loc (Term.TermMatch ("get_string_param", t, "too many params"))

(************************************************************************
 * GENERIC CONSTRUCTION                                                 *
 ************************************************************************)

(*
 * Need some info about types and extraction.
 *)
module type FilterInfoSig =
sig
   type proof
   type expr
   type ctyp
   type item

   val extract : (proof, ctyp, expr, item) module_info ->
      (module_path * ctyp resource_info) list ->
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
    with type select    = select_type) =
struct
   (*
    * Processors include both the cache and the name of the module.
    *)
   type t =
      { cache : FilterCache.info;
        name : string
      }

   (*
    * When a module is inlined, add the resources and infixes.
    *)
   let inline_hook root_path cache (path, info) (paths, resources) =
      (* Include all the resources *)
      if !debug_resource then
         eprintf "Inline_hook: %s, %s%t" (string_of_path root_path) (string_of_path path) eflush;
      let add_resource rsrc =
         if !debug_resource then
            eprintf "Adding resource: %s.%s%t" (string_of_path path) rsrc.resource_name eflush;
         FilterCache.add_resource cache path rsrc
      in
      let nresources, nresources' =
         (*
          * nresources: all the resources
          * nresources': just the new ones
          *)
         let rec collect nresources nresources' = function
            rsrc::tl ->
               if mem_resource rsrc nresources then
                  collect nresources nresources' tl
               else
                  collect (rsrc :: nresources) (rsrc :: nresources') tl
          | [] ->
               nresources, nresources'
         in
            collect resources [] (get_resources info)
      in
         List.iter add_resource nresources';
         
         (* Add all the infix words *)
         List.iter add_infix (get_infixes info);
         
         (* Add the path to the list of parents *)
         path :: paths, nresources
      
   (*
    * Include a parent.
    * This performs the following tasks:
    *    1. incorporates the parents:
    *       a. adds the resources
    *       b. adds the infix directives.
    *)
   let declare_parent proc loc path =
      (* Lots of errors can occur here *)
      let _, (opens, nresources) = FilterCache.inline_module proc.cache path (inline_hook path) ([], []) in
      let info =
         { parent_name = path;
           parent_opens = opens;
           parent_resources = nresources
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
         FilterCache.rm_opname proc.cache s;
         FilterCache.add_opname proc.cache s opname';
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
   let simple_rewrite proc name redex contractum pf =
      (* Check that rewrite will succeed *)
      Refiner.check_rewrite name [||] [] [] redex contractum;
   
      (* Construct the command *)
      Rewrite { rw_name = name;
                rw_redex = redex;
                rw_contractum = contractum;
                rw_proof = pf
      }
   
   let cond_rewrite proc name params args pf =
      (* Print the type to the .mli file *)
      let cvars = context_vars args in
      let bvars = binding_vars args in
      let params' = extract_params cvars bvars params in
      let args', redex, contractum = unzip_rewrite name args in
         (* Check the rewrite *)
         Refiner.check_rewrite (**)
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
                       crw_proof = pf
         }
   
   (*
    * Compile the rewrite.
    *)
   let rewrite_command proc name params args pf =
      match params, args with
         [], MetaIff (MetaTheorem redex, MetaTheorem contractum) ->
            (* This is a simple rewrite *)
            simple_rewrite proc name redex contractum pf
       | _ ->
            (* Conditional rewrite *)
            cond_rewrite proc name params args pf
   
   (*
    * Add the command and return the declaration.
    *)
   let declare_rewrite proc loc name params args pf =
      let cmd = rewrite_command proc name params args pf in
         FilterCache.add_command proc.cache (cmd, loc)
   
   (*
    * Declare a term, and define a rewrite in one step.
    *)
   let define_term proc loc name redex contractum pf =
      let redex' = declare_term proc loc redex in
         declare_rewrite proc loc name [] (MetaIff (MetaTheorem redex', MetaTheorem contractum)) pf
   
   (*
    * Declare an axiom in an interface.  This has a similar flavor
    * as rewrites, but context args have to be extracted from the args.
    *)
   let simple_axiom proc name arg pf =
      (* Check it *)
      Refiner.check_axiom arg;
   
      (* Save it in the transcript *)
      Axiom { axiom_name = name; axiom_stmt = arg; axiom_proof = pf }
   
   let rec print_terms out = function
      h::t ->
         eprintf "\t%s\n" (string_of_term h);
         print_terms out t
    | [] ->
         flush stderr
   
   let rec print_vterms out = function
      (Some v, h)::t ->
         eprintf "\t%s. %s\n" (string_of_term v) (string_of_term h);
         print_vterms out t
    | (None, h)::t ->
         eprintf "\t%s\n" (string_of_term h);
         print_vterms out t
    | [] ->
         flush stderr
   
   let print_non_vars out params =
      print_terms out (collect_non_vars params)
      
   let cond_axiom proc name params args pf =
      (* Extract context names *)
      let cvars = context_vars args in
      let bvars = binding_vars args in
      let params' = extract_params cvars bvars params in
         (* Do some checking on the rule *)
         if !debug_grammar then
            begin
               let args, result = unzip_mfunction args in
                  eprintf "Checking rule: %s\n" name;
                  eprintf "Non vars:\n%a" print_non_vars params';
                  eprintf "Args:\n%a --> %s\n" print_vterms args (string_of_term result)
            end;
         Refiner.check_rule (**)
            name
            (Array.of_list (collect_cvars params'))
            (Array.of_list (collect_vars params'))
            (collect_non_vars params')
            (strip_mfunction args);
         if !debug_grammar then
            eprintf "Checked rule: %s%t" name eflush;
   
         (* If checking completes, add the rule *)
         Rule { rule_name = name;
                rule_params = params';
                rule_stmt = args;
                rule_proof = pf
         }
   
   let axiom_command proc name params args pf =
      match params, args with
         [], MetaTheorem a ->
            simple_axiom proc name a pf
       | _ ->
            cond_axiom proc name params args pf
         
   let declare_axiom proc loc name params args pf =
      let cmd = axiom_command proc name params args pf in
         FilterCache.add_command proc.cache (cmd, loc)
   
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
   let declare_mlterm proc loc ((name, _, _) as t) def =
      let t' = declare_term proc loc t in
         FilterCache.add_command proc.cache (MLTerm { mlterm_term = t';
                                                      mlterm_contracta = end_rewrite ();
                                                      mlterm_def = def
                                             }, loc)
   
   (*
    * Declare a condition term for a rule.
    *)
   let declare_ml_condition proc loc ((name, _, _) as t) =
      let t' = declare_term proc loc t in
         FilterCache.add_command proc.cache (Condition { mlterm_term = t';
                                                         mlterm_contracta = end_rewrite ();
                                                         mlterm_def = None
                                             }, loc)
   
   (*
    * Record a resource.
    *
    * type resource_name
    *)
   let declare_resource proc loc r =
      FilterCache.add_command proc.cache (Resource r, loc);
      FilterCache.add_resource proc.cache [] r
   
   (*
    * Extract the options and return the mode paired with
    * the list of string defining the forms.
    *)
   let get_dform_options proc loc options =
      let rec compile_options = function
         hd::tl ->
            begin
               let modes, options = compile_options tl in
                  match Opname.dest_opname (opname_of_term hd) with
                     "parens" :: _ ->
                        modes, DFormParens :: options
                   | "prec" :: _ ->
                        modes, (DFormPrec (get_string_param loc hd)) :: options
                   | "inherit" :: _ ->
                        modes, DFormParens :: options
                   | "mode" :: _ ->
                        (get_string_param loc hd)::modes, options
                   | _ ->
                        eprintf "warning: unknown option %s%t" (string_of_term hd) eflush;
                        modes, options
            end
       | [] ->
            [], []
      in
      let modes, options = compile_options options in
      let modes' =
         if modes = [] then
            ["all"]
         else
            modes
      in
         modes, List.rev options
            
   (*
    * Dform declaration.
    *)
   let declare_dform proc loc options t =
      let modes, options' = get_dform_options proc loc options in
      let df =
         DForm { dform_modes = modes;
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
   let define_dform proc loc options t expansion =
      let modes, options' = get_dform_options proc loc options in
         FilterCache.add_command proc.cache (DForm { dform_modes = modes;
                                                     dform_options = options';
                                                     dform_redex = t;
                                                     dform_def = TermDForm expansion
                                             }, loc)
   
   (*
    * An ml dterm is a display form that is computed in ML.
    *
    * Within the body, terms may expand to contracta.
    *)
   let define_ml_dform proc loc options t printer buffer code =
      let modes, options' = get_dform_options proc loc options in
      let ml_def =
         { dform_ml_printer = printer;
           dform_ml_buffer = buffer;
           dform_ml_contracta = end_rewrite ();
           dform_ml_code = code
         }
      in
      let info =
         { dform_modes = modes;
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
      if FilterCache.find_prec proc.cache s then
         Stdpp.raise_with_loc loc (Failure (sprintf "prec '%s' already declared" s));
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
   let save proc =
      FilterCache.save proc.cache

   (*
    * Extract an item list.
    *)
   let extract proc =
      Info.extract (FilterCache.info proc.cache) (FilterCache.resources proc.cache) proc.name
   
   (*
    * Check the implementation with its interface.
    *)
   let check proc alt_select =
      FilterCache.check proc.cache alt_select
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

   let extract = extract_sig
end

module StrFilterInfo =
struct
   type proof = proof_type
   type expr  = MLast.expr
   type ctyp  = MLast.ctyp
   type item  = MLast.str_item

   let extract = extract_str
end

module SigFilter = MakeFilter (SigFilterInfo) (SigFilterCache)
module StrFilter = MakeFilter (StrFilterInfo) (StrFilterCache)
                   
(************************************************************************
 * DEFINITION COMMANDS                                                  *
 ************************************************************************)

(*
 * A primitive rule specifies the extract.
 *)
let define_rule proc loc name
    (params : term list)
    (args : aterm list)
    (goal : term)
    (extract : proof_type) =
   let avars = collect_anames args in
   let assums = List.map (function { aname = name; aterm = t } -> name, t) args in
   let mterm = zip_mfunction assums goal in
   let cmd = StrFilter.axiom_command proc name params mterm extract in
      StrFilter.add_command proc (cmd, loc)
   
let define_prim proc loc name params args goal extract =
   define_rule proc loc name params args goal (Primitive extract)
   
let define_thm proc loc name params args goal tac =
   define_rule proc loc name params args goal (Derived tac)
   
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

EXTEND
   GLOBAL: interf implem sig_item str_item expr;

   interf:
      [[ interf_opening; st = LIST0 interf_item; EOI ->
          let proc = SigFilter.get_proc loc in
          let id = Hashtbl.hash proc in
             SigFilter.add_command proc (Id id, (0, 0));
             SigFilter.save proc;
             SigFilter.extract proc
       ]];
   
   interf_opening:
      [[ OPT "PRL_interface" ->
          SigFilter.get_proc loc
       ]];
   
   interf_item:
      [[ s = sig_item; OPT ";;" ->
          if !debug_filter_parse then
             eprintf "Filter_parse.interf_item: adding item%t" eflush;
          SigFilter.add_command (SigFilter.get_proc loc) (SummaryItem s, loc);
          s, loc
       ]];
   
   implem:
      [[ implem_opening; st = LIST0 implem_item; EOI ->
          let proc = StrFilter.get_proc loc in
             StrFilter.save proc;
             StrFilter.check proc InterfaceType;
             StrFilter.extract proc
       ]];
   
   implem_opening:
      [[ OPT "PRL_implementation" ->
          StrFilter.get_proc loc
       ]];
   
   implem_item:
      [[ s = str_item; OPT ";;" ->
          StrFilter.add_command (StrFilter.get_proc loc) (SummaryItem s, loc);
          s, loc
       ]];

   sig_item:
      [[ "include"; path = mod_ident ->
          SigFilter.declare_parent (SigFilter.get_proc loc) loc path;
          empty_sig_item loc
        | "declare"; t = quote_term ->
          SigFilter.declare_term (SigFilter.get_proc loc) loc t;
          empty_sig_item loc
        | "define"; name = LIDENT; ":"; t = quote_term; "<-->"; def = term ->
          SigFilter.define_term (SigFilter.get_proc loc) loc name t def ();
          empty_sig_item loc
        | "rewrite"; name = LIDENT; args = optarglist; ":"; t = mterm ->
          SigFilter.declare_rewrite (SigFilter.get_proc loc) loc name args t ();
          empty_sig_item loc
        | "axiom"; name = LIDENT; args = optarglist; ":"; t = mterm ->
          SigFilter.declare_axiom (SigFilter.get_proc loc) loc name args t ();
          empty_sig_item loc
        | "mlterm"; t = quote_term ->
          SigFilter.declare_mlterm (SigFilter.get_proc loc) loc t None;
          empty_sig_item loc
        | "mlcondition"; t = quote_term ->
          SigFilter.declare_ml_condition (SigFilter.get_proc loc) loc t;
          empty_sig_item loc
        | "resource"; "("; improve = ctyp; ","; extract = ctyp; ","; data = ctyp; ")"; name = LIDENT ->
          SigFilter.declare_resource (SigFilter.get_proc loc) loc (**)
             { resource_name = name;
               resource_extract_type = extract;
               resource_improve_type = improve;
               resource_data_type = data
             };
          empty_sig_item loc
        | "dform"; options = df_options ->
          let options', t = options in
             SigFilter.declare_dform (SigFilter.get_proc loc) loc options' t;
             empty_sig_item loc
        | "infix"; name = ident ->
          SigFilter.declare_infix (SigFilter.get_proc loc) loc name;
          empty_sig_item loc
        | "prec"; name = LIDENT ->
          SigFilter.declare_prec (SigFilter.get_proc loc) loc name;
          empty_sig_item loc
       ]];
   
   str_item:
      [[ "include"; path = mod_ident ->
          StrFilter.declare_parent (StrFilter.get_proc loc) loc path;
          empty_str_item loc
        | "declare"; t = quote_term ->
          StrFilter.declare_term (StrFilter.get_proc loc) loc t;
          empty_str_item loc
        | "primrw"; name = LIDENT; args = optarglist; ":"; t = mterm ->
          StrFilter.declare_rewrite (StrFilter.get_proc loc) loc name args t (Primitive xnil_term);
          empty_str_item loc
        | "rwthm"; name = LIDENT; args = optarglist; ":"; t = mterm; "="; body = expr ->
          StrFilter.declare_rewrite (StrFilter.get_proc loc) loc name args t (Derived body);
          empty_str_item loc
        | "prim"; name = LIDENT; params = optarglist; ":"; (**)
             (args, goal) = opt_binding_arglist; "="; (**)
             extract = term ->
          define_prim (StrFilter.get_proc loc) loc name params args goal.aterm extract;
          empty_str_item loc
        | "thm"; name = LIDENT; params = optarglist; ":"; (**)
             (args, goal) = opt_binding_arglist; "="; tac = expr ->
          define_thm (StrFilter.get_proc loc) loc name params args goal.aterm tac;
          empty_str_item loc
        | "mlterm"; t = quote_term; rewrite_equal; code = expr; "|"; ext = expr ->
          StrFilter.declare_mlterm (StrFilter.get_proc loc) loc t (Some (code, ext));
          empty_str_item loc
        | "resource"; "("; improve = ctyp; ","; extract = ctyp; ","; data = ctyp; ")"; name = LIDENT ->
          StrFilter.declare_resource (StrFilter.get_proc loc) loc (**)
             { resource_name = name;
               resource_extract_type = extract;
               resource_improve_type = improve;
               resource_data_type = data
             };
          empty_str_item loc
        | "dform"; options = df_options; "="; form = xdform ->
          let options', t = options in
             StrFilter.define_dform (StrFilter.get_proc loc) loc options' t form;
             empty_str_item loc
        | "mldform"; options = df_options; buf = LIDENT; format = LIDENT; "="; code = expr ->
          let options', t = options in
             StrFilter.define_ml_dform (StrFilter.get_proc loc) loc options' t buf format code;
             empty_str_item loc
        | "infix"; name = ident ->
          StrFilter.declare_infix (StrFilter.get_proc loc) loc name;
          empty_str_item loc
        | "prec"; name = LIDENT ->
          StrFilter.declare_prec (StrFilter.get_proc loc) loc name;
          empty_str_item loc
        | "prec"; name1 = LIDENT; "<"; name2 = LIDENT ->
          StrFilter.define_prec_rel (StrFilter.get_proc loc) loc name1 name2 LTRelation;
          empty_str_item loc
        | "prec"; name1 = LIDENT; "="; name2 = LIDENT ->
          StrFilter.define_prec_rel (StrFilter.get_proc loc) loc name1 name2 EQRelation;
          empty_str_item loc
        | "prec"; name1 = LIDENT; ">"; name2 = LIDENT ->
          StrFilter.define_prec_rel (StrFilter.get_proc loc) loc name1 name2 GTRelation;
          empty_str_item loc
        | "magic_block"; name = LIDENT; "=";
          "struct"; st = LIST0 [ s = str_item; OPT ";;" -> s ]; "end" ->
          StrFilter.define_magic_block (StrFilter.get_proc loc) loc name st;
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
   
   opt_binding_arglist:
      [[ ":"; goal = singleterm ->
          [], goal
        | arg = bound_term; ":"; goal = singleterm ->
          [arg], goal
        | arg = bound_term; "-->"; args = LIST0 bound_term SEP "-->" ->
          List_util.split_last (arg :: args)
        | arg = bound_term; args = LIST0 bound_term; ":"; goal = singleterm ->
          arg :: args, goal
       ]];
   
   (*
    * Equality beginning a rewrite block.
    *)
   rewrite_equal:
      [[ "=" ->
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
    * Pre-add some infix operators.
    *)
   expr: AFTER "expr1" (**)
      [LEFTA
       [ t1 = expr; op = "THEN"; t2 = expr ->
          make_infix loc op t1 t2
        | t1 = expr; op = "THENL"; t2 = expr ->
          make_infix loc op t1 t2
        | t1 = expr; op = "orelseT"; t2 = expr ->
          make_infix loc op t1 t2
        | t1 = expr; op = "andalsoT"; t2 = expr ->
          make_infix loc op t1 t2
        | t1 = expr; op = "orthenT"; t2 = expr ->
          make_infix loc op t1 t2
        | t1 = expr; op = "thenT"; t2 = expr ->
          make_infix loc op t1 t2
        | t1 = expr; op = "thenLT"; t2 = expr ->
          make_infix loc op t1 t2
        | t1 = expr; op = "thenFLT"; t2 = expr ->
          make_infix loc op t1 t2
        | t1 = expr; op = "then_OnEachT"; t2 = expr ->
          make_infix loc op t1 t2
        | t1 = expr; op = "then_OnFirstT"; t2 = expr ->
          make_infix loc op t1 t2
        | t1 = expr; op = "then_OnLastT"; t2 = expr ->
          make_infix loc op t1 t2
        | t1 = expr; op = "then_OnSameConclT"; t2 = expr ->
          make_infix loc op t1 t2
        | t1 = expr; op = "thenLabLT"; t2 = expr ->
          make_infix loc op t1 t2
        | t1 = expr; op = "thenMT"; t2 = expr ->
          make_infix loc op t1 t2
        | t1 = expr; op = "thenMLT"; t2 = expr ->
          make_infix loc op t1 t2
        | t1 = expr; op = "thenAT"; t2 = expr ->
          make_infix loc op t1 t2
        | t1 = expr; op = "thenALT"; t2 = expr ->
          make_infix loc op t1 t2
        | t1 = expr; op = "thenWT"; t2 = expr ->
          make_infix loc op t1 t2
        | t1 = expr; op = "thenET"; t2 = expr ->
          make_infix loc op t1 t2
        | t1 = expr; op = "thenPT"; t2 = expr ->
          make_infix loc op t1 t2
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
 * $Log$
 * Revision 1.19  1998/04/24 19:38:26  jyh
 * Updated debugging.
 *
 * Revision 1.18  1998/04/24 02:41:59  jyh
 * Added more extensive debugging capabilities.
 *
 * Revision 1.17  1998/04/21 20:58:01  jyh
 * Fixed typing problems introduced by refiner msequents.
 *
 * Revision 1.16  1998/04/21 19:53:33  jyh
 * Upgraded refiner for program extraction.
 *
 * Revision 1.15  1998/04/15 12:39:56  jyh
 * Updating editor packages to Filter_summarys.
 *
 * Revision 1.14  1998/04/13 17:08:32  jyh
 * Adding interactive proofs.
 *
 * Revision 1.13  1998/04/09 18:25:50  jyh
 * Working compiler once again.
 *
 * Revision 1.12  1998/04/09 15:26:32  jyh
 * Added strip_mfunction.
 *
 * Revision 1.11  1998/02/23 14:46:09  jyh
 * First implementation of binary file compilation.
 *
 * Revision 1.10  1998/02/21 20:57:44  jyh
 * Two phase parse/extract.
 *
 * Revision 1.9  1998/02/19 21:08:22  jyh
 * Adjusted proof type to be primitive or derived.
 *
 * Revision 1.8  1998/02/19 17:13:59  jyh
 * Splitting filter_parse.
 *
 * Revision 1.7  1998/02/18 18:46:15  jyh
 * Initial ocaml semantics.
 *
 * Revision 1.6  1998/02/12 23:38:12  jyh
 * Added support for saving intermediate files to the library.
 *
 * Revision 1.5  1998/01/27 23:04:17  jyh
 * Adding OCaml1.07 syntax.
 *
 * Revision 1.4  1997/09/12 17:21:37  jyh
 * Added MLast <-> term conversion.
 * Splitting filter_parse into two phases:
 *    1. Compile into Filter_summary
 *    2. Compile Filter_summary into code.
 *
 * Revision 1.3  1997/08/07 19:43:41  jyh
 * Updated and added Lori's term modifications.
 * Need to update all pattern matchings.
 *
 * Revision 1.2  1997/08/06 16:17:31  jyh
 * This is an ocaml version with subtyping, type inference,
 * d and eqcd tactics.  It is a basic system, but not debugged.
 *
 * Revision 1.1  1997/04/28 15:50:55  jyh
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
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
