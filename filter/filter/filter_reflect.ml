(*
 * Produce a reflected version of a theory.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2006 Mojave Group, Caltech
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
 * Author: Jason Hickey
 * @email{jyh@cs.caltech.edu}
 * @end[license]
 *)
open Lm_debug
open Lm_printf
open Lm_symbol

open Term_sig
open Term_ty_sig
open Term_ty_infer

open Refiner.Refiner
open Refiner.Refiner.TermType
open Refiner.Refiner.Term
open Refiner.Refiner.TermTy
open Refiner.Refiner.TermMan
open Refiner.Refiner.TermMeta
open Refiner.Refiner.TermSubst
open Refiner.Refiner.Rewrite
open Refiner.Refiner.RefineError
open Refiner.Refiner.Refine

open Opname
open Simple_print
open Filter_base_type
open Filter_type
open Filter_shape
open Filter_summary
open Filter_summary_util
open Filter_summary_type
open Filter_util
open Term_grammar
open Proof_boot
open Proof_convert

open Filter_prog.ProofCaches

let debug_filter_reflect =
   create_debug (**)
      { debug_name = "filter_reflect";
        debug_description = "debug filter processing for reflected theories";
        debug_value = false
      }

(*
 * Show the file loading.
 *)
let () = show_loading "Loading Filter_reflect%t"

(************************************************************************
 * Variables.
 *)
let var_p = Lm_symbol.add "p"
let var_d = Lm_symbol.add "d"

let maybe_new_var v vars =
   if SymbolSet.mem vars v then
      new_name v (SymbolSet.mem vars)
   else
      v

(************************************************************************
 * Base term grammar
 *)
let term_parsing_state = ref None

module TermGrammarBefore : TermGrammarSig =
struct
   let parsing_state loc =
      match !term_parsing_state with
         Some st -> st
       | None -> Stdpp.raise_with_loc loc (Failure "Filter_reflect.parsing_state is uninitialized")

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
 * The bindings contain terms.
 *)
let add_binding bind =
   let bind =
      match bind with
         BindTerm t ->
            BindTerm t
       | BindOpname _
       | BindNum _ as bind ->
            bind
   in
      add_binding bind

let bind_item i =
   { item_item = i;
     item_bindings = get_bindings ()
   }

(************************************************************************
 * Utilities.
 *)

(*
 * Reflect theories are prefixed with "reflect_"
 *)
let reflect_prefix = "reflect_"
let reflect_prefix_cap = "Reflect_"

let reflect_filename new_name orig_path =
   let old_base =
      match new_name with
         Some new_name ->
            new_name
       | None ->
            Lm_filename_util.basename orig_path
   in
   let new_base = reflect_prefix ^ old_base in
   let new_path = Lm_filename_util.replace_basename orig_path new_base in
      new_base, new_path

(*
 * Check whether the location of an exn is already defined.
 *)
let not_exn_located exn =
   match exn with
      Stdpp.Exc_located _ ->
         false
    | _ ->
         true

(*
 * Create a declaration for a 0-arity opname.
 *)
let declare_simple_term opname =
   let t = mk_term (mk_op opname []) [] in
   let quote =
      { ty_term   = t;
        ty_opname = opname;
        ty_params = [];
        ty_bterms = [];
        ty_type   = term_type
      }
   in
      t, shape_normal, quote

(************************************************************************
 * Signature conversion.
 *
 * The only thing we care about is the extends directives,
 * where we need to convert the extensions to point to the
 * reflect_ theories.
 *)
let opname_prefix info loc =
   (SigFilterCache.get_parsing_state info).opname_prefix loc

let copy_sig_item cache loc item =
   SigFilterCache.add_command cache (item, loc)

let declare_parent_path cache loc path =
   (* Lots of errors can occur here *)
   let () =
      try SigFilterCache.inline_module cache () path with
         exn ->
            Stdpp.raise_with_loc loc exn
   in

   (* Add resources and grammar start symbols *)
   let info =
      { parent_name = path;
        parent_resources = SigFilterCache.sig_resources cache path
      }
   in
      SigFilterCache.add_command cache (Parent info, loc)

let declare_parent cache loc item =
   let { parent_name = path } = item in
   let head, name =
      try Lm_list_util.split_last path with
         Failure _ ->
            Stdpp.raise_with_loc loc (EmptyModulePath "Filter_reflect.declare_parent")
   in
   let name = reflect_prefix ^ String.uncapitalize name in
   let path = head @ [name] in
      declare_parent_path cache loc path

(*
 * When a term is declared, we will also generate a type-checking
 * term, prefixed with "term_".
 *)
let add_simple_term info loc name =
   let opname = Opname.mk_opname name (opname_prefix info loc) in
   let _, sc, t = declare_simple_term opname in
      SigFilterCache.declare_term info sc t;
      copy_sig_item info loc (DeclareTerm (sc, t))

let add_declare_simple info loc sc t =
   let name, _ = dst_opname t.ty_opname in
      add_simple_term info loc ("term_" ^ name)

let add_declare_sequent info loc quote cases =
   let name, _ = Opname.dst_opname quote.ty_opname in
   let _ =
      List.fold_left (fun index _ ->
            add_simple_term info loc (Printf.sprintf "term_step_%s_%d" name index);
            succ index) 1 cases
   in
      add_simple_term info loc ("term_concl_" ^ name)

let add_declare info loc sc quote =
   (* Declare the original term *)
   SigFilterCache.declare_term info sc quote;
   copy_sig_item info loc (DeclareTerm (sc, quote));

   try
      (* Declare all the sequent checking terms *)
      let cases, _, _ = dest_ty_sequent_cases quote.ty_type in
         add_declare_sequent info loc quote cases
   with
      RefineError _ ->
         (* Declare the type checking term *)
         add_declare_simple info loc sc quote

(*
 * Translate the interface.
 *)
let compile_sig_item (info : SigFilterCache.info) ((item : StrFilterCache.str_elem), loc) =
   match item with
      (*
       * Supported items.
       *)
      Parent { parent_name = ["itt_hoas_theory"] } ->
         ()
    | Parent ({ parent_name = name } as parent) ->
         if !debug_filter_reflect then
            eprintf "Filter_reflect.extract_sig_item: parent: %s@." (string_of_path name);
         declare_parent info loc parent

      (*
       * Copy these parts verbatim.
       *)
    | DefineTerm (sc, t, _)
    | DeclareTerm (sc, t) ->
         add_declare info loc sc t
    | DeclareTypeClass (sc, opname, ty_term, ty_parent) as item ->
         SigFilterCache.declare_typeclass info sc opname ty_term ty_parent;
         copy_sig_item info loc item
    | DeclareType (sc, ty_term, ty_parent) as item ->
         SigFilterCache.declare_type info sc ty_term ty_parent;
         copy_sig_item info loc item
    | DeclareTypeRewrite _ ->
         (* JYH: probably we will never support type equalities *)
         Stdpp.raise_with_loc loc (Failure "Filter_reflect.compile_sig_item: type rewrites are not supported")

      (*
       * Illegal items.
       *)
    | Module _ ->
         Stdpp.raise_with_loc loc (Failure "Filter_sig.extract_sig_item: nested modules are not implemented")
    | Improve _ ->
         Stdpp.raise_with_loc loc (Invalid_argument "Filter_reflect.extract_sig_item")

      (*
       * MetaPRL will ensure that the implementation has exactly the same
       * rule, so we just declare the term corresponding to the rule here.
       *)
    | Rule { rule_name = name } ->
         add_simple_term info loc ("rule_" ^ name)

      (*
       * We want to support rewrites eventually, but they are currently
       * unsupported.
       *)
    | Rewrite { rw_name = name }
    | CondRewrite { crw_name = name } ->
         Stdpp.raise_with_loc loc (Failure (Printf.sprintf "Filter_reflect.compile_sig_item: %s: rewrites are not implemented" name))

      (*
       * The rest are ignored.
       *)
    | MLAxiom _
    | MLRewrite _
    | SummaryItem _
    | MagicBlock _
    | ToploopItem _
    | Resource _
    | Prec _
    | InputForm _
    | DForm _
    | PrecRel _
    | Id _
    | Comment _
    | MLGramUpd _
    | PRLGrammar _ ->
         ()

(*
 * Declare the term that defines the logic.
 *)
let declare_logic info name loc =
   let opname = Opname.mk_opname name (opname_prefix info loc) in
   let _, sc, t = declare_simple_term opname in
      SigFilterCache.declare_term info sc t;
      copy_sig_item info dummy_loc (DeclareTerm(sc, t))

(*
 * The entire signature.
 *)
let compile_sig info orig_name orig_info =
   declare_parent_path info dummy_loc ["itt_hoas_theory"];
   List.iter (compile_sig_item info) (info_items orig_info);
   declare_logic info orig_name dummy_loc

(************************************************************************
 * Implementation conversion.
 *)

(*
 * Build our descriptions of the items in the file.
 *)
type ref_rule =
   { ref_rule_name      : string;
     ref_rule_resources : (ProofCaches.StrFilterCache.str_expr, term) resource_def;
     ref_rule_params    : term_param list;
     ref_rule_term      : meta_term
   }

(*
 * The state of the processor.
 *)
type info =
   { info_cache         : StrFilterCache.info;
     info_parsing_state : parsing_state;
     info_parse_state   : parse_state;
     info_parse_info    : Filter_reflection.parse_info
   }

let mk_parse_state loc id =
   { parse_quotation =
        (fun name s ->
              TermGrammar.raw_term_of_parsed_term (TermGrammar.parse_quotation loc id name s));
     parse_opname = TermGrammar.mk_opname_kind loc;
     parse_shape  = TermGrammar.find_shape_class loc;
     parse_param  = TermGrammar.dest_xparam loc
   }

let create_info loc cache =
   let parsing_state = StrFilterCache.get_parsing_state cache in
   let () = term_parsing_state := Some parsing_state in
   let parse_state = mk_parse_state loc "term" in
   let parse_info = Filter_reflection.create_parse_info parse_state in
      { info_cache         = cache;
        info_parsing_state = parsing_state;
        info_parse_state   = parse_state;
        info_parse_info    = parse_info
      }

(*
 * Create opnames in this theory.
 *)
let opname_prefix info loc =
   info.info_parsing_state.opname_prefix loc

let check_input_term info loc t =
   info.info_parsing_state.check_input_term loc t

let check_input_mterm info loc mt =
   info.info_parsing_state.check_input_mterm loc mt

let check_input_terms info loc terms =
   List.iter (check_input_term info loc) terms

let check_rule info loc mt terms =
   info.info_parsing_state.check_rule loc mt terms

(*
 * Add an ML open command.
 *)
let add_open info _loc name =
   let cache = info.info_cache in
   let item = <:str_item< open $uid:name$ >> in
      StrFilterCache.add_command cache (SummaryItem (bind_item item), _loc)

(*
 * Adding to the cache.
 *)
let declare_term info shapeclass ty_term =
   StrFilterCache.declare_term info.info_cache shapeclass ty_term

let define_term info loc shapeclass name ty_term contractum res =
   let term_def =
      { term_def_name = name;
        term_def_value = contractum;
        term_def_resources = res;
        term_def_opaque = false;
      }
   in
      StrFilterCache.add_command info.info_cache (DefineTerm (shapeclass, ty_term, term_def), loc)

(************************************************
 * The parents are the _reflected_ theories.
 *)
let define_parent_path info loc path =
   let cache = info.info_cache in

   (* Lots of errors can occur here *)
   let () =
      try StrFilterCache.inline_module cache () path with
         exn ->
            Stdpp.raise_with_loc loc exn
   in

   (* Add resources and grammar start symbols *)
   let info =
      { parent_name = path;
        parent_resources = StrFilterCache.sig_resources cache path
      }
   in
      StrFilterCache.add_command cache (Parent info, loc)

let define_parent info loc item =
   let { parent_name = path } = item in
   let head, name =
      try Lm_list_util.split_last path with
         Failure _ ->
            Stdpp.raise_with_loc loc (EmptyModulePath "Filter_reflect.define_parent")
   in
   let name = reflect_prefix ^ String.uncapitalize name in
   let path = head @ [name] in
      define_parent_path info loc path

(************************************************
 * Rule reflection.
 *)

(*
 * Parse the parameter lists.
 *)
let parse_params info loc mt params =
   extract_params (context_vars mt) params

(*
 * Type checking.
 *)
let parse_rule info loc name mt params =
   (* Check with the refiner first for rewrite errors *)
   let terms  = collect_terms params in
      Refine.check_rule name (collect_cvars params) terms (strip_mfunction mt);

      (* Then check for type errors *)
      check_input_mterm info loc mt;
      check_input_terms info loc terms;
      check_rule info loc mt terms;
      mt

(*
 * Build the command that becomes part of the summary.
 *)
let rule_command name params mt pf res =
   Rule { rule_name      = name;
          rule_params    = params;
          rule_stmt      = mt;
          rule_proof     = pf;
          rule_resources = res
   }

(*
 * Add the rule definition to the summary.
 *)
let define_rule info loc name
    (params : term_param list)
    (mterm : meta_term)
    (extract : Convert.cooked proof_type)
    (res : ((MLast.expr, term) resource_def)) =
   try
      let cmd = rule_command name params mterm extract res in
      let () =
         match cmd, extract with
            Rule r, Primitive extract ->
               let _, ext_args, _ = split_mfunction mterm in
               let addrs = collect_cvars r.rule_params in
                  Refine.check_prim_rule name addrs (collect_terms params) (strip_mfunction mterm) ext_args extract
          | _ ->
               ()
      in
         StrFilterCache.add_command info.info_cache (cmd, loc)
   with
      exn ->
         Stdpp.raise_with_loc loc exn

(*
 * We use this internally to create theorems for the various
 * reflected facts.  "s" is the tactic text.
 *)
let define_thm info loc name params mterm s res =
   let assums, goal = unzip_mfunction mterm in
   let assums = List.map (fun (_, _, assum) -> assum) assums in
   let mseq   = mk_msequent goal assums in
   let proof  = Proof.create_io_rulebox mseq s in
   let proof  = Convert.of_raw () s proof in
      define_rule info loc name params mterm (Interactive proof) res

(*
 * Add a term definition.  The term is defined in the parent theory.
 * We need to add a rule for well-formedness of the reflected
 * version.
 *)
let add_define info rules loc item =
   let { ref_rule_name = name;
         ref_rule_term = def;
         ref_rule_params = params
       } = item
   in

   (* Declare the term itself *)
   let opname = Opname.mk_opname name (opname_prefix info loc) in
   let t_rule, sc, quote = declare_simple_term opname in
   let () = declare_term info sc quote in

   (* Add the definition as the proof-checking predicate *)
   let def = parse_rule info loc name def params in
   let def = Filter_reflection.mk_rule_term info.info_parse_info def in
   let () = define_term info loc sc ("unfold_" ^ name) quote def no_resources in

   (* The wf theorem is (<H> >- t IN ProofRule) *)
   let name_wf = "wf_" ^ name in
   let mt = Filter_reflection.mk_rule_wf_thm info.info_parse_info t_rule in
   let res = intro_resources loc in
   let mt = parse_rule info loc name_wf mt [] in
   let tac = Printf.sprintf "rwh unfold_%s 0 thenT proofRuleWFT" name in
   let () = define_thm info loc name_wf [] mt tac res in
      (loc, item) :: rules

(*
 * When a term is declared, add the type-checking rule.
 *)
let add_declare_simple info rules loc quote =
   let name, _ = Opname.dst_opname quote.ty_opname in
   let mt = Filter_reflection.mk_type_check_thm info.info_parse_info quote in
   let item =
      { ref_rule_name      = "term_" ^ name;
        ref_rule_resources = no_resources;
        ref_rule_params    = [];
        ref_rule_term      = mt
      }
   in
      add_define info rules loc item

(*
 * Add a sequent declaration.
 *)
let add_declare_sequent info rules loc quote cases ty_concl ty_seq =
   let name, _ = Opname.dst_opname quote.ty_opname in

   (* Base case, when the sequent has no hyps *)
   let mt = Filter_reflection.mk_sequent_concl_check_thm info.info_parse_info quote ty_concl ty_seq in
   let item =
      { ref_rule_name      = "term_concl_" ^ name;
        ref_rule_resources = no_resources;
        ref_rule_params    = [];
        ref_rule_term      = mt
      }
   in
   let rules = add_define info rules loc item in

   (* Add another step rule for each of the cases *)
   let rules, _ =
      List.fold_left (fun (rules, index) (ty_var, ty_hyp) ->
            let t_hyp, mt = Filter_reflection.mk_sequent_step_check_thm info.info_parse_info quote ty_var ty_hyp ty_seq in
            let item =
               { ref_rule_name      = Printf.sprintf "term_step_%s_%d" name index;
                 ref_rule_resources = no_resources;
                 ref_rule_params    = [TermParam (mk_ty_constrain_term t_hyp ty_hyp)];
                 ref_rule_term      = mt
               }
            in
            let rules = add_define info rules loc item in
               rules, succ index) (rules, 1) cases
   in
      rules

(*
 * Add a general declaration.
 *)
let add_declare info rules loc quote =
   let kind =
      try Some (dest_ty_sequent_cases quote.ty_type) with
         RefineError _ ->
            None
   in
      match kind with
         Some (cases, ty_concl, ty_seq) ->
            add_declare_sequent info rules loc quote cases ty_concl ty_seq
       | None ->
            add_declare_simple info rules loc quote

(*
 * Define a declared rule.
 *
 * JYH: this is of course completely broken.
 * Will fix 3/18/2006.
 *)
let add_rule info theorems rules loc item =
   let { rule_name      = name;
         rule_params    = params;
         rule_stmt      = mt;
         rule_proof     = proof;
         rule_resources = res
       } = item
   in

   (* Declare only primitive rules *)
   let is_prim =
      match proof with
         Primitive _ ->
            true
       | Derived _
       | Interactive _
       | Incomplete ->
            false
   in

   (* Build the reflected rule *)
   let item =
      { ref_rule_name      = "rule_" ^ name;
        ref_rule_resources = res;
        ref_rule_params    = params;
        ref_rule_term      = mt
      }
   in
      if is_prim then
         theorems, add_define info rules loc item
      else
         (loc, item) :: theorems, rules

(************************************************
 * Postprocessing.  This adds:
 *    1. A membership theorem for each rule in the logic
 *    2. A "pretty" reflected introduction rule
 *    3. An elimination rule
 *)

(*
 * Add a logic membership rule.
 *)
let add_mem_logic info logic_name t_logic loc item =
   let { ref_rule_name = name } = item in
   let opname = Opname.mk_opname name (opname_prefix info loc) in
   let t_rule = mk_term (mk_op opname []) [] in
   let mt = Filter_reflection.mk_mem_logic_thm info.info_parse_info t_logic t_rule in
   let tac =
      Printf.sprintf "mem_logic_trans << %s >>
then_OnLastT (rwh unfold_%s 0 thenT mem_rules_logic)
thenT autoT" (**)
         logic_name logic_name
   in
   let name = Printf.sprintf "mem_%s_%s" name logic_name in
   let res = intro_resources loc in
      define_thm info loc name [] mt tac res

let add_mem_logic info logic_name t_logic (loc, item) =
   try add_mem_logic info logic_name t_logic loc item with
      exn when not_exn_located exn ->
         Stdpp.raise_with_loc loc exn

(*
 * Add an introduction rule in "Provable" form.
 *
 * XXX: JYH: this is a minor hack.  If the rule is a meta_type
 * judgment, then we use ProvableSequent.
 *)
let add_intro info t_logic loc is_prim item =
   let { ref_rule_name      = name;
         ref_rule_params    = params;
         ref_rule_resources = res;
         ref_rule_term      = mt
       } = item
   in
   let provable_kind =
      let _, goal = unzip_mfunction mt in
         if is_sequent_term goal && Filter_reflection.is_meta_type_term info.info_parse_info (explode_sequent goal).sequent_args then
            ProvableSequent
         else
            ProvableJudgment
   in
   let rule_name = "intro_" ^ name in
   let mt_rule = parse_rule info loc rule_name mt params in
   let _, mt, params = Filter_reflection.mk_intro_thm info.info_parse_info t_logic provable_kind mt_rule params in
   let tac =
      if is_prim then
         Printf.sprintf "provableRuleT << %s >> unfold_%s" name name
      else
         "idT"
   in
      define_thm info loc rule_name params mt tac res;
      { item with ref_rule_term = mt_rule }

let add_intro info t_logic is_prim (loc, item) =
   try add_intro info t_logic loc is_prim item with
      exn when not_exn_located exn ->
         Stdpp.raise_with_loc loc exn

(************************************************
 * Add the multi-step elimination rule for proof induction.
 *)
let add_proof_check_elim info loc item =
   let { ref_rule_name = name;
         ref_rule_term = mt
       } = item
   in
   let rule_name = "elim_check_" ^ name in

   (* Name of the proof rule *)
   let opname = Opname.mk_opname name (opname_prefix info loc) in
   let t = mk_term (mk_op opname []) [] in

   (* Build the rule *)
   let h_v, mt = Filter_reflection.mk_proof_check_elim_thm info.info_parse_info t mt in
   let params = [IntParam h_v] in
   let mt = parse_rule info loc rule_name mt params in

   let tac = "elimProofCheckT unfold_" ^ name in
   let res = elim_resources loc in
      define_thm info loc rule_name params mt tac res

let add_simple_step_elim info loc name t_logic rules parents =
   let rule_name = "elim_step_" ^ name in

   (* Build the rule *)
   let h_v, mt = Filter_reflection.mk_simple_step_elim_thm info.info_parse_info t_logic rules parents in
   let params = [IntParam h_v] in
   let mt = parse_rule info loc rule_name mt params in

   let tac = "elimSimpleStepT unfold_" ^ name in
   let res = elim_resources loc in
      define_thm info loc rule_name params mt tac res

let add_elim_start info loc name t_logic =
   let rule_name = "elim_start_" ^ name in

   (* Build the rule *)
   let h_v, mt = Filter_reflection.mk_elim_start_thm info.info_parse_info t_logic in
   let params = [IntParam h_v] in
   let mt = parse_rule info loc rule_name mt params in

   let tac = "elimRuleStartT" in
      define_thm info loc rule_name params mt tac no_resources

let add_multi_step_elim info loc name t_logic rules intro_rules parents =
   try
      List.iter (add_proof_check_elim info loc) intro_rules;
      add_simple_step_elim info loc name t_logic rules parents;
      add_elim_start info loc name t_logic
   with
      exn when not_exn_located exn ->
         Stdpp.raise_with_loc loc exn

(************************************************
 * Add the huge elimination rule for proof induction.
 *)
let add_elim info loc name t_logic items parents =
   let rule_name = "elim_" ^ name in
   let rules = List.map (fun item -> item.ref_rule_term) items in

   (* Build the rule *)
   let h_v, mt = Filter_reflection.mk_elim_thm info.info_parse_info t_logic rules parents in
   let params = [IntParam h_v] in
   let mt = parse_rule info loc rule_name mt params in

   (* TODO: more accurate tactic *)
   let tac = "elimRuleT" in
      define_thm info loc rule_name params mt tac no_resources

let add_elim info loc name t_logic rules =
   try add_elim info loc name t_logic rules with
      exn when not_exn_located exn ->
         Stdpp.raise_with_loc loc exn

(*
 * Postprocessing primitive rules.
 *)
let postprocess_rules info current loc name parents items =
   (* Collect all the names of the term, and build a logic *)
   let rules =
      List.fold_left (fun terms (loc, item) ->
            let name = item.ref_rule_name in
            let opname = Opname.mk_opname name (opname_prefix info loc) in
            let t = mk_term (mk_op opname []) [] in
               t :: terms) [] items
   in
   let rules = List.rev rules in
   let t_rules = Filter_reflection.mk_rules_logic_term info.info_parse_info rules current in

   (* Define the logic term *)
   let opname = Opname.mk_opname name (opname_prefix info loc) in
   let t_logic, sc, quote = declare_simple_term opname in
   let () = declare_term info sc quote in
   let () = define_term info loc sc ("unfold_" ^ name) quote t_rules no_resources in

   (* State that it is a logic *)
   let name_wf = "wf_" ^ name in
   let logic_wf = Filter_reflection.mk_logic_wf_thm info.info_parse_info t_logic in
   let logic_wf = parse_rule info loc name_wf logic_wf [] in
   let logic_res = intro_resources loc in
   let logic_tac = Printf.sprintf "rwh unfold_%s 0 thenT autoT" name in
   let () = define_thm info loc name_wf [] logic_wf logic_tac logic_res in

   (* Add a membership term for each of the rules *)
   let () = List.iter (add_mem_logic info name t_logic) items in

   (* Add an introduction form for each of the rules *)
   let intro_rules = List.map (add_intro info t_logic true) items in
      (* Add the multi-step elimination rule for the entire logic *)
      add_multi_step_elim info loc name t_logic rules intro_rules parents;

      (* Add an elimination rule for the entire logic *)
      add_elim info loc name t_logic intro_rules parents;

      (* Need the logic for the theorems *)
      t_logic

(*
 * Postprocessing theorems.
 *)
let postprocess_theorems info t_logic items =
   List.iter (fun item -> ignore (add_intro info t_logic false item)) items

(*
 * Copy items directly.
 *)
let copy_str_item info loc item =
   StrFilterCache.add_command info.info_cache (item, loc)

(************************************************
 * Build the parent logic.
 *)
let make_parent_term info parent =
   let opname = Opname.make_opname [parent; reflect_prefix_cap ^ parent] in
      mk_simple_term opname []

let rec compile_parent_logic info parents =
   match parents with
      [] ->
         Filter_reflection.mk_empty_logic_term info.info_parse_info
    | [parent] ->
         parent
    | parent :: parents ->
         let parents = compile_parent_logic info parents in
            Filter_reflection.mk_union_logic_term info.info_parse_info parent parents

(************************************************
 * Copy open statements.
 *)
let define_summary_item info loc item =
   match item.item_item with
      <:str_item< open $_sl$ >> ->
         StrFilterCache.add_command info.info_cache (SummaryItem item, loc)
    | _ ->
         ()

(************************************************
 * Process the summary.
 *)

(*
 * Process parents first.
 * We need these so that the definitions inherited from the interface
 * are well-formed.
 *)
let compile_str_item_parent info parents item loc =
   match item with
      (*
       * If the original theory extends Name,
       * then the reflected theory extends Reflect_name.
       *)
      Parent { parent_name = ["itt_hoas_theory"] } ->
         parents
    | Parent ({ parent_name = name } as parent) ->
         if !debug_filter_reflect then
            eprintf "Filter_reflect.extract_sig_item: parent: %s@." (string_of_path name);
         define_parent info loc parent;
         List.hd name :: parents
    | _ ->
         parents

let compile_str_item info theorems rules item loc =
   match item with
      (* We have already processed the parents *)
      Parent _ ->
         theorems, rules

      (*
       * Term declarations are coded as type-checking rules.
       *)
    | DefineTerm (sc, t, _)
    | DeclareTerm (sc, t) ->
         (* Treat them both as declarations *)
         StrFilterCache.declare_term info.info_cache sc t;
         copy_str_item info loc item;
         theorems, add_declare info rules loc t
    | DeclareTypeClass (sc, opname, ty_term, ty_parent) ->
         (* Type declarations can be ignored -- we just use the terms directly *)
         StrFilterCache.declare_typeclass info.info_cache sc opname ty_term ty_parent;
         copy_str_item info loc item;
         theorems, rules
    | DeclareType (sc, ty_term, ty_parent) ->
         (* Type declarations can be ignored -- we just use the terms directly *)
         StrFilterCache.declare_type info.info_cache sc ty_term ty_parent;
         copy_str_item info loc item;
         theorems, rules
    | DeclareTypeRewrite _ ->
         (* JYH: probably we will never support type equalities *)
         Stdpp.raise_with_loc loc (Failure "Filter_reflect.compile_str_item: type rewrites are not supported")

      (*
       * Convert a rule.
       *)
    | Rule item ->
         add_rule info theorems rules loc item

      (*
       * DForms are copied in quoted form.
       *)
    | DForm df ->
         copy_str_item info loc item;
         theorems, rules

      (*
       * Open statements need to be copied.
       *)
    | SummaryItem item ->
         define_summary_item info loc item;
         theorems, rules

      (*
       * We want to support rewrites eventually, but they are currently
       * unsupported.
       *)
    | Rewrite { rw_name = name }
    | CondRewrite { crw_name = name } ->
         Stdpp.raise_with_loc loc (Failure (Printf.sprintf "Filter_reflect.compile_str_item: %s: rewrites are not implemented" name))

      (*
       * Illegal items.
       *)
    | Module _ ->
         Stdpp.raise_with_loc loc (Failure "Filter_reflect.compile_str_item: nested modules are not implemented")
    | Improve _ ->
         Stdpp.raise_with_loc loc (Failure "Filter_reflect.compile_str__item: bogus Improve item")
    | MLAxiom _
    | MLRewrite _ ->
         Stdpp.raise_with_loc loc (Failure "Filter_reflect.compile_str_item: ML rules are not supported")

      (*
       * The rest are ignored.
       *)
    | InputForm _
    | MagicBlock _
    | ToploopItem _
    | Resource _
    | Prec _
    | PrecRel _
    | Id _
    | Comment _
    | MLGramUpd _
    | PRLGrammar _ ->
         theorems, rules

let compile_str_item_parent info parents (item, loc) =
   try compile_str_item_parent info parents item loc with
      exn when not_exn_located exn ->
         Stdpp.raise_with_loc loc exn

let compile_str_item info theorems rules (item, loc) =
   try compile_str_item info theorems rules item loc with
      exn when not_exn_located exn ->
         Stdpp.raise_with_loc loc exn

let compile_str cache orig_name orig_info =
   let info = create_info dummy_loc cache in
   let items = info_items orig_info in
   let parents =
      define_parent_path info dummy_loc ["itt_hoas_theory"];
      add_open info dummy_loc "Basic_tactics";
      List.fold_left (compile_str_item_parent info) [] items
   in
   let parents = List.rev_map (make_parent_term info) parents in
   let theorems, rules =
      List.fold_left (fun (theorems, rules) item ->
            compile_str_item info theorems rules item) ([], []) items
   in
   let theorems = List.rev theorems in
   let rules = List.rev rules in
   let t_logic = compile_parent_logic info parents in
   let t_logic = postprocess_rules info t_logic dummy_loc orig_name parents rules in
      postprocess_theorems info t_logic theorems

(*
 * -*-
 * Local Variables:
 * Fill-column: 100
 * End:
 * -*-
 * vim:ts=3:et:tw=100
 *)
