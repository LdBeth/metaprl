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

open Term_sig
open Term_ty_sig
open Term_ty_infer

open Refiner.Refiner
open Refiner.Refiner.TermType
open Refiner.Refiner.Term
open Refiner.Refiner.TermTy
open Refiner.Refiner.TermMan
open Refiner.Refiner.TermMeta
open Refiner.Refiner.Rewrite
open Refiner.Refiner.RefineError
open Refiner.Refiner.Refine

open Opname
open Simple_print
open Filter_type
open Filter_shape
open Filter_summary
open Filter_summary_util
open Filter_summary_type
open Filter_prog
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

let reflect_filename orig_path =
   let old_base = Lm_filename_util.basename orig_path in
   let new_base = reflect_prefix ^ old_base in
   let new_path = Lm_filename_util.replace_basename orig_path new_base in
      new_base, new_path

(************************************************************************
 * Signature conversion.
 *
 * The only thing we care about is the extends directives,
 * where we need to convert the extensions to point to the
 * reflect_ theories.
 *)
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

let compile_sig_item info (item, loc) =
   match item with
      (*
       * Supported items.
       *)
      Parent ({ parent_name = name } as parent) ->
         if !debug_filter_reflect then
            eprintf "Filter_reflect.extract_sig_item: parent: %s@." (string_of_path name);
         declare_parent info loc parent

      (*
       * Illegal items.
       *)
    | Module _ ->
         Stdpp.raise_with_loc loc (Failure "Filter_sig.extract_sig_item: nested modules are not implemented")
    | Improve _ ->
         Stdpp.raise_with_loc loc (Invalid_argument "Filter_reflect.extract_sig_item")

      (*
       * The rest are ignored.
       *)
    | Rewrite _
    | CondRewrite _
    | MLAxiom _
    | MLRewrite _
    | Rule _
    | SummaryItem _
    | MagicBlock _
    | ToploopItem _
    | Resource _
    | Prec _
    | InputForm _
    | DefineTerm _
    | DeclareTypeClass _
    | DeclareType _
    | DeclareTerm _
    | DeclareTypeRewrite _
    | DForm _
    | PrecRel _
    | Id _
    | Comment _
    | MLGramUpd _
    | PRLGrammar _ ->
         ()

let compile_sig info orig_name orig_info =
   declare_parent_path info dummy_loc ["itt_hoas_theory"];
   declare_parent_path info dummy_loc [orig_name];
   List.iter (compile_sig_item info) (info_items orig_info)

(************************************************************************
 * Implementation conversion.
 *)

(*
 * Build our descriptions of the items in the file.
 *)
type ref_rule =
   { ref_rule_name      : string;
     ref_rule_resources : (ProofCaches.StrFilterCache.str_expr, term) resource_def;
     ref_rule_params    : term list;
     ref_rule_term      : meta_term
   }

(*
 * The state of the processor.
 *)
type info =
   { info_cache         : StrFilterCache.info;
     info_parsing_state : parsing_state;
     info_parse_state   : Filter_reflection.parse_state;
     info_parse_info    : Filter_reflection.parse_info
   }

let mk_parse_state loc id =
   { Filter_reflection.parse_quotation =
        (fun name s ->
              TermGrammar.raw_term_of_parsed_term (TermGrammar.parse_quotation loc id name s));
     Filter_reflection.parse_opname = TermGrammar.mk_opname_kind loc;
     Filter_reflection.parse_shape  = TermGrammar.find_shape_class loc;
     Filter_reflection.parse_param  = TermGrammar.dest_xparam loc
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
 * Add a parent.
 *)
let add_open info _loc name =
   let cache = info.info_cache in
   let item = <:str_item< open $uid:name$ >> in
      StrFilterCache.add_command cache (SummaryItem (bind_item item), _loc)

(*
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

(*
 * Adding to the cache.
 *)
let declare_define_term info shapeclass ty_term =
   StrFilterCache.declare_term info.info_cache shapeclass ty_term

let define_term info loc shapeclass name ty_term contractum res =
   let term_def =
      { term_def_name = name;
        term_def_value = contractum;
        term_def_resources = res
      }
   in
      StrFilterCache.add_command info.info_cache (DefineTerm (shapeclass, ty_term, term_def), loc)

(*
 * A primitive rule specifies the extract.
 *)
let rule_command name params mt pf res =
   let cvars = context_vars mt in
   let params = extract_params cvars params in
      Rule { rule_name      = name;
             rule_params    = params;
             rule_stmt      = mt;
             rule_proof     = pf;
             rule_resources = res
      }

let define_rule info loc name
    (params : term list)
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
                  Refine.check_prim_rule name addrs (collect_terms r.rule_params) (strip_mfunction mterm) ext_args extract
          | _ ->
               ()
      in
         StrFilterCache.add_command info.info_cache (cmd, loc)
   with
      exn ->
         Stdpp.raise_with_loc loc exn

let define_prim info loc name params mterm extract res =
   define_rule info loc name params mterm (Primitive extract) res

let define_thm info loc name params mterm s res =
   let assums, goal = unzip_mfunction mterm in
   let assums = List.map (fun (_, _, assum) -> assum) assums in
   let mseq = mk_msequent goal assums in
   let proof = Proof.create_io_rulebox mseq s in
   let proof = Convert.of_raw () s proof in
      define_rule info loc name params mterm (Interactive proof) res

let define_int_thm info loc name params mterm res =
   define_rule info loc name params mterm Incomplete res

(*
 * Type checking.
 *)
let parse_rule info loc name mt args =
   (* Check with the refiner first for rewrite errors *)
   let cvars = context_vars mt in
   let params = extract_params cvars args in
   let terms = collect_terms params in
      Refine.check_rule name (collect_cvars params) terms (strip_mfunction mt);

      (* Then check for type errors *)
      check_input_mterm info loc mt;
      check_input_terms info loc terms;
      check_rule info loc mt terms;
      cvars, mt, List.map erase_arg_term args

(*
 * Add a term definition.  The term is defined in the parent theory.
 * We need to add a rule for well-formedness of the reflected
 * version.
 *)
let add_define info rules loc item =
   let { ref_rule_name = name;
         ref_rule_term = def
       } = item
   in
   let opname = Opname.mk_opname name (opname_prefix info loc) in
   let t_rule = mk_term (mk_op opname []) [] in
   let quote =
      { ty_term   = t_rule;
        ty_opname = opname;
        ty_params = [];
        ty_bterms = [];
        ty_type   = term_type
      }
   in
   let sc = shape_normal in
   let _, def, _ = parse_rule info loc name def [] in
   let def = Filter_reflection.mk_rule_term info.info_parse_info def in
   let () = declare_define_term info sc quote in
   let () = define_term info loc sc ("unfold_" ^ name) quote def no_resources in

   (* The wf theorem is (<H> >- t IN ProofRule) *)
   let name_wf = "wf_" ^ name in
   let mt = Filter_reflection.mk_rule_wf_thm info.info_parse_info t_rule in
   let res = intro_resources loc in
   let _, mt, _ = parse_rule info loc name_wf mt [] in
   let tac = Printf.sprintf "rwh unfold_%s 0 thenT proofRuleWFT" name in
   let () = define_thm info loc name_wf [] mt tac res in
      (loc, item) :: rules

let add_prim_rule info rules loc item =
   add_define info rules loc item

let add_interactive_rule info rules loc item =
   add_define info rules loc item

(*
 * When a term is declared, add the type-checking rule.
 *)
let add_declare info rules loc quote =
   let name, _ = Opname.dst_opname quote.ty_opname in
   let mt = Filter_reflection.mk_type_check_thm info.info_parse_info quote in
   let item =
      { ref_rule_name      = "term_" ^ name;
        ref_rule_resources = no_resources;
        ref_rule_params    = [];
        ref_rule_term      = mt
      }
   in
      add_prim_rule info rules loc item

(*
 * Compile each of the items in the summary.
 *)
let compile_str_item info rules item loc =
   match item with
      (*
       * If the original theory extends Name,
       * then the reflected theory extends Reflect_name.
       *)
      Parent ({ parent_name = name } as parent) ->
         if !debug_filter_reflect then
            eprintf "Filter_reflect.extract_sig_item: parent: %s@." (string_of_path name);
         define_parent info loc parent;
         rules

      (*
       * Term declarations are coded as type-checking rules.
       *)
    | DefineTerm (sc, t, _)
    | DeclareTerm (sc, t) ->
         (* Treat them both as declarations *)
         add_declare info rules loc t
    | DeclareTypeClass _
    | DeclareType _ ->
         (* Type declarations can be ignored -- we just use the terms directly *)
         rules
    | DeclareTypeRewrite _ ->
         (* JYH: probably we will never support type equalities *)
         Stdpp.raise_with_loc loc (Failure "Filter_reflect.compile_str_item: type rewrites are not supported")

      (*
       * Convert a rule.
       *)
    | Rule info ->
         eprintf "Processing rule %s@." info.rule_name;
         rules

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
    | SummaryItem _
    | MagicBlock _
    | ToploopItem _
    | Resource _
    | Prec _
    | DForm _
    | PrecRel _
    | Id _
    | Comment _
    | MLGramUpd _
    | PRLGrammar _ ->
         rules

let compile_str_item info rules (item, loc) =
   try compile_str_item info rules item loc with
      exn when (match exn with
                   Stdpp.Exc_located _ ->
                      false
                 | _ ->
                      true) ->
         Stdpp.raise_with_loc loc exn

let compile_str cache orig_name orig_info =
   let info = create_info dummy_loc cache in
   let () =
      define_parent_path info dummy_loc ["itt_hoas_theory"];
      define_parent_path info dummy_loc [orig_name];
      add_open info dummy_loc "Basic_tactics"
   in
   let _rules = List.fold_left (compile_str_item info) [] (info_items orig_info) in
      ()

(*
 * -*-
 * Local Variables:
 * Fill-column: 100
 * End:
 * -*-
 * vim:ts=3:et:tw=100
 *)
