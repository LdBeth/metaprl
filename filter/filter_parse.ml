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
open Simple_print
open Precedence

open Infix
open Free_vars
open Term_grammar
open Filter_debug
open Filter_type
open Filter_util
open Filter_ast
open Filter_ocaml
open Filter_summary
open Filter_summary_io
open Filter_cache
open Filter_summary_param
open Filter_summary_modules
open Filter_process_type

(************************************************************************
 * CONTRACTUM BUILDING                                                  *
 ************************************************************************)

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
   
(************************************************************************
 * AST CONSTRUCTORS                                                     *
 ************************************************************************)

(*
 * Empty sig_item.
 *)
let list_sig_item loc l =
   <:sig_item< declare $list:l$ end >>

let list_str_item loc l =
   <:str_item< declare $list:l$ end >>

(*
 * Various constructors.
 *)
let sig_open loc path = <:sig_item< open $path$ >>
let str_open loc path = <:str_item< open $path$ >>

(*
 * Special words.
 *)
let rewrite_ctyp loc =
   <:ctyp< $uid:"Refine"$ . $uid:"Refiner"$ . $lid:"rw"$ '$"a"$ >>

let cond_rewrite_ctyp loc =
   let result = <:ctyp< $uid:"Refine"$ . $uid:"Refiner"$ . $lid:"cond_rewrite"$ '$"a"$ >> in
   let sarray = <:ctyp< $lid:"array"$ $lid:"string"$ >> in
   let term = <:ctyp< $lid:"list"$ ($uid:"Term"$ . $lid:"term"$) >> in
   let arg = <:ctyp< ($sarray$ * $term$) >> in
      <:ctyp< $arg$ -> $result$ >>

let create_rewrite_expr loc =
   <:expr< $uid:"Refine"$ . $uid:"Refiner"$ . $lid:"create_rewrite"$ >>

let prim_rewrite_expr loc =
   <:expr< $uid:"Refine"$ . $uid:"Refiner"$ . $lid:"prim_rewrite"$ >>

let rewrite_of_rewrite_expr loc =
   <:expr< $uid:"Refine"$ . $uid:"Refiner"$ . $lid:"rewrite_of_rewrite"$ >>

let create_cond_rewrite_expr loc =
   <:expr< $uid:"Refine"$ . $uid:"Refiner"$ . $lid:"create_cond_rewrite"$ >>

let prim_cond_rewrite_expr loc =
   <:expr< $uid:"Refine"$ . $uid:"Refiner"$ . $lid:"prim_cond_rewrite"$ >>

let rewrite_of_cond_rewrite_expr loc =
   <:expr< $uid:"Refine"$ . $uid:"Refiner"$ . $lid:"rewrite_of_cond_rewrite"$ >>

let prim_cond_rewrite_expr loc =
   <:expr< $uid:"Refine"$ . $uid:"Refiner"$ . $lid:"prim_cond_rewrite"$ >>

let rewrite_theorem_expr loc =
   <:expr< $uid:"Refine"$ . $uid:"Refiner"$ . $lid:"rewrite_theorem"$ >>

let cond_rewrite_theorem_expr loc =
   <:expr< $uid:"Refine"$ . $uid:"Refiner"$ . $lid:"cond_rewrite_theorem"$ >>

let prim_theorem_expr loc =
   <:expr< $uid:"Refine"$ . $uid:"Refiner"$ . $lid:"prim_theorem"$ >>

let create_axiom_expr loc =
   <:expr< $uid:"Refine"$ . $uid:"Refiner"$ . $lid:"create_axiom"$ >>

let create_rule_expr loc =
   <:expr< $uid:"Refine"$ . $uid:"Refiner"$ . $lid:"create_rule"$ >>

let theorem_expr loc =
   <:expr< $uid:"Refine"$ . $uid:"Refiner"$ . $lid:"theorem"$ >>

let create_ml_term_expr loc =
   <:expr< $uid:"Refine"$ . $uid:"Refiner"$ . $lid:"create_ml_condition"$ >>

let apply_redex_expr loc =
   <:expr< $uid:"Rewrite"$ . $lid:"apply_redex"$ >>

let construct_redex_expr loc =
   <:expr< $uid:"Term_util"$ . $lid:"construct_redex"$ >>

let make_seq_addr_expr loc =
   <:expr< $uid:"Term"$ . $lid:"make_seq_address"$ >>

let compile_redex_expr loc =
   <:expr< $uid:"Rewrite"$ . $lid:"compile_redex"$ >>

let compile_contractum_expr loc =
   <:expr< $uid:"Rewrite"$ . $lid:"compile_contractum"$ >>

let thy_name_expr loc =
   <:expr< $uid:"Theory"$ . $lid:"thy_name"$ >>

let thy_refiner_expr loc =
   <:expr< $uid:"Theory"$ . $lid:"thy_refiner"$ >>

let thy_dformer_expr loc =
   <:expr< $uid:"Theory"$ . $lid:"thy_dformer"$ >>

let thy_id_expr loc =
   <:expr< $uid:"Theory"$ . $lid:"thy_id"$ >>

let record_theory_expr loc =
   <:expr< $uid:"Theory"$ . $lid:"record_theory"$ >>

let label_refiner_expr loc =
   <:expr< $uid:"Refine"$ . $uid:"Refiner"$ . $lid:"label_refiner"$ >>

let refiner_ctyp loc =
   <:ctyp< $uid:"Refine"$ . $uid:"Refiner"$ . $lid:"refiner"$ >>

let join_refiner_expr loc =
   <:expr< $uid:"Refine"$ . $uid:"Refiner"$ . $lid:"join_refiner"$ >>

let join_mode_base_expr loc =
   <:expr< $uid:"Dform_print"$ . $lid:"join_mode_base"$ >>

let dformer_ctyp loc =
   <:ctyp< $uid:"Dform_print"$ . $lid:"dform_mode_base"$ >>

let resource_rsrc_ctyp loc =
   <:ctyp< $uid:"Resource"$ . $lid:"rsrc"$ >>

let resource_join_expr loc =
   <:expr< $uid:"Resource"$ . $lid:"resource_join"$ >>

let tactic_ctyp loc =
   <:ctyp< $uid:"Refine"$ . $uid:"Refiner"$ . $lid:"tactic"$ '$"a"$ >>

let tactic_of_rule_expr loc =
   <:expr< $uid:"Refine"$ . $uid:"Refiner"$ . $lid:"tactic_of_rule"$ >>

let dform_pattern_expr loc =
   <:expr< $uid:"Dform"$ . $lid:"dform_pattern"$ >>

let dform_options_expr loc =
   <:expr< $uid:"Dform"$ . $lid:"dform_options"$ >>

let dform_print_expr loc =
   <:expr< $uid:"Dform"$ . $lid:"dform_print"$ >>

let dform_stack_patt loc =
   <:patt< $uid:"Dform"$ . $lid:"dform_stack"$ >>

let dform_printer_patt loc =
   <:patt< $uid:"Dform"$ . $lid:"dform_printer"$ >>

let dform_items_patt loc =
   <:patt< $uid:"Dform"$ . $lid:"dform_items"$ >>

let dform_buffer_patt loc =
   <:patt< $uid:"Dform"$ . $lid:"dform_buffer"$ >>

let dform_expansion_expr loc =
   <:expr< $uid:"Dform"$ . $uid:"DFormExpansion"$ >>

let dform_printer_expr loc =
   <:expr< $uid:"Dform"$ . $uid:"DFormPrinter"$ >>

let dform_parens_expr loc =
   <:expr< $uid:"Dform"$ . $uid:"DFormParens"$ >>

let dform_prec_expr loc s =
   <:expr< $uid:"Dform"$ . $uid:"DFormPrec"$ $lid:s$ >>

let dform_inherit_prec_expr loc =
   <:expr< $uid:"Dform"$ . $uid:"DFormInheritPrec"$ >>

let dform_inherit_prec_expr loc =
   <:expr< $uid:"Dform"$ . $uid:"DFormInheritPrec"$ >>

let create_dform_expr loc =
   <:expr< $uid:"Dform_print"$ . $lid:"create_dform"$ >>

let refiner_id = "refiner"
let dformer_id = "dformer"

let local_refiner_id = "dont_use_this_refiner_name"
let local_dformer_id = "dont_use_this_dformer_name"
let stack_id = "rewrite_stack"

let null_refiner_expr loc =
   <:expr< $uid:"Refine"$ . $uid:"Refiner"$ . $lid:"null_refiner"$ >>

let null_mode_base_expr loc =
   <:expr< $uid:"Dform_print"$ . $lid:"null_mode_base"$ >>

let nil_array loc =
   <:expr< [| $list:[]$ |] >>

let nil_list loc =
   <:expr< [] >>

let precedence_ctyp loc =
   <:ctyp< $uid:"Precedence"$ . $lid:"precedence"$ >>

let new_prec_expr loc =
   <:expr< $uid:"Precedence"$ . $lid:"new_prec"$ >>

let add_lt_expr loc =
   <:expr< $uid:"Precedence"$ . $lid:"add_lt"$ >>

let add_eq_expr loc =
   <:expr< $uid:"Precedence"$ . $lid:"add_eq"$ >>

(*
 * Make a new variable.
 *)
let new_var loc v vars =
   let rec find i =
      let name = sprintf "%s%d" v i in
         if List.mem name vars then
            find (i + 1)
         else
            name
   in
      find 0

(************************************************************************
 * TERM INTERPRETATION                                                  *
 ************************************************************************)

(*
 * Interpretation on terms.
 *)
let interp = ref (function t ->
                   raise (Failure "Filter_parse.interp: term interpretation is not bound"))

let set_interp f = interp := f

(************************************************************************
 * ITEM EMBEDDING                                                       *
 ************************************************************************)

(*
 * Flatten the sig_item list.
 * Extract the Nuprl commands.
 * Place normal items in a SummaryItem item.
 *
 * is_prl_item: item -> bool: determine if the item is a nuprl command
 * dest_prl_item: item -> command * (item list): pull it apart
 * dest_item: item -> item flatten_item: get part of the normal item
 *)
type 'a flatten_item =
   FlattenList of 'a list
 | FlattenItem of 'a

let flatten_item_list
    (is_prl_item : 'a -> bool)
    (dest_prl_item : FilterCache.info -> 'a -> FilterCache.proof summary_item * 'a list)
    (dest_item : 'a -> 'a flatten_item)
    (term_of_item : 'a -> term)
    (info : FilterCache.info)
    (l : ('a * (int * int)) list)
=
   (* Capture functions may record the expression *)
   let null_capture item loc (commands, items) =
      commands, (item, loc) :: items
   in
   let item_capture item loc (commands, items) =
      SummaryItem (term_of_item item) :: commands, (item, loc) :: items
   in

   (* Add the items to the item list *)
   let rec flatten capture ((commands, items) as commands_items) loc item =
      if is_prl_item item then
         let command, items' = dest_prl_item info item in
            flatten_list null_capture (command :: commands, items) loc items'
      else
         match dest_item item with
            FlattenList items' ->
               flatten_list capture commands_items loc items'
          | FlattenItem item ->
               capture item loc commands_items
   and flatten_list capture commands_items loc = function
      [] ->
         commands_items
    | h::t ->
         flatten_list capture (flatten capture commands_items loc h) loc t
   in
   
   (* Flatten the entire list *)
   let rec flatten_top result = function
      [] ->
         result
    | (s, loc)::tl ->
         if is_prl_item s then
            let command, items = dest_prl_item info s in
            let commands, items' = result in
               flatten_top (flatten_list null_capture (command :: commands, items') loc items) tl
         else
            flatten_top (flatten item_capture result loc s) tl
   in
   let commands, items = flatten_top ([], []) l in
      List.rev commands, List.rev items

(*
 * HACK:
 *
 * This is a total and complete hack.
 * We want to extend the sig_item type with
 * new elements (commands that specify Nuprl
 * constructs).  We keep a table on the side
 * that contains the commands, and then embed
 * the commands as the special sig_item:
 *    declare val $prl_item : <index>; exprs end
 * where $prl_item is a special symbol, and
 * index is a type name that is the number of the
 * item we want to use.
 *)
let prl_item_placeholder = "$prl_item"

let prl_sig_item loc items index =
   let t = <:ctyp< $lid: string_of_int index$ >> in
   let head = <:sig_item< value $prl_item_placeholder$ : $t$ >> in
      <:sig_item< declare $list: head :: items$ end >>

let is_prl_sig_item = function
   <:sig_item< declare $list:l$ end >> ->
      begin
         match l with
            <:sig_item< value $name$ : $_$ >> :: _ ->
               name = prl_item_placeholder
          | _ ->
               false
      end
 | _ ->
      false

(*
 * If this is a PRL item,
 * return the command and the exprs that accompany it.
 *)
let dest_prl_sig_item info = function
   <:sig_item< declare $list:l$ end >> ->
      begin
         match l with
            <:sig_item< value $name$ : $t$ >> :: items
            when name = prl_item_placeholder ->
               begin
                  match t with
                     <:ctyp< $lid:s$ >> ->
                        let index = int_of_string s in
                           FilterCache.get_command info index, items
                    | _ ->
                        raise Not_found
               end
          | _ ->
               raise Not_found
     end
 | _ ->
      raise Not_found

let dest_sig_item = function
   <:sig_item< declare $list:l'$ end >> ->
       FlattenList l'
 | item ->
       FlattenItem item

let flatten_sig_item_list =
   flatten_item_list
   is_prl_sig_item
   dest_prl_sig_item
   dest_sig_item
   term_of_sig_item

(*
 * Structure items.
 *)
let prl_str_item loc items index =
   let patt = <:patt< $lid: prl_item_placeholder$ >> in
   let expr = <:expr< $int: string_of_int index$ >> in
   let head = <:str_item< value $rec:false$ $list: [ (patt, expr) ]$ >> in
      <:str_item< declare $list: head :: items$ end >>

let is_prl_str_item = function
   <:str_item< declare $list:l$ end >> ->
      begin
         match l with
            <:str_item< value $rec:false$ $list: [ (patt, expr) ]$ >> :: _ ->
               begin
                  match patt with
                     <:patt< $lid: name$ >> ->
                         name = prl_item_placeholder
                    | _ ->
                         false
               end
          | _ ->
               false
      end
 | _ ->
      false

(*
 * If this is a PRL item,
 * return the command and the exprs that accompany it.
 *)
let dest_prl_str_item info = function
   <:str_item< declare $list:l$ end >> ->
      begin
         match l with
            <:str_item< value $rec:false$ $list: [ pe ]$ >> :: items ->
               begin
                  match pe with
                     <:patt< $lid: name$ >>, <:expr< $int: s$ >>
                     when name = prl_item_placeholder ->
                        let index = int_of_string s in
                        let command = FilterCache.get_command info index in
                           command, items
                    | _ ->
                        raise Not_found
               end
          | _ ->
               raise Not_found
     end
 | _ ->
      raise Not_found

(*
 * A Flattener returns a sublist or a regular item.
 *)
let dest_str_item = function
   <:str_item< declare $list:l'$ end >> ->
      FlattenList l'
 | item ->
      FlattenItem item

let flatten_str_item_list =
   flatten_item_list
   is_prl_str_item
   dest_prl_str_item
   dest_str_item
   term_of_str_item

(************************************************************************
 * INTERFACE IMPLEMENTATION                                             *
 ************************************************************************)

let () = ()

(*
 * Convert a module path to an expression.
 *)
let rec parent_path_expr loc = function
   [h] ->
      <:expr< $uid:String.capitalize h$ >>
 | h::t ->
      <:expr< $uid:String.capitalize h$ . $parent_path_expr loc t$ >>
 | [] ->
      raise (Invalid_argument "parent_path")

let rec parent_path_ctyp loc = function
   [h] ->
      <:ctyp< $uid:String.capitalize h$ >>
 | h::t ->
      <:ctyp< $uid:String.capitalize h$ . $parent_path_ctyp loc t$ >>
 | [] ->
      raise (Invalid_argument "parent_path")

(*
 * Do this when a module is inlined.
 *)
let inline_hook proc root_path open_f cache (path, info) (paths, resources) =
   (* Include all the resources *)
   if debug_resource then
      eprintf "Inline_hook: %s, %s%t" (string_of_path root_path) (string_of_path path) eflush;
   let add_resource' rsrc =
      if debug_resource then
         eprintf "Adding resource: %s.%s%t" (string_of_path path) rsrc.resource_name eflush;
      FilterCache.add_resource cache path rsrc
   in
   let nresources' = get_resources info in
   let nresources =
      let rec collect resources = function
         rsrc::tl ->
            if List.mem rsrc resources then
               collect resources tl
            else
               collect (rsrc :: resources) tl
       | [] ->
            resources
      in
         collect resources nresources'
   in
      List.iter add_resource' nresources';
      
      (* Add all the infix words *)
      List.iter add_infix (get_infixes info);
      
      (* Return the "open" command *)
      open_f path :: paths, nresources

(*
 * Include a parent.
 * This incorporates the parent names into this module, as well
 * as opening the ML module.
 *)
let declare_parent proc loc open_f path =
   (* Lots of errors can occur here *)
   let info, (opens, _) = FilterCache.inline_module proc.cache path (inline_hook proc path open_f) ([], []) in
   let index = FilterCache.add_command proc.cache (Parent path) in
      prl_sig_item loc opens index

(*
 * Declare a term.
 * Just given the opname for now.
 *)
let declare_term prl_item proc loc (s, params, bterms) =
   let opname' = Opname.mk_opname s (FilterCache.op_prefix proc.cache) in
   let t = mk_term (mk_op opname' params) bterms in
   let _ = FilterCache.rm_opname proc.cache s in
   let _ = FilterCache.add_opname proc.cache s opname' in
   let index = FilterCache.add_command proc.cache (Opname { opname_name = s; opname_term = t }) in
      prl_item loc [] index, t

let declare_sig_term = declare_term prl_sig_item
let declare_str_term = declare_term prl_str_item

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
         (collect_vars params')
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
let declare_rewrite proc loc name params args =
   let cmd = rewrite_command proc name params args InterfaceProof in
   let index = FilterCache.add_command proc.cache cmd in
   let ctyp =
      match cmd with
         Rewrite _ ->
            rewrite_ctyp loc
       | CondRewrite _ ->
            cond_rewrite_ctyp loc
       | _ ->
            failwith "define_rewrite"
   in
      prl_sig_item loc [<:sig_item< value $name$ : $ctyp$ >>] index

(*
 * Declare a term, and define a rewrite in one step.
 *)
let define_term proc loc name redex contractum =
   let command, redex' = declare_sig_term proc loc redex in
   let command' = declare_rewrite proc loc name [] (MetaIff (MetaTheorem redex', MetaTheorem contractum)) in
      <:sig_item< declare $list: [command; command']$ end >>

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

let print_non_vars out params =
   print_terms out (collect_non_vars params)
   
let cond_axiom proc name params args pf =
   (* Extract context names *)
   let cvars = context_vars args in
   let bvars = binding_vars args in
   let params' = extract_params cvars bvars params in
      (* Do some checking on the rule *)
      if debug_grammar then
         begin
            eprintf "Checking rule: %s\n" name;
            eprintf "Non vars:\n%a" print_non_vars params';
            eprintf "Args:\n%a" print_terms (unzip_mimplies args)
         end;
      Refiner.check_rule (**)
         name
         (collect_cvars params')
         (collect_vars params')
         (collect_non_vars params')
         args;
      if debug_grammar then
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

      
let declare_axiom proc loc name params args =
   let cmd = axiom_command proc name params args InterfaceProof in
   let index = FilterCache.add_command proc.cache cmd in
   let params' =
      match cmd with
         Axiom _ -> []
       | Rule { rule_params = params' } -> params'
       | _ -> failwith "declare_axiom"
   in
   let ctyp = params_ctyp loc (tactic_ctyp loc) params' in
      prl_sig_item loc [<:sig_item< value $name$ : $ctyp$ >>] index

(*
 * Infix directive.
 *)
let declare_infix prl_item proc loc s =
   let index = FilterCache.add_command proc.cache (Infix s) in
      add_infix s;
      prl_item loc [] index

let declare_sig_infix = declare_infix prl_sig_item
let declare_str_infix = declare_infix prl_str_item

(*
 * Declare an ML term rewrite.
 *)
let declare_mlterm prl_item prl_pair proc loc ((name, _, _) as t) =
   let command, t' = declare_term prl_item proc loc t in
   let index = FilterCache.add_command proc.cache (MLTerm t') in
   let command' = prl_item loc [] index in
      prl_pair loc command command', t'

let declare_sig_mlterm =
   let pair loc command command' =
      <:sig_item< declare $list: [command; command']$ end >>
   in
      declare_mlterm prl_sig_item pair

let declare_str_mlterm =
   let pair loc command command' =
      <:str_item< declare $list: [command; command']$ end >>
   in
      declare_mlterm prl_str_item pair

(*
 * Declare a condition term for a rule.
 *)
let declare_ml_condition proc loc ((name, _, _) as t) =
   let command, t' = declare_sig_term proc loc t in
   let index = FilterCache.add_command proc.cache (Condition t') in
      prl_sig_item loc [command] index

(*
 * Record a resource.
 *
 * type resource_name
 *)
let declare_resource proc loc r =
   let { resource_name = name;
         resource_extract_type = extract_type;
         resource_improve_type = improve_type;
         resource_data_type = data_type
       } = r
   in
   let rsrc_type = <:ctyp< $resource_rsrc_ctyp loc$ $improve_type$ $extract_type$ $data_type$>> in
   let decl = (<:sig_item< type $list:[name, [], rsrc_type]$ >>) in
   let index = FilterCache.add_command proc.cache (Resource r) in
      FilterCache.add_resource proc.cache [] r;
      prl_sig_item loc [decl] index

(*
 * Dform declaration.
 *)
let declare_dform proc loc options t =
   let index = FilterCache.add_command proc.cache (DForm { dform_options = options;
                                                           dform_redex = t;
                                                           dform_def = None
                                                   })
   in
      prl_sig_item loc [] index

(*
 * Precedence declaration.
 *)
let declare_prec proc loc s =
   if FilterCache.find_prec proc.cache s then
      Stdpp.raise_with_loc loc (Failure (sprintf "prec '%s' already declared" s))
   else
      let index = FilterCache.add_command proc.cache (Prec s) in
         FilterCache.add_prec proc.cache s;
         prl_sig_item loc [<:sig_item< value $s$ : $precedence_ctyp loc$ >>] index

(*
 * List the resource in the postlog.
 *)
let interf_resources proc loc =
   let { name = my_name } = proc in
   let rec print names stmts = function
      (path, ({ resource_name = name } as rsrc))::t ->
         if List.mem name names then
            print names stmts t
         else
            let name_ctyp =
               if path = [] then
                  <:ctyp< $lid:name$ >>
               else
                  <:ctyp< $parent_path_ctyp loc path$ . $lid:name$ >>
            in
            let rsrc_item = (<:sig_item< value $name$ : $name_ctyp$ >>) in
(*
            let index = FilterCache.add_command proc.cache (InheritedResource rsrc) in
            let command = prl_sig_item loc [rsrc_item] index in
 *)
               print (name :: names) (rsrc_item :: stmts) t

    | [] ->
         List.rev stmts
   in
   let resources = FilterCache.resources proc.cache in
      print [] [] resources

(*
 * Trailer declares a new refiner.
 *)
let interf_postlog proc loc =
   let rsrcs = interf_resources proc loc in
   let refiner_decl = (<:sig_item< value $refiner_id$ : $refiner_ctyp loc$ >>) in
   let dformer_decl = (<:sig_item< value $dformer_id$ : $dformer_ctyp loc$ >>) in
   let stmts = refiner_decl :: dformer_decl :: rsrcs in
      <:sig_item< declare $list:stmts$ end >>

(************************************************************************
 * DEFINITION COMMANDS                                                  *
 ************************************************************************)

(*
 * Print the resources.
 * This requires the first element of the list,
 * and the second to last element (which contains the name of
 * the enclosing module).
 *)
let print_resources proc loc ppath nresources resources =
   (* Choice depends on whether resource exists yet or not *)
   let mem_resource name =
      let rec aux = function
         (_, { resource_name = name' })::t ->
            if debug_resource then
               eprintf "Resource: %s%t" name' eflush;
            if name = name' then
               true
            else
               aux t
       | [] -> false
      in
         aux nresources
   in
   let rec add_resource' names = function
      rsrc::t ->
         let { resource_name = name } = rsrc in
            if debug_resource then
               eprintf "Looking for resource %s%t" name eflush;
            if List.mem name names then
               add_resource' (name :: names) t
            else
               let name_expr = <:expr< $lid:name$ >> in
               let name_patt = <:patt< $lid:name$ >> in
               let parent_value = <:expr< $parent_path_expr loc ppath$ . $name_expr$ >> in
               let _ = FilterCache.add_resource proc.cache ppath rsrc in
               let item =
                  if mem_resource name then
                     (*
                      * type name = Parent.name
                      * let name = name.resource_join name Parent.name
                      *)
                     let rsrc_val = <:expr< $name_expr$ . $resource_join_expr loc$ $name_expr$ $parent_value$ >> in
                        (<:str_item< value $rec:false$ $list:[ name_patt, rsrc_val ]$ >>)
                  else
                     (*
                      * type name = Parent.name
                      * let name = Parent.name
                      *)
                     (<:str_item< value $rec:false$ $list:[ name_patt, parent_value ]$ >>)
               in
                  item :: (add_resource' (name :: names) t)
    
      | [] ->
         []
   in
      add_resource' [] resources

(*
 * Include a parent.
 * This incorporates the parent names into this module, as well
 * as opening the ML module.
 *
 * ... opens ...
 * join_refiner refiner_id Parent.refiner
 * join_mode_base dformer_id Parent.dformer
 * ... resources ...
 *)
let define_parent proc loc open_f path =
   (* Lots of errors can occur here *)
   let nresources = FilterCache.resources proc.cache in
   let info, (opens, resources) = FilterCache.inline_module proc.cache path (inline_hook proc path open_f) ([], []) in
   let parent_expr = <:expr< $parent_path_expr loc path$ >> in
   let join_refiner = <:expr< $join_refiner_expr loc$ $lid:local_refiner_id$ $parent_expr$ . $lid:refiner_id$ >> in
   let join_dformer = <:expr< $join_mode_base_expr loc$ $lid:local_dformer_id$ $parent_expr$ . $lid:dformer_id$ >> in
   let _ =
      if debug_resource then
         eprintf "Adding parent %s: %d%t" (string_of_path path) (List.length resources) eflush
   in
   let items = print_resources proc loc path nresources resources in
   let index = FilterCache.add_command proc.cache (Parent path) in
   let items = opens @ [<:str_item< $exp:join_refiner$ >>; <:str_item< $exp:join_dformer$ >>] @ items in
   let item = list_str_item loc items in
      prl_str_item loc [item] index

(*
 * A primitive rewrite is taken a true by fiat.
 *)
let prim_rewrite proc loc name params args =
   let cmd = rewrite_command proc name params args (ImplementationProof <:expr< () >>) in
   let index = FilterCache.add_command proc.cache cmd in
   let item =
      (* Analyze the rewrite term to decide what type of rewrite *)
      match cmd with
         Rewrite { rw_redex = redex; rw_contractum = contractum } ->
            (*
             * let name_rewrite =
             *    let redex_id = redex in
             *    let contractum_id = contractum in
             *    let rw = create_rewrite refiner name redex contractum in
             *    let _ = prim_rewrite refiner_id name redex contractum in
             *       rw
             * let name x = rewrite_of_rewrite name_rewrite x
             *)
            (* Names *)
            let rw_id = name ^ "_rewrite" in
            let rw_expr = <:expr< $lid:rw_id$ >> in
            let rw_patt = <:patt< $lid:rw_id$ >> in
            let x_patt = <:patt< $lid:"x"$ >> in
            let name_patt = <:patt< $lid:name$ >> in
            let wild_patt = <:patt< _ >> in
            let redex_expr = <:expr< $lid:"redex"$ >> in
            let con_expr = <:expr< $lid:"contractum"$ >> in
            let redex_patt = <:patt< $lid:"redex"$ >> in
            let con_patt = <:patt< $lid:"contractum"$ >> in
                                                            
            (* Expressions *)
            let redex_term = build_ml_term loc redex in
            let con_term = build_ml_term loc contractum in
            let create_expr =
               <:expr< $create_rewrite_expr loc$ $lid:local_refiner_id$ $str:name$ (**)
                  $lid:"redex"$ $lid:"contractum"$ >>
            in
            let prim_expr =
               <:expr< $prim_rewrite_expr loc$ $lid:local_refiner_id$ $str:name$ (**)
                  $lid:"redex"$ $lid:"contractum"$ >>
            in
            let rw_body_expr = <:expr< $rewrite_of_rewrite_expr loc$ $lid:rw_id$ $lid:"x"$ >> in
            let rw_fun_expr = <:expr< fun [ $list:[ x_patt, None, rw_body_expr ]$ ] >> in

            (* Let expressions *)            
            let body =
               <:expr< let $rec:false$ $list:[ redex_patt, redex_term;
                                               con_patt, con_term ]$
                       in
                       let $rec:false$ $list:[ name_patt, create_expr;
                                               wild_patt, prim_expr ]$
                       in
                          $lid:name$ >>
             in
             let name_rewrite_let =
                <:str_item< value $rec:false$ $list:[ rw_patt, body ]$ >>
             in
             let name_let =
                <:str_item< value $rec:false$ $list:[ name_patt, rw_fun_expr ]$ >>
             in
                <:str_item< declare $list:[ name_rewrite_let; name_let ]$ end >>

       | CondRewrite { crw_params = params';
                       crw_args = args';
                       crw_redex = redex;
                       crw_contractum = contractum
         } ->
            (*
             * let name_rewrite =
             *    let vars_id = [| vars |] in
             *    let params_id = [ params ] in
             *    let subgoals_id = [ subgoals ] in
             *    let redex_id = redex in
             *    let contractum_id = contractum in
             *    let rw = create_cond_rewrite refiner name vars_id params_id subgoals_id redex contractum in
             *    let _ = prim_cond_rewrite refiner_id name vars_id params_id subgoals_id redex contractum in
             *       rw
             * let name x = rewrite_of_cond_rewrite name_rewrite x
             *)
            (* Names *)
            let rw_id = name ^ "_rewrite" in
            let rw_expr = <:expr< $lid:rw_id$ >> in
            let rw_patt = <:patt< $lid:rw_id$ >> in
            let x_patt = <:patt< $lid:"x"$ >> in
            let name_patt = <:patt< $lid:name$ >> in
            let wild_patt = <:patt< _ >> in
            let vars_expr = <:expr< $lid:"vars"$ >> in
            let params_expr = <:expr< $lid:"params"$ >> in
            let subgoals_expr = <:expr< $lid:"subgoals"$ >> in
            let redex_expr = <:expr< $lid:"redex"$ >> in
            let con_expr = <:expr< $lid:"contractum"$ >> in
            let vars_patt = <:patt< $lid:"vars"$ >> in
            let params_patt = <:patt< $lid:"params"$ >> in
            let subgoals_patt = <:patt< $lid:"subgoals"$ >> in
            let redex_patt = <:patt< $lid:"redex"$ >> in
            let con_patt = <:patt< $lid:"contractum"$ >> in
                                                            
            (* Expressions *)
            let cvars = context_vars args in
            let bvars = binding_vars args in
            let string_expr s = <:expr< $str:s$ >> in
            let vars_val = <:expr< [| $list:List.map string_expr (cvars @ bvars)$ |] >> in
            let params_val = list_expr loc (param_expr loc) params' in
            let subgoals_val = list_expr loc (build_ml_term loc) args' in
            let redex_val = build_ml_term loc redex in
            let con_val = build_ml_term loc contractum in
            let create_expr =
               <:expr< $create_cond_rewrite_expr loc$ $lid:local_refiner_id$ $str:name$ (**)
                  $vars_expr$ $params_expr$ $subgoals_expr$ $redex_expr$ $con_expr$ >>
            in
            let prim_expr =
               <:expr< $prim_cond_rewrite_expr loc$ $lid:local_refiner_id$ $str:name$ (**)
                  $vars_expr$ $params_expr$ $subgoals_expr$ $redex_expr$ $con_expr$ >>
            in
            let rw_body_expr = <:expr< $rewrite_of_cond_rewrite_expr loc$ $lid:rw_id$ $lid:"x"$ >> in
            let rw_fun_expr = <:expr< fun [ $list:[ x_patt, None, rw_body_expr ]$ ] >> in

            (* Let construction *)
            let body =
               <:expr< let $rec:false$ $list:[ vars_patt, vars_val;
                                               params_patt, params_val;
                                               subgoals_patt, subgoals_val;
                                               redex_patt, redex_val;
                                               con_patt, con_val ]$
                       in
                       let $rec:false$ $list:[ name_patt, create_expr;
                                               wild_patt, prim_expr ]$
                       in
                          $lid:name$ >>
             in
             let name_rewrite_let =
                <:str_item< value $rec:false$ $list:[ rw_patt, body ]$ >>
             in
             let name_let =
                <:str_item< value $rec:false$ $list:[ name_patt, rw_fun_expr ]$ >>
             in
                <:str_item< declare $list:[ name_rewrite_let; name_let ]$ end >>
       | _ ->
            failwith "prim_rewrite"
   in
      prl_str_item loc [item] index

(*
 * Justify a rewrite with a tactic.
 *)
let rewrite_theorem proc loc name params args expr =
   let cmd = rewrite_command proc name params args (ImplementationProof expr) in
   let index = FilterCache.add_command proc.cache cmd in
   let item =
      (* Analyze the rewrite term to decide what type of rewrite *)
      match cmd with
         Rewrite { rw_redex = redex; rw_contractum = contractum } ->
            (* Check that this tactic actually works *)
            let redex_expr = build_ml_term loc redex in
            let con_expr = build_ml_term loc contractum in
            let expr = <:expr< $rewrite_theorem_expr loc$ $lid:local_refiner_id$ (**)
                          $str:name$ $redex_expr$ $con_expr$ $expr$ >>
            in
               <:str_item< $exp:expr$ >>

       | CondRewrite { crw_params = params';
                       crw_args = args';
                       crw_redex = redex;
                       crw_contractum = contractum
         } ->
            let params_expr = List.map (param_expr loc) params' in
            let args_expr = list_expr loc (build_ml_term loc) args' in
            let redex_expr = build_ml_term loc redex in
            let con_expr = build_ml_term loc contractum in
            let params_expr' = <:expr< [| $list:params_expr$ |] >> in
            let expr = <:expr< $cond_rewrite_theorem_expr loc$ $lid:local_refiner_id$ (**)
                          $str:name$ $params_expr'$
                          $args_expr$ $redex_expr$ $con_expr$ $expr$ >>
            in
               <:str_item< $exp:expr$ >>
       | _ ->
            failwith "rewrite_theorem"
   in
      prl_str_item loc [item] index

(*
 * Split the parameters into those that:
 *    1. specify contexts
 *    2. specify variable names
 *    3. specify parameters
 *)
let split_params name vars params params' =
   let rec aux vars = function
      v::t, (ContextParam v')::t' ->
         (* Context params are ignored *)
         if is_var_term v then
            let cvs, vs, terms, vars' = aux vars (t, t') in
               dest_var v::cvs, vs, terms, vars'
         else
            raise (BadCommand (name ^ ": mismatch on argument " ^ v'))
    | v::t, (VarParam v')::t' ->
         (* Var params are saved for naming *)
         if is_var_term v then
            let v = dest_var v in
            let cvs, vs, terms, vars' = aux vars (t, t') in
            let vars'' =
               try List_util.add_assoc (v, v') vars' with
                  _ -> raise (BadCommand (name ^ ": variable name mismatch: " ^ v ^ " -> " ^ v'))
            in
               cvs, v::vs, terms, vars''
         else
            raise (BadCommand (name ^ ": mismatch on argument " ^ v'))
    | t::tl, (TermParam t')::tl' ->
         begin
            try let vars' = generalization vars t t' in
                let cvs, vs, terms, vars'' = aux vars' (tl, tl') in
                   cvs, vs, t::terms, vars''
            with
               Invalid_argument "generalization" ->
                  raise (BadCommand (name ^ ": parameter mismatch"))
         end
    | [], [] ->
         [], [], [], vars

    | [], _::_ ->
         raise (BadCommand (name ^ ": too few arguments"))
    | _::_, [] ->
         raise (BadCommand (name ^ ": too many arguments"))
   in
      aux vars (params, params')

(*
 * Give names to the params.
 *    1. all names
 *    2. context names
 *    3. variable names
 *    4. term names
 *)
let param_names params =
   let rec name i = function
      [] ->
         [], [], [], []
    | h::t ->
         let names, cnames, vnames, tnames = name (i + 1) t in
         let name = sprintf "v%d" i in
         let names' = name :: names in
            match h with
               ContextParam _ ->
                  names', name :: cnames, vnames, tnames
             | VarParam _ ->
                  names', cnames, name :: vnames, tnames
             | TermParam _ ->
                  names', cnames, vnames, name :: tnames
   in
      name 0 params

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
 * A primitive rule specifies the extract.
 *)
let define_rule (code : MLast.expr) proc loc name
    (params : term list) (args : aterm list) (goal : term) (extract : MLast.expr) =
   let avars = collect_anames args in
   let assums = List.map (function { aterm = t } -> t) args in
   let mterm = zip_mimplies (assums @ [goal]) in
   let cmd = axiom_command proc name params mterm (ImplementationProof extract) in
   let index = FilterCache.add_command proc.cache cmd in
   let item =
      (* Pass it to the refiner *)
      match cmd with
         Axiom { axiom_stmt = stmt } ->
            (* Issue the command *)
            let goal_expr = build_ml_term loc stmt in
            let goals = list_expr loc (function x -> x) [goal_expr] in
            let axiom_value =
               <:expr< $create_axiom_expr loc$ $lid:local_refiner_id$ $str:name$ $goal_expr$ >>
            in
            let axiom_patt = <:patt< $lid:name$ >> in
            let thm =
               <:expr< $code$ $lid:local_refiner_id$ $str:name$ (**)
                  $nil_array loc$ $nil_list loc$ $goals$ $extract$ >>
            in
            let axiom_item = (<:str_item< value $rec:false$ $list:[axiom_patt, axiom_value]$ >>) in
            let thm_item = <:str_item< $exp:thm$ >> in
               (<:str_item< declare $list:[axiom_item; thm_item]$ end >>)

       | Rule { rule_params = params'; rule_stmt = mterm' } ->
            (* Check the specifications *)
            let string s = <:expr< $str:s$ >> in
            let lid_patt s = <:patt< $lid:s$ >> in
            let lid_expr s = <:expr< $lid:s$ >> in
   
            (*
             * Split var params from regular params.
             * let name_rule =
             *    let cvars = cvars_expr
             *    and tvars = tvars_expr
             *    and avars = avars_expr
             *    and params = tparams_expr
             *    and assums = assums_expr
             *    and extract = extract_expr
             *    in
             *    let rule = create_rule refiner "name" cvars tvars params assums
             *    and _ = prim_theorem refiner "name" tvars params avars extract
             *    in
             *       rule
             * let name params x = tactic_of_rule name_rule ([| cvars |], [| vars |]) [non_vars] x
             *)
            let name_rule_id = name ^ "_rule" in
            let cvars, tvars, tparams, vars' = split_params name [] params params' in
            let all_ids, cvar_ids, tvar_ids, tparam_ids = param_names params' in
            let cvars_expr = List.map string cvars in
            let cvars_expr' = <:expr< [| $list:cvars_expr$ |] >> in
            let tvars_expr = List.map string tvars in
            let tvars_expr' = <:expr< [| $list:tvars_expr$ |] >> in
            let avars_expr = list_expr loc (build_ml_term loc) avars in
            let tparams_expr = list_expr loc (build_ml_term loc) tparams in
            let assums_expr = build_ml_mterm loc mterm' in
            let axiom_value =
               <:expr< $create_rule_expr loc$ $lid:local_refiner_id$ $str:name$ (**)
                  $lid:"cvars"$ $lid:"tvars"$ $lid:"params"$ $lid:"assums"$ >>
            in
            let thm_value =
               <:expr< $code$ $lid:local_refiner_id$ $str:name$ (**)
                  $lid:"tvars"$ $lid:"params"$ $lid:"avars"$ $lid:"extract"$ >>
            in
            let name_value =
               let addr_expr id = <:expr< $make_seq_addr_expr loc$ $lid:id$ >> in
               let cvars_id_expr = <:expr< [| $list:List.map addr_expr cvar_ids$ |] >> in
               let tvars_id_expr = <:expr< [| $list:List.map lid_expr tvar_ids$ |] >> in
               let tparams_ids_expr = list_expr loc lid_expr tparam_ids in
               let body = <:expr< $tactic_of_rule_expr loc$ $lid:name_rule_id$
                                  ( $list:[ cvars_id_expr; tvars_id_expr ]$ )
                                  $tparams_ids_expr$ $lid:"x"$ >>
               in
                  fun_expr loc (all_ids @ ["x"]) body
            in
            let cvars_patt, tvars_patt, avars_patt,
                params_patt, assums_patt, extract_patt,
                rule_patt, name_patt, name_rule_patt, wild_patt =
               lid_patt "cvars", lid_patt "tvars", lid_patt "avars",
               lid_patt "params", lid_patt "assums", lid_patt "extract",
               lid_patt "rule", lid_patt name, lid_patt name_rule_id, <:patt< _ >>
            in
            let rule_expr =
               (<:expr< let $rec:false$ $list:[ cvars_patt, cvars_expr';
                                                tvars_patt, tvars_expr';
                                                avars_patt, avars_expr;
                                                params_patt, tparams_expr;
                                                assums_patt, assums_expr;
                                                extract_patt, extract ]$
                        in
                        let $rec:false$ $list:[ rule_patt, axiom_value;
                                                wild_patt, thm_value ]$
                        in
                            $lid:"rule"$ >>)
            in
            let rule_def = <:str_item< value $rec:false$ $list:[ name_rule_patt, rule_expr ]$ >> in
            let tac_def = <:str_item< value $rec:false$ $list:[ name_patt, name_value ]$ >> in
               <:str_item< declare $list:[ rule_def; tac_def ]$ end >>
       | _ ->
            failwith "define_rule"
   in
      prl_str_item loc [item] index

let prim_rule proc loc name params args goal extract =
   let code = prim_theorem_expr loc in
   let extract_expr = build_ml_term loc extract in
      define_rule code proc loc name params args goal extract_expr

let theorem proc loc name params args goal tac =
   let code = theorem_expr loc in
      define_rule code proc loc name params args goal tac

(*
 * An mlterm is a side condition that is checked in ML.
 * The term expands to the code production.
 *
 * Within the body, terms may expand to contracta.
 *
 * This is the code we create:
 * let _ =
 *    let term_id = redex in
 *    let redex_id = compile_redex [||] redex in
 *    let contractum_0_id = compile_contractum redex_id contractum_0_term in
 *    ...
 *    let contractum_n_id = compile_contractum redex_id contractum_n_term in
 *       code
 *)
let define_ml_program proc loc t term_id redex_id code =
   (* Identifier names *)
   let vars = free_vars code in
   let term_patt = <:patt< $lid:term_id$ >> in
   let redex_patt = <:patt< $lid:redex_id$ >> in
   let term_expr = <:expr< $lid:term_id$ >> in
   let redex_expr = <:expr< $lid:redex_id$ >> in
   
   (* Build a contractum *)
   let rec contracta_bind index = function
      t::tl ->
         let term_expr = build_ml_term loc t in
         let let_patt = <:patt< $lid: sprintf "contractum_%d" index$ >> in
         let let_value =
            <:expr< $compile_contractum_expr loc$ $redex_expr$ $term_expr$ >>
         in
            (let_patt, let_value) :: (contracta_bind (index + 1) tl)
    | [] ->
         []
   in
   
   (* Build the program *)
   let contracta_binding = contracta_bind 0 (end_rewrite ()) in
   let contracta_expr =
      if contracta_binding = [] then
         code
      else
         <:expr< let $rec:false$ $list:contracta_binding$ in $code$ >>
   in
   let redex_let_expr =
      <:expr< $compile_redex_expr loc$ $nil_array loc$ $term_expr$ >>
   in
   let redex_value_expr =
      <:expr< let $rec:false$ $list:[redex_patt, redex_let_expr]$ in $contracta_expr$ >>
   in
   let t_expr = build_ml_term loc t in
   let code_value_expr =
      <:expr< let $rec:false$ $list:[term_patt, t_expr]$ in $redex_value_expr$ >>
   in
      <:str_item< $exp:code_value_expr$ >>

let _ = ()

(*
 * An mlterm is a side condition that is checked in ML.
 * The term expands to the code production.
 *
 * Within the body, terms may expand to contracta.
 *
 * This is the code we create:
 * let _ =
 *    ... define_ml_program proc loc t ...
 *    let rewrite_id arg_id =
 *       let value_id = construct_redex [||] [] [arg_id] in
 *       let stack_id = apply_redex redex_id [||] value_id in
 *          code
 *    in
 *       create_ml_term refiner redex rewrite_id
 *)
let define_ml_term proc loc ((name, _, _) as t) code =
   let command, t' = declare_str_mlterm proc loc t in
   
   (* Identifier names *)
   let vars = free_vars code in
   let term_id = new_var loc "term" vars in
   let redex_id = new_var loc "redex" vars in
   let rewrite_id = new_var loc "rewrite" vars in
   let value_id = new_var loc "value" vars in
   let arg_id = new_var loc "arg" vars in
   
   let term_patt = <:patt< $lid:term_id$ >> in
   let redex_patt = <:patt< $lid:redex_id$ >> in
   let rewrite_patt = <:patt< $lid:rewrite_id$ >> in
   let value_patt = <:patt< $lid:value_id$ >> in
   let arg_patt = <:patt< $lid:arg_id$ >> in
   let stack_patt = <:patt< $lid:stack_id$ >> in
   
   let term_expr = <:expr< $lid:term_id$ >> in
   let redex_expr = <:expr< $lid:redex_id$ >> in
   let rewrite_expr = <:expr< $lid:rewrite_id$ >> in
   let value_expr = <:expr< $lid:value_id$ >> in
   let arg_expr = <:expr< $lid:arg_id$ >> in
   
   (* Build the program *)
   let body_expr =
      <:expr< $create_ml_term_expr loc$ $lid:local_refiner_id$ $term_expr$ $rewrite_expr$ >>
   in
   let stack_value_expr =
      <:expr< $apply_redex_expr loc$ $redex_expr$ $nil_array loc$ $value_expr$ >>
   in
   let stack_let_expr =
      <:expr< let $rec:false$ $list:[stack_patt, stack_value_expr]$ in $code$ >>
   in
   let value_value_expr =
      <:expr< $construct_redex_expr loc$ $nil_array loc$ $nil_list loc$ [ $arg_expr$ :: [] ] >>
   in
   let value_let_expr =
      <:expr< let $rec:false$ $list:[value_patt, value_value_expr]$ in $stack_let_expr$ >>
   in
   let rewrite_let_expr =
      <:expr< let $rec:false$ $list:[rewrite_patt, fun_expr loc [arg_id] value_let_expr]$ in $body_expr$ >>
   in
   let item = define_ml_program proc loc t' term_id redex_id rewrite_let_expr in
      <:str_item< declare $list:[ command; item ]$ end >>

let _ = ()

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
                  String s -> s
                | _ -> Stdpp.raise_with_loc loc (Term.TermMatch ("get_string_param", t, "param type"))
            end
       | { op_params = [] } ->
            Stdpp.raise_with_loc loc (Term.TermMatch ("get_string_param", t, "no params"))
       | _ ->
            Stdpp.raise_with_loc loc (Term.TermMatch ("get_string_param", t, "too many params"))

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
                  ["parens"] -> modes, (dform_parens_expr loc) :: options
                | ["prec"] -> modes, (dform_prec_expr loc (get_string_param loc hd)) :: options
                | ["inherit"] -> modes, (dform_inherit_prec_expr loc) :: options
                | ["mode"] -> (get_string_param loc hd)::modes, options
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
   let string_expr s = <:expr< $str:s$ >> in
   let modes_expr = list_expr loc string_expr modes in
   let options_expr = list_expr loc (fun x -> x) options' in
   let expansion_expr = <:expr< $dform_expansion_expr loc$ $build_ml_term loc expansion$ >> in
   let t_expr = build_ml_term loc t in
   let rec_value =
      <:expr< { $list:[ dform_pattern_expr loc, t_expr;
                        dform_options_expr loc, options_expr;
                        dform_print_expr loc, expansion_expr ]$ } >>
   in
   let expr = <:expr< $create_dform_expr loc$ $lid:local_dformer_id$ $modes_expr$ $rec_value$ >> in
      <:str_item< $exp:expr$ >>

(*
 * Precedence definition relation.
 *)
let define_prec proc loc s =
   if FilterCache.find_prec proc.cache s then
      Stdpp.raise_with_loc loc (Failure (sprintf "prec '%s' already declared" s))
   else
      let prec_patt = <:patt< $lid:s$ >> in
      let new_prec = <:expr< $new_prec_expr loc$ () >> in
      let index = FilterCache.add_command proc.cache (Prec s) in
      let _ = FilterCache.add_prec proc.cache s in
      let item = (<:str_item< value $rec:false$ $list:[ prec_patt, new_prec ]$ >>) in
         prl_str_item loc [item] index

let define_prec_rel proc loc s s' rel =
   if not (FilterCache.find_prec proc.cache s) then
      Stdpp.raise_with_loc loc (Failure (sprintf "prec '%s' not defined" s));
   if not (FilterCache.find_prec proc.cache s') then
      Stdpp.raise_with_loc loc (Failure (sprintf "prec '%s' not defined" s'));
   let expr =
      match rel with
         NoRelation ->
            <:expr< () >>
       | LTRelation ->
            <:expr< $add_lt_expr loc$ $lid:s$ $lid:s'$ >>
       | EQRelation ->
            <:expr< $add_eq_expr loc$ $lid:s$ $lid:s'$ >>
       | GTRelation ->
            <:expr< $add_lt_expr loc$ $lid:s'$ $lid:s$ >>
   in
      <:str_item< $exp:expr$ >>

(*
 * Pattern to match rewrite destruction.
 *)
let rewrite_type_patt loc = function
   RewriteTermType name ->
      <:patt< $uid:"Rewrite"$ . $uid:"RewriteTerm"$ $lid:name$ >>
 | RewriteFunType name ->
      <:patt< $uid:"Rewrite"$ . $uid:"RewriteFun"$ $lid:name$ >>
 | RewriteContextType name ->
      <:patt< $uid:"Rewrite"$ . $uid:"RewriteContext"$ $lid:name$ >>
 | RewriteStringType name ->
      <:patt< $uid:"Rewrite"$ . $uid:"RewriteString"$ $lid:name$ >>
 | RewriteIntType name ->
      <:patt< $uid:"Rewrite"$ . $uid:"RewriteInt"$ $lid:name$ >>
 | RewriteLevelType name ->
      <:patt< $uid:"Rewrite"$ . $uid:"RewriteLevel"$ $lid:name$ >>

(*
 * An ml dterm is a display form that is computed in ML.
 *
 * Within the body, terms may expand to contracta.
 *
 * This is the code we create:
 * let _ =
 *    ... define_ml_program proc loc t ...
 *    let printer { dform_stack = stack_id;
 *                  dform_items = [items];
 *                  dform_printer = printer;
 *                  dform_buffer = buffer
 *                } =
 *       code
 *    in
 *       create_dform name [modes] { dform_pattern = t;
 *                                   dform_options = [options];
 *                                   dform_print = DFormPrinter printer
 *                                 }
 *)
let define_ml_dform proc loc options t printer buffer code =
   (* Dform info *)
   let modes, options' = get_dform_options proc loc options in
   let string_expr s = <:expr< $str:s$ >> in
   let modes_expr = list_expr loc string_expr modes in
   let options_expr = list_expr loc (fun x -> x) options' in

   (* Identifier names *)
   let vars = printer :: buffer :: free_vars code in
   let term_id = new_var loc "term" vars in
   let redex_id = new_var loc "redex" vars in
   let dprinter_id = new_var loc "printer" vars in
   
   let term_patt = <:patt< $lid:term_id$ >> in
   let redex_patt = <:patt< $lid:redex_id$ >> in
   let dprinter_patt = <:patt< $lid:dprinter_id$ >> in
   let printer_patt = <:patt< $lid:printer$ >> in
   let buffer_patt = <:patt< $lid:buffer$ >> in
   let stack_patt = <:patt< $lid:stack_id$ >> in
   
   let term_expr = <:expr< $lid:term_id$ >> in
   let redex_expr = <:expr< $lid:redex_id$ >> in
   let dprinter_expr = <:expr< $lid:dprinter_id$ >> in
   let printer_expr = <:expr< $lid:printer$ >> in
   let buffer_expr = <:expr< $lid:buffer$ >> in
   
   (* Items *)
   let redex = compile_redex [||] t in
   let items = extract_redex_types redex in
   let items_patt = list_patt loc (rewrite_type_patt loc) items in
   
   (* Build the program *)
   let dprinter = <:expr< $dform_printer_expr loc$ $dprinter_expr$ >> in
   let rec_value =
      <:expr< { $list:[ dform_pattern_expr loc, term_expr;
                        dform_options_expr loc, options_expr;
                        dform_print_expr loc, dprinter ]$ } >>
   in
   let body_expr =
      <:expr< $create_dform_expr loc$ $lid:local_dformer_id$ $modes_expr$ $rec_value$ >>
   in
   let dprinter_rec_patt =
      <:patt< { $list:[ dform_stack_patt loc, stack_patt;
                        dform_items_patt loc, items_patt;
                        dform_printer_patt loc, printer_patt;
                        dform_buffer_patt loc, buffer_patt ]$ } >>
   in
   let wild_patt = <:patt< _ >> in
   let wild_code = <:expr< $lid:"failwith"$ $str:"bad match"$ >> in
   let dprinter_fun_expr =
      <:expr< fun [ $list:[dprinter_rec_patt, None, code; wild_patt, None, wild_code]$ ] >>
   in
   let dprinter_let_expr =
      <:expr< let $rec:false$ $list:[ dprinter_patt, dprinter_fun_expr ]$ in $body_expr$ >>
   in
      define_ml_program proc loc t term_id redex_id dprinter_let_expr

let _ = ()

(*
 * Record a resource.
 *
 * type resource_name
 *)
let define_resource proc loc r =
   let { resource_name = name;
         resource_extract_type = extract_type;
         resource_improve_type = improve_type;
         resource_data_type = data_type
       } = r
   in
   let rsrc_type = <:ctyp< $resource_rsrc_ctyp loc$ $improve_type$ $extract_type$ $data_type$>> in
   let decl = (<:str_item< type $list:[name, [], rsrc_type]$ >>) in
   let index = FilterCache.add_command proc.cache (Resource r) in
      FilterCache.add_resource proc.cache [] r;
      prl_str_item loc [decl] index

(*
 * A magic block computes a hash value from the definitions
 * in the block.
 *)
let define_magic_block loc name stmts =
   let index = List.fold_left Filter_hash.hash_str_item 0 stmts in
   let name_patt = <:patt< $lid:name$ >> in
   let index_val = <:expr< $int:string_of_int index$ >> in
   let hash_def = <:str_item< value $rec:false$ $list:[ name_patt, index_val ]$ >> in
   let l = hash_def :: stmts in
      <:str_item< declare $list:l$ end >>
   
(*
 * Prolog declares the refiner and dformer.
 *
 * let refiner_name = ref Refine.Refiner.null_refiner
 * let dformer_name = ref Dform_print.null_mode_base
 *)
let implem_prolog proc loc =
   let refiner_val = <:expr< $lid:"ref"$ $null_refiner_expr loc$ >> in
   let dformer_val = <:expr< $lid:"ref"$ $null_mode_base_expr loc$ >> in
   let refiner_patt = <:patt< $lid:local_refiner_id$ >> in
   let dformer_patt = <:patt< $lid:local_dformer_id$ >> in
      (<:str_item< value $rec:false$ $list:[ refiner_patt, refiner_val; dformer_patt, dformer_val ]$ >>)

(*
 * Trailing declarations.
 *
 * let _ = Refiner.label_refiner refiner_name "module_name"
 * let refiner = !refiner_name
 * let dformer = !dformer_name
 * let theory_name =
 *    { thy_name = "module_name";
 *      thy_refiner = refiner;
 *      thy_dformer = dformer
 *    }
 * let _ = record_theory theory_name
 *)
let implem_postlog proc loc name id =
   let thy_elems =
      [(<:expr< $thy_name_expr loc$ >>, <:expr< $str:name$ >>);
       (<:expr< $thy_refiner_expr loc$ >>, <:expr< $lid:refiner_id$ >>);
       (<:expr< $thy_dformer_expr loc$ >>, <:expr< $lid:dformer_id$ >>);
       (<:expr< $thy_id_expr loc$ >>, <:expr< $int:string_of_int id$ >>)]
   in
   let thy_rec = <:expr< { $list:thy_elems$ } >> in
   let thy = <:expr< $record_theory_expr loc$ $thy_rec$ >> in
   let refiner_patt = <:patt< $lid:refiner_id$ >> in
   let dformer_patt = <:patt< $lid:dformer_id$ >> in
   let refiner_val = <:expr< $lid:local_refiner_id$ . $lid:"val"$ >> in
   let dformer_val = <:expr< $lid:local_dformer_id$ . $lid:"val"$ >> in
   let label_expr = <:expr< $label_refiner_expr loc$ $lid:local_refiner_id$ $str:name$ >> in
   let stmts =
      [(<:str_item< $exp:label_expr$ >>);
       (<:str_item< value $rec:false$
                           $list:[refiner_patt, refiner_val;
                                  dformer_patt, dformer_val]$ >>);
       (<:str_item< $exp:thy$ >>)]
   in
      <:str_item< declare $list:stmts$ end >>

(************************************************************************
 * INPUT PROCESSING                                                     *
 ************************************************************************)

(*
 * File saving.
 *)
let interface_postlog proc loc =
   let { name = name } = proc in
   let id = Hashtbl.hash proc in
   let index = FilterCache.add_command proc.cache (Id id) in
      prl_sig_item loc [interf_postlog proc loc] index

let save_interface proc commands =
   FilterCache.set_commands proc.cache commands;
   FilterCache.save proc.cache

(*
 * Check that the implementation matches the interfaces.
 * BUG: Need to adjust the id (it shouldn't be 0).
 *)
let implementation_postlog proc loc =
   let { name = name; cache = cache } = proc in
      implem_postlog proc loc name 0

let save_implementation proc commands =
   FilterCache.set_commands proc.cache commands;
   FilterCache.save proc.cache

(*
 * Save the include path.
 *)
let include_path = ref ["."]

let set_include_path path =
   include_path := path

(*
 * Processor.
 *)
let proc_ref = ref None

let get_proc loc =
   match !proc_ref with
      Some proc -> proc
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
            proc_ref := Some proc;
            proc

(************************************************************************
 * TERM GRAMMAR                                                         *
 ************************************************************************)

(*
 * Base term grammar.
 *)
module TermGrammarBefore : TermGrammarSig =
struct
   let mk_opname loc l = 
      try FilterCache.mk_opname (get_proc loc).cache l with
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
 * GRAMMAR MUNGING                                                      *
 ************************************************************************)

(************************************************************************
 * GRAMMAR EXTENSION                                                    *
 ************************************************************************)

(*
 * Extend the programming language.
 *)
let _ =
   Grammar.Unsafe.clear_entry interf;
   Grammar.Unsafe.clear_entry implem

EXTEND
   GLOBAL: interf implem sig_item str_item expr;

   interf:
      [[ interf_opening; st = LIST0 [ s = sig_item; OPT ";;" -> (s, loc) ]; EOI ->
          let proc = get_proc loc in
          let l = st @ [interface_postlog proc loc, loc] in
          let commands, items = flatten_sig_item_list proc.cache l in
             save_interface proc commands;
             items
       ]];
   
   interf_opening:
      [[ OPT "PRL_interface" ->
          get_proc loc
       ]];
   
   implem:
      [[ o = implem_opening; st = LIST0 [ s = str_item; OPT ";;" -> (s, loc) ]; EOI ->
          let proc = get_proc loc in
          let l = o @ st @ [implementation_postlog proc loc, loc] in
          let commands, items = flatten_str_item_list proc.cache l in
             save_implementation proc commands;
             items
       ]];
   
   implem_opening:
      [[ OPT "PRL_implementation" ->
          [implem_prolog (get_proc loc) loc, loc]
       ]];

   sig_item:
      [[ "include"; path = mod_ident ->
          declare_parent (get_proc loc) loc (sig_open loc) path
        | "declare"; t = quote_term ->
          fst (declare_sig_term (get_proc loc) loc t)
        | "define"; name = LIDENT; ":"; t = quote_term; "<-->"; def = term ->
          define_term (get_proc loc) loc name t def
        | "rewrite"; name = LIDENT; args = optarglist; ":"; t = mterm ->
          declare_rewrite (get_proc loc) loc name args t
        | "axiom"; name = LIDENT; args = optarglist; ":"; t = mterm ->
          declare_axiom (get_proc loc) loc name args t
        | "mlterm"; t = quote_term ->
          fst (declare_sig_mlterm (get_proc loc) loc t)
        | "mlcondition"; t = quote_term ->
          declare_ml_condition (get_proc loc) loc t
        | "resource"; "("; improve = ctyp; ","; extract = ctyp; ","; data = ctyp; ")"; name = LIDENT ->
          declare_resource (get_proc loc) loc (**)
             { resource_name = name;
               resource_extract_type = extract;
               resource_improve_type = improve;
               resource_data_type = data
             }
        | "dform"; options = df_options ->
          let options', t = options in
             declare_dform (get_proc loc) loc options' t
        | "infix"; name = ident ->
          declare_sig_infix (get_proc loc) loc name
        | "prec"; name = LIDENT ->
          declare_prec (get_proc loc) loc name
       ]];
   
   str_item:
      [[ "jyh_test"; t = TermGrammar.term ->
          raise (Failure "jyh")
        | "include"; path = mod_ident ->
          define_parent (get_proc loc) loc (str_open loc) path
        | "declare"; t = quote_term ->
          fst (declare_str_term (get_proc loc) loc t)
        | "primrw"; name = LIDENT; args = optarglist; ":"; t = mterm ->
          prim_rewrite (get_proc loc) loc name args t
        | "rwthm"; name = LIDENT; args = optarglist; ":"; t = mterm; "="; body = expr ->
          rewrite_theorem (get_proc loc) loc name args t body
        | "prim"; name = LIDENT; params = optarglist; ":"; (**)
             (args, goal) = opt_binding_arglist; "="; (**)
             extract = term ->
          prim_rule (get_proc loc) loc name params args goal.aterm extract
        | "thm"; name = LIDENT; params = optarglist; ":"; (**)
             (args, goal) = opt_binding_arglist; "="; tac = expr ->
          theorem (get_proc loc) loc name params args goal.aterm tac
        | "mlterm"; t = quote_term; rewrite_equal; code = expr ->
          define_ml_term (get_proc loc) loc t code
        | "resource"; "("; improve = ctyp; ","; extract = ctyp; ","; data = ctyp; ")"; name = LIDENT ->
          define_resource (get_proc loc) loc (**)
             { resource_name = name;
               resource_extract_type = extract;
               resource_improve_type = improve;
               resource_data_type = data
             }
        | "dform"; options = df_options; "="; form = xdform ->
          let options', t = options in
             define_dform (get_proc loc) loc options' t form
        | "mldform"; options = df_options; buf = LIDENT; format = LIDENT; "="; code = expr ->
          let options', t = options in
             define_ml_dform (get_proc loc) loc options' t buf format code
        | "infix"; name = ident ->
          declare_str_infix (get_proc loc) loc name
        | "prec"; name = LIDENT ->
          define_prec (get_proc loc) loc name
        | "prec"; name1 = LIDENT; "<"; name2 = LIDENT ->
          define_prec_rel (get_proc loc) loc name1 name2 LTRelation
        | "prec"; name1 = LIDENT; "="; name2 = LIDENT ->
          define_prec_rel (get_proc loc) loc name1 name2 EQRelation
        | "prec"; name1 = LIDENT; ">"; name2 = LIDENT ->
          define_prec_rel (get_proc loc) loc name1 name2 GTRelation
        | "magic_block"; name = LIDENT; "=";
          "struct"; st = LIST0 [ s = str_item; OPT ";;" -> s ]; "end" ->
             define_magic_block loc name st
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
