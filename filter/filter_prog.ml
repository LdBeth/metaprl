(*
 * Conversion from module_info to program text.
 *)

open Printf

open Debug

open Term
open Term_util
open Precedence
open Rewrite

open Free_vars
open Filter_debug
open Filter_util
open Filter_ast
open Filter_cache
open Filter_summary_type
open Filter_summary_util
open Filter_summary

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

(*
 * Param expression.
 *)
let param_expr loc = function
   ContextParam s ->
      <:expr< $uid:"Filter_summary"$ . $uid:"ContextParam"$ $str:s$ >>
 | VarParam v ->
      <:expr< $uid:"Filter_summary"$ . $uid:"VarParam"$ $str:v$ >>
 | TermParam t ->
      let t' = build_ml_term loc t in
         <:expr< $uid:"Filter_summary"$ . $uid:"TermParam"$ $t'$ >>
                                           
(*
 * Create function type.
 *)
let params_ctyp loc ctyp params =
   let rec convert = function
      [] -> ctyp
    | h::t ->
         let ctyp' = convert t in
         let arg_type =
            match h with
               ContextParam _ ->
                  <:ctyp< $lid:"int"$ >>
             | VarParam _ ->
                  <:ctyp< $lid:"string"$ >>
             | TermParam _ ->
                  <:ctyp< $uid:"Term"$ . $lid:"term"$ >>
         in
            <:ctyp< $arg_type$ -> $ctyp'$ >>
   in
      convert params

(*
 * Convert display form options to expressions.
 *)
let dform_option_expr loc = function
   DFormParens ->
      dform_parens_expr loc
 | DFormPrec p ->
      dform_prec_expr loc p
 | DFormInheritPrec ->
      dform_inherit_prec_expr loc

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

(************************************************************************
 * SIGNATURES                                                           *
 ************************************************************************)

(*
 * Rewrites.
 *)
let declare_rewrite loc { rw_name = name } =
   let ctyp = rewrite_ctyp loc in
      [<:sig_item< value $name$ : $ctyp$ >>]

let declare_cond_rewrite loc { crw_name = name } =
   let ctyp = cond_rewrite_ctyp loc in
      [<:sig_item< value $name$ : $ctyp$ >>]

(*
 * Rules.
 *)
let declare_axiom loc { axiom_name = name } =
   let ctyp = params_ctyp loc (tactic_ctyp loc) [] in
      [<:sig_item< value $name$ : $ctyp$ >>]

let declare_rule loc { rule_name = name; rule_params = params } =
   let ctyp = params_ctyp loc (tactic_ctyp loc) params in
      [<:sig_item< value $name$ : $ctyp$ >>]

(*
 * Precedence.
 *)
let declare_prec loc name =
   [<:sig_item< value $name$ : $precedence_ctyp loc$ >>]

(*
 * Resource.
 *)
let declare_resource loc { resource_name = name;
                           resource_extract_type = extract_type;
                           resource_improve_type = improve_type;
                           resource_data_type = data_type
    } =
   let rsrc_type = <:ctyp< $resource_rsrc_ctyp loc$ $improve_type$ $extract_type$ $data_type$>> in
      [<:sig_item< type $list:[name, [], rsrc_type]$ >>]

(*
 * When a parent is declared, we need to open all the ancestors.
 *)
let declare_parent loc { parent_opens = opens } =
   let mk_open path =
      <:sig_item< open $path$ >>
   in
      List.map mk_open opens

(*
 * Standard summary item.
 *)
let declare_summary_item loc item =
   [item]

(*
 * Magic block is a block of items.
 *)
let declare_magic_block loc { magic_code = items } =
   items

(*
 * Collect the inherited resources.
 *)
let interf_resources resources loc =
   let rec loop names = function
      (mname, { resource_name = name;
                resource_extract_type = extract_type;
                resource_improve_type = improve_type;
                resource_data_type = data_type
       } as rsrc)::t ->
         if debug_resource then
            if mname = [] then
               eprintf "Resource: %s%t" name eflush
            else
               eprintf "Resource: %s/%s%t" (string_of_path mname) name eflush;
         if not (List.mem name names) then
            let ctyp =
               if mname = [] then
                  (<:ctyp< $lid: name$ >>)
               else
                  let ctyp = parent_path_ctyp loc mname in
                     (<:ctyp< $ctyp$ . $lid: name$ >>)
            in
               (<:sig_item< value $name$ : $ctyp$ >>) :: (loop (name :: names) t)
         else
            loop names t
   
    | [] ->
         []
   in
      loop [] resources

(*
 * Trailer declares a new refiner.
 *)
let interf_postlog info loc =
   let refiner_decl = (<:sig_item< value $refiner_id$ : $refiner_ctyp loc$ >>) in
   let dformer_decl = (<:sig_item< value $dformer_id$ : $dformer_ctyp loc$ >>) in
   let resources = interf_resources info loc in
      refiner_decl :: dformer_decl :: resources

(*
 * Extract a signature item.
 *)
let extract_sig_item (item, loc) =
   match item with
      Rewrite ({ rw_name = name } as rw) ->
         if debug_filter_prog then
            eprintf "Filter_prog.extract_sig_item: rewrite: %s%t" name eflush;
         declare_rewrite loc rw
    | CondRewrite ({ crw_name = name } as crw) ->
         if debug_filter_prog then
            eprintf "Filter_prog.extract_sig_item: cond rewrite: %s%t" name eflush;
         declare_cond_rewrite loc crw
    | Axiom ({ axiom_name = name } as ax) ->
         if debug_filter_prog then
            eprintf "Filter_prog.extract_sig_item: axiom: %s%t" name eflush;
         declare_axiom loc ax
    | Rule ({ rule_name = name } as rule) ->
         if debug_filter_prog then
            eprintf "Filter_prog.extract_sig_item: rule: %s%t" name eflush;
         declare_rule loc rule
    | Prec name ->
         if debug_filter_prog then
            eprintf "Filter_prog.extract_sig_item: prec: %s%t" name eflush;
         declare_prec loc name
    | Resource ({ resource_name = name } as rsrc) ->
         if debug_filter_prog then
            eprintf "Filter_prog.extract_sig_item: resource: %s%t" name eflush;
         declare_resource loc rsrc
    | Parent ({ parent_name = name } as parent) ->
         if debug_filter_prog then
            eprintf "Filter_prog.extract_sig_item: parent: %s%t" (string_of_path name) eflush;
         declare_parent loc parent
    | SummaryItem item ->
         if debug_filter_prog then
            eprintf "Filter_prog.extract_sig_item: summary_item%t" eflush;
         declare_summary_item loc item
    | MagicBlock block ->
         if debug_filter_prog then
            eprintf "Filter_prog.extract_sig_item: magic block%t" eflush;
         declare_magic_block loc block
    | Opname _ ->
         if debug_filter_prog then
            eprintf "Filter_prog.extract_sig_item: opname%t" eflush;
         []
    | MLTerm _ ->
         if debug_filter_prog then
            eprintf "Filter_prog.extract_sig_item: mlterm%t" eflush;
         []
    | Condition _ ->
         if debug_filter_prog then
            eprintf "Filter_prog.extract_sig_item: condition%t" eflush;
         []
    | DForm _ ->
         if debug_filter_prog then
            eprintf "Filter_prog.extract_sig_item: dform%t" eflush;
         []
    | PrecRel _ ->
         if debug_filter_prog then
            eprintf "Filter_prog.extract_sig_item: prec rel%t" eflush;
         []
    | Id id ->
         if debug_filter_prog then
            eprintf "Filter_prog.extract_sig_item: id: 0x%08x%t" id eflush;
         []
    | Infix name ->
         if debug_filter_prog then
            eprintf "Filter_prog.extract_sig_item: infix: %s%t" name eflush;
         []
    | Module (name, _) ->
         if debug_filter_prog then
            eprintf "Filter_prog.extract_sig_item: module: %s%t" name eflush;
         raise (Failure "Filter_sig.extract_sig_item: nested modules are not implemented")

(*
 * Extract a signature.
 *)
let extract_sig info resources path =
   let _ =
      if debug_filter_prog then
         eprintf "Filter_prog.extract_sig: begin%t" eflush
   in
   let items = List_util.flat_map extract_sig_item (info_items info) in
   let postlog = interf_postlog resources (0, 0) in
      List.map (fun item -> item, (0, 0)) (items @ postlog)

(************************************************************************
 * IMPLEMENTATIONS                                                      *
 ************************************************************************)

(*
 * For implementations, we maintain a state, which contains
 *    1. a list of the resources that have been defined
 *)
type state =
   { mutable imp_resources : MLast.ctyp resource_info list
   }

(*
 * A primitive rewrite is assumed true by fiat.
 *
 * let name_rewrite =
 *    let redex_id = redex in
 *    let contractum_id = contractum in
 *    let rw = create_rewrite refiner name redex contractum in
 *    let _ = prim_rewrite refiner_id name redex contractum in
 *       rw
 * let name x = rewrite_of_rewrite name_rewrite x
 *)
let prim_rewrite proc loc
    { rw_name = name;
      rw_redex = redex;
      rw_contractum = contractum
    } =
   (* Names *)
   let rw_id      = name ^ "_rewrite" in
   let rw_expr    = <:expr< $lid:rw_id$ >> in
   let rw_patt    = <:patt< $lid:rw_id$ >> in
   let x_patt     = <:patt< $lid:"x"$ >> in
   let name_patt  = <:patt< $lid:name$ >> in
   let wild_patt  = <:patt< _ >> in
   let redex_expr = <:expr< $lid:"redex"$ >> in
   let con_expr   = <:expr< $lid:"contractum"$ >> in
   let redex_patt = <:patt< $lid:"redex"$ >> in
   let con_patt   = <:patt< $lid:"contractum"$ >> in
                                                   
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
       [name_rewrite_let; name_let ]

let ()  = ()

(*
 * Conditional rewrite is a little more complicated.
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
let prim_cond_rewrite proc loc
    { crw_name       = name;
      crw_params     = params;
      crw_args       = args;
      crw_redex      = redex;
      crw_contractum = contractum
    } =
   (* Names *)
   let rw_id         = name ^ "_rewrite" in
   let rw_expr       = <:expr< $lid:rw_id$ >> in
   let rw_patt       = <:patt< $lid:rw_id$ >> in
   let x_patt        = <:patt< $lid:"x"$ >> in
   let name_patt     = <:patt< $lid:name$ >> in
   let wild_patt     = <:patt< _ >> in
   let vars_expr     = <:expr< $lid:"vars"$ >> in
   let params_expr   = <:expr< $lid:"params"$ >> in
   let subgoals_expr = <:expr< $lid:"subgoals"$ >> in
   let redex_expr    = <:expr< $lid:"redex"$ >> in
   let con_expr      = <:expr< $lid:"contractum"$ >> in
   let vars_patt     = <:patt< $lid:"vars"$ >> in
   let params_patt   = <:patt< $lid:"params"$ >> in
   let subgoals_patt = <:patt< $lid:"subgoals"$ >> in
   let redex_patt    = <:patt< $lid:"redex"$ >> in
   let con_patt      = <:patt< $lid:"contractum"$ >> in
                                                   
   (* Expressions *)
   let cvars = collect_cvars params in
   let bvars = collect_vars params in
   let string_expr s = <:expr< $str:s$ >> in
   let vars_val = <:expr< [| $list:List.map string_expr (cvars @ bvars)$ |] >> in
   let params_val = list_expr loc (param_expr loc) params in
   let subgoals_val = list_expr loc (build_ml_term loc) args in
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
       [name_rewrite_let; name_let]

let () = ()

(*
 * Justify a rewrite with a tactic.
 *)
let rewrite_theorem proc loc
    { rw_name = name;
      rw_redex = redex;
      rw_contractum = contractum
    }
    expr =
   (* Check that this tactic actually works *)
   let redex_expr = build_ml_term loc redex in
   let con_expr = build_ml_term loc contractum in
   let expr = <:expr< $rewrite_theorem_expr loc$ $lid:local_refiner_id$ (**)
                      $str:name$ $redex_expr$ $con_expr$ $expr$
              >>
   in
      [<:str_item< $exp:expr$ >>]

let cond_rewrite_theorem proc loc
    { crw_name = name;
      crw_params = params;
      crw_args = args;
      crw_redex = redex;
      crw_contractum = contractum
    }
    expr =
   let params_expr = List.map (param_expr loc) params in
   let args_expr = list_expr loc (build_ml_term loc) args in
   let redex_expr = build_ml_term loc redex in
   let con_expr = build_ml_term loc contractum in
   let params_expr' = <:expr< [| $list:params_expr$ |] >> in
   let expr = <:expr< $cond_rewrite_theorem_expr loc$ $lid:local_refiner_id$ (**)
                      $str:name$ $params_expr'$
                      $args_expr$ $redex_expr$ $con_expr$ $expr$ >>
   in
      [<:str_item< $exp:expr$ >>]

let () = ()

(*
 * A primitive rule specifies the extract.
 *)
let define_axiom code proc loc { axiom_name = name; axiom_stmt = stmt } extract =
   let goal_expr = build_ml_term loc stmt in
   let goals = list_expr loc (function x -> x) [goal_expr] in
   let axiom_value =
      <:expr< $create_axiom_expr loc$ $lid:local_refiner_id$ $str:name$ $goal_expr$ >>
   in
   let axiom_patt = <:patt< $lid:name$ >> in
   let thm =
      <:expr< $code$ $lid:local_refiner_id$ $str:name$ (**)
              $nil_array loc$ $nil_list loc$ $goals$ $extract$
      >>
   in
   let axiom_item = (<:str_item< value $rec:false$ $list:[axiom_patt, axiom_value]$ >>) in
   let thm_item = <:str_item< $exp:thm$ >> in
      [axiom_item; thm_item]

let prim_axiom proc loc ax extract =
   let code = prim_theorem_expr loc in
   let extract_expr = build_ml_term loc extract in
      define_axiom code proc loc ax extract_expr

let thm_axiom proc loc ax tac =
   let code = theorem_expr loc in
      define_axiom code proc loc ax tac

let () = ()

(*
 * A rule is an axiom with parameters.
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
let define_rule code proc loc
    { rule_name = name;
      rule_params = params;
      rule_stmt = stmt
    }
    extract =
   (* Check the specifications *)
   let string s   = <:expr< $str:s$ >> in
   let lid_patt s = <:patt< $lid:s$ >> in
   let lid_expr s = <:expr< $lid:s$ >> in

   (* Expressions *)
   let name_rule_id = name ^ "_rule" in
   let cvars, tvars, tparams = split_params params in
   let all_ids, cvar_ids, tvar_ids, tparam_ids = name_params params in
   let cvars_expr = List.map string cvars in
   let cvars_expr' = <:expr< [| $list:cvars_expr$ |] >> in
   let tvars_expr = List.map string tvars in
   let tvars_expr' = <:expr< [| $list:tvars_expr$ |] >> in
   let avars, mterm = split_mfunction stmt in
   let avars_expr = list_expr loc (build_ml_term loc) avars in
   let tparams_expr = list_expr loc (build_ml_term loc) tparams in
   let assums_expr = build_ml_mterm loc mterm in
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
      [rule_def; tac_def]

let prim_rule proc loc ax extract =
   let code = prim_theorem_expr loc in
   let extract_expr = build_ml_term loc extract in
      define_rule code proc loc ax extract_expr

let thm_rule proc loc ax tac =
   let code = theorem_expr loc in
      define_rule code proc loc ax tac

let () = ()

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
let define_ml_program proc loc term_id redex_id t cons code =
   (* Identifier names *)
   let vars       = free_vars code in
   let term_patt  = <:patt< $lid:term_id$ >> in
   let redex_patt = <:patt< $lid:redex_id$ >> in
   let term_expr  = <:expr< $lid:term_id$ >> in
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
   let contracta_binding = contracta_bind 0 cons in
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
      [<:str_item< $exp:code_value_expr$ >>]

let () = ()

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
let define_ml_term proc loc t cons code =
   let vars       = free_vars code in
   let term_id    = new_var loc "term" vars in
   let redex_id   = new_var loc "redex" vars in
   let rewrite_id = new_var loc "rewrite" vars in
   let value_id   = new_var loc "value" vars in
   let arg_id     = new_var loc "arg" vars in
   
   let term_patt    = <:patt< $lid:term_id$ >> in
   let redex_patt   = <:patt< $lid:redex_id$ >> in
   let rewrite_patt = <:patt< $lid:rewrite_id$ >> in
   let value_patt   = <:patt< $lid:value_id$ >> in
   let arg_patt     = <:patt< $lid:arg_id$ >> in
   let stack_patt   = <:patt< $lid:stack_id$ >> in
   
   let term_expr    = <:expr< $lid:term_id$ >> in
   let redex_expr   = <:expr< $lid:redex_id$ >> in
   let rewrite_expr = <:expr< $lid:rewrite_id$ >> in
   let value_expr   = <:expr< $lid:value_id$ >> in
   let arg_expr     = <:expr< $lid:arg_id$ >> in
   
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
      define_ml_program proc loc term_id redex_id t cons rewrite_let_expr

let () = ()

(*
 * Define a display form expansion.
 *
 * create_dform dformer [modes]
 *    { dform_pattern = t;
 *      dform_options = [options];
 *      dform_print = DFormExpansion expansion
 *    }
 *)
let define_dform proc loc
    { dform_modes = modes;
      dform_options = options;
      dform_redex = t
    }
    expansion =
   let string_expr s = <:expr< $str:s$ >> in
   let modes_expr = list_expr loc string_expr modes in
   let options_expr = list_expr loc (dform_option_expr loc) options in
   let expansion_expr = <:expr< $dform_expansion_expr loc$ $build_ml_term loc expansion$ >> in
   let t_expr = build_ml_term loc t in
   let rec_value =
      <:expr< { $list:[ dform_pattern_expr loc, t_expr;
                        dform_options_expr loc, options_expr;
                        dform_print_expr loc, expansion_expr ]$ } >>
   in
   let expr = <:expr< $create_dform_expr loc$ $lid:local_dformer_id$ $modes_expr$ $rec_value$ >> in
      [<:str_item< $exp:expr$ >>]

(*
 * Precedence definition relation.
 *)
let define_prec proc loc s =
   let prec_patt = <:patt< $lid:s$ >> in
   let new_prec = <:expr< $new_prec_expr loc$ () >> in
      [<:str_item< value $rec:false$ $list:[ prec_patt, new_prec ]$ >>]

let define_prec_rel proc loc
    { prec_left = s;
      prec_right = s';
      prec_rel = rel
    } =
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
      [<:str_item< $exp:expr$ >>]

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
let define_ml_dform proc loc
    { dform_modes = modes;
      dform_options = options;
      dform_redex = t
    }
    { dform_ml_printer = printer;
      dform_ml_buffer = buffer;
      dform_ml_contracta = cons;
      dform_ml_code = code
    } =
   (* Dform info *)
   let string_expr s = <:expr< $str:s$ >> in
   let modes_expr = list_expr loc string_expr modes in
   let options_expr = list_expr loc (dform_option_expr loc) options in

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
      define_ml_program proc loc term_id redex_id t cons dprinter_let_expr

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
      proc.imp_resources <- r :: proc.imp_resources;
      [<:str_item< type $list:[name, [], rsrc_type]$ >>]

(*
 * When a parent is included, we need to open all the ancestors,
 * and we need to patch in all the resources.
 *)
let define_parent proc loc
    { parent_name = path;
      parent_opens = opens;
      parent_resources = nresources
    } =
   let print_resource resources resource =
      let { resource_name = name } = resource in
      let name_expr = (<:expr< $lid:name$ >>) in
      let name_patt = (<:patt< $lid:name$ >>) in
      let parent_value = (<:expr< $parent_path_expr loc path$ . $name_expr$ >>) in
      if mem_resource resource resources then
         (*
          * let name = name.resource_join name Parent.name
          *)
         let rsrc_val = <:expr< $name_expr$ . $resource_join_expr loc$ $name_expr$ $parent_value$ >> in
            (resources, <:str_item< value $rec:false$ $list:[ name_patt, rsrc_val ]$ >>)
      else
         (*
          * let name = Parent.name
          *)
         (resource :: resources, <:str_item< value $rec:false$ $list:[ name_patt, parent_value ]$ >>)
   in
   let { imp_resources = resources } = proc in
   let resources, items = List_util.fold_left print_resource resources nresources in
   let mk_str_open path = (<:str_item< open $path$ >>) in
   let opens = List.map mk_str_open opens in
      proc.imp_resources <- resources;
      items @ opens

(*
 * An regular item.
 *)
let define_summary_item proc loc item =
   [item]

(*
 * A magic block computes a hash value from the definitions
 * in the block.
 *)
let define_magic_block proc loc { magic_name = name; magic_code = stmts } =
   let index = List.fold_left Filter_hash.hash_str_item 0 stmts in
   let name_patt = <:patt< $lid:name$ >> in
   let index_val = <:expr< $int:string_of_int index$ >> in
   let hash_def = <:str_item< value $rec:false$ $list:[ name_patt, index_val ]$ >> in
      hash_def :: stmts
   
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
      [<:str_item< value $rec:false$ $list:[ refiner_patt, refiner_val; dformer_patt, dformer_val ]$ >>]

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
let implem_postlog proc loc name =
   let thy_elems =
      [(<:expr< $thy_name_expr loc$ >>, <:expr< $str:name$ >>);
       (<:expr< $thy_refiner_expr loc$ >>, <:expr< $lid:refiner_id$ >>);
       (<:expr< $thy_dformer_expr loc$ >>, <:expr< $lid:dformer_id$ >>)]
   in
   let thy_rec = <:expr< { $list:thy_elems$ } >> in
   let thy = <:expr< $record_theory_expr loc$ $thy_rec$ >> in
   let refiner_patt = <:patt< $lid:refiner_id$ >> in
   let dformer_patt = <:patt< $lid:dformer_id$ >> in
   let refiner_val = <:expr< $lid:local_refiner_id$ . $lid:"val"$ >> in
   let dformer_val = <:expr< $lid:local_dformer_id$ . $lid:"val"$ >> in
   let label_expr = <:expr< $label_refiner_expr loc$ $lid:local_refiner_id$ $str:name$ >> in
      [(<:str_item< $exp:label_expr$ >>);
       (<:str_item< value $rec:false$
                           $list:[refiner_patt, refiner_val;
                                  dformer_patt, dformer_val]$ >>);
       (<:str_item< $exp:thy$ >>)]

let _ = ()

(*
 * Now extract the program.
 *)
let extract_str_item proc (item, loc) =
   match item with
      Rewrite ({ rw_name = name; rw_proof = Primitive _ } as rw) ->
         if debug_filter_prog then
            eprintf "Filter_prog.extract_str_item: primrw: %s%t" name eflush;
         prim_rewrite proc loc rw
    | Rewrite ({ rw_name = name; rw_proof = Derived tac } as rw) ->
         if debug_filter_prog then
            eprintf "Filter_prog.extract_str_item: rwthm: %s%t" name eflush;
         rewrite_theorem proc loc rw tac
    | CondRewrite ({ crw_name = name; crw_proof = Primitive _ } as crw) ->
         if debug_filter_prog then
            eprintf "Filter_prog.extract_str_item: prim condrw: %s%t" name eflush;
         prim_cond_rewrite proc loc crw
    | CondRewrite ({ crw_name = name; crw_proof = Derived tac } as crw) ->
         if debug_filter_prog then
            eprintf "Filter_prog.extract_str_item: thm condrw: %s%t" name eflush;
         cond_rewrite_theorem proc loc crw tac
    | Axiom ({ axiom_name = name; axiom_proof = Primitive t } as ax) ->
         if debug_filter_prog then
            eprintf "Filter_prog.extract_str_item: prim axiom: %s%t" name eflush;
         prim_axiom proc loc ax t
    | Axiom ({ axiom_name = name; axiom_proof = Derived tac } as ax) ->
         if debug_filter_prog then
            eprintf "Filter_prog.extract_str_item: thm axiom: %s%t" name eflush;
         thm_axiom proc loc ax tac
    | Rule ({ rule_name = name; rule_proof = Primitive t } as rule) ->
         if debug_filter_prog then
            eprintf "Filter_prog.extract_str_item: prim rule: %s%t" name eflush;
         prim_rule proc loc rule t
    | Rule ({ rule_name = name; rule_proof = Derived tac } as rule) ->
         if debug_filter_prog then
            eprintf "Filter_prog.extract_str_item: thm rule: %s%t" name eflush;
         thm_rule proc loc rule tac
    | MLTerm { mlterm_term = term; mlterm_contracta = cons; mlterm_def = Some def } ->
         if debug_filter_prog then
            eprintf "Filter_prog.extract_str_item: mlterm%t" eflush;
         define_ml_term proc loc term cons def
    | MLTerm { mlterm_def = None } ->
         if debug_filter_prog then
            eprintf "Filter_prog.extract_str_item: mlterm%t" eflush;
         raise (Failure "Filter_proof.extract_str_item: mlterm is not defined")
    | Condition _ ->
         if debug_filter_prog then
            eprintf "Filter_prog.extract_str_item: condition%t" eflush;
         raise (Failure "Filter_proof.extract_str_item: condition is not implemented")
    | DForm ({ dform_def = TermDForm expansion} as df) ->
         if debug_filter_prog then
            eprintf "Filter_prog.extract_str_item: dform%t" eflush;
         define_dform proc loc df expansion
    | DForm ({ dform_def = MLDForm code} as df) ->
         if debug_filter_prog then
            eprintf "Filter_prog.extract_str_item: dform%t" eflush;
         define_ml_dform proc loc df code
    | DForm { dform_def = NoDForm } ->
         if debug_filter_prog then
            eprintf "Filter_prog.extract_str_item: dform%t" eflush;
         raise (Failure "Filter_proof.extract_str_item: dform is not defined")
    | Prec name ->
         if debug_filter_prog then
            eprintf "Filter_prog.extract_str_item: prec: %s%t" name eflush;
         define_prec proc loc name
    | PrecRel rel ->
         if debug_filter_prog then
            eprintf "Filter_prog.extract_str_item: prec_rel%t" eflush;
         define_prec_rel proc loc rel
    | Resource ({ resource_name = name } as rsrc) ->
         if debug_filter_prog then
            eprintf "Filter_prog.extract_str_item: resource: %s%t" name eflush;
         define_resource proc loc rsrc
    | Parent ({ parent_name = name } as parent) ->
         if debug_filter_prog then
            eprintf "Filter_prog.extract_str_item: parent: %s%t" (string_of_path name) eflush;
         define_parent proc loc parent
    | SummaryItem item ->
         if debug_filter_prog then
            eprintf "Filter_prog.extract_str_item: summary item%t" eflush;
         define_summary_item proc loc item
    | MagicBlock block ->
         if debug_filter_prog then
            eprintf "Filter_prog.extract_str_item: magic block%t" eflush;
         define_magic_block proc loc block
    | Opname _ ->
         if debug_filter_prog then
            eprintf "Filter_prog.extract_str_item: opname%t" eflush;
         []
    | Id _ ->
         if debug_filter_prog then
            eprintf "Filter_prog.extract_str_item: id%t" eflush;
         []
    | Infix _ ->
         if debug_filter_prog then
            eprintf "Filter_prog.extract_str_item: infix%t" eflush;
         []
    | Module _ ->
         if debug_filter_prog then
            eprintf "Filter_prog.extract_str_item: infix%t" eflush;
         raise (Failure "Filter_prog.extract_str_item: nested modules are not implemented")

(*
 * Extract a signature.
 *)
let extract_str info resources name =
   let proc = { imp_resources = [] } in
   let prolog = implem_prolog proc (0, 0) in
   let items = List_util.flat_map (extract_str_item proc) (info_items info) in
   let postlog = implem_postlog proc (0, 0) name in
      List.map (fun item -> item, (0, 0)) (prolog @ items @ postlog)

(*
 * $Log$
 * Revision 1.3  1998/04/09 18:25:51  jyh
 * Working compiler once again.
 *
 * Revision 1.2  1998/02/23 14:46:14  jyh
 * First implementation of binary file compilation.
 *
 * Revision 1.1  1998/02/21 20:57:47  jyh
 * Two phase parse/extract.
 *
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
