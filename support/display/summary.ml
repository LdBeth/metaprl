doc <:doc< 
   @begin[spelling]
   ML prl tex
   @end[spelling]
  
   @begin[doc]
   @module[Summary]
  
   The @tt{Summary} module implements display forms for top level
   module summaries.  A @emph{summary} is a term representation of
   a top-level module.  In most cases, @MetaPRL produces the summary
   when the ML file corresponding to the module is compiled.  The term
   contains @emph{all} the information in the original file, and it
   also contains terms that represent the proof structure of the
   module.
  
   The term representation of the summary is used to represent the module
   in the logical library (or the filesystem).  It is also used by the
   @MetaPRL{} editor to display the module so that it can be browsed.
   Various parts of the summary, such as terms representing rules,
   proofs, are used during proof editing to display the current node
   and goal in a proof.
  
   The @tt{Summary} module is built-in: the summary term is constructed by
   the @MetaPRL{} compiler.  This module is the connection to the compiler,
   and its purpose is to allow the inner data structures of the compiler
   to be browsed and edited.
  
   The file contains a declaration for each of the types of term produced by
   the compiler, and it also contains display definitions to display the
   terms in a natural way.  In some cases, such as for @LaTeX{} and HTML
   mode, special display forms are used to produce code to display
   the terms correctly.  The display forms can be quite complex; we do not
   include their description in this document.
  
   @docoff
   @end[doc]
  
   ----------------------------------------------------------------
  
   @begin[license]
  
   This file is part of MetaPRL, a modular, higher order
   logical framework that provides a logical programming
   environment for OCaml and other languages.
  
   See the file doc/index.html for information on Nuprl,
   OCaml, and more information about this system.
  
   Copyright (C) 1998 Jason Hickey, Cornell University
  
   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License
   as published by the Free Software Foundation; either version 2
   of the License, or (at your option) any later version.
  
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
  
   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
  
   Author: Jason Hickey
   @email{jyh@cs.caltech.edu}
  
   @end[license]
>>

extends Perv
extends Nuprl_font
extends Base_dform
extends Comment

open Printf
open Mp_debug

open Refiner.Refiner.TermType
open Refiner.Refiner.Term
open Refiner.Refiner.TermOp
open Refiner.Refiner.TermMan

(*
 * Show that the file is loading.
 *)
let _ =
   show_loading "Loading Summary%t"

(************************************************************************
 * HTML                                                                 *
 ************************************************************************)

declare package_link[name:s]

(************************************************************************
 * TERMS                                                                *
 ************************************************************************)

doc <:doc< 
   @begin[doc]
   @terms
  
   There are two outermost terms for modules: the interface is
   wrapped with @tt{interface} term, and the
   implementation is wrapped with @tt{implementation}.
   @end[doc]
>>
declare "interface"{'intf}
declare "implementation"{'impl}

doc <:doc< 
   @begin[doc]
  
   The @tt{comment} term is used to enclose structured comments (term
   comments).
   @end[doc]
>>
declare comment{'t}

doc <:doc< 
   @begin[doc]
   Each of the components in an interface or implementation
   also has a term description.
   The @tt[parent] term describes an
   @bf[extends] statement: the @it{path} is the pathname of the parent
   module; the @it{opens} is the complete list of parents (the transitive
   closure) and the @it{resources} are the complete list of resources
   defined or used by the module.
  
   The @tt[opname] term described a @bf{declare}
   statement.  The @it{term} is the term being declared, and the @it{name} is
   the operator name of the declaration.

   The @tt[definition] term describes a @bf{define} statement. The @it{term} 
   is the term being declared, the @it{definition} is the term the definition
   expands to and @it[res] lists the resource annotations.
   @end[doc]
>>
declare "parent"{'path; 'opens; 'resources}
declare "opname"[name:s]{'term}
declare "definition"[name:s]{'term; 'definition; 'res}

doc <:doc< @docoff >>
declare "parent"[name:s]

doc <:doc< 
   @begin[doc]
   Rewrites are defined with the @tt[rewrite] and @tt[cond_rewrite]
   terms.  The @it{redex} and @it{contractum} define the rewrite; the @it{proof} is
   the proof of the rewrite (which is empty if the rewrite is primitive).  for the conditional
   rewrite the @it[params] and @it[args] are the terms the defined the assumptions
   under which the rewrite is valid.  The @it{name} is the name of the rewrite.
   @end[doc]
>>
declare "rewrite"[name:s]{'redex; 'contractum; 'proof; 'res}
declare "cond_rewrite"[name:s]{'params; 'args; 'redex; 'contractum; 'proof; 'res}

doc <:doc< 
   @begin[doc]
   Rules are defined using the @tt[rule] term.
   The @it[stmt] is the statement of the rule, and @it{proof}
   is it's proof.  The rule also includes a @it[param] list that defines the subgoals
   of the rule and the @it[res] list of resource annotations.
   @end[doc]
>>
declare "rule"[name:s]{'params; 'stmt; 'proof; 'res}

doc <:doc< 
   @begin[doc]
   Resources are declared with the @tt[resource]
   term, which has three subterms corresponding to the three types in the resource definition.  The
   @it{extract} is the type of values provided by the resource, @it{data} is the internal
   type used to represent the resource data, and @it{improve} is the type of arguments that
   are required to make additions to the resource.
  
   Resources are improved with the @tt[improve] term which has the name of a resource
   to improve and the expression to improve the resource with.
   @end[doc]
>>
declare "resource"[name:s]{'expr}
declare "resource_defs"[name:s]{'res}
declare "improve"[name:s]{'expr}
doc <:doc< @docoff >>
declare "resource_defs"[start:n, finish:n, name:s]{'res}

doc <:doc< 
   @begin[doc]
   Infix definitions (like the tacticals @tt[thenT] and @tt[orelseT]) are defined with
   the @tt{infix} declaration.
  
   OCaml definitions are also represented as terms using the
   @tt{summary_item} term, where the @it{term} is the
   term that represents the OCaml code.
   @end[doc]
>>
declare "infix"[name:s]
declare "summary_item"{'term}
doc <:doc< @docoff >>

declare "magic_block"[name:s]{'items}
declare "id"[n:n]
declare "module"[name:s]{'info}
declare "mlterm"{'term; 'cons; 'oexpr}
declare "condition"{'term; 'cons; 'oexpr}
declare "mlrewrite"[name:s]{'params; 'redex; 'contracta; 'body; 'resources}

doc <:doc< 
   @begin[doc]
   Display forms are represented using the @tt[dform] term.
   The @it{modes} are the valid modes of the display form (for example,
   the normal display mode ``prl'', or the ``html'' or ``tex'' display modes),
   as well as parenthesization and precedence definitions.
   The @it{redex} is the term to be displayed, and the @it{@misspelled{def}} is
   the term describing the displayed definition.
   @end[doc]
>>
declare "dform"[name:s]{'modes; 'redex; 'def}
declare "prec"[name:s]
declare "prec_rel"[op:s, left:s, right:s]
declare "inherit_df"
declare "prec_df"[name:s]
declare "parens_df"
declare "mode_df"[mode:s]
declare "except_mode_df"[mode:s]
doc <:doc< @docoff >>

declare "df_none"
declare "df_term"{'t}
declare "df_ml"[printer:s, buffer:s]{'contracta; 'code}

declare "none"
declare "some"{'t}

doc <:doc< 
   @begin[doc]
   Rules and axioms are described with @emph{meta}-terms.
   The meta-terms are defined inductively:
   a term (a @tt[meta_theorem]) is a meta-term,
   and given two meta-terms $A$ and $B$, so are the
   meta-implication @tt[meta_implies], the meta-@misspelled{bi}-implication
   @tt[meta_iff], and the dependent meta-function @tt{meta_function}
   where @it[arg] is a variable quantified over $A$ and bound in $B$.  The
   @tt{meta_labeled} term is used to add a label to a meta-term.
   @end[doc]
>>
declare "meta_theorem"{'A}
declare "meta_implies"{'A; 'B}
declare "meta_function"{'arg; 'A; 'B}
declare "meta_iff"{'A; 'B}
declare "meta_labeled"[label:s]{'meta}
doc <:doc< @docoff >>

declare "context_param"[name:s]
declare "term_param"{'t}

(* Arguments *)
declare "int_arg"[i:n]
declare "term_arg"{'t}
declare "type_arg"{'t}
declare "bool_arg"[s:t]
declare "string_arg"[s:s]
declare "subst_arg"{'t}
declare "term_list_arg"{'t}
declare "arglist"{'t}

(* Proofs *)
declare "interactive"{'t}

declare "href"[command:s]{'t}

doc <:doc< 
   @begin[doc]
   A proof has a goal define with the @tt[goal] term,
   where the @it{status} is the status of the proof, the @tt{label} is the label
   of the outermost proof node, the @it[assums] are the assumptions (the subgoals)
   of the theorem being proved, and the @it{goal} is the goal term.
  
   The status of a proof can have four values: a proof is @it{bad} if
   it can be shown that the proof is inconsistent; it is @it{partial}
   if it has unproven subgoals; it is @it{asserted} if it is primitive
   or if it has not been checked; and it is @it{complete} if it has been checked.
   @end[doc]
>>
declare "goal"{'status; 'label; 'assums; 'goal}
declare "status_bad"
declare "status_partial"
declare "status_asserted"
declare "status_complete"
declare "status"{'sl}
doc <:doc< @docoff >>

declare "goal_status"{'sl}
declare "goal_label"[s:s]
declare "goal_list"{'goals}
declare "subgoals"{'subgoals; 'extras}
declare "subgoals"{'number; 'subgoals; 'extras}
declare "rule_box"[text:s]
declare "rule_box"{'text}
declare "rule_box"[text:s]{'text}
declare "proof"{'main; 'goal; 'text; 'subgoals}
declare "tactic_arg"[label:s]{'goal; 'attrs; 'parents}

(* Packages *)
declare "package"[name:s]
declare "packages"{'pl}

(* Location *)
declare "location"[start:n, finish:n]{'body}

(************************************************************************
 * DISPLAY FORMS                                                        *
 ************************************************************************)

(*
 * Names of items.
 *)
declare opname_name[name:s]

dform opname_name_df2 : mode[tex] :: opname_name[name:s] =
   izone `"\\labelterm{" slot[name:s] `"}{" ezone slot[name:s] izone `"}" ezone

dform opname_name_df1 : except_mode[tex] :: opname_name[name:s] =
   slot[name:s]

declare rule_name[name:s]

dform rule_name_df2 : mode[tex] :: rule_name[name:s] =
   izone `"\\labelrule{" slot[name:s] `"}{" ezone slot[name:s] izone `"}" ezone

dform rule_name_df1 : except_mode[tex] :: rule_name[name:s] =
   slot[name:s]

declare rewrite_name[name:s]

dform rewrite_name_df2 : mode[tex] :: rewrite_name[name:s] =
   izone `"\\labelrewrite{" slot[name:s] `"}{" ezone slot[name:s] izone `"}" ezone

dform rewrite_name_df1 : except_mode[tex] :: rewrite_name[name:s] =
   slot[name:s]

declare resource_name[name:s]

dform resource_name_df2 : mode[tex] :: resource_name[name:s] =
   izone `"\\labelresource{" slot[name:s] `"}{" ezone slot[name:s] izone `"}" ezone

dform resource_name_df1 : except_mode[tex] :: resource_name[name:s] =
   slot[name:s]

(*
 * Modes.
 *)
declare dform_modes{'l}

dform dform_modes_df1 : dform_modes{cons{'hd; 'tl}} =
   slot{'hd} " " keyword["::"] " " dform_modes{'tl}

dform dform_modes_df2 : internal :: dform_modes{nil} =
   `""

(*
 * Print term in raw format.
 *)
declare raw_list{'l}

dform raw_list_df1 : internal :: raw_list{cons{'hd; cons{'tl1; 'tl2}}} =
   slot["raw"]{'hd} " " raw_list{cons{'tl1; 'tl2}}

dform raw_list_df2 : internal :: raw_list{cons{'hd; nil}} =
   slot["raw"]{'hd}

dform raw_list_df2 : internal :: raw_list{nil} =
   `""

(*
 * Interface just declares it.
 *)
declare lines{'e}

dform lines_nil_df : internal :: lines{nil} =
   `""

dform lines_cons_df : internal :: lines{cons{'e1; 'e2}} =
   newline szone{'e1} lines{'e2}

dform interface_df : "interface"{'body} =
   szone pushm[0]
   info["Interface:"] newline
   pushm[4] info["begin"] lines{'body} popm newline
   info["end"] newline
   popm ezone

dform implementation_df : "implementation"{'body} =
   szone pushm[0]
   info["Implementation:"] newline
   pushm[4] info["begin"] lines{'body} popm newline
   info["end"] newline
   popm ezone

dform location_df : "location"[start:n, finish:n]{'body} =
   slot{'body}

(*
 * Resource annotations
 *)
declare res_def_list{'res}
declare resources{'resources}

dform resources_nil_df : internal :: resources{nil} =
   " "

dform resources_cons_df : internal :: resources{cons{'h; 't}} =
   hspace szone keyword["{| "] pushm res_def_list{cons{'h; 't}} popm keyword[" |} "] ezone

dform res_def_list_df1 : internal :: res_def_list{cons{'a; nil}} =
   slot{'a}

dform res_def_list_df2 : internal :: res_def_list{cons{'a; 'b}} =
   slot{'a} keyword[";"] hspace res_def_list{'b}

dform resource_defs_df1 : resource_defs[name:s]{'args} =
   slot[name:s] " " slot{'args}

dform resource_defs_df1 : resource_defs[name:s]{nil} =
   slot[name:s]

dform resource_defs_dfs : internal :: resource_defs[start:n, finish:n, name:s]{'args} =
   resource_defs[name:s]{'args}

(*
 * Display a simple rewrite.
 *)
dform rewrite_df : "rewrite"[name:s]{'redex; 'contractum; 'v; 'res} =
   szone pushm[4]
   ensuremath{'v} info[" rewrite"] " " szone rewrite_name[name:s] resources{'res} keyword[":"] ezone hspace
   szone pushm[4]
   ensuremath{'redex} `" " ensuremath{longleftrightarrow} hspace ensuremath{'contractum}
   popm ezone
   popm ezone

(*
 * A conditional rewrite requires special handling of the params.
 *)
dform context_param_df : except_mode[src] :: "context_param"[name:s] =
   df_context_var[name:s]

dform context_param_df2 : mode[src] :: "context_param"[name:s] =
   `"'" slot[name:s]

dform term_param_df : "term_param"{'t} =
   szone pushm[4]
   ensuremath{'t}
   popm ezone

dform cond_rewrite_df : "cond_rewrite"[name:s]{'params; 'args; 'redex; 'contractum; 'proof; 'res} =
   szone pushm[4]
   ensuremath{'proof} info[" rewrite"] " " szone rewrite_name[name:s] resources{'res} df_concat{slot[" "];'params} keyword[":"] ezone hspace ensuremath{'args} " " ensuremath{longrightarrow} hspace
   szone pushm[4]
   ensuremath{'redex} " " ensuremath{longleftrightarrow} hspace ensuremath{'contractum}
   popm ezone
   popm ezone

dform rule_df : "rule"[name:s]{'params; 'stmt; 'proof; 'res} =
   hzone pushm[4]
   ensuremath{'proof} info[" rule"] " " szone rule_name[name:s] resources{'res} df_concat{slot[" "];'params} keyword[":"] ezone hspace ensuremath{'stmt}
   ezone popm
   
declare displayed_as{'t}

dform displayed_as_df : displayed_as{'t} =
   szone info["(displayed as"] hspace ensuremath{slot["decl"]{'t}} info[")"] ezone

dform opname_df : "opname"[name:s]{'term} =
   pushm[4] szone
   info["declare"] hspace szone tt{slot["raw"]{'term}} hspace displayed_as{'term} ezone
   ezone popm

dform definition_df : "definition"[name:s]{'term; 'definition; 'res} =
   szone pushm[4]
   info["define"] " " szone rewrite_name[name:s] resources{'res} keyword[":"] ezone hspace
   szone pushm[4]
   tt{slot["raw"]{'term}} space displayed_as{'term} `" " ensuremath{longleftrightarrow} hspace ensuremath{'definition}
   popm ezone
   popm ezone

dform mlterm_df : "mlterm"{'term; 'cons; 'oexpr} =
   pushm[4] szone
   info["mlterm"] " " slot{'term}
   ezone popm

dform condition_df : "condition"{'term; 'cons; 'oexpr} =
   pushm[4] szone
   info["condition"] " " slot{'term}
   ezone popm

dform mlrewrite_df1 : "mlrewrite"[name:s]{'params; 'redex; 'contracta; 'body; 'res} =
   szone pushm[4]
   info["mlrewrite"] " " rewrite_name[name:s] " " resources{'res} df_concat{slot[" "];'params} keyword[":"] hspace
   ensuremath{'redex} keyword["="] slot{'body}
   popm ezone

(*
 * Parent path is separated by dots.
 *)
declare path{'t}
declare begin_cd{'t}
declare cdinternal{'t}
declare end_cd

dform begin_cd_tex_df : internal :: begin_cd{'path} =
   `""

dform end_cd_tex_df : internal :: end_cd =
   `""

dform begin_cd_df1 : internal :: mode[html] :: begin_cd{'path} =
   izone `"<a href=\"http://cd.metaprl.local//" cdinternal{'path}

dform cd_internal_df1 : internal :: mode[html] :: cdinternal{cons{."parent"[name:s]; cons{'n2; 'n3}}} =
   slot[name:s] `"/" cdinternal{cons{'n2; 'n3}}

dform cd_internal_df2 : internal :: mode[html] :: cdinternal{cons{."parent"[name:s]; nil}} =
   slot[name:s] cdinternal{nil}

dform cd_internal_df3 : internal :: mode[html] :: cdinternal{nil} =
   `"\">" ezone

dform end_cd_df1 : internal :: mode[html] :: end_cd =
   izone `"</a>" ezone

dform path_parent_nil_df2 : internal :: mode[tex] :: path{cons{."parent"[name:s]; nil}} =
   slot[name:s]

dform path_parent_nil_df : internal :: path{cons{."parent"[name:s]; nil}} =
   slot[name:s]

dform path_parent_cons_df : internal :: path{cons{."parent"[name:s]; .cons{'n1; 'n2}}} =
   slot[name:s] keyword["."] cons{'n1; 'n2}

dform parent_df : "parent"{'path; 'opens; 'resources} =
   info["extends"] " " begin_cd{'path} path{'path} end_cd

(*
 * Nested module is indented.
 *)
dform module_df : "module"[name:s]{'info} =
   szone pushm[4]
   info["module"] " " slot[name:s] `" = " hspace slot{'info}
   ezone popm

dform dform_df : "dform"[name:s]{'modes; 'redex; 'def} =
   szone pushm[4]
   info["dform"] " " slot[name:s]
   " " keyword[": "] dform_modes{'modes} hspace tt{slot["raw"]{'redex}}
   " " keyword["="] hspace pushm szone{'def} popm
   ezone popm

(*
 * Precedence relations.
 *)
declare "rel"[op:s]

dform rel_lt_df : internal :: "rel"["lt"] = keyword["<"]
dform rel_eq_df : internal :: "rel"["eq"] = keyword["="]
dform rel_gt_df : internal :: "rel"["gt"] = keyword[">"]

dform prec_df : "prec"[name:s] =
   info["prec"] " " slot[name:s]

dform prec_rel_df : prec_rel[op, left, right] =
   info["prec "] slot[left] `" " "rel"[op] `" " slot[right]

dform id_df : "id"[n:n] =
   info["Id: "] slot[n:n]

dform resource_df : "resource"[name]{'expr} =
   pushm[3] szone
   info["let"] " " info["resource"] " " resource_name[name:s] " " keyword ["="] hspace
   szone{'expr} ezone popm

dform improve_df : "improve"[name]{'expr} =
   pushm[3] szone
   info["let"] " " info["resource"] " " resource_name[name:s] " " keyword ["+="] hspace
   szone{'expr} ezone popm

dform infix_df : "infix"[name:s] =
   info["infix"] " " slot[name:s]

dform magic_block_df : "magic_block"[name:s]{'items} =
   info["magic_block"] " " slot[name:s] keyword[" ="] space slot{'items}

dform summary_item_df : "summary_item"{'term} =
   szone{'term}

dform df_term_df : df_term{'t} =
   raw_list{'t}

dform meta_theorem_df : meta_theorem{'A} =
   slot{'A}

dform meta_implies_df : meta_implies{'A; 'B} =
   slot{'A} " " longrightarrow hspace slot{'B}

dform meta_function_df : meta_function{'arg; 'A; 'B} =
   szone pushm[0] slot{'arg} keyword[": "] slot{'A} `" " popm ezone longrightarrow hspace slot{'B}

dform meta_labeled_df : meta_labeled[label:s]{'A} =
   keyword["["] slot[label] keyword["] "] slot{'A}

dform mode_df : mode_df[s:s] =
   keyword["mode["] slot[s:s] keyword["]"]

dform except_mode_df : except_mode_df[s:s] =
   keyword["except_mode["] slot[s:s] keyword["]"]

dform prec_df : prec_df[s:s] =
   keyword["prec["] slot[s:s] keyword["]"]

dform parens_df : parens_df =
   keyword["parens"]

(*
 * Packages.
 *)
declare packages_df{'t}

dform packages_df1 : internal :: packages{'packages} =
   szone pushm[0] pushm[4] info["Root theories:"] hspace
       packages_df{'packages} popm hspace
   info["end"] popm ezone

dform packages_df2 : internal :: packages_df{cons{package[name:s]; 'next}} =
   info["module "] cd_begin[name:s] slot[name:s] cd_end hspace packages_df{'next}

dform packages_df3 : internal :: packages_df{nil} =
   `""

(********************************
 * Argument lists
 *)
dform int_arg_df : internal :: int_arg[i:n] =
   slot[i:n]

dform term_arg_df : internal :: term_arg{'t} =
   't

dform type_arg_df : internal :: type_arg{'t} =
   't

dform bool_arg_df : internal :: bool_arg[s:t] =
   slot[s:t]

dform string_arg_df : internal :: string_arg[s:s] =
   slot[s:s]

dform subst_arg_df : internal :: subst_arg{'t} =
   't

dform term_list_arg_df : internal :: term_list_arg{'terms} =
   df_concat{slot[" "];'terms}

dform arglist_df1 : internal :: arglist{'args} =
   szone pushm df_concat{slot[" "];'args} popm ezone

(********************************
 * Proofs.
 *)
declare msequent{'goal; 'assums}
declare goals_df{'main; 'goal}
declare goal_list_status{'cache}

dform proof_df : internal :: proof{'main; goal_list{'goal}; 'text; 'subgoals} =
   szone pushm pagebreak goals_df{'main; 'goal} 'text 'subgoals popm ezone

dform goals_df : internal :: goals_df{goal{'status; 'label; 'assums; 'goal}; 'cache} =
   'status `"[" goal_list_status{'cache} `"]" newline 'label msequent{'goal; 'assums}

dform goal_df : internal :: goal{'status; 'label; 'assums; 'goal} =
   'status `"[]" newline 'label msequent{'goal; 'assums}

dform goal_list_status_df1 : internal :: goal_list_status{cons{goal{goal_status{'status}; 'label; 'assums; 'goal}; cons{'hd; 'tl}}} =
   df_last{'status} `" " goal_list_status{cons{'hd; 'tl}}

dform goal_list_status_df2 : internal :: goal_list_status{cons{goal{goal_status{'status}; 'label; 'assums; 'goal}; nil}} =
   df_last{'status}

(*
 * Display the meta-sequent.
 *)
declare numbered_assums{'number; 'assums}

dform msequent_df1 : internal :: msequent{'goal; nil} =
   'goal newline

dform msequent_df2 : internal :: msequent{'goal; 'assums} =
   numbered_assums{cons{nil; nil}; 'assums} 'goal newline

dform numbered_assums_df1 : internal :: numbered_assums{'number; cons{'a; 'b}} =
   szone df_length{'number} `". " pushm 'a popm newline ezone numbered_assums{cons{nil; 'number}; 'b}

dform numbered_assums_df2 : internal :: numbered_assums{'number; nil} =
   `"====" newline

(*
 * Status line includes commands to move up the tree.
 *)
declare goal_status_df{'a; 'b}
declare goal_status_cd{'a; 'b}
declare goal_cd_dot
declare goal_cd_up
declare goal_cd_begin{'cd}
declare goal_cd_middle{'cd}
declare goal_cd_end

dform status_df2 : internal :: goal_status{nil} = `""

dform status_df1 : internal :: goal_status{cons{'a; 'b}} =
   goal_status_df{cons{'a; nil}; 'b}

dform status_df3 : internal :: goal_status_df{'l; cons{'a; 'b}} =
   goal_status_df{cons{'a; 'l}; 'b}

dform status_df4 : internal :: goal_status_df{'l; nil} =
   goal_status_cd{goal_cd_dot; 'l}

dform status_df5 : internal :: goal_status_cd{'cd; cons{'a; 'b}} =
   goal_status_cd{cons{goal_cd_up; 'cd}; 'b} goal_cd_begin{'cd} 'a `" " goal_cd_end

dform status_df5b : internal :: goal_status_cd{'cd; nil} =
   `""

dform status_df6a : internal :: goal_cd_begin{'cd} =
   `""

dform status_df9a : internal :: goal_cd_end =
   `""

dform status_df6 : internal :: mode[html] :: goal_cd_begin{'cd} =
   izone `"<a href=\"cd.metaprl.local/" goal_cd_middle{'cd}

dform status_df7 : internal :: mode[html] :: goal_cd_middle{goal_cd_dot} =
   `".\">" ezone

dform status_df8 : internal :: mode[html] :: goal_cd_middle{cons{goal_cd_up; 'cd}} =
   `"/.." goal_cd_middle{'cd}

dform status_df9 : internal :: mode[html] :: goal_cd_end =
   izone `"</a>" ezone

(*
 * Label is printed with surrounding dots.
 *)
dform label_df : internal :: goal_label[name:s] =
   `"...." slot[name:s] `"...." newline

dform status_bad_df : status_bad =
   keyword["-"]

dform status_partial_df : status_partial =
   keyword["#"]

dform status_asserted_df : status_asserted =
   keyword["?"]

dform status_complete : status_complete =
   keyword["*"]

(*
 * Rule box.
 *)
dform rule_box_df1 : internal :: rule_box[text:s] =
   newline szone info["BY "] pushm slot[text:s] popm ezone

dform rule_box_df2 : internal :: rule_box{'t} =
   newline szone info_begin `"BY [" pushm 't `"]" popm ezone info_end

dform rule_box_df3 : internal :: rule_box[text:s]{'t} =
   newline szone info_begin `"BY [" pushm 't `"]" space slot[text:s] popm ezone info_end

(*
 * Subgoals are printed in simplified form.
 *)
declare child_df{'number; 'child}
declare child_df{'child}

dform subgoals_df1 : internal :: subgoals{'children; 'extras} =
   newline subgoals{cons{nil; nil}; 'children; 'extras}

dform subgoals_df2 : internal :: subgoals{'number; cons{'child; 'tl}; 'extras} =
   newline child_df{'number; 'child} subgoals{cons{nil; 'number}; 'tl; 'extras}

dform subgoals_df3 : internal :: subgoals{'number; nil; cons{'a; 'b}} =
   newline info["===="] newline subgoals{'number; cons{'a; 'b}; nil}

dform subgoals_df4 : internal :: subgoals{'number; cons{goal{goal_status{'status}; 'label; 'assums; 'goal}; 'tl}; nil} =
   szone info_begin df_length{'number} `". " pushm df_last{'status} info_end newline 'label 'goal popm ezone newline
   subgoals{cons{nil; 'number}; nil; 'tl}

dform subgoals_df5 : internal :: subgoals{'number; nil; nil} =
   `""

dform child_df1 : internal :: child_df{'number; goal_list{'child}} =
   szone info_begin df_length{'number} `". " pushm `"[" goal_list_status{'child} `"]" info_end newline child_df{'child} popm ezone

dform child_df2 : internal :: child_df{cons{goal{'status; 'label; 'assums; 'goal}; 'tl}} =
   'label 'goal

dform tactic_arg_df1 : internal :: tactic_arg[label:s]{'goal;'args;'parents} =
   szone `"[" slot[label:s] `"] " pushm 'goal popm ezone

dform tactic_arg_df2 : internal :: tactic_arg[label:s]{'goal;'args;'parents} =
   szone `"[" slot[label:s] `"] " pushm 'goal ezone `" " popm szone `"with args " pushm `"<" 'args `">" popm ezone

(*
 * Comments.
 *)
dform comment_df1 : mode[tex] :: comment{'t} =
   tex_comment{'t}

dform comment_df2 : mode["prl"] :: comment{'t} =
   prl_comment{'t}

(************************************************************************
 * ML INTERFACE                                                         *
 ************************************************************************)

let interface_term = << "interface"{'intf} >>
let interface_opname = opname_of_term interface_term
let mk_interface_term tl =
   mk_dep0_term interface_opname (mk_xlist_term tl)

let implementation_term = << "implementation"{'impl} >>
let implementation_opname = opname_of_term implementation_term
let mk_implementation_term tl =
   mk_dep0_term implementation_opname (mk_xlist_term tl)

let package_term = << "package"[name:s] >>
let package_opname = opname_of_term package_term
let mk_package_term = mk_string_term package_opname

let packages_term = << "packages"{'pl} >>
let packages_opname = opname_of_term packages_term
let mk_packages_term tl =
   mk_simple_term packages_opname [mk_xlist_term tl]

let href_term = << "href"[s:s]{'t} >>
let href_opname = opname_of_term href_term
let mk_href_term = mk_string_dep0_term href_opname

let status_bad_term = << "status_bad" >>
let status_partial_term = << "status_partial" >>
let status_asserted_term = << "status_asserted" >>
let status_complete_term = << "status_complete" >>

let status_term = << "goal_status"{'sl} >>
let status_opname = opname_of_term status_term
let mk_status_term tl =
   mk_simple_term status_opname [mk_xlist_term tl]

let goal_label_term = << "goal_label"[s:s] >>
let goal_label_opname = opname_of_term goal_label_term
let mk_goal_label_term s =
   mk_string_term goal_label_opname s

let mk_labeled_goal_term label goal =
   mk_simple_term goal_label_opname [label; goal]

let goal_term = << "goal"{'status; 'label; 'assums; 'goal} >>
let goal_opname = opname_of_term goal_term
let mk_goal_term status label assums goal =
   mk_simple_term goal_opname [status; label; mk_xlist_term assums; goal]

let goal_list_term = << "goal_list"{'goals} >>
let goal_list_opname = opname_of_term goal_list_term
let mk_goal_list_term goals =
   mk_simple_term goal_list_opname [mk_xlist_term goals]

let subgoals_term = << "subgoals"{'subgoals; 'extras} >>
let subgoals_opname = opname_of_term subgoals_term
let mk_subgoals_term subgoals extras =
   mk_simple_term subgoals_opname [mk_xlist_term subgoals; mk_xlist_term extras]

let rule_box_term = << "rule_box"[s:s] >>
let rule_box_opname = opname_of_term rule_box_term
let mk_rule_box_string_term s =
   mk_string_term rule_box_opname s
let mk_rule_box_term t =
   mk_dep0_term rule_box_opname t
let append_rule_box t s =
   if is_dep0_term rule_box_opname t then
      let t = one_subterm t in
      mk_string_dep0_term rule_box_opname s t
   else if is_string_term rule_box_opname t then
      let s' = dest_string_term rule_box_opname t in
      let s = if s'="" then s else s' ^ " " ^ s in
      mk_string_term rule_box_opname s
   else let s',t = dest_string_dep0_term rule_box_opname t in
      let s = if s'="" then s else s' ^ " " ^ s in
      mk_string_dep0_term rule_box_opname s t

let proof_term = << "proof"{'main; 'goal; 'text; 'subgoals} >>
let proof_opname = opname_of_term proof_term
let mk_proof_term main goal text subgoals =
   mk_simple_term proof_opname [main; goal; text; subgoals]
let dest_proof = four_subterms

let int_arg_term = << "int_arg"[i:n] >>
let int_arg_opname = opname_of_term int_arg_term
let mk_int_arg_term i =
   mk_number_term int_arg_opname (Mp_num.num_of_int i)

let term_arg_term = << "term_arg"{'t} >>
let term_arg_opname = opname_of_term term_arg_term
let mk_term_arg_term t =
   mk_simple_term term_arg_opname [t]

let type_arg_term = << "type_arg"{'t} >>
let type_arg_opname = opname_of_term type_arg_term
let mk_type_arg_term t =
   mk_simple_term type_arg_opname [t]

let bool_arg_term = << "bool_arg"[s:t] >>
let bool_arg_opname = opname_of_term bool_arg_term
let mk_bool_arg_term b =
   mk_string_term bool_arg_opname (if b then "true" else "false")

let string_arg_term = << "string_arg"[s:s] >>
let string_arg_opname = opname_of_term string_arg_term
let mk_string_arg_term s =
   mk_string_term string_arg_opname s

let subst_arg_term = << "subst_arg"{'t} >>
let subst_arg_opname = opname_of_term subst_arg_term
let mk_subst_arg_term t =
   mk_simple_term subst_arg_opname [t]

let term_list_arg_term = << "term_list_arg"{'t} >>
let term_list_arg_opname = opname_of_term term_list_arg_term
let mk_term_list_arg_term tl =
   mk_simple_term term_list_arg_opname [mk_xlist_term tl]

let arglist_term = << "arglist"{'t} >>
let arglist_opname = opname_of_term arglist_term
let mk_arglist_term tl =
   mk_simple_term arglist_opname [mk_xlist_term tl]

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
