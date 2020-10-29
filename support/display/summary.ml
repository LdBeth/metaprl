doc <:doc<
   @spelling{ML prl tex}

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

   ----------------------------------------------------------------

   @begin[license]

   This file is part of MetaPRL, a modular, higher order
   logical framework that provides a logical programming
   environment for OCaml and other languages.

   See the file doc/htmlman/default.html or visit http://metaprl.org/
   for more information.

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

   Author: Jason Hickey @email{jyh@cs.caltech.edu}
   Modified By: Aleksey Nogin @email{nogin@cs.caltech.edu}

   @end[license]
>>

extends Perv
extends Mpsymbols
extends Base_dform
extends Comment
extends Ocaml_df

open Term_addr_sig
open Refiner.Refiner.TermOp
open Refiner.Refiner.TermMan
open Tactic_type

(************************************************************************
 * HTML                                                                 *
 ************************************************************************)

declare package_link[name:s] : Dform

(************************************************************************
 * TERMS                                                                *
 ************************************************************************)

doc <:doc<
   @terms

   There are two outermost terms for modules: the interface is
   wrapped with @tt{interface} term, and the
   implementation is wrapped with @tt{implementation}.
>>
declare "interface"{'intf} : Dform
declare "implementation"{'impl} : Dform

doc <:doc<

   The @tt{comment} term is used to enclose structured comments (term
   comments).
>>
declare comment{'t : Dform} : Dform

doc <:doc<
   Each of the components in an interface or implementation
   also has a term description.
   The @tt[parent] term describes an
   @bf[extends] statement: the @it{path} is the pathname of the parent
   module; the @it{opens} is the complete list of parents (the transitive
   closure) and the @it{resources} are the complete list of resources
   defined or used by the module.

   The @tt[opname] term describes a @bf{declare}
   statement.  The @it{term} is the term being declared, and the @it{name} is
   the operator name of the declaration.

   The @tt[definition] term describes a @bf{define} statement. The @it{term}
   is the term being declared, the @it{definition} is the term the definition
   expands to and @it[res] lists the resource annotations.
>>
declare type ShapeClass -> Dform
declare shape_normal : ShapeClass
declare shape_iform : ShapeClass
declare shape_class[n:n] : ShapeClass
declare "parent"{'path : Dform; 'resources : Dform} : Dform
declare declare_typeclass{'shape : ShapeClass; 'term : Dform; 'ty: Dform; 'parent : Dform} : Dform
declare declare_type{'shape : ShapeClass; 'term : Dform; 'ty: Dform} : Dform
declare declare_type_rewrite{'ty1 : Dform; 'ty2 : Dform} : Dform
declare declare_term{'shape : ShapeClass; 'term : Dform} : Dform
declare define_term{'shape : ShapeClass; 'term : Dform; 'def : Dform} : Dform
declare "ty_term"{'term : Dform; 'opname : Dform; 'params : Dform; 'bterms : Dform; 'ty : Dform} : Dform
declare "term_def"[name:s]{'def : Dform; 'res : Dform; 'flag : Dform} : Dform
declare parent_kind[name:s] : Dform
declare parent_kind[name:s]{'parent : Dform} : Dform
declare "private" : Dform
declare "public" : Dform
declare "opaque" : Dform

doc docoff
declare "parent"[name:s] : Dform

doc <:doc<
   Rewrites are defined with the @tt[rewrite] and @tt[cond_rewrite]
   terms.  The @it{redex} and @it{contractum} define the rewrite; the @it{proof} is
   the proof of the rewrite (which is empty if the rewrite is primitive).  For the conditional
   rewrite the @it[params] are the conversion parameters and @it[args] are the terms
   that define the assumptions
   under which the rewrite is valid.  The @it{name} is the name of the rewrite.
>>
declare "rewrite"[name:s]{'redex : Dform; 'contractum : Dform; 'proof : Dform; 'res : Dform} : Dform
declare cond_rewrite[name:s]{'params : Dform; 'args : Dform; 'redex : Dform; 'contractum : Dform; 'proof : Dform; 'res : Dform} : Dform
declare input_form[name:s]{'redex : Dform; 'contractum} : Dform

doc <:doc<
   Rules are defined using the @tt[rule] term.
   The @it[stmt] is the statement of the rule, and @it{proof}
   is its proof.  The rule also includes a @it[param] list that defines the subgoals
   of the rule and the @it[res] list of resource annotations.
>>
declare "rule"[name:s]{'params : Dform; 'stmt : Dform; 'proof : Dform; 'res : Dform} : Dform

doc <:doc<
   Resources are defined with the @tt[resource] term, which has three subterms.  The
   The @it[inp] is the type of arguments that are required to make additions to the resource,
   @it[outp] is the type of values provided by the resource and @it[expr] is an ML expression
   that provides the resource implementation.

   Resources are improved with the @tt[improve] term which has the name of a resource
   to improve and the expression to improve the resource with.
>>
declare "resource"[name:s]{'expr : Dform} : Dform
declare "resource_defs"[name:s]{'flag : Dform; 'res : Dform} : Dform
declare "resource"{'inp : Dform; 'outp : Dform; 'expr : Dform} : Dform
declare "improve"[name:s]{'flag : Dform; 'expr : Dform} : Dform
doc docoff
declare flag{'flag: Dform} : Dform
declare "resource_defs"[start:n, finish:n, name:s]{'flag: Dform; 'res : Dform} : Dform

doc <:doc<
   Infix definitions (like the tacticals @tt[thenT] and @tt[orelseT]) are defined with
   the @tt{infix} declaration.

   OCaml definitions are also represented as terms using the
   @tt{summary_item} term, where the @it{term} is the
   term that represents the OCaml code.
>>
declare "infix"[name:s] : Dform
declare "suffix"[name:s] : Dform
declare "summary_item"{'term : Dform} : Dform
doc docoff

declare "magic_block"[name:s]{'items : Dform} : Dform
declare "id"[n:n] : Dform
declare "module"[name:s]{'info : Dform} : Dform
declare "mlterm"{'term : Dform; 'cons : Dform; 'oexpr : Dform} : Dform
declare "condition"{'term : Dform; 'cons : Dform; 'oexpr : Dform} : Dform
declare "mlrewrite"[name:s]{'params : Dform; 'redex : Dform; 'body : Dform; 'resources : Dform} : Dform

doc <:doc<
   Display forms are represented using the @tt[dform] term.
   The @it{modes} are the valid modes of the display form (for example,
   the normal display mode ``prl'', or the ``html'' or ``tex'' display modes),
   as well as parenthesization and precedence definitions.
   The @it{redex} is the term to be displayed, and the @it{@misspelled{def}} is
   the term describing the displayed definition.
>>
declare "dform"[name:s]{'modes : Dform; 'redex : Dform; 'def : Dform} : Dform
declare "prec"[name:s] : Dform
declare "prec_rel"[op:s, left:s, right:s] : Dform
declare "inherit_df" : Dform
declare "prec_df"[name:s] : Dform
declare "parens_df" : Dform
declare "mode_df"[mode:s] : Dform
declare "except_mode_df"[mode:s] : Dform
doc docoff

declare "df_none" : Dform
declare "df_term"{'t} : Dform
declare "df_ml"[printer:s, buffer:s]{'contracta : Dform; 'code : Dform} : Dform

declare "none" : Dform
declare "some"{'t} : Dform

doc <:doc<
   Rules and axioms are described with @emph{meta}-terms.
   The meta-terms are defined inductively:
   a term (a @tt[meta_theorem]) is a meta-term,
   and given two meta-terms $A$ and $B$, so are the
   meta-implication @tt[meta_implies], the meta-@misspelled{bi}-implication
   @tt[meta_iff], and the dependent meta-function @tt{meta_function}
   where @it[arg] is a variable quantified over $A$ and bound in $B$.  The
   @tt{meta_labeled} term is used to add a label to a meta-term.
>>
declare typeclass MTerm -> Dform
declare "meta_theorem"{'A : 'a} : MTerm
declare "meta_implies"{'A : MTerm; 'B : MTerm} : MTerm
declare "meta_function"{'arg : Judgment; 'A : MTerm; 'B : MTerm} : MTerm
declare "meta_iff"{'A : 'a; 'B : 'a} : MTerm
declare "meta_labeled"[label:s]{'meta : MTerm} : MTerm
doc docoff

declare "int_param"[name:v] : Dform
declare "addr_param"[name:v] : Dform
declare "term_param"{'t : Dform} : Dform

declare addr_subterm[i:n] : Dform
declare addr_arg : Dform
declare addr_clause[i:n] : Dform

(* Arguments *)
declare int_arg[i:n] : Dform
declare term_arg{'t : Dform}      : Dform
declare type_arg{'t : Dform}      : Dform
declare addr_arg{'t : Dform}      : Dform
declare string_arg[s:s]           : Dform
declare term_list_arg{'t : Dform} : Dform
declare arglist{'t : Dform}       : Dform

declare typeclass Bool -> Token
declare "true"  : Bool
declare "false" : Bool
declare "bool_arg"[s:Bool] : Dform

(* Proofs *)

declare "href"[command:s]{'t} : Dform

doc <:doc<
   A proof has a goal defined with the @tt[goal] term,
   where the @it{status} is the status of the proof, the @tt{label} is the label
   of the outermost proof node, the @it[assums] are the assumptions (the subgoals)
   of the theorem being proved, and the @it{goal} is the goal term.

   The status of a proof can have four values: a proof is @it{bad} if
   it can be shown that the proof is inconsistent; it is @it{partial}
   if it has unproven subgoals; it is @it{asserted} if it is primitive
   or if it has not been checked; and it is @it{complete} if it has been checked.
>>
declare "goal"{'status : Dform; 'label : Dform; 'assums : Dform; 'goal : Dform} : Dform
declare "status"{'sl : Dform} : Dform
doc docoff

declare "goal_status"{'sl : Dform} : Dform
declare "goal_label"[s:s] : Dform
declare "goal_list"{'goals : Dform} : Dform
declare "subgoals"{'subgoals : Dform; 'extras : Dform} : Dform
declare "subgoals"{'number : Dform; 'subgoals : Dform; 'extras : Dform} : Dform
declare "rule_box"[text:s] : Dform
declare "rule_box"{'text : Dform} : Dform
declare "rule_box"[text:s]{'text : Dform} : Dform
declare "proof"{'main : Dform; 'goal : Dform; 'status : Dform; 'text : Dform; 'subgoals : Dform} : Dform
declare "tactic_arg"[label:s]{'goal : Dform; 'attrs : Dform; 'parents : Dform} : Dform

(* Location *)
declare "location"[start:n, finish:n]{'body : Dform} : Dform

(************************************************************************
 * DISPLAY FORMS                                                        *
 ************************************************************************)

(*
 * Names of items.
 *)
(* XXX: BUG: opname_name is no longer used; but it should be *)
declare opname_name[name:s] : Dform

dform opname_name_df2 : mode[tex] :: opname_name[name:s] =
   izone `"\\labelterm{" slot[name:s] `"}{" ezone slot[name:s] izone `"}" ezone

dform opname_name_df1 : except_mode[tex] :: opname_name[name:s] =
   slot[name:s]

declare rule_name[name:s] : Dform

dform rule_name_df1 : except_mode[tex] :: except_mode[html] :: rule_name[name:s] =
   slot[name:s]

dform rule_name_df2 : mode[tex] :: rule_name[name:s] =
   izone `"\\labelrule{" slot[name:s] `"}{" ezone slot[name:s] izone `"}" ezone

dform rule_name_df3 : mode[html] :: rule_name[name:s] =
   html["<span class=\"rule_name\">"] cd_begin[name] slot[name:s] cd_end html["</span>"]

declare rewrite_name[name:s] : Dform

dform rewrite_name_df2 : mode[tex] :: rewrite_name[name:s] =
   izone `"\\labelrewrite{" slot[name:s] `"}{" ezone slot[name:s] izone `"}" ezone

dform rewrite_name_df1 : except_mode[tex] :: rewrite_name[name:s] =
   rule_name[name:s]

declare resource_name[name:s] : Dform

dform resource_name_df2 : mode[tex] :: resource_name[name:s] =
   izone `"\\labelresource{" slot[name:s] `"}{" ezone slot[name:s] izone `"}" ezone

dform resource_name_df1 : except_mode[tex] :: resource_name[name:s] =
   slot[name:s]

(*
 * Modes.
 *)
declare dform_modes{'l : Dform} : Dform

dform dform_modes_df1 : dform_modes{xcons{'hd; 'tl}} =
   slot{'hd} " " keyword["::"] " " dform_modes{'tl}

dform dform_modes_df2 : dform_modes{xnil} =
   `""

(*
 * Print term in raw format.
 *)
declare raw_list{'l : Dform} : Dform

dform raw_list_df1 : raw_list{xcons{'hd; xcons{'tl1; 'tl2}}} =
   slot["raw"]{'hd} " " raw_list{xcons{'tl1; 'tl2}}

dform raw_list_df2 : raw_list{xcons{'hd; xnil}} =
   slot["raw"]{'hd}

dform raw_list_df2 : raw_list{xnil} =
   `""

(*
 * Interface just declares it.
 *)
declare lines{'e : Dform} : Dform

dform lines_xnil_df : lines{xnil} =
   `""

dform lines_cons_df : lines{xcons{'e1; 'e2}} =
   newline szone{'e1} lines{'e2}

dform interface_df : "interface"{'body} =
   szone pushm[0] lines{'body} popm ezone

dform implementation_df : "implementation"{'body} =
   szone pushm[0] lines{'body} popm ezone

dform location_df : "location"[start:n, finish:n]{'body} =
   'body

(*
 * Resource annotations
 *)
declare res_def_list{'res : Dform} : Dform
declare resources{'resources : Dform} : Dform

dform resources_xnil_df : resources{xnil} =
   " "

dform resources_cons_df : resources{xcons{'h; 't}} =
   hspace szone keyword["{| "] pushm res_def_list{xcons{'h; 't}} popm keyword[" |} "] ezone

dform res_def_list_df1 : res_def_list{xcons{'a; xnil}} =
   'a

dform res_def_list_df2 : res_def_list{xcons{'a; 'b}} =
   'a keyword[";"] hspace res_def_list{'b}

dform resource_defs_df1 : resource_defs[name:s]{'flag; 'args} =
   flag{'flag} slot[name:s] " " 'args

dform resource_defs_df1 : resource_defs[name:s]{'flag; xnil} =
   flag{'flag} slot[name:s]

dform resource_defs_dfs : resource_defs[start:n, finish:n, name:s]{'flag; 'args} =
   resource_defs[name:s]{'flag; 'args}

dform declare_type_rewrite_df : declare_type_rewrite{'t1; 't2} =
   szone pushm[4]
   info["declare rewrite"] " "
   szone slot{'t1} ezone " " ensuremath{longleftrightarrow} hspace szone slot{'t2} ezone
   popm ezone

(*
 * Display a simple rewrite.
 *)
declare rewrite_like[name:s, kind:s]{'redex : Dform; 'contractum : Dform; 'proof : Dform; 'res : Dform} : Dform

dform rewrite_like_df : rewrite_like[name, kind]{'redex; 'contractum; 'v; 'res} =
   szone pushm[4]
   ensuremath{'v} " " info[kind:s] " " szone rewrite_name[name:s] resources{'res} keyword[":"] ezone hspace
   szone pushm[0]
   szone ensuremath{slot{'redex}} ezone
   hbreak["   ", " "] ensuremath{longleftrightarrow} hspace
   szone ensuremath{slot{'contractum}} ezone
   popm ezone
   popm ezone

dform rewrite_df : "rewrite"[name]{'redex; 'contractum; 'v; 'res} =
   rewrite_like[name, "rewrite"]{'redex; 'contractum; 'v; 'res}

dform iform_df : input_form[name]{'redex; 'contractum} =
   rewrite_like[name, "input form"]{'redex; 'contractum; xnil; xnil}

dform fake_mlrw_df : fake_mlrw[name]{'redex; 'contractum} =
   rewrite_like[name, "ml_rewrite"]{'redex; 'contractum; status_primitive{xnil}; xnil}

dform int_param_df : except_mode[src] :: "int_param"[name:v] =
   df_context_var[name]

dform int_param_df2 : mode[src] :: "int_param"[name:v] =
   `"'" slot[name:v]

dform addr_param_df : except_mode[src] :: "addr_param"[name:v] =
   df_context_var[name]

dform addr_param_df2 : mode[src] :: "addr_param"[name:v] =
   `"'" slot[name:v]

dform term_param_df : "term_param"{'t} =
   szone pushm[4]
   ensuremath{'t}
   popm ezone

declare df_rewrite_conds{'conds : Dform} : Dform

(*
 * A conditional rewrite requires special handling of the params.
 *)
dform cond_rewrite_df : "cond_rewrite"[name:s]{'params; 'conds; 'redex; 'contractum; 'proof; 'res} =
   szone pushm[4]
   ensuremath{'proof} info[" rewrite"] " " szone rewrite_name[name:s] resources{'res} df_concat{slot[" "];'params} keyword[":"] ezone
   hspace df_rewrite_conds{'conds}
   szone pushm[0]
   ensuremath{slot{'redex}} " " ensuremath{longleftrightarrow} hspace ensuremath{slot{'contractum}}
   popm ezone
   popm ezone

dform df_rewrite_conds_nil : df_rewrite_conds{xnil} = `""

dform df_rewrite_conds_cons : df_rewrite_conds{xcons{'c; 'res}} =
   ensuremath{slot{'c}} " " ensuremath{longrightarrow} hspace df_rewrite_conds{'res}

dform rule_df : "rule"[name:s]{'params; 'stmt; 'proof; 'res} =
   hzone pushm[4]
   ensuremath{'proof} info[" rule"] " " szone rule_name[name:s] resources{'res} df_concat{slot[" "];'params} keyword[":"] ezone hspace ensuremath{'stmt}
   ezone popm

declare displayed_as{'t : Dform} : Dform

dform displayed_as_df : except_mode[tex] :: displayed_as{'t} =
   szone info["(displayed as"] hspace ensuremath{slot["decl"]{'t}} info[")"] ezone

dform displayed_as_df : mode[tex] :: displayed_as{'t} =
   szone info["(displayed as"] hspace `"``" ensuremath{slot["decl"]{'t}} `"''" info[")"] ezone

dform shape_normal_df : shape_normal = `""
dform shape_iform_df : shape_iform = info["iform"] " "

dform parent_kind_none : parent_kind["none"] = `""

dform parend_kind_extends : parent_kind["extends"]{'ty} =
   `" " ensuremath{"subset"} hspace slot{'ty}

dform parend_kind_include : parent_kind["include"]{'ty} =
   `" " ensuremath{supset} hspace slot{'ty}

dform declare_typeclass_df : "declare_typeclass"{'shape; 'term; 'parent; 'ty} =
   pushm[4] szone
   info["declare typeclass"] " " 'shape
   pushm[0] szone szone tt{slot["raw"]{'term}} hspace displayed_as{'term} ezone 'ty ezone popm
   ezone popm

dform declare_type_df : "declare_type"{'shape; 'info; 'ty} =
   pushm[4] szone
   info["declare type"] " " 'shape pushm[0] szone 'info `" " ensuremath{"subset"} hspace slot{'ty} ezone popm
   ezone popm

dform declare_term_df : "declare_term"{'shape; 'info} =
   pushm[4] szone
   info["declare"] " " 'shape szone 'info ezone
   ezone popm

dform shape_class_normal_df : shape_class[0] =
   `""

dform shape_class_iform_df : shape_class[1] =
   keyword["iform "]

dform shape_class_const_df : shape_class[2] =
   keyword["const "]

dform shape_class_const_iform_df : shape_class[3] =
   keyword["const iform "]

dform define_term_df : "define_term"{'shape; 'info; term_def[name:s]{'contractum; 'res; 'flag}} =
   pushm[4] szone
      info["define"] " " flag{'flag} 'shape szone rewrite_name[name:s] resources{'res} keyword[":"] ezone hspace
      szone pushm[4]
         'info `" " ensuremath{longleftrightarrow}
         hspace ensuremath{slot{'contractum}}
      popm ezone
   popm ezone

declare ty_constraint{'ty : Dform} : Dform

dform ty_con_df : ty_constraint{'ty} =
   `" :" space slot{'ty}

dform ty_con_seq_df : ty_constraint{ty_sequent{'ty_hyp; 'ty_concl; 'ty_seq}} =
  hspace szone pushm
  szone pushm[3] `"{ " slot{'ty_hyp} `" " ensuremath{vdash} hspace slot{'ty_concl} popm hspace `"}" ezone
  `" :" hspace slot{'ty_seq} popm ezone

dform ty_con_term_df : ty_constraint{Term} = `""

dform class_term_df : "ty_term"{'term; 'opname; 'params; 'bterms; 'ty} =
   pushm[4] szone
   szone tt{slot["raw"]{'term}} hspace displayed_as{'term} ezone ty_constraint{'ty}
   popm ezone

dform class_term_df : "ty_term"{'term; 'opname; 'params; 'bterms; ty_sequent{'ty_hyp; 'ty_concl; 'ty_seq}} =
   info["sequent"] " " pushm[4] szone
   szone tt{slot["raw"]{'term}} ty_constraint{ty_sequent{'ty_hyp; 'ty_concl; 'ty_seq}} ezone hspace displayed_as{seq_sep{'term}}
   popm ezone

dform mlterm_df : "mlterm"{'term; 'cons; 'oexpr} =
   pushm[4] szone
   info["mlterm"] " " slot{'term}
   ezone popm

dform condition_df : "condition"{'term; 'cons; 'oexpr} =
   pushm[4] szone
   info["condition"] " " slot{'term}
   ezone popm

dform mlrewrite_df1 : "mlrewrite"[name:s]{'params; 'redex; some{'body}; 'res} =
   szone pushm[4]
   info["ml_rewrite"] " " rewrite_name[name:s] resources{'res} df_concat{slot[" "];'params} keyword[":"] hspace
   ensuremath{'redex} " " keyword["="] hspace slot{'body}
   popm ezone

(*
 * Parent path is separated by dots.
 *)
declare begin_cd{'t : Dform} : Dform
declare path{'t : Dform} : Dform
declare cdinternal{'t : Dform} : Dform

dform begin_cd_df : except_mode[html] :: begin_cd{'path} =
   `""

dform begin_cd_df1 : mode[java] :: begin_cd{'path} =
   izone `"<a href=\"http://cd.metaprl.local/" cdinternal{'path} `"\">" ezone

dform begin_cd_df2 : mode[html] :: begin_cd{'path} =
   izone `"<a href=\"../../" cdinternal{'path} `"/\">" ezone

dform cd_internal_df1 : cdinternal{xcons{."parent"[name:s]; xcons{'n2; 'n3}}} =
   slot[name:s] `"/" cdinternal{xcons{'n2; 'n3}}

dform cd_internal_df2 : mode[html] :: cdinternal{xcons{."parent"[name:s]; xnil}} =
   slot[name:s]

dform cd_internal_df2 : except_mode[html] :: cdinternal{xcons{."parent"[name:s]; xnil}} =
   hrefmodule[name:s]

dform cd_internal_df3 : cdinternal{xnil} = `""

dform path_parent_xnil_df2 : mode[tex] :: path{xcons{."parent"[name:s]; xnil}} =
   slot[name:s]

dform path_parent_xnil_df : path{xcons{."parent"[name:s]; xnil}} =
   slot[name:s]

dform path_parent_cons_df : path{xcons{."parent"[name:s]; .xcons{'n1; 'n2}}} =
   slot[name:s] keyword["."] xcons{'n1; 'n2}

dform parent_df : except_mode[tex] :: "parent"{'path; 'resources} =
   info["Extends"] " " begin_cd{'path} path{'path} cd_end

dform parent_df2 : mode[tex] :: "parent"{'path; 'resources} =
   info["Extends"] " " cdinternal{'path}

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
declare "rel"[op:s] : Dform

dform rel_lt_df : "rel"["lt"] = keyword["<"]
dform rel_eq_df : "rel"["eq"] = keyword["="]
dform rel_gt_df : "rel"["gt"] = keyword[">"]

dform prec_df : "prec"[name:s] =
   info["prec"] " " slot[name:s]

dform prec_rel_df : prec_rel[op, left, right] =
   info["prec "] slot[left] `" " "rel"[op] `" " slot[right]

dform id_df : "id"[n:n] =
   info["Id: "] slot[n:n]

dform resource_df : "resource"[name]{"resource"{'inp; 'outp; 'expr}} =
   pushm[3] szone
   info["let"] " " info["resource"] `" (" 'inp `", " 'outp `") " resource_name[name:s] " " keyword ["="] hspace
   szone{'expr} ezone popm

dform public_df : flag{"public"} = `""
dform private_df : flag{"private"} = info["private"] " "
dform opaque_df : flag{"opaque"} = info["opaque"] " "

dform improve_df : "improve"[name]{'flag; 'expr} =
   pushm[3] szone
   flag{'flag} info["let"] " " info["resource"] " " resource_name[name:s] " " keyword ["+="] hspace
   szone{'expr} ezone popm

dform infix_df : "infix"[name:s] =
   info["infix"] " " slot[name:s]

dform suffix_df : "suffix"[name:s] =
   info["suffix"] " " slot[name:s]

dform magic_block_df : "magic_block"[name:s]{'items} =
   info["magic_block"] " " slot[name:s] keyword[" ="] space 'items

dform summary_item_df : "summary_item"{'term} =
   szone{'term}

dform df_term_df : df_term{'t} =
   raw_list{'t}

dform meta_theorem_df : meta_theorem{'A} =
   slot{'A}

dform meta_implies_df : meta_implies{'A; 'B} =
   slot{'A} " " longrightarrow hspace 'B

dform meta_function_df : meta_function{'arg; 'A; 'B} =
   szone pushm[0] slot{'arg} keyword[": "] slot{'A} `" " popm ezone longrightarrow hspace 'B

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

(********************************
 * Argument lists
 *)
dform int_arg_df : int_arg[i:n] =
   slot[i:n]

dform term_arg_df : term_arg{'t} =
   't

dform type_arg_df : type_arg{'t} =
   't

dform addr_arg_df : addr_arg{'t} =
   szone pushm[2] `"[ " df_concat{hspace; 't} popm hspace `"]" ezone

dform addr_arg_xnil : addr_arg{xnil} =
   `"[]"

dform bool_arg_df : bool_arg[s:t] =
   slot[s:t]

dform string_arg_df : string_arg[s:s] =
   slot[s:s]

dform term_list_arg_df : term_list_arg{'terms} =
   df_concat{slot[" "];'terms}

dform arglist_df1 : arglist{'args} =
   szone pushm df_concat{slot[" "];'args} popm ezone

dform addr_subterm_df : addr_subterm[i:n] = slot[i:n]
dform addr_arg_df : addr_arg = `"Arg"
dform addr_clause_df : addr_clause[i:n] = `"Hyp " slot[i:n]
dform addr_clause_df2 : addr_clause[0] = `"Goal"

(********************************
 * Proofs.
 *)
declare msequent{'goal : Dform; 'assums : Dform} : Dform
declare goals_df{'main : Dform; 'goal : Dform; 'status : Dform} : Dform
declare status_df{'path_status : Dform; 'node_status : Dform; 'cache_status : Dform} : Dform
declare goal_list_status{'cache : Dform} : Dform

dform proof_df : proof{'main; goal_list{'goal}; 'status; 'text; 'subgoals} =
   szone pushm pagebreak goals_df{'main; 'goal; 'status} 'text 'subgoals popm ezone

dform goals_df : goals_df{goal{'path_status; 'label; 'assums; 'goal}; 'cache; 'node_status} =
   status_df{'path_status;'node_status;goal_list_status{'cache}} newline 'label msequent{'goal; 'assums}

dform goal_df : goal{'status; 'label; 'assums; 'goal} =
   status_df{'status; status_partial; xnil} newline 'label msequent{'goal; 'assums}

(* XXX display of the cache status is turned off since there is no way to access the cache anyway *)
dform cache_status_df : status_df{'path_status; 'node_status; 'cache_status} =
   'path_status `"<" 'node_status `">" (* space `"[" 'cache_status `"]" *)

dform goal_list_status_df1 : goal_list_status{xcons{goal{goal_status{'status}; 'label; 'assums; 'goal}; xcons{'hd; 'tl}}} =
   df_last{'status} `" " goal_list_status{xcons{'hd; 'tl}}

dform goal_list_status_df2 : goal_list_status{xcons{goal{goal_status{'status}; 'label; 'assums; 'goal}; xnil}} =
   df_last{'status}

(*
 * Display the meta-sequent.
 *)
declare numbered_assums{'number : Dform; 'assums : Dform} : Dform

dform msequent_df1 : msequent{'goal; xnil} =
   slot{'goal} newline

dform msequent_df2 : msequent{'goal; 'assums} =
   numbered_assums{xcons{xnil; xnil}; 'assums} slot{'goal} newline

dform numbered_assums_df1 : numbered_assums{'number; xcons{'a; 'b}} =
   szone df_length{'number} `". " pushm slot{'a} popm newline ezone numbered_assums{xcons{xnil; 'number}; 'b}

dform numbered_assums_df2 : numbered_assums{'number; xnil} =
   `"====" newline

(*
 * Status line includes commands to move up the tree.
 *)
declare goal_status_df{'a : Dform; 'b : Dform} : Dform
declare goal_status_cd{'a : Dform; 'b : Dform} : Dform
declare goal_cd_dot : Dform
declare goal_cd_up : Dform
declare goal_cd_begin{'cd : Dform} : Dform
declare goal_cd_middle{'cd : Dform} : Dform
declare goal_cd_end : Dform

dform status_df2 : goal_status{xnil} = `""

dform status_df1 : goal_status{xcons{'a; 'b}} =
   goal_status_df{xcons{'a; xnil}; 'b}

dform status_df3 : goal_status_df{'l; xcons{'a; 'b}} =
   goal_status_df{xcons{'a; 'l}; 'b}

dform status_df4 : goal_status_df{'l; xnil} =
   goal_status_cd{goal_cd_dot; 'l}

dform status_df5 : goal_status_cd{'cd; xcons{'a; 'b}} =
   goal_status_cd{xcons{goal_cd_up; 'cd}; 'b} goal_cd_begin{'cd} 'a `" " goal_cd_end

dform status_df5b : goal_status_cd{'cd; xnil} =
   `""

dform status_df6a : goal_cd_begin{'cd} =
   `""

dform status_df9a : goal_cd_end =
   `""

dform status_df6_java : mode[java] :: goal_cd_begin{'cd} =
   izone `"<a href=\"cd.metaprl.local/" goal_cd_middle{'cd}

dform status_df6_html : mode[html] :: goal_cd_begin{'cd} =
   izone `"<a href=\"." goal_cd_middle{'cd}

dform status_df7 : mode[html] :: goal_cd_middle{goal_cd_dot} =
   `"/.\">" ezone

dform status_df8 : mode[html] :: goal_cd_middle{xcons{goal_cd_up; 'cd}} =
   `"/.." goal_cd_middle{'cd}

dform status_df9 : mode[html] :: goal_cd_end =
   izone `"</a>" ezone

(*
 * Label is printed with surrounding dots.
 *)
dform label_df : goal_label[name:s] =
   `"...." slot[name:s] `"...." newline

dform status_bad_df : status_bad =
   keyword["-"]

dform status_partial_df : status_partial =
   keyword["#"]

dform status_asserted_df : status_asserted =
   keyword["?"]

dform status_complete : status_complete =
   keyword["*"]

dform status_primitive : status_primitive{'t} =
   szone keyword["!"] `"[" slot{'t} `"]" ezone

dform status_interactive : status_interactive[rules:n,nodes:n]{'status} =
   'status `"[" slot[rules:n] `"," slot[nodes:n] `"]"

(*
 * Rule box.
 *)
dform rule_box_df1 : rule_box[text:s] =
   newline szone info["BY "] pushm slot[text:s] popm ezone

dform rule_box_df2 : rule_box{'t} =
   newline szone info_begin `"BY [" pushm 't `"]" popm ezone info_end

dform rule_box_df3 : rule_box[text:s]{'t} =
   newline szone info_begin `"BY [" pushm 't `"]" space slot[text:s] popm ezone info_end

(*
 * Subgoals are printed in simplified form.
 *)
declare child_df{'number : Dform; 'child : Dform} : Dform
declare child_df{'child : Dform} : Dform

dform subgoals_df1 : subgoals{'children; 'extras} =
   newline subgoals{xcons{xnil; xnil}; 'children; 'extras}

dform subgoals_df2 : subgoals{'number; xcons{'child; 'tl}; 'extras} =
   newline child_df{'number; 'child} subgoals{xcons{xnil; 'number}; 'tl; 'extras}

dform subgoals_df3 : subgoals{'number; xnil; xcons{'a; 'b}} =
   newline info["===="] newline subgoals{'number; xcons{'a; 'b}; xnil}

dform subgoals_df4 : subgoals{'number; xcons{goal{goal_status{'status}; 'label; 'assums; 'goal}; 'tl}; xnil} =
   szone info_begin df_length{'number} `". " pushm df_last{'status} info_end newline 'label 'goal popm ezone newline
   subgoals{xcons{xnil; 'number}; xnil; 'tl}

dform subgoals_df5 : subgoals{'number; xnil; xnil} =
   `""

dform subgoals_df6 : subgoals{xnil; xnil} =
   `""

dform child_df1 : child_df{'number; goal_list{'child}} =
   szone info_begin df_down{'number} `". " pushm `"[" goal_list_status{'child} `"]" info_end newline child_df{'child} popm ezone

dform child_df2 : child_df{xcons{goal{'status; 'label; 'assums; 'goal}; 'tl}} =
   'label slot{'goal}

dform tactic_arg_df1 : tactic_arg[label:s]{'goal; 'args; 'parents} =
   szone `"[" slot[label:s] `"] " pushm 'goal popm ezone

dform tactic_arg_df2 : tactic_arg[label:s]{'goal; 'args; 'parents} =
   szone `"[" slot[label:s] `"] " pushm 'goal ezone `" " popm szone `"with args " pushm `"<" 'args `">" popm ezone

(*
 * Comments.
 *)
dform comment_df1 : mode[tex] :: comment{'t} =
   tex_comment{'t}

dform comment_df2 : mode[prl] :: comment{'t} =
   prl_comment{'t}

dform comment_df3 : mode[html] :: comment{'t} =
   html_comment{'t}

(*
 * PRL Bindings
 *
 * XXX TODO: display forms for term constructor bindings.
 *)
declare term_binding{'t : Dform; v : Dform. 't2['v] : Dform} : Dform
declare opname_binding{'t : Dform; v : Dform. 't2['v] : Dform} : Dform
declare bound_term{'t : Dform} : Dform

dform term_binding : term_binding{'t; v. 't2['v]} = 't2[bound_term{'t}]
dform term_binding2 : resources{term_binding{'t; v. 't2['v]}} = resources{'t2[bound_term{'t}]}

declare opname_of_term : TyOCaml
declare opname_bound_term{'t : Dform} : TyOCaml

dform opname_of_term_df : opname_of_term =
   Ocaml!lid["opname_of_term"]

dform opname_bound_term_df : opname_bound_term{'t} =
   bound_term{'t}

dform opname_binding : opname_binding{'t; v. 't2['v]} =
   't2[Ocaml!apply{opname_of_term; opname_bound_term{'t}}]

dform bound_term : bound_term{'t} =
   szone pushm[3] tt["<<"] hspace ensuremath{'t} popm hspace tt[">>"] ezone

(************************************************************************
 * ML INTERFACE                                                         *
 ************************************************************************)

let mk_interface_term tl = <:con< "interface"{ $mk_xlist_term tl$ } >>
let mk_implementation_term tl = <:con< "implementation"{ $mk_xlist_term tl$ } >>
let mk_href_term s t = <:con< "href"[$s$:s]{$t$} >>

let mk_status_term tl = <:con< "goal_status"{$mk_xlist_term tl$} >>

let mk_goal_label_term s = <:con< "goal_label"[$s$:s] >>
let mk_goal_list_term goals = <:con< "goal_list"{$mk_xlist_term goals$} >>
let mk_goal_term status label assums goal =
   <:con< "goal"{$status$; $label$; $mk_xlist_term assums$; $goal$} >>

let mk_subgoals_term subgoals extras =
   <:con< "subgoals"{$mk_xlist_term subgoals$; $mk_xlist_term extras$} >>

let mk_rule_box_string_term s = <:con< "rule_box"[$s$:s] >>
let mk_rule_box_term t = <:con< "rule_box"{$t$} >>

let append_rule_box t s =
   match explode_term t with
      << "rule_box"{'t} >> ->
         <:con< "rule_box"[$s$:s]{$t$} >>
    | << "rule_box"[s':s] >> ->
            <:con< "rule_box"[$if s'="" then s else s' ^ " " ^ s$:s] >>
    | << "rule_box"[s':s]{'t} >> ->
            <:con< "rule_box"[$if s'="" then s else s' ^ " " ^ s$:s]{$t$} >>
    | _ ->
         raise(Invalid_argument "Summary.append_rule_box")

let dest_rule_box t =
   match explode_term t with
      << "rule_box"[s:s] >> ->
         s
    | << "rule_box"[s:s]{'t} >> ->
         s
    | _ ->
         ""

let mk_proof_term main goal status text subgoals = <:con< "proof"{$main$; $goal$; $status$; $text$; $subgoals$} >>
let dest_proof = five_subterms

let mk_addr_term = function
   Subterm i -> <:con< addr_subterm[$int:i$] >>
 | ArgAddr -> << addr_arg >>
 | ClauseAddr i -> <:con< addr_clause[$int:i$] >>

let mk_int_arg_term i = <:con< int_arg[$int:i$] >>
let mk_term_arg_term t = <:con< term_arg{$t$} >>
let mk_type_arg_term t = <:con< type_arg{$t$} >>
let mk_bool_arg_term b = if b then << bool_arg["true":t] >> else << bool_arg["false":t] >>
let mk_string_arg_term s = <:con< string_arg[$s$:s] >>
let mk_term_list_arg_term tl = <:con< term_list_arg{$mk_xlist_term tl$} >>
let mk_addr_arg_term al = <:con< addr_arg{$mk_xlist_term (List.map mk_addr_term al)$} >>
let mk_arglist_term tl = <:con< arglist{$mk_xlist_term tl$} >>

(*
 * Turn the status into a char.
 *)
let term_of_proof_status = function
   Proof.StatusBad ->
      <<status_bad>>
 | Proof.StatusIncomplete ->
      <<status_asserted>>
 | Proof.StatusPartial ->
      <<status_partial>>
 | Proof.StatusComplete ->
      <<status_complete>>

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
