(*
 * We add a layer to filter Summary, to allow inlined modules
 * and cached info about opnames, axioms, and precedences.
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
 * Copyright (C) 1998-2005 MetaPRL Group, Cornell University and Caltech
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
open Lm_debug

open Lm_printf

open Opname
open Term_sig
open Term_ty_sig
open Rewrite_sig
open Refiner.Refiner.TermType
open Refiner.Refiner.Term
open Refiner.Refiner.TermOp
open Refiner.Refiner.TermMan
open Refiner.Refiner.TermMeta
open Refiner.Refiner.TermShape
open Refiner.Refiner.TermTy
open Refiner.Refiner.Rewrite
open Refiner.Refiner.RefineError

open Simple_print
open File_base_type
open Term_ty_infer
open Term_match_table
open Term_hash_code

open Filter_type
open Filter_util
open Filter_shape
open Filter_summary
open Filter_summary_type

(************************************************************************
 * Operator shape.
 *)

(*
 * Show the file loading.
 *)
let _ =
   show_loading "Loading Filter_cache_fun%t"

let debug_filter_cache =
   create_debug (**)
      { debug_name = "filter_cache";
        debug_description = "display cache operations during compiling";
        debug_value = false
      }

let debug_filter_path =
   create_debug (**)
      { debug_name = "filter_path";
        debug_description = "display path expansions";
        debug_value = false
      }

type op_shape =
   { sh_name    : string;
     sh_kind    : op_kind;
     sh_params  : shape_param list;
     sh_arities : int list
   }

let string_of_op_shape shape =
   let buf = Buffer.create 64 in
   let rec string_of_list f = function
      [] ->
         ()
    | [a] ->
         f a
    | hd::tl ->
         f hd;
         Buffer.add_string buf "; ";
         string_of_list f tl
   in
   let string_of_param param =
      let s =
         match param with
            ShapeString   -> "S"
          | ShapeNumber   -> "N"
          | ShapeVar      -> "V"
          | ShapeLevel    -> "L"
          | ShapeToken    -> "T"
          | ShapeShape    -> "Sh"
          | ShapeOperator -> "Op"
          | ShapeQuote    -> "Q"
      in
         Buffer.add_string buf s
   in
   let string_of_params = function
      [] ->
         ()
    | p ->
         Buffer.add_string buf "[";
         string_of_list string_of_param p;
         Buffer.add_string buf "]"
   in
   let string_of_arity i =
      Buffer.add_string buf "<";
      Buffer.add_string buf (string_of_int i);
      Buffer.add_string buf ">"
   in
   let { sh_name = name;
         sh_kind = kind;
         sh_params = params;
         sh_arities = arities
       } = shape
   in
      Buffer.add_string buf name;
      string_of_params params;
      Buffer.add_string buf "{";
      string_of_list string_of_arity arities;
      Buffer.add_string buf "}";
      (match kind with
          NormalKind ->
             ()
        | TokenKind ->
             Buffer.add_string buf ":token");
      Buffer.contents buf

(************************************************************************
 * Define an abstract optable.
 *)
module type OptableSig =
sig
   type t

   val create : unit -> t
   val add : t -> op_shape -> opname -> unit
   val find : t -> op_shape -> opname
   val find_all : t -> op_shape -> opname list
   val fold : (op_shape -> opname -> 'a -> 'a) -> t -> 'a -> 'a
end

module Optable : OptableSig =
struct
   type t = (op_shape, opname) Hashtbl.t

   let create () =
      Hashtbl.create 79

   let add = Hashtbl.add
   let find = Hashtbl.find
   let find_all = Hashtbl.find_all
   let fold = Hashtbl.fold
end

(************************************************************************
 * The cache.
 *)
module FilterSummaryTerm = Filter_summary.FilterSummaryTerm (Refiner.Refiner)

open FilterSummaryTerm

(*
 * Make the enhanced base from a normal summary base.
 *)
module MakeFilterCache (**)
   (SigMarshal : MarshalSig
    with type ctyp = MLast.ctyp
    with type resource = MLast.ctyp resource_sig)
   (StrMarshal : MarshalSig
    with type ctyp = SigMarshal.ctyp
    with type cooked = SigMarshal.cooked)
   (Base : SummaryBaseSig
    with type cooked = SigMarshal.cooked) =
struct
   (************************************************************************
    * TYPES                                                                *
    ************************************************************************)

   (*
    * Base types.
    *)
   type sig_proof = SigMarshal.proof
   type sig_ctyp  = SigMarshal.ctyp
   type sig_expr  = SigMarshal.expr
   type sig_item  = SigMarshal.item
   type sig_info  = (term, meta_term, sig_proof, sig_ctyp resource_sig, sig_ctyp, sig_expr, sig_item) module_info
   type sig_elem  = (term, meta_term, sig_proof, sig_ctyp resource_sig, sig_ctyp, sig_expr, sig_item) summary_item

   type str_proof = StrMarshal.proof
   type str_ctyp  = StrMarshal.ctyp
   type str_expr  = StrMarshal.expr
   type str_item  = StrMarshal.item
   type str_resource = StrMarshal.resource
   type str_info  = (term, meta_term, str_proof, str_resource, str_ctyp, str_expr, str_item) module_info
   type str_elem  = (term, meta_term, str_proof, str_resource, str_ctyp, str_expr, str_item) summary_item

   type arg = Base.arg

   (*
    * We save summaries with inherited attributes.
    * The resources are always sorted in order of their names.
    *
    * This is a "compressed" version of the complete info
    * for the file.
    *
    * The includes has the list of the summaries for all the immediate parents
    * of a given module, with the last included parent first in the list.
    *)
   type sig_summary =
      { sig_summary      : Base.info;
        sig_resources    : (string * sig_ctyp resource_sig) list;
        sig_infixes      : Infix.Set.t;
        sig_grammar      : Filter_grammar.t;
        sig_includes     : sig_summary list;

        sig_typeclasses  : (shape_class * opname * opname * typeclass_parent) list;
        sig_typeenv      : (shape_class * ty_term * opname) list;
        sig_termenv      : (shape_class * ty_term) list;
        sig_typereduce   : (term * term) list
      }

   (*
    * The main base keeps the filesystem base.
    * The summaries are saved with inherited attributes.
    *)
   type t =
      { lib : Base.t;
        mutable str_summaries : Base.info list;
        mutable sig_summaries : sig_summary list
      }

   (*
    * A generic delayed value.
    *)
   type 'a delayed =
      Value of 'a
    | Delayed

   (*
    * This is the extra info we keep for each module.
    * 1. There is an opname translator that maps strings
    *    to complete opnames.
    * 2. Collect all the precedences
    * 3. Collect all the resources
    * 4. A list of all inlined modules
    * 5. The current module_info
    *)
   type info =
      { opprefix : opname;
        optable : Optable.t;
        mutable summaries : sig_summary list;

        (* Types of terms *)
        mutable typedelayed    : typeclasses delayed;
        mutable typeclasses    : typeclasses;
        mutable typereduce     : typereduce;
        mutable typereductions : typereductions;
        mutable typeenv        : typeenv;
        mutable termenv        : termenv;
        mutable shapes         : (bool * shape_class) ShapeTable.t;

        (* Input grammar *)
        mutable grammar : Filter_grammar.t;

        (* Names of precedences in this module *)
        mutable precs : string list;

        (* List of resources, and where they come from *)
        mutable resources : (module_path * string * str_ctyp resource_sig ) list;

        (*
         * Info about self.
         * info is the summary of this module.
         *)
        mutable info : str_info;
        self : Base.info;
        name : string;

        (*
         * Keep a link to the summary base.
         *)
        base : t;
        select : select_type
      }

   (************************************************************************
    * Opname classes.
    *)

   (*
    * Explanation.
    *
    *    - Each term has a type description (of type ty_term).
    *      This is just a fully-annotated version of the term, so
    *      we know what the types of the subterms and the result.
    *      These are stored in the field "termenv".
    *
    *    - Each term belongs to a "type".
    *      A type is just another term, belonging to typeclass "type".
    *
    *    - A "typeclass" is used to group together terms of various types.
    *      Every type declares what typeclass it belongs to in the
    *      field "typeenv".
    *
    *    - Kinds have subtyping, stored in the field "typeclasses".
    *
    * Some properties.
    *
    *    - Every term that denotes a typeclass has the following.
    *      - It has an entry in "typeclass".
    *      - It has an entry in "typenv" belonging to the kind "typeclass".
    *      - It has an entry in "types" belonging to itself.
    *
    *    - Every term that denotes a type has the following.
    *      - It has an entry in "typenv" telling what typeclass it belongs to.
    *      - It has an entry in "types" of type "type".
    *
    *    - Every normal term has the following.
    *      - It has an entry in "types" giving its ty_term.
    *)
   let root_typeclasses, root_typeenv, root_termenv =
      let builtin_typeclasses =
         [type_opname; term_opname]
      in
         List.fold_left (fun (typeclasses, typeenv, termenv) name_op ->
               (* Add the typeclass *)
               let typeclasses = OpnameTable.add typeclasses name_op (OpnameSet.singleton name_op) in

               (*
                * Add the type.
                * Any term in this type belongs to the typeclass.
                *)
               let term = mk_term (mk_op name_op []) [] in
               let shape = shape_of_term term in
               let typeenv = ShapeTable.add typeenv shape name_op in

               (*
                * Add the type of this term.  It is a typeclass.
                *)
               let ty_info =
                  { ty_term = term;
                    ty_opname = name_op;
                    ty_params = [];
                    ty_bterms = [];
                    ty_type   = type_type
                  }
               in
               let termenv = ShapeTable.add termenv shape ty_info in
                  typeclasses, typeenv, termenv) (OpnameTable.empty, ShapeTable.empty, ShapeTable.empty) builtin_typeclasses

   (************************************************************************
    * BASE OPERATIONS                                                      *
    ************************************************************************)

   (*
    * Create the bases.
    *)
   let create path =
      { lib = Base.create path;
        str_summaries = [];
        sig_summaries = []
      }

   (*
    * Clear out all the cached summaries.
    *)
   let clear cache =
      Base.clear cache.lib;
      cache.str_summaries <- [];
      cache.sig_summaries <- []

   let clear_info info =
      let { base = cache; name = name; _ } = info in
      let { lib = base;
            str_summaries = str_summaries;
            _
          } = cache
      in
      let path = [name] in
      let str_summaries =
         List.fold_left (fun str_summaries info ->
               if Base.pathname base info = path then begin
                  Base.remove_info base info;
                  str_summaries
               end
               else
                  info :: str_summaries) [] str_summaries
      in
         cache.str_summaries <- str_summaries

   (*
    * Take a partial pathname and expand it with all the intervening modules.
    * This function works within a summary.  Raises Not_found on failure.
    *)
   let expand_in_summary =
      let rec aux path sum = function
         [modname] ->
            (* If only one name left, it should name a term *)
            let rec search = function
               (DeclareTypeClass (_, opname, _, _), _) :: tl
             | (DeclareType (_, { ty_opname = opname; _ }, _), _) :: tl
             | (DeclareTerm (_, { ty_opname = opname; _ }), _) :: tl
             | (DefineTerm (_, { ty_opname = opname; _ }, _), _) :: tl
               when fst (dst_opname opname) = modname ->
                  modname :: path
             | _ :: tl ->
                  search tl
             | [] ->
                  raise Not_found
            in
               search (info_items sum)

       | modname::tl ->
            (* Modname should name a module in the current summary *)
            let rec search path' = function
               (Module (n, sum''), _)::sum' ->
                  (* Check if this name matches *)
                  let path'' = n :: path' in
                     if n = modname then
                        aux path'' sum'' tl
                     else
                        begin
                           try search path'' (info_items sum'') with
                              Not_found -> search path' sum'
                        end
             | _::sum' ->
                  search path' sum'
             | [] ->
                  raise Not_found
            in
               search path (info_items sum)

       | [] ->
            raise (Invalid_argument "expand_in_summary")
      in
      let aux path sum =
         if !debug_filter_cache then
            eprintf "Filter_cache.expand_in_summary: %s%t" (string_of_path path) eflush;
         let path' = List.rev (aux [] sum path) in
            if !debug_filter_cache then
               eprintf "Filter_cache.expand_in_summary: expanded to %s%t" (string_of_path path') eflush;
            path'
      in
         aux

   (*
    * Expand the summary across all the opened modules.
    * Search for the head module in the list of modules, and
    * if that fails, search for the module in all submodules.
    *
    * XXX: jyh: isn't this incredibly expensive?
    * We re-unmarshal the signature every time we expand the path!?
    *)
   let expand_path cache path =
      if !debug_filter_cache || !debug_filter_path then
         eprintf "Filter_cache.expand_path: %s%t" (string_of_path path) eflush;
      let { base = { lib = base; sig_summaries = summaries; _ }; _ } = cache in
         match path with
            modname :: modpath ->
               (* First search for head module in top level modules *)
               let rec head_search = function
                  [] ->
                     None
                | { sig_summary = info; _ } :: t ->
                     let modname' = String.capitalize (Base.name base info) in
                        if !debug_filter_path then
                           eprintf "Filter_cache.expand_path.head_search: %s vs. %s%t" modname' modname eflush;
                        if modname' = modname then
                           Some info
                        else
                           head_search t
               in
               let rec mod_search = function
                  [] ->
                     raise Not_found
                | { sig_summary = info; _ } :: tl ->
                     let modname' = String.capitalize (Base.name base info) in
                        try modname' :: (expand_in_summary path (SigMarshal.unmarshal (Base.info base info))) with
                           Not_found ->
                              mod_search tl
               in
               let path' =
                  match head_search summaries with
                     Some info ->
                        modname :: (expand_in_summary modpath (SigMarshal.unmarshal (Base.info base info)))
                   | None ->
                        mod_search summaries
               in
                  path'
          | [] ->
               raise (Invalid_argument "expand_path")

   (************************************************************************
    * OPERATOR NAME TABLE                                                  *
    ************************************************************************)

   let rec strip_quotations = function
      ShapeQuote :: l ->
         strip_quotations l
    | l ->
         l

   (*
    * Construct an opname assuming it is declared in the current module.
    *)
   let mk_opname_kind cache kind names params arities =
      if names = [] then
         raise (Invalid_argument "Filter_cache_fun.mk_opname");
      let name = Lm_list_util.last names in
      let shape = { sh_name = name; sh_kind = kind; sh_params = strip_quotations params; sh_arities = arities } in
         match names with
            [_] ->
               begin
                  try
                     let opname = Optable.find cache.optable shape in
                        if !debug_opname then
                           eprintf "Filter_cache_fun.mk_opname: %s -> %s%t" (**)
                              (string_of_op_shape shape) (**)
                              (SimplePrint.string_of_opname opname) eflush;
                        opname
                  with
                     Not_found ->
                        raise (Failure ("undeclared name: " ^ string_of_op_shape shape))
               end
          | _ ->
               begin
                  (* Opname is prefixed by one or more module names, which specify it more exactly. *)
                  let path =
                     try expand_path cache names with
                        Not_found ->
                           raise (Failure ("Filter_cache_fun.mk_opname: no object with name: " ^ (string_of_opname_list names)))
                  in
                  let opname = make_opname (List.rev path) in
                  let () =
                     if !debug_opname then
                        eprintf "Filter_cache_fun.mk_opname: path: %s%t" (**)
                           (SimplePrint.string_of_opname opname) eflush
                  in
                  let all_opnames = Optable.find_all cache.optable shape in
                     if List.mem opname all_opnames then
                        opname
                     else
                        raise (Failure ("opname " ^ SimplePrint.string_of_opname opname ^ " is not declared with shape name: " ^ string_of_op_shape shape))
               end

   let find_shape_class cache shape =
      let shapes = cache.shapes in
         snd (ShapeTable.find shapes shape)

   let op_shape_of_term_kind name kind t =
      let params = List.map param_type (dest_op (dest_term t).term_op).op_params in
         if strip_quotations params != params then
            raise (Invalid_argument "Filter_cache_fun.op_shape_of_term: quoted opnames must not be declared");
         { sh_name    = name;
           sh_kind    = kind;
           sh_params  = params;
           sh_arities = subterm_arities t
         }

   let op_shape_of_term name t =
      op_shape_of_term_kind name NormalKind t

   (*
    * Flatten the typeclasses table.
    *)
   let close_typeclasses typeclasses =
      let step typeclasses =
         OpnameTable.fold (fun (typeclasses, changed) v names ->
               let names' =
                  OpnameSet.fold (fun names v ->
                        let names' =
                           try OpnameTable.find typeclasses v with
                              Not_found ->
                                 raise (Failure ("close_typeclasses: unknown typeclass: " ^ string_of_opname v))
                        in
                           OpnameSet.union names names') names names
               in
               let changed' = OpnameSet.cardinal names' <> OpnameSet.cardinal names in
               let typeclasses =
                  if changed' then
                     OpnameTable.add typeclasses v names'
                  else
                     typeclasses
               in
                  typeclasses, changed || changed') (typeclasses, false) typeclasses
      in
      let rec fixpoint typeclasses =
         let typeclasses', changed = step typeclasses in
            if changed then
               fixpoint typeclasses'
            else
               typeclasses
      in
         fixpoint typeclasses

   let get_typeclasses cache =
      match cache.typedelayed with
         Value typeclasses ->
            typeclasses
       | Delayed ->
            let typeclasses = close_typeclasses cache.typeclasses in
               cache.typedelayed <- Value typeclasses;
               typeclasses

   (*
    * Check that the shape is never seen before.
    *)
   let check_redeclaration watch cache shapeclass shape t =
      let shapes = cache.shapes in
         if ShapeTable.mem shapes shape then begin
            let watch', shapeclass' = ShapeTable.find shapes shape in
               if watch' && watch then
                  raise (Failure ("Filter_cache_fun.check_redeclaration: redefining term:\n" ^ SimplePrint.string_of_term t));
               if shapeclass' <> shapeclass then
                  raise (Failure ("Filter_cache_fun.check_redeclaration: shape class does not match the interface:\n" ^ SimplePrint.string_of_term t));
         end;
         cache.shapes <- ShapeTable.add shapes shape (watch, shapeclass)

   (*
    * Check that the opname denotes a typeclass.
    *)
   let check_is_typeclass cache opname =
      if not (OpnameTable.mem cache.typeclasses opname) then
         raise (Failure ("unknown typeclass: " ^ string_of_opname opname))

   (*
    * Check that the term denotes a typeclass.
    *)
   let check_is_typeclass_term cache term =
      let { term_op = op; term_terms = bterms } = dest_term term in
      let { op_name = opname; op_params = params } = dest_op op in
         match bterms, params with
            [], [] ->
               check_is_typeclass cache opname
          | _ ->
               raise (Failure ("unknown typeclass: " ^ string_of_opname opname))

   (*
    * Add a typeclass with the given opname.
    *
    * declare typeclass current_opname : current_type -> current_parent
    * declare typeclass current_opname : current_type <- current_parent
    * declare typeclass current_opname : current_type
    *
    * Constraints:
    *   1. current_type is a subclass of Ty
    *   2. current_parent is a typeclass
    *
    * Properties:
    *   1. << current_opname >> is a type
    *   2. Subtyping:
    *      a. Given ":> current_parent"
    *         Any element of << current_opname >> is also a member of << current_parent >>
    *      b. Given "<: current_parent"
    *         Any element of << current_parent >> is also a member of << current_opname >>
    *   3. The term << current_opname >> has type << current_opname >>
    *
    * Actions:
    *   1. current_opname is declared as a typeclass
    *      a. if the parent is "-> current_parent"
    *         -- the parents are { current_opname, current_parent }
    *      b. if the parent is "<- current_parent"
    *         -- the parents are { current_opname }
    *         -- current_opname is added as a parent of current_parent
    *      c. otherwise
    *         -- the parents are { current_opname }
    *
    *   2. The term << current_opname >> is declared as a type
    *      with typeclass << current_opname >>
    *
    *   3. The term << current_opname >> is declared with typeclass
    *      << current_type >>, which must be a sub-typeclass
    *      of Ty.
    *)
   let declare_typeclass_env watch cache shapeclass current_opname current_type current_parent =
      let current_term = mk_term (mk_op current_opname []) [] in
      let current_name = fst (dst_opname current_opname) in
      let current_shape = shape_of_term current_term in
      let current_op_shape = op_shape_of_term current_name current_term in

      (* Update typeclasses *)
      let typeclasses = OpnameSet.singleton current_opname in
      let typeclasses =
         match current_parent with
            ParentExtends parent_opname ->
               check_is_typeclass cache parent_opname;
               OpnameSet.add typeclasses parent_opname
          | ParentInclude parent_opname ->
               (try
                   let typeclasses = OpnameTable.find cache.typeclasses parent_opname in
                      cache.typedelayed <- Delayed;
                      cache.typeclasses <- OpnameTable.add cache.typeclasses parent_opname (OpnameSet.add typeclasses current_opname)
                with
                   Not_found ->
                      raise (Failure (sprintf "not a typeclass: '%s'" (string_of_opname parent_opname))));
               typeclasses
          | ParentNone ->
               typeclasses
      in

      (* Update termenv entry *)
      let current_class =
         { ty_term   = current_term;
           ty_opname = current_opname;
           ty_params = [];
           ty_bterms = [];
           ty_type   = mk_term (mk_op current_type []) []
         }
      in
         (* Make sure never seen before *)
         check_redeclaration watch cache shapeclass current_shape current_term;

         (* Make sure the type is a typeclass *)
         check_is_typeclass cache current_type;

         (* Add the typeclass *)
         cache.typedelayed <- Delayed;
         cache.typeclasses <- OpnameTable.add cache.typeclasses current_opname typeclasses;

         (* Add the type and the term *)
         cache.typeenv <- ShapeTable.add cache.typeenv current_shape current_opname;
         cache.termenv <- ShapeTable.add cache.termenv current_shape current_class;
         Optable.add cache.optable current_op_shape current_opname

   (*
    * Add an opname class.
    *
    * declare type current : current_type -> current_parent
    *
    * Constraints.
    *    1. current_type must be sub-typeclass of Ty
    *    2. current_parent must be a typeclass
    *
    * Properties:
    *    1. << current >> is a type
    *    2. The term << current >> has type << current_type >>
    *    3. Any element of << current >> is also a member of << current_parent >>
    *
    * Actions:
    *    1. Declare the type << current >> with parent typeclass << current_parent >>
    *    2. Declare the term << current >> with type << current_type >>
    *)
   let declare_type_env watch cache shapeclass current_class current_parent =
      let current_term = term_of_ty current_class in
      let current_opname = opname_of_term current_term in
      let current_shape = shape_of_term current_term in
      let current_name = fst (dst_opname current_opname) in
      let current_op_shape = op_shape_of_term current_name current_term in
         (* Check that the term has never been defined before *)
         check_redeclaration watch cache shapeclass current_shape current_term;

         (* Check that the term is a typeclass *)
         check_is_typeclass_term cache current_class.ty_type;

         (* Check that the parent is a typeclass *)
         check_is_typeclass cache current_parent;

         (* Add to the kind specified *)
         cache.typeenv <- ShapeTable.add cache.typeenv current_shape current_parent;
         cache.termenv <- ShapeTable.add cache.termenv current_shape current_class;
         Optable.add cache.optable current_op_shape current_opname

   (*
    * Add a term in a class.
    *
    * declare current : current_type
    *
    * Constraints:
    *    1. current_type must be a type
    *
    * Properties:
    *    1. The term << current >> has type << current_type >>
    *
    * Actions:
    *    1. Get the kind of term
    *       a. If it derives from Token, its kind is TokenKind
    *       b. Otherwise, it is NormalKind
    *    2. Add the entry
    *)
   let declare_term_env watch cache shapeclass current_class =
      (* Get the class *)
      let ty_term = current_class.ty_type in
      let current_kind =
         if is_fso_var_term ty_term then
            NormalKind
         else
            let ty_shape = shape_of_term ty_term in
            let ty_class_opname =
               try ShapeTable.find cache.typeenv ty_shape with
                  Not_found ->
                     raise (Failure (sprintf "declare_term %s/%s: type '%s' not found" (**)
                                        cache.name
                                        (string_of_shape (shape_of_term current_class.ty_term))
                                        (string_of_shape ty_shape)))
            in
            let typeclasses = get_typeclasses cache in
            let typeclasses = OpnameTable.find typeclasses ty_class_opname in
               if OpnameSet.mem typeclasses token_opname then
                  TokenKind
               else
                  NormalKind
      in
      let () =
         if false then
            let kind =
               match current_kind with
                  TokenKind ->
                     "token"
                | NormalKind ->
                     "normal"
            in
               eprintf "@[<hv 3>declare_term: %s %s@ : %s@]@." kind (**)
                  (SimplePrint.string_of_term current_class.ty_term)
                  (SimplePrint.string_of_term ty_term)
      in

      (* Build the class description *)
      let current_term = term_of_ty current_class in
      let current_shape = shape_of_term current_term in
      let current_opname = opname_of_term current_term in
      let current_name = fst (dst_opname current_opname) in
      let current_op_shape = op_shape_of_term_kind current_name current_kind current_term in
         (* Check the the term has nver been seen before *)
         check_redeclaration watch cache shapeclass current_shape current_term;

         (* Add the type *)
         if !debug_opname then
            eprintf "Declare term: %s@." (string_of_opname current_opname);
         cache.termenv <- ShapeTable.add cache.termenv current_shape current_class;
         Optable.add cache.optable current_op_shape current_opname

   (*
    * Add a rewrite.
    *)
   let declare_type_rewrite cache redex contractum =
      let redex_shape = shape_of_term redex in
      let contractum_shape = shape_of_term contractum in
      let () =
         if not (ShapeTable.mem cache.typeenv redex_shape) then
            raise (Failure (sprintf "not a type: '%s'" (string_of_opname (opname_of_term redex))));
         if not (ShapeTable.mem cache.typeenv contractum_shape) then
            raise (Failure (sprintf "not a type: '%s'" (string_of_opname (opname_of_term contractum))))
      in
      let rw = term_rewrite Strict empty_args_spec [redex] [contractum] in
      let () =
         if false then
            eprintf "@[<v 3>declare_type_rewrite:@ %s@ <-->@ %s@]@." (**)
               (SimplePrint.string_of_term redex)
               (SimplePrint.string_of_term contractum)
      in
         cache.typereduce <- Term_match_table.add_item cache.typereduce redex rw;
         cache.typereductions <- Shape2Table.add cache.typereductions (redex_shape, contractum_shape) (redex, contractum)

   (*************************************************************************
    * Parsing accessors.
    *)

   (*
    * Check that a term does not contain residual input forms.
    *)
   let check_input_term_error debug t1 t2 =
      raise (RefineError ("check_term", StringErrorError (debug,
                                        StringErrorError ("parsed term",
                                        TermErrorError (t1,
                                        StringErrorError ("illegal subterm",
                                        TermError t2))))))

   let check_input_term cache t_root =
      let shapes = cache.shapes in
         iter_down (fun t ->
               if not (is_var_term t || is_so_var_term t || is_context_term t || is_sequent_term t) then
                  let info =
                     try snd (ShapeTable.find shapes (unquote_shape (shape_of_term t))) with
                        Not_found ->
                           check_input_term_error "undeclared term" t_root t
                  in
                     if is_shape_iform info then
                        check_input_term_error "unexpected iform term" t_root t) t_root

   let tenv_of_cache cache =
      let { typeenv        = typeenv;
            termenv        = termenv;
            typereduce     = typereduce;
            typereductions = typereductions;
            _
          } = cache
      in
         { tenv_typeclasses    = get_typeclasses cache;
           tenv_typeenv        = typeenv;
           tenv_typereduce     = typereduce;
           tenv_typereductions = typereductions;
           tenv_termenv        = termenv
         }

   let allow_seq_bindings tenv t =
      try
         not (Term_ty_infer.is_seq_ignore_bindings_tp (ShapeTable.find tenv.tenv_termenv (shape_of_term t)))
      with
         Not_found ->
            true

   let wrap1 f loc arg =
      try f arg with
         exn ->
            Ploc.raise loc exn

   let wrap2 f loc arg1 arg2 =
      try f arg1 arg2 with
         exn ->
            Ploc.raise loc exn

   let wrap3 f loc arg1 arg2 arg3 =
      try f arg1 arg2 arg3 with
         exn ->
            Ploc.raise loc exn

   let wrap4 f loc arg1 arg2 arg3 arg4 =
      try f arg1 arg2 arg3 arg4 with
         exn ->
            Ploc.raise loc exn

   let get_parsing_state cache =
      let delay_tenv f arg = f (tenv_of_cache cache) arg in
         { opname_prefix          = (fun _ -> cache.opprefix);
           mk_opname_kind         = wrap4 (mk_opname_kind cache);
           find_shape_class       = wrap1 (find_shape_class cache);
           mk_var_contexts        = (fun _ _ _ -> None);
           infer_term             = wrap1 (delay_tenv Term_ty_infer.infer_term);
           check_rule             = wrap2 (delay_tenv Term_ty_infer.check_rule);
           check_rewrite          = wrap2 (delay_tenv Term_ty_infer.check_rewrite);
           check_type_rewrite     = wrap2 (delay_tenv check_type_rewrite);
           check_dform            = wrap2 (delay_tenv Term_ty_infer.check_dform);
           check_iform            = wrap1 (delay_tenv Term_ty_infer.check_iform);
           check_production       = wrap2 (delay_tenv Term_ty_infer.check_production);
           check_input_term       = wrap1 (check_input_term cache);
           check_input_mterm      = wrap1 (iter_mterm (check_input_term cache));
           allow_seq_bindings     = wrap1 (delay_tenv allow_seq_bindings);
           apply_iforms           = wrap2 (fun quote t -> Filter_grammar.apply_iforms quote cache.grammar t);
           apply_iforms_mterm     = wrap3 (fun quote mt args -> Filter_grammar.apply_iforms_mterm quote cache.grammar mt args);
           term_of_string         = (fun loc quote name s -> Filter_grammar.term_of_string quote cache.grammar name loc s)
         }

   (************************************************************************
    * ACCESS                                                               *
    ************************************************************************)

   (*
    * Projection.
    *)
   let info { info = info; _ } =
      info

   (*
    * XXX: BUG: JYH: this is a hack until we fix the recursive inlining
    * of summarized modules.
    *
    * Reset the base.
    *)
   let reset_hack cache =
      let base = cache.base in
         base.sig_summaries <- [];
         base.str_summaries <- []

   (*
    * Find a summary by its module path.
    *)
   let find_summarized_sig_module cache path =
      let { base = { lib = base; sig_summaries = summaries; _ }; _ } = cache in
      let compare { sig_summary = info; _ } =
         Base.pathname base info = path
      in
         Lm_list_util.find compare summaries

   (*
    * Get a previous module.
    *)
   let sub_info cache path =
      let info = find_summarized_sig_module cache path in
         SigMarshal.unmarshal (Base.info cache.base.lib info.sig_summary)

   (*
    * Find a summary by its module path.
    *)
   let find_summarized_str_module base path =
      let { lib = base; str_summaries = summaries; _ } = base in
      let compare info =
         Base.pathname base info = path
      in
         Lm_list_util.find compare summaries

   (*
    * Inherited access.
    *)
   let proofs cache         = Filter_summary.get_proofs cache.info
   let parents cache        = Filter_summary.parents cache.info
   let find cache           = Filter_summary.find cache.info
   let find_rewrite cache   = Filter_summary.find_rewrite cache.info
   let find_mlrewrite cache = Filter_summary.find_mlrewrite cache.info
   let find_mlaxiom cache   = Filter_summary.find_mlaxiom cache.info
   let find_dform cache     = Filter_summary.find_dform cache.info
   let find_prec cache name = List.mem name cache.precs
   let resources cache      = cache.resources

   let all_infixes cache =
      List.fold_left Infix.Set.union (**)
         (Filter_summary.get_infixes cache.info)
         (List.map (fun sum -> sum.sig_infixes) cache.summaries)

   let sig_resources cache path =
      (find_summarized_sig_module cache path).sig_resources

   let sig_infixes cache path =
      (find_summarized_sig_module cache path).sig_infixes

(* unused
   let sig_grammar cache path =
      (find_summarized_sig_module cache path).sig_grammar
*)

   (************************************************************************
    * Grammar.
    *)

   (*
    * The external interface.
    *)
   type precedence = Filter_grammar.precedence

   let add_token cache lexer_id id redex contractum =
      cache.grammar <- Filter_grammar.add_token cache.grammar lexer_id id redex contractum

   let add_token_pair cache lexer_id id redex1 redex2 contractum =
      cache.grammar <- Filter_grammar.add_token_pair cache.grammar lexer_id id redex1 redex2 contractum

   let add_production cache id args opt_prec contractum =
      let opt_prec =
         match opt_prec with
            Some t ->
               Some (shape_of_term t)
          | None ->
               None
      in
         cache.grammar <- Filter_grammar.add_production cache.grammar id args opt_prec contractum

   let find_input_prec cache t =
      Filter_grammar.find_prec cache.grammar (shape_of_term t)

   let input_prec_new cache assoc =
      let gram, pre = Filter_grammar.create_prec_new cache.grammar assoc in
         cache.grammar <- gram;
         pre

   let input_prec_lt cache t assoc =
      let gram, pre = Filter_grammar.create_prec_lt cache.grammar (shape_of_term t) assoc in
         cache.grammar <- gram;
         pre

   let input_prec_gt cache t assoc =
      let gram, pre = Filter_grammar.create_prec_gt cache.grammar (shape_of_term t) assoc in
         cache.grammar <- gram;
         pre

   let add_input_prec cache pre t =
      cache.grammar <- Filter_grammar.add_prec cache.grammar pre (shape_of_term t)

   let add_start cache s t lexer_id =
      cache.grammar <- Filter_grammar.add_start cache.grammar s (shape_of_term t) lexer_id

   let get_start cache =
      Filter_grammar.get_start cache.grammar

   let add_iform cache id redex contractum =
      cache.grammar <- Filter_grammar.add_iform cache.grammar id redex contractum

   let parse parse_quotation cache pos opname s =
      Filter_grammar.parse parse_quotation cache.grammar opname pos s

   let compile_parser cache =
      Filter_grammar.compile cache.grammar;
      eprintf "%a@." Filter_grammar.pp_print_grammar cache.grammar

   (************************************************************************
    * Hashing.
    *)

   (*
    * Note, for these hashing operations, the order of entries is undefined,
    * so we have to combine the individual hashes using an associative
    * commutative operation.
    *)
   let hash_int code i =
      code lxor i

   let hash_item code item =
      hash_int code (Hashtbl.hash_param max_int max_int item)

   let hash_optable code optable =
      Optable.fold (fun op_shape op code ->
            hash_item (hash_item code op_shape) op) optable code

   let hash_typeclasses code typeclasses =
      OpnameTable.fold (fun code opname opnames ->
            OpnameSet.fold hash_item (hash_item code opname) opnames) code typeclasses

   let hash_typeenv code typeenv =
      ShapeTable.fold (fun code shape opname ->
            hash_item (hash_item code shape) opname) code typeenv

   let hash_termenv code termenv =
      ShapeTable.fold (fun code shape ty ->
            let code = hash_item code shape in
            let code = hash_int code (hash_ty ty) in
               code) code termenv

   let hash_typereductions code typereductions =
      Shape2Table.fold (fun code shapes (term1, term2) ->
            let code = hash_item code shapes in
            let code = hash_int code (hash_term term1) in
            let code = hash_int code (hash_term term2) in
               code) code typereductions

   let hash cache =
      let { optable = optable;
            typeclasses = typeclasses;
            typereductions = typereductions;
            typeenv = typeenv;
            termenv = termenv;
            grammar = grammar;
            _
          } = cache
      in
      let code = 0x6ace12d4 in
      let code = hash_optable code optable in
      let code = hash_typeclasses code typeclasses in
      let code = hash_typereductions code typereductions in
      let code = hash_typeenv code typeenv in
      let code = hash_termenv code termenv in
      let code = hash_int code (Filter_grammar.hash_grammar grammar) in
      (* Squash the hash code so that it can be used on 32bit machines *)
      let code =
         match Sys.word_size with
            64 -> code land 0x7fffffff
          | 32 -> code
          | i  -> raise (Invalid_argument (Printf.sprintf "Filter_cache_fun: unknown word size: %d" i))
      in
         code

   (************************************************************************
    * UPDATE                                                               *
    ************************************************************************)

   (*
    * Add a command to the summary.
    *)
   let add_command cache item =
      cache.info <- Filter_summary.add_command cache.info item

   let add_prefix_commands cache items =
      cache.info <- Filter_summary.add_prefix_commands cache.info items

   let set_command cache item =
      try cache.info <- Filter_summary.set_command cache.info item with
         Not_found ->
            add_command cache item

   (*
    * Add a resource.
    *)
   let add_resource cache name r =
      cache.resources <- ([], name, r) :: cache.resources

   (*
    * Add a precedence.
    *)
   let add_prec cache s =
      cache.precs <- s :: cache.precs

   (*
    * See if a resource is in a list.
    *)
   let resource_member name resources =
      let rec search = function
         (_, name', _) :: t ->
            name = name' or search t
       | [] ->
            false
      in
         search resources

(* unused
   let compare_resources (name1, _) (name2, _) =
      name1 <= name2
*)

   (*
    * Merge two lists, removing duplicates.
    * The two lists must be sorted.
    *)
   let rec merge_resources rsrc1 rsrc2 =
      match rsrc1, rsrc2 with
         ((n1, r1) :: t1) as l1, (((n2, r2) :: t2) as l2) ->
               if n1 = n2 then
                  (n1, r1) :: merge_resources t1 t2
               else if n1 < n2 then
                  (n1, r1) :: merge_resources t1 l2
               else
                  (n2, r2) :: merge_resources l1 t2
       | [], l2 ->
            l2
       | l1, [] ->
            l1

   (*
    * Inline the opname information.
    *)
   let inline_sig_opnames watch cache summ =
      if !debug_filter_cache then
         eprintf "FilterCache.inline_sig_opnames: %s%t" (string_of_path (Base.pathname cache.base.lib summ.sig_summary)) eflush;
      let { sig_typeclasses = typeclasses;
            sig_typeenv     = typeenv;
            sig_termenv     = termenv;
            sig_typereduce  = typereduce;
            _
          } = summ
      in
         List.iter (fun (shapeclass, opname, typeclass_type, typeclass_parent) ->
               declare_typeclass_env watch cache shapeclass opname typeclass_type typeclass_parent) (List.rev typeclasses);
         List.iter (fun (shapeclass, ty_term, ty_opname) ->
               declare_type_env watch cache shapeclass ty_term ty_opname) (List.rev typeenv);
         List.iter (fun (shapeclass, ty_term) ->
               declare_term_env watch cache shapeclass ty_term) (List.rev termenv);
         List.iter (fun (redex, contractum) ->
               declare_type_rewrite cache redex contractum) (List.rev typereduce)

   (*
    * Collect opnames from all the ancestors.
    * We assume that all summaries have been loaded.
    *)
   let rec collect_opnames cache info =
      if not (List.memq info cache.summaries) then begin
         Lm_list_util.rev_iter (collect_opnames cache) info.sig_includes;
         inline_sig_opnames true cache info;
         cache.summaries <- info :: cache.summaries
      end

   (*
    * Inline a module into the current one.
    * The inline_{sig,str}_components function return
    * the inherited attributes.
    *)
   let rec inline_sig_components watch barg cache path self items =
      if !debug_filter_cache then
         eprintf "FilterCache.inline_sig_components: %s%t" (string_of_path path) eflush;

      (* Get all the sub-summaries *)
      let inline_component summ (item, _) =
         match item with
            Module (n, _) ->
               (*
                * The contained summaries become top level.
                * BUG: this code needs to compute resources
                * and opnames correctly if we want nested modules.
                *)
               let base = cache.base in
               let info = Base.sub_info base.lib self n in
               let info =
                  { sig_summary     = info;
                    sig_resources   = [];
                    sig_infixes     = Infix.Set.empty;
                    sig_grammar     = Filter_grammar.empty;
                    sig_includes    = [];

                    sig_typeclasses = [];
                    sig_typereduce  = [];
                    sig_typeenv     = [];
                    sig_termenv     = []
                  }
               in
                  base.sig_summaries <- info :: base.sig_summaries;
                  summ

          | DeclareTypeClass (shapeclass, opname, type_opname, parent) ->
               { summ with sig_typeclasses = (shapeclass, opname, type_opname, parent) :: summ.sig_typeclasses }
          | DeclareType (shapeclass, ty_term, type_opname) ->
               { summ with sig_typeenv = (shapeclass, ty_term, type_opname) :: summ.sig_typeenv }
          | DeclareTerm (shapeclass, ty_term)
          | DefineTerm (shapeclass, ty_term, _) ->
               { summ with sig_termenv = (shapeclass, ty_term) :: summ.sig_termenv }
          | DeclareTypeRewrite (redex, contractum) ->
               { summ with sig_typereduce = (redex, contractum) :: summ.sig_typereduce }

          | Parent { parent_name = path; _ } ->
               (* Recursive inline of all ancestors *)
               let info = inline_sig_module barg cache path in
                  { summ with sig_resources = merge_resources info.sig_resources summ.sig_resources;
                              sig_infixes   = Infix.Set.union summ.sig_infixes info.sig_infixes;
                              sig_includes  = info :: summ.sig_includes;
                              sig_grammar   = Filter_grammar.union summ.sig_grammar info.sig_grammar
                  }

          | Resource (name, r) ->
               if not (resource_member name cache.resources) then
                  cache.resources <- (path, name, r) :: cache.resources;
               { summ with sig_resources = merge_resources [name, r] summ.sig_resources }

          | Prec p ->
               let precs = cache.precs in
                  if not (List.mem p precs) then
                     cache.precs <- p :: precs;
                  summ

          | MLGramUpd upd ->
               { summ with sig_infixes = Infix.Set.add summ.sig_infixes upd }

          | PRLGrammar gram ->
               { summ with sig_grammar = Filter_grammar.unmarshal gram }

          | InputForm _
          | Comment _
          | MagicBlock _
          | ToploopItem _
          | SummaryItem _
          | Improve _
          | Id _
          | PrecRel _
          | DForm _
          | MLAxiom _
          | MLRewrite _
          | Rule _
          | CondRewrite _
          | Rewrite _ ->
               summ
      in
      let summ =
         { sig_summary     = self;
           sig_resources   = [];
           sig_infixes     = Infix.Set.empty;
           sig_grammar     = Filter_grammar.empty;
           sig_includes    = [];

           sig_typeclasses = [];
           sig_typereduce  = [];
           sig_typeenv     = [];
           sig_termenv     = []
         }
      in
      let summ = List.fold_left inline_component summ items in
         inline_sig_opnames watch cache summ;
         summ

   and inline_str_components barg cache path self (items : (term, meta_term, str_proof, str_resource, str_ctyp, str_expr, str_item) summary_item_loc list) =

      (* Get all the sub-summaries *)
      let inline_component summ (item, _) =
         match item with
            Module (n, _) ->
               (*
                * The contained summaries become top level.
                * BUG: see BUG in inline_sig_components above.
                *)
               let base = cache.base in
               let info = Base.sub_info base.lib self n in
               let info =
                  { sig_summary     = info;
                    sig_resources   = [];
                    sig_infixes     = Infix.Set.empty;
                    sig_grammar     = Filter_grammar.empty;
                    sig_includes    = [];

                    sig_typeclasses = [];
                    sig_typereduce  = [];
                    sig_typeenv     = [];
                    sig_termenv     = []
                  }
               in
                  base.sig_summaries <- info :: base.sig_summaries;
                  summ

          | DeclareTypeClass (shapeclass, opname, type_opname, parent) ->
               { summ with sig_typeclasses = (shapeclass, opname, type_opname, parent) :: summ.sig_typeclasses }
          | DeclareType (shapeclass, ty_term, type_opname) ->
               { summ with sig_typeenv = (shapeclass, ty_term, type_opname) :: summ.sig_typeenv }
          | DeclareTerm (shapeclass, ty_term)
          | DefineTerm (shapeclass, ty_term, _) ->
               { summ with sig_termenv = (shapeclass, ty_term) :: summ.sig_termenv }
          | DeclareTypeRewrite (redex, contractum) ->
               { summ with sig_typereduce = (redex, contractum) :: summ.sig_typereduce }

          | Parent { parent_name = path; _ } ->
               (* Recursive inline of all ancestors *)
               let info = inline_sig_module barg cache path in
                  cache.grammar <- Filter_grammar.union cache.grammar info.sig_grammar;
                  summ

          | PRLGrammar gram ->
               cache.grammar <- Filter_grammar.unmarshal gram;
               summ

          | InputForm _
          | Comment _
          | MagicBlock _
          | ToploopItem _
          | SummaryItem _
          | MLGramUpd _
          | Improve _
          | Resource (_, _)
          | Id _
          | PrecRel _
          | Prec _
          | DForm _
          | MLAxiom _
          | MLRewrite _
          | Rule _
          | CondRewrite _
          | Rewrite _ ->
               summ
      in
      let summ =
         { sig_summary     = self;
           sig_resources   = [];
           sig_infixes     = Infix.Set.empty;
           sig_grammar     = Filter_grammar.empty;
           sig_includes    = [];

           sig_typeclasses = [];
           sig_typereduce  = [];
           sig_typeenv     = [];
           sig_termenv     = []
         }
      in
      let summ = List.fold_left inline_component summ items in
         inline_sig_opnames true cache summ

   and inline_sig_module barg cache path =
      let base = cache.base.lib in
      let base_info =
         try Base.find base barg path SigMarshal.select AnySuffix with
            Not_found as exn ->
               eprintf "Can't find module %s%t" (string_of_path path) eflush;
               raise exn
      in
         if !debug_filter_cache then
            eprintf "FilterCache.inline_sig_module: %s: %s%t" cache.name (string_of_path path) eflush;
         try
            (*
             * XXX: BUG: JYH: in addition to opnames, we need to collect
             * resources and grammars.  This is wrong!  Should be fixed.
             *)
            let info = find_summarized_sig_module cache path in
               if !debug_filter_cache then
                  eprintf "FilterCache.inline_sig_module: %s: already loaded%t" (string_of_path path) eflush;
               collect_opnames cache info;
               info
         with
            Not_found ->
               if !debug_filter_cache then
                  eprintf "FilterCache.inline_sig_module: %s: finding: %s%t" cache.name (Base.file_name base base_info) eflush;
               let info' = SigMarshal.unmarshal (Base.info base base_info) in
               let info =
                  (* Inline the subparts *)
                  inline_sig_components true barg cache path base_info (info_items info')
               in
                  (* This module gets listed in the inline stack *)
                  cache.base.sig_summaries <- info :: cache.base.sig_summaries;
                  cache.summaries <- info :: cache.summaries;

                  if !debug_filter_cache then
                     begin
                        eprintf "Summary: %s: %s%t" cache.name (Base.file_name base base_info) eflush;
                        eprint_info info'
                     end;

                  info

   let inline_module cache barg path =
      try
         let info = inline_sig_module barg cache path in
            cache.grammar <- Filter_grammar.union cache.grammar info.sig_grammar
      with
         Not_found ->
            raise (BadCommand (sprintf "Theory is not found: %s" (String.concat "." path)))

   (*
    * To create, need:
    *    1. module_base
    *    2. Load path
    *)
   let create_cache base name self_select =
      let dir = Filename.dirname name in
      let name = Filename.basename name in
         { opprefix       = Opname.mk_opname (String.capitalize name) nil_opname;
           optable        = Optable.create ();
           summaries      = [];
           precs          = [];
           resources      = [];
           info           = new_module_info ();
           self           = Base.create_info base.lib self_select dir name;
           name           = name;
           base           = base;
           grammar        = Filter_grammar.empty;
           select         = self_select;
           typedelayed    = Delayed;
           typeclasses    = root_typeclasses;
           typereduce     = empty_table;
           typereductions = Shape2Table.empty;
           typeenv        = root_typeenv;
           termenv        = root_termenv;
           shapes         = ShapeTable.empty
         }

   (*
    * Include the grammar from the .cmiz file.
    * Also include the term declarations.
    *)
   let load_sig_grammar cache barg alt_select =
      let { base = base; self = self; name = name; _ } = cache in
      let { lib = lib; sig_summaries = summaries; _ } = base in
      let info =
         try find_summarized_sig_module cache [name] with
            Not_found ->
               let base_info = Base.find_match lib barg self alt_select AnySuffix in
               let info = SigMarshal.unmarshal (Base.info lib base_info) in
               let info = inline_sig_components false barg cache [name] base_info (info_items info) in
                  (* This module gets listed in the inline stack *)
                  cache.base.sig_summaries <- info :: cache.base.sig_summaries;
                  info
      in
         cache.grammar <- info.sig_grammar

   (*
    * When a cache is loaded follow the steps to inline
    * the file into a new cache.
    *)
   let load base barg (name : module_name) (my_select : select_type) suffix =
      let path = [name] in
      let info =
         try find_summarized_str_module base path with
            Not_found ->
               let info = Base.find base.lib barg path my_select suffix in
                  base.str_summaries <- info :: base.str_summaries;
                  info
      in
      let info' = StrMarshal.unmarshal (Base.info base.lib info) in
      let cache =
         { opprefix       = Opname.mk_opname (String.capitalize name) nil_opname;
           optable        = Optable.create ();
           summaries      = [];
           precs          = [];
           resources      = [];
           info           = info';
           self           = info;
           name           = name;
           base           = base;
           grammar        = Filter_grammar.empty;
           select         = my_select;
           typedelayed    = Delayed;
           typeclasses    = root_typeclasses;
           typereduce     = empty_table;
           typereductions = Shape2Table.empty;
           typeenv        = root_typeenv;
           termenv        = root_termenv;
           shapes         = ShapeTable.empty
         }
      in
         if !debug_filter_cache then begin
            eprintf "Filter_cache.load: loaded %s%t" name eflush;
            eprint_info info'
         end;
         if my_select = ImplementationType then
            load_sig_grammar cache barg InterfaceType;
         inline_str_components barg cache [String.capitalize name] info (info_items info');
         cache

   (*
    * Get the filename of the info.
    *)
   let filename { lib = base; _ } { self = info; _ } =
      Base.file_name base info

   let name { name = name; _ } =
      name

   (*
    * Get the signature for the module.
    *)
   let sig_info cache barg alt_select =
      let { base = base; self = self; name = name; optable = optable; _ } = cache in
      let { lib = lib; sig_summaries = summaries; _ } = base in
      let info =
         try (find_summarized_sig_module cache [name]).sig_summary with
            Not_found ->
               (*
                * nogin: This used to also create a sig_summary and add it
                * to base.sig_summaries, but we no longer have enough information
                * to do that (BUG?)
                *)
               Base.find_match lib barg self alt_select AnySuffix
      in
         SigMarshal.unmarshal (Base.info lib info)

   (*
    * Check the implementation with its interface.
    *)
   let check cache barg alt_select =
      let sig_info = sig_info cache barg alt_select in
      let items = check_implementation cache.info sig_info in
         add_prefix_commands cache items;
         sig_info

   (*
    * Parse the comments in the sig.
    *)
   let parse_comments cache parse_comment =
      let { info = info; _ } = cache in
         cache.info <- FilterSummaryTerm.parse_comments parse_comment info

   (*
    * Copy the proofs from the summary.
    *)
   let copy_proofs cache barg copy_proof =
      let { name = name;
            base = base;
            info = info;
            select = my_select;
            grammar = grammar;
            _
          } = cache
      in
      let path = [name] in
      let info' = Base.find_file base.lib barg path my_select AnySuffix in
      let _ =
         (* Save the .prlb if it doesn't exist *)
         Filter_grammar.prepare_to_marshal grammar name;
         Base.set_magic base.lib info' 1;
         Base.save_if_missing base.lib barg info' (OnlySuffixes ["prlb"])
      in
      let info' = StrMarshal.unmarshal (Base.info base.lib info') in
         cache.info <- FilterSummaryTerm.copy_proofs copy_proof info info'

   (*
    * Revert the proofs.  This is just a little different:
    * we force loading from the .prla, and we save the .prlb.
    *)
   let revert_proofs cache barg =
      let revert_proof _ proof =
         proof
      in
      let { name = name;
            base = { lib = base; _ };
            self = self;
            info = info;
            select = my_select;
            grammar = grammar;
            _
          } = cache
      in
      let path = [name] in
      let base_info = Base.find_file base barg path my_select (OnlySuffixes ["prlb"]) in
      let str_info = StrMarshal.unmarshal (Base.info base base_info) in
      let info  = FilterSummaryTerm.copy_proofs revert_proof info str_info in
         (* Save the .prlb to the .cmoz *)
         Filter_grammar.prepare_to_marshal grammar name;
         Base.set_info base self (StrMarshal.marshal info);
         Base.set_magic base self 1;
         Base.save base barg self (OnlySuffixes ["cmoz"]);
         cache.info <- info

   (*
    * Upgrade the file mode.
    *)
   let set_mode cache mode =
      let magic =
         match mode with
            CompiledSummary ->
               0
          | InteractiveSummary ->
               1
      in
      let { base = { lib = base; _ }; self = self; _ } = cache in
         Base.set_magic base self magic

   (*
    * Save the cache.
    *)
   let save cache barg suffix =
      let { base = { lib = base; _ }; name = name; self = self; info = info; grammar = grammar; _ } = cache in
      let info =
         if Filter_grammar.is_modified grammar then
            let filename = Base.file_name base self in
            let loc = Ploc.make_loc filename 1 0 (0, 0) "" in
               Filter_summary.add_command info (PRLGrammar grammar, loc)
         else
            info
      in
         Filter_grammar.prepare_to_marshal grammar name;
         if !debug_filter_cache then
            begin
               eprintf "Filter_cache.save: begin%t" eflush;
               eprint_info info
         end;
         Base.set_info base self (StrMarshal.marshal info);
         Base.save base barg self suffix;
         if !debug_filter_cache then
            eprintf "Filter_cache.save: done%t" eflush

   (*
    * Debugging.
    *)
   let eprint_info { info = info; _ } =
      Filter_summary.eprint_info info

   (*
    * The external functions watch for redeclarations.
    *)
   let declare_typeclass =
      declare_typeclass_env true

   let declare_type =
      declare_type_env true

   let declare_term =
      declare_term_env true
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
