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
 * Modified by: Aleksey Nogin <nogin@cs.cornell.edu>
 *)
open Lm_debug

open Lm_printf
open Lm_symbol

open Opname
open Term_sig
open Term_shape_sig
open Term_ty_sig
open Rewrite_sig
open Refiner.Refiner.TermType
open Refiner.Refiner.Term
open Refiner.Refiner.TermMan
open Refiner.Refiner.TermShape
open Refiner.Refiner.TermTy
open Refiner.Refiner.Rewrite

open Simple_print
open File_base_type
open Term_ty_infer
open Term_match_table

open Filter_type
open Filter_util
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
            ShapeString -> "S"
          | ShapeNumber -> "N"
          | ShapeVar    -> "V"
          | ShapeLevel  -> "L"
          | ShapeToken  -> "T"
          | ShapeQuote  -> "Q"
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
    with type select = SigMarshal.select
    with type cooked = SigMarshal.cooked)
   (Base : SummaryBaseSig
    with type select = SigMarshal.select
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

   type select = Base.select
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

        sig_typeclasses  : (opname * opname * typeclass_parent) list;
        sig_typereduce   : (term * term) list;
        sig_typeenv      : (ty_term * opname) list;
        sig_termenv      : ty_term list
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
        mutable shapes         : ShapeSet.t;

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
        select : select
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

   let set_path { lib = base } path =
      Base.set_path base path

   (*
    * Take a partial pathname and expand it with all the intervening modules.
    * This function works within a summary.  Raises Not_found on failure.
    *)
   let expand_in_summary =
      let rec aux path sum = function
         [modname] ->
            (* If only one name left, it should name a term *)
            let rec search = function
               (DeclareTypeClass (opname, _, _), _) :: tl
             | (DeclareType ({ ty_opname = opname }, _), _) :: tl
             | (DeclareTerm { ty_opname = opname }, _) :: tl
             | (DefineTerm ({ ty_opname = opname }, _), _) :: tl
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
      let { base = { lib = base; sig_summaries = summaries } } = cache in
         match path with
            modname :: modpath ->
               (* First search for head module in top level modules *)
               let rec head_search = function
                  [] ->
                     None
                | { sig_summary = info } :: t ->
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
                | { sig_summary = info } :: tl ->
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

   (*
    * Get the prefix.
    *)
   let op_prefix cache =
      cache.opprefix

   let rec strip_quotations = function
      ShapeQuote :: l -> strip_quotations l
    | l -> l

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

   let mk_opname cache names params arities =
      mk_opname_kind cache NormalKind names params arities

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
   let check_redeclaration watch cache shape t =
      if ShapeSet.mem cache.shapes shape then
         raise (Failure ("Filter_cache_fun.check_redeclaration: redefining term " ^ SimplePrint.string_of_term t));
      if watch then
         cache.shapes <- ShapeSet.add cache.shapes shape

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
    * declare typeclass current_opname : current_type :> current_parent
    * declare typeclass current_opname : current_type <: current_parent
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
   let declare_typeclass_env watch cache current_opname current_type current_parent =
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
         check_redeclaration watch cache current_shape current_term;

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
    * declare type current : current_type :> current_parent
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
   let declare_type_env watch cache current_class current_parent =
      let current_term = term_of_ty current_class in
      let current_opname = opname_of_term current_term in
      let current_shape = shape_of_term current_term in
      let current_name = fst (dst_opname current_opname) in
      let current_op_shape = op_shape_of_term current_name current_term in
         (* Check that the term has never been defined before *)
         check_redeclaration watch cache current_shape current_term;

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
   let declare_term_env watch cache current_class =
      (* Get the class *)
      let ty_term = current_class.ty_type in
      let current_kind =
         if is_fso_var_term ty_term then
            NormalKind
         else
            let ty_shape = shape_of_term ty_term in
            let ty_opname = opname_of_term ty_term in
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
         check_redeclaration watch cache current_shape current_term;

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

   (*
    * Check that a term is well-formed.
    *)
   let tenv_of_cache cache =
      let { typeenv        = typeenv;
            termenv        = termenv;
            typereduce     = typereduce;
            typereductions = typereductions
          } = cache
      in
         { tenv_typeclasses    = get_typeclasses cache;
           tenv_typeenv        = typeenv;
           tenv_typereduce     = typereduce;
           tenv_typereductions = typereductions;
           tenv_termenv        = termenv
         }

   let infer_term cache t =
      Term_ty_infer.infer_term (tenv_of_cache cache) t

   let check_rule cache mt args =
      Term_ty_infer.check_rule (tenv_of_cache cache) mt args

   let infer_rewrite cache mt args =
      Term_ty_infer.infer_rewrite (tenv_of_cache cache) mt args

   let check_type_rewrite cache redex contractum =
      Term_ty_infer.check_type_rewrite (tenv_of_cache cache) redex contractum

   let check_dform cache t form =
      Term_ty_infer.check_dform (tenv_of_cache cache) t form

   let check_iform cache mt args =
      Term_ty_infer.check_iform (tenv_of_cache cache) mt args

   let check_production cache redices contractum =
      Term_ty_infer.check_production (tenv_of_cache cache) redices contractum

   let check_term cache t =
      ignore (infer_term cache t);
      t

   (************************************************************************
    * ACCESS                                                               *
    ************************************************************************)

   (*
    * Projection.
    *)
   let info { info = info } =
      info

   (*
    * Find a summary by its module path.
    *)
   let find_summarized_sig_module cache path =
      let { base = { lib = base; sig_summaries = summaries } } = cache in
      let compare { sig_summary = info } =
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
      let { lib = base; str_summaries = summaries } = base in
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
   let find_axiom cache     = Filter_summary.find_axiom cache.info
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

   let sig_grammar cache path =
      (find_summarized_sig_module cache path).sig_grammar

   (************************************************************************
    * Grammar.
    *)

   (*
    * The external interface.
    *)
   type precedence = Filter_grammar.precedence

   let add_token cache id redex contractum =
      cache.grammar <- Filter_grammar.add_token cache.grammar id redex contractum

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

   let add_start cache t =
      cache.grammar <- Filter_grammar.add_start cache.grammar (shape_of_term t)

   let get_start cache =
      Filter_grammar.get_start cache.grammar

   let add_iform cache id redex contractum =
      cache.grammar <- Filter_grammar.add_iform cache.grammar id redex contractum

   let parse cache pos opname s =
      Filter_grammar.parse cache.grammar opname pos s

   let compile_parser cache =
      Filter_grammar.compile cache.grammar;
      eprintf "%a@." Filter_grammar.pp_print_grammar cache.grammar

   let get_grammar cache =
      cache.grammar

   let set_grammar cache =
      Filter_grammar.set_grammar cache.grammar

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

   let hash cache =
      let aux ops op i =
         (*
          * Note: since the order is undefined, the hashes of the individual entries
          * have to be combined using an associative commutative operation.
          *)
         (Hashtbl.hash_param max_int max_int (ops, op)) lxor i
      in
         Optable.fold aux cache.optable (Filter_summary.hash cache.info)

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

   let compare_resources (name1, _) (name2, _) =
      name1 <= name2

   (*
    * Merge two lists, removing duplicates.
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
      let { sig_typeclasses = typeclasses;
            sig_typeenv     = typeenv;
            sig_termenv     = termenv;
            sig_typereduce  = typereduce
          } = summ
      in
         List.iter (fun (opname, typeclass_type, typeclass_parent) ->
               declare_typeclass_env watch cache opname typeclass_type typeclass_parent) (List.rev typeclasses);
         List.iter (fun (ty_term, ty_opname) ->
               declare_type_env watch cache ty_term ty_opname) (List.rev typeenv);
         List.iter (fun ty_term ->
               declare_term_env watch cache ty_term) (List.rev termenv);
         List.iter (fun (redex, contractum) ->
               declare_type_rewrite cache redex contractum) (List.rev typereduce)

   (*
    * Collect opnames from all the ancestors.
    * We assume that all summaries have been loaded.
    *)
   let rec collect_opnames cache info =
      if not (List.memq info cache.summaries) then
         begin
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
      (* Get the opname for this path *)
      let opprefix = make_opname path in

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

          | DeclareTypeClass (opname, type_opname, parent) ->
               { summ with sig_typeclasses = (opname, type_opname, parent) :: summ.sig_typeclasses }
          | DeclareType (ty_term, type_opname) ->
               { summ with sig_typeenv = (ty_term, type_opname) :: summ.sig_typeenv }
          | DeclareTerm ty_term
          | DefineTerm (ty_term, _) ->
               { summ with sig_termenv = ty_term :: summ.sig_termenv }
          | DeclareTypeRewrite (redex, contractum) ->
               { summ with sig_typereduce = (redex, contractum) :: summ.sig_typereduce }

          | Parent { parent_name = path } ->
               (* Recursive inline of all ancestors *)
               let info = inline_sig_module barg cache path in

               (*
                * The module save a PRLGrammar item only when the
                * grammar has been modified.  Otherwise, we get the
                * grammar from the parents.
                *)
               let grammar =
                  if Filter_grammar.is_empty info.sig_grammar then
                     summ.sig_grammar
                  else
                     info.sig_grammar
               in
                  { summ with sig_resources = merge_resources info.sig_resources summ.sig_resources;
                              sig_infixes   = Infix.Set.union summ.sig_infixes info.sig_infixes;
                              sig_includes  = info :: summ.sig_includes;
                              sig_grammar   = grammar
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
               { summ with sig_grammar = gram }

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
      (* Get the opname for this path *)
      let opprefix = make_opname path in

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

          | DeclareTypeClass (opname, type_opname, parent) ->
               { summ with sig_typeclasses = (opname, type_opname, parent) :: summ.sig_typeclasses }
          | DeclareType (ty_term, type_opname) ->
               { summ with sig_typeenv = (ty_term, type_opname) :: summ.sig_typeenv }
          | DeclareTerm ty_term
          | DefineTerm (ty_term, _) ->
               { summ with sig_termenv = ty_term :: summ.sig_termenv }
          | DeclareTypeRewrite (redex, contractum) ->
               { summ with sig_typereduce = (redex, contractum) :: summ.sig_typereduce }

          | Parent { parent_name = path } ->
               (* Recursive inline of all ancestors *)
               let info = inline_sig_module barg cache path in
                  cache.grammar <- info.sig_grammar;
                  summ

          | PRLGrammar gram ->
               cache.grammar <- gram;
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
           shapes         = ShapeSet.empty
         }

   (*
    * When a cache is loaded follow the steps to inline
    * the file into a new cache.
    *)
   let load base barg (name : module_name) (my_select : select) suffix =
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
           shapes         = ShapeSet.empty
         }
      in
         if !debug_filter_cache then
            begin
               eprintf "Filter_cache.load: loaded %s%t" name eflush;
               eprint_info info'
            end;
         inline_str_components barg cache [String.capitalize name] info (info_items info');
         cache

   (*
    * Get the filename of the info.
    *)
   let filename { lib = base } { self = info } =
      Base.file_name base info

   let name { name = name } =
      name

   (*
    * Get the signature for the module.
    *)
   let sig_info cache barg alt_select =
      let { base = base; self = self; name = name; optable = optable } = cache in
      let { lib = lib; sig_summaries = summaries } = base in
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
    * Include the grammar from the .cmiz file.
    * Also include the term declarations.
    *)
   let load_sig_grammar cache barg alt_select =
      let { base = base; self = self; name = name } = cache in
      let { lib = lib; sig_summaries = summaries } = base in
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
      let loc =
         { Lexing.pos_fname = name ^ ".mli";
           Lexing.pos_lnum = 0;
           Lexing.pos_bol = 0;
           Lexing.pos_cnum = 0
         }
      in
      let loc = loc, loc in
         cache.grammar <- info.sig_grammar

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
      let { info = info } = cache in
         cache.info <- FilterSummaryTerm.parse_comments parse_comment info

   (*
    * Copy the proofs from the summary.
    *)
   let copy_proofs cache barg copy_proof =
      let { name = name;
            base = base;
            info = info;
            select = my_select
          } = cache
      in
      let path = [name] in
      let info' = Base.find_file base.lib barg path my_select AnySuffix in
      let _ =
         (* Save the .prlb if it doesn't exist *)
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
            base = { lib = base };
            self = self;
            info = info;
            select = my_select;
            grammar = grammar
          } = cache
      in
      let path = [name] in
      let base_info = Base.find_file base barg path my_select (OnlySuffixes ["prlb"]) in
      let str_info = StrMarshal.unmarshal (Base.info base base_info) in
      let info  = FilterSummaryTerm.copy_proofs revert_proof info str_info in
         (* Save the .prlb to the .cmoz *)
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
      let { base = { lib = base }; self = self } = cache in
         Base.set_magic base self magic

   (*
    * Save the cache.
    *)
   let save cache barg suffix =
      let { base = { lib = base }; name = name; self = self; info = info; grammar = grammar } = cache in
      let info =
         if Filter_grammar.is_modified grammar then
            let pos = { Lexing.dummy_pos with Lexing.pos_fname = Base.file_name base self } in
            let loc = pos, pos in
               Filter_summary.add_command info (PRLGrammar (Filter_grammar.prepare_to_marshal grammar name), loc)
         else
            info
      in
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
   let eprint_info { info = info } =
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
