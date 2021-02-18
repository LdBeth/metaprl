(*
 * Convert between terms and ocaml asts.
 *
 * WARNING: If you modify this file, please make sure that
 * you've updated display forms in theories/ocaml accordingly.
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
 * Modified By: Aleksey Nogin <nogin@cs.caltech.edu>
 *)
open Lm_debug
(* open Lm_symbol *)

open MLast

open Lm_printf

open Opname
(* open Term_sig *)
open Refiner_sig
open Filter_type
open Filter_util

(*
 * Show the file loading.
 *)
let _ =
   show_loading "Loading Filter_ocaml%t"

let _debug_ocaml =
   create_debug (**)
      { debug_name = "ocaml";
        debug_description = "debug ocaml term parsing";
        debug_value = false
      }

(************************************************************************
 * TYPES                                                                *
 ************************************************************************)

(*
 * Location is a pair of bignums.
 *)
type loc = Lm_num.num * Lm_num.num

(*
 * Term parsing errors pass through here.
 *)
exception FormatError of string * Refiner.Refiner.TermType.term

(*
 * Conversion is to any term module.
 *)
module FilterOCaml (ToTerm : RefinerSig) =
struct
   open ToTerm.Term
   open ToTerm.TermOp
   (* open ToTerm.RefineError *)

   module SimplePrint = Simple_print.MakeSimplePrint (ToTerm)
   module TermCopy2 = Term_copy2_weak.TermCopy2Weak (Refiner.Refiner) (ToTerm)

   (************************************************************************
    * BASIC TERM OPERATIONS                                                *
    ************************************************************************)
(*

   (*
    * We don't support antiquations.
    *)
   let dest_vala name p =
      match p with
         Ploc.VaVal v -> v
       | Ploc.VaAnt _ ->
            raise (RefineError (name, StringError "antiquotations are not supported"))

   let mk_vala name f p =
      match p with
         Ploc.VaVal v -> f v
       | Ploc.VaAnt _ ->
            raise (RefineError (name, StringError "antiquotations are not supported"))

   (*
    * Raise an error with the right format term.
    *)
   let raise_format_error str t =
      raise (FormatError (str, TermCopy2.revert t))

   (*
    * OCaml operators.
    *)
   let mk_ocaml_op =
      let tbl = Hashtbl.create 19 in
      let ocaml_op = mk_opname "Ocaml" nil_opname in
         (fun s ->
            if Hashtbl.mem tbl s then invalid_arg ("Filter_ocaml.mk_ocaml_op: " ^ s ^ " already exists");
            Hashtbl.add tbl s ();
            mk_opname s ocaml_op)

   let one_subterm s t =
      if false then
         begin
            eprintf "one_subterm: %s: begin: %s%t" s (SimplePrint.string_of_term t) eflush;
            let t = one_subterm t in
               eprintf "one_subterm: done%t" eflush;
               t
         end
      else
         one_subterm t

   let dest_dep1_term t =
      match dest_term t with
         { term_terms = [bterm]; _ } ->
            begin
               match dest_bterm bterm with
                  { bvars = [v]; bterm = t } ->
                     v, t
                | _ ->
                     raise (RefineError ("Filter_ocaml.dest_dep1_term", TermMatchError (t, "bad arity")))
            end
       | _ ->
            raise (RefineError ("Filter_ocaml.dest_dep1_term", TermMatchError (t,"bad arity")))

   (************************************************************************
    * OCaml lists.
    *)

   (*
    * Lists.
    *)
   let onil_opname = mk_ocaml_op "onil"
   let onil_term = mk_term (mk_op onil_opname []) []

   let ocons_opname = mk_ocaml_op "ocons"

   let dest_olist t =
      let rec aux trm =
         match dest_term trm with
            { term_op = op; term_terms = [bterm1; bterm2] }
               when let op = dest_op op in
                  (Opname.eq op.op_name ocons_opname || Opname.eq op.op_name xcons_opname) && op.op_params = [] ->
               begin
                  match (dest_bterm bterm1, dest_bterm bterm2) with
                     ({ bvars = []; bterm = a },
                      { bvars = []; bterm = b }) -> a::(aux b)
                   | _ -> raise (RefineError ("dest_olist", TermMatchError (t, "not a list")))
               end
          | { term_op = op; term_terms = [] }
               when let op = dest_op op in
                  (Opname.eq op.op_name onil_opname || Opname.eq op.op_name xnil_opname) && op.op_params = [] ->
               []
          | _ ->
               raise (RefineError ("dest_olist", TermMatchError (t, "not a list")))
      in
         aux t

   let ocons_op = mk_op ocons_opname []

   let rec mk_olist_term = function
      h::t ->
         mk_term ocons_op [mk_simple_bterm h; mk_simple_bterm (mk_olist_term t)]
    | [] ->
         onil_term

   (************************************************************************
    * TERM DESTRUCTORS                                                     *
    ************************************************************************)

   (*
    * Standard term ops.
    *)
   let some_op           = mk_ocaml_op "some"
   let none_op           = mk_ocaml_op "none"
   let pattern_op        = mk_ocaml_op "pattern"
   let no_pattern_op     = mk_ocaml_op "no_pattern"
   let true_op           = mk_ocaml_op "true"
   let false_op          = mk_ocaml_op "false"

   let row_field_tag_op  = mk_ocaml_op "row_field_tag"
   let row_field_inh_op  = mk_ocaml_op "row_field_inh"

   let patt_in_op	 = mk_ocaml_op "patt_in"
   let patt_fix_arg_op   = mk_ocaml_op "patt_fix_arg"
   let patt_fix_and_op   = mk_ocaml_op "patt_fix_and"
   let patt_with_op      = mk_ocaml_op "patt_with"
   let patt_if_op        = mk_ocaml_op "patt_if"
   let patt_ifelse_op    = mk_ocaml_op "patt_ifelse"
   let patt_fail_op      = mk_ocaml_op "patt_fail"

   (*
    * Loc has two integer describing character offsets.
    * Ignore remaining params.
    * XXX: TODO: This converts the modern location data into the old-style one.
    * Ideally, we should be able to embed location data as comments (bug 256).
    *)
   let dest_loc_params name t =
      if !debug_ocaml then
         eprintf "Filter_ocaml.%s: %a%t" name SimplePrint.print_simple_term_fp t eflush;
      let { term_op = op; _ } = dest_term t in
         match dest_params (dest_op op).op_params with
            (Number start) :: (Number finish) :: params
               when (Lm_num.is_integer_num start && Lm_num.is_integer_num finish) ->
               (mk_proper_loc start finish), params
          | _ ->
               raise_format_error "dest_loc: needs two numbers" t

   let dest_loc name t =
      fst (dest_loc_params name t)

   let dest_loc_term name t =
      dest_loc name t, one_subterm "dest_loc_term" t

   (*
    * Location and string take exactly three params.
    *)
   let dest_loc_string name t =
      if !debug_ocaml then
         eprintf "Filter_ocaml.%s: %a%t" name SimplePrint.print_simple_term_fp t eflush;
         match dest_loc_params name t with
            loc, [String s] ->
               loc, s
          | _ ->
               raise_format_error (Lm_printf.sprintf "dest_loc_string: %s: needs two numbers and a string" name) t

   let dest_loc_string_term name t =
      let loc, s = dest_loc_string name t in
         loc, s, one_subterm "dest_loc_string_term" t

   (*
    * Optional argument.
    *)
   let dest_opt f =
      let dest t =
         let op = opname_of_term t in
            if Opname.eq op none_op then
               None
            else
               Some (f (one_subterm "dest_opt" t))
      in
         dest

   (*
    * Strings.
    *)
   let dest_string =
      dest_string_param

   (************************************************************************
    * MLAST -> TERM                                                        *
    ************************************************************************)

   (*
    * Make an operator that contains the location.
    *    opname[start:int; finish:int]
    *)
   let mk_op_loc opname (start, finish) =
      let p1 = make_param (Number start) in
      let p2 = make_param (Number finish) in
         mk_op opname [p1; p2]

   (*
    * Also include a string name.
    *)
   let mk_op_loc_name opname (start, finish) name =
      let p1 = make_param (Number start) in
      let p2 = make_param (Number finish) in
      let p3 = make_param (String name) in
         mk_op opname [p1; p2; p3]

   (*
    * Make term with opname and location.
    *   opname[start:int; finish:int]{subterms}
    *)
   let mk_simple_term opname loc subterms =
      mk_any_term (mk_op_loc opname loc) subterms

   (*
    * This term also contains a name.
    *)
   let mk_simple_named_term opname loc name subterms =
      mk_any_term (mk_op_loc_name opname loc name) subterms

   (*
    * Optional term.
    *)
   let mk_opt f = function
      None ->
         ToTerm.Term.mk_simple_term none_op []
    | Some t ->
         ToTerm.Term.mk_simple_term some_op [f t]

   (*
    * String without location.
    *)
   let mk_loc_string_aux opname (start, finish) s tl =
      let p1 = make_param (Number start) in
      let p2 = make_param (Number finish) in
      let p3 = make_param (String s) in
      let op = mk_op opname [p1; p2; p3] in
         mk_term op (List.map (mk_bterm []) tl)

   let mk_loc_string opname loc s =
      mk_loc_string_aux opname loc s []

   let mk_loc_string_term opname loc s t =
      mk_loc_string_aux opname loc s [t]

   let mk_string opname s =
      let p1 = make_param (String s) in
      let op = mk_op opname [p1] in
         mk_term op []

   let mk_string_opt op =
      mk_opt (mk_string op)

   let mk_simple_string =
      mk_string expr_string_op

   let mk_string_list sl =
      mk_olist_term (List.map mk_simple_string sl)

   (*
    * Number with location.
    *)
   let mk_loc_int_aux opname (start, finish) i tl =
      let p1 = make_param (Number start) in
      let p2 = make_param (Number finish) in
      let p3 = make_param (Number (Lm_num.num_of_string i)) in
      let op = mk_op opname [p1; p2; p3] in
         mk_term op (List.map (mk_bterm []) tl)

   let mk_loc_int opname loc i =
      mk_loc_int_aux opname loc i []

   let mk_loc_int_term opname loc i t =
      mk_loc_int_aux opname loc i [t]

   (*
    * Other utilities
    *)
   let mk_bool flag =
      ToTerm.Term.mk_simple_term (if flag then true_op else false_op) []

   (*
    * Variables are enclosed in terms that mark
    * the variable type.
    *
    * If the var is bound, then we produce a real var,
    * Otherwise, we produce a string to lookup from the environment.
    *)
   let mk_var_aux opname vars loc s l =
      let v =
         (* XXX HACK: internal "_$" vars are always vars *)
         if List.mem s vars || (s.[0] = '_' && s.[1] = '$') then
            mk_var_term (Lm_symbol.add s)
         else
            mk_string_term opname s
      in
         mk_any_term (mk_op_loc opname loc) (v :: l)

   let mk_var opname vars loc s =
      mk_var_aux opname vars loc s []

   let mk_var_term opname vars loc s t =
      mk_var_aux opname vars loc s [t]

   let mk_patt_var opname loc s t =
      let op = mk_op_loc opname loc in
      let bterm = mk_bterm [s] t in
         mk_term op [bterm]

   let class_type_infos_op = mk_ocaml_op "class_type_infos" *)
(*
   let mk_expr vars = function
      <:expr< *)

   (*
    * Expressions.
    *)

   (************************************************************************
    * EXPORTS                                                              *
    ************************************************************************)

   (*
    * Default functions.
    *
   let dest_loc = dest_loc "external"
   let dest_loc_string = dest_loc_string "external"
   let dest_loc_int = dest_loc_int "external"
*)
   (*
    * Some default terms to return on error.
    *)
   let loc = dummy_loc
   let def_str_item = StDcl (loc, Ploc.VaVal [])

   let stub_opname = mk_opname "Ocaml_stub" nil_opname
   let stub_term = mk_any_term (mk_op stub_opname []) []

   let dest_expr (_ : ToTerm.TermType.term) = <:expr< $lid: "stub"$ >>
   let dest_patt (a : ToTerm.TermType.term) = <:patt< $lid: "stub"$ >>, a
   let dest_type (_ : ToTerm.TermType.term) = <:ctyp< $lid: "stub"$ >>
   let dest_sig  (_ : ToTerm.TermType.term) = raise (Failure "sig")
   let dest_str  (_ : ToTerm.TermType.term) = raise (Failure "str")
   let dest_mt   (_ : ToTerm.TermType.term) = raise (Failure "mt")
   let dest_me   (_ : ToTerm.TermType.term) = raise (Failure "me")

   let mk_expr _ _ = stub_term
   let mk_patt _ _ _ = stub_term
   let mk_type _ = stub_term
   let mk_sig_item _ = stub_term
   let mk_str_item _ _ = stub_term
   let mk_module_type _ = stub_term
   let mk_module_expr _ _ = stub_term

   (*
    * Terms to MLAst.
    *)
   let wrap_error s dest t =
      try dest t with
         FormatError (s', _) as exn ->
            eprintf "FormatError: %s.%s%t" s s' eflush;
            raise exn

   let wrap_default s dest def t =
      try dest t with
         FormatError (s', t) ->
            eprintf "Warning: FormatError: %s.%s term is omitted\n" s s';
            eprintf "Term: %s\n" (Simple_print.SimplePrint.short_string_of_term t);
            eprintf "\tThis is usually because the OCaml term format has changed\n";
            eprintf "\tand the .prla files haven't been updated.  This error\n";
            eprintf "\tmessage can usually be ignored, and it will go away\n";
            eprintf "\tonce the .prla files are regenerated%t" eflush;
            def

   let expr_of_term             = wrap_error "expr_of_term" dest_expr
   let patt_of_term             = wrap_error "patt_of_term" dest_patt
   let type_of_term             = wrap_error "type_of_term" dest_type
   let sig_item_of_term         = wrap_error "sig_item_of_term" dest_sig
   let str_item_of_term         = wrap_error "str_item_of_term" dest_str
   let module_type_of_term      = wrap_error "module_type_of_term" dest_mt
   let module_expr_of_term      = wrap_error "module_expr_of_term" dest_me

   let str_item_of_term_nofail  = wrap_default "str_item_of_term" dest_str def_str_item

   (*
    * MLast to term.
    *)
   let term_of_expr = mk_expr
   let term_of_patt = mk_patt
   let term_of_type = mk_type
   let term_of_sig_item = mk_sig_item
   let term_of_str_item = mk_str_item
   let term_of_module_type = mk_module_type
   let term_of_module_expr = mk_module_expr

   let term_of_resource_sig resource_op {
      resource_input = input;
      resource_output = output
   } =
      ToTerm.Term.mk_simple_term resource_op
         [ mk_type input; mk_type output ]

   let resource_sig_of_term t =
      let input, output = two_subterms t in
         { resource_input = dest_type input;
           resource_output = dest_type output
         }

   let resource_sig_of_term = wrap_error "resource_sig_of_term" resource_sig_of_term

   let term_of_resource_str resource_op res =
      ToTerm.Term.mk_simple_term resource_op
         [ mk_type res.res_input; mk_type res.res_output; mk_expr [] res.res_body ]

   let resource_str_of_term t =
      let inp, outp, expr = three_subterms t in
         { res_input = dest_type inp;
           res_output = dest_type outp;
           res_body = dest_expr expr;
         }

end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
