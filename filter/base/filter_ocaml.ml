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
open Lm_symbol

open MLast

open Lm_printf

open Opname
open Term_sig
open Refiner_sig
open Filter_type
open Filter_util

(*
 * Show the file loading.
 *)
let _ =
   show_loading "Loading Filter_ocaml%t"

let debug_ocaml =
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
   open ToTerm.RefineError

   module SimplePrint = Simple_print.MakeSimplePrint (ToTerm)
   module TermCopy2 = Term_copy2_weak.TermCopy2Weak (Refiner.Refiner) (ToTerm)

   (************************************************************************
    * BASIC TERM OPERATIONS                                                *
    ************************************************************************)

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

   let jc_op             = mk_ocaml_op "joinclause_case"
   let jcl_op            = mk_ocaml_op "joinclause_list"
   let joinclause_op     = mk_ocaml_op "joinclause"

   let expr_rpl_string_op = mk_ocaml_op "rpl_string"

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

   (*
    * Variables are wrapped.
    *)
   let dest_var t =
      let t = one_subterm "dest_var_t" t in
         if is_var_term t then
            string_of_symbol (ToTerm.Term.dest_var t)
         else
            dest_string_param t

   (*
    * Integers are also wrapped.
    *)
   let dest_loc_int name t =
      if !debug_ocaml then
         eprintf "Filter_ocaml.%s: %a%t" name SimplePrint.print_simple_term_fp t eflush;
         match dest_loc_params name t with
            loc, [ Number i ] ->
               loc, Lm_num.string_of_num i
          | _ ->
               raise_format_error "dest_loc_int: needs three numbers" t

   (*
    * Redefine some functions to tag results.
    * XXX: TODO: This converts the modern location data into the old-style one.
    * Ideally, we should be able to embed location data as comments (bug 256).
    *)
   let num_of_loc loc =
     Lm_num.num_of_int (Ploc.first_pos loc), Lm_num.num_of_int (Ploc.last_pos loc)

   let loc_of_expr,
       loc_of_patt,
       loc_of_ctyp,
       loc_of_sig_item,
       loc_of_str_item,
       loc_of_module_type,
       loc_of_module_expr =
      let loc_of_aux f x = num_of_loc (f x)
      in
         loc_of_aux loc_of_expr,
         loc_of_aux loc_of_patt,
         loc_of_aux loc_of_ctyp,
         loc_of_aux loc_of_sig_item,
         loc_of_aux loc_of_str_item,
         loc_of_aux loc_of_module_type,
         loc_of_aux loc_of_module_expr

   (*
    * Conversion between pattern and expression identifiers.
    *)
   let rec expr_of_patt_ident p =
      let _loc = MLast.loc_of_patt p in
         match p with
            <:patt< $p1$ . $p2$ >> ->
              <:expr< $expr_of_patt_ident p1$ . $expr_of_patt_ident p2$ >>
          | <:patt< $uid: uid$ >> ->
              <:expr< $uid: uid$ >>
          | <:patt< $lid: lid$ >> ->
              <:expr< $lid: lid$ >>
          | _ ->
              Stdpp.raise_with_loc _loc (Failure "Filter_ocaml.expr_of_patt_ident: not an identifier")

   let rec patt_of_expr_ident e =
      let _loc = MLast.loc_of_expr e in
         match e with
          | <:expr< $e1$ . $e2$ >> ->
               <:patt< $patt_of_expr_ident e1$ . $patt_of_expr_ident e2$ >>
          | <:expr< $uid: uid$ >> ->
               <:patt< $uid: uid$ >>
          | <:expr< $lid: lid$ >> ->
               <:patt< $lid: lid$ >>
          | _ ->
               Stdpp.raise_with_loc _loc (Failure "Filter_ocaml.patt_of_expr_ident: not an identifier")

   (*
    * For looking up destructors in a hashtable.
    *)
   let dest_tbl code table t =
      let opname = opname_of_term t in
         try Hashtbl.find table opname t with
            Not_found ->
               raise_format_error ("Filter_ocaml.dest_" ^ code ^ " : unrecognized opname: " ^ string_of_opname opname) t
          | (RefineError (_, TermMatchError (t, s))) ->
               raise_format_error ("Filter_ocaml.dest_" ^ code ^ " : " ^ s) t


   let add_tbl table name f =
      let opname = mk_ocaml_op name in
         Hashtbl.add table opname f;
         opname

   (*
    * Destruction uses hashtables.
    *)
   let dest_expr, add_expr =
      let table = Hashtbl.create 17 in
         dest_tbl "expr" table, add_tbl table

   let dest_type, add_type =
      let table = Hashtbl.create 17 in
         dest_tbl "type" table, add_tbl table

   let dest_sig,  add_sig =
      let table = Hashtbl.create 17 in
         dest_tbl "sig"  table, add_tbl table

   let dest_str,  add_str =
      let table = Hashtbl.create 17 in
         dest_tbl "str"  table, add_tbl table

   let dest_mt,   add_mt =
      let table = Hashtbl.create 17 in
         dest_tbl "mt"  table, add_tbl table

   let dest_me,   add_me =
      let table = Hashtbl.create 17 in
         dest_tbl "me"  table, add_tbl table

   let dest_wc,   add_wc =
      let table = Hashtbl.create 5  in
         dest_tbl "wc"  table, add_tbl table

   let dest_ct,   add_ct =
      let table = Hashtbl.create 5  in
         dest_tbl "ct"  table, add_tbl table

   let dest_ce,   add_ce =
      let table = Hashtbl.create 5  in
         dest_tbl "ce"  table, add_tbl table

   let dest_cf,   add_cf =
      let table = Hashtbl.create 17 in
         dest_tbl "cf"  table, add_tbl table

   let dest_ctf,  add_ctf =
      let table = Hashtbl.create 17 in
         dest_tbl "ctf" table, add_tbl table

   (*
    * Utilities.
    *)
   let dest_bool t =
      let op = opname_of_term t in
         not (Opname.eq op false_op)

   let dest_ident_pe t =
      let p, e = two_subterms t in
         patt_of_expr_ident (dest_expr p), dest_expr e

   let dest_st t =
      let s, t = two_subterms t in
         dest_string s, dest_type t

   let dest_smt t =
      let s, mt = two_subterms t in
         Ploc.VaVal (Some (Ploc.VaVal (dest_string s))), dest_mt mt

   let dest_sme t =
      let s, me = two_subterms t in
         Ploc.VaVal (Some (Ploc.VaVal (dest_string s))), dest_me me

   let dest_sl t =
      let sl = one_subterm "dest_sl" t in
      let sl = dest_olist sl in
         List.map dest_string sl

   let dest_sbb t =
      let s, b1, b2 = three_subterms t in
      let b1 = dest_bool b1 in
      let b2 = dest_bool b2 in
         Ploc.VaVal (Some (dest_string s)), (if b1 then Some b2 else None)

   let dest_string_opt_bool_opt t =
      let s, b = two_subterms t in
         dest_opt dest_string s, dest_opt dest_bool b

   let dest_tdl =
      let dest_tc t =
         let t1, t2 = two_subterms t in
            dest_type t1, dest_type t2
      in fun t ->
         let s, sl, t, tl = four_subterms t in
         let sl = dest_olist sl in
         let tl = dest_olist tl in
         let loc, s = dest_loc_string "dest_tdl" s in
         let prm = List.map dest_sbb sl in
            { MLast.tdNam = Ploc.VaVal (loc, Ploc.VaVal s);
              MLast.tdPrm = Ploc.VaVal prm;
              MLast.tdPrv = Ploc.VaVal false;
              MLast.tdDef = dest_type t;
              MLast.tdCon = Ploc.VaVal (List.map dest_tc tl)
            }

   let dest_expr_opt t = dest_opt dest_expr t

   let dest_patt, add_patt =
      let table = Hashtbl.create 17 in
         dest_tbl "patt" table, add_tbl table

   let dest_patt_opt t =
      let op = opname_of_term t in
      let t = one_subterm "dest_patt_op" t in
         if Opname.eq op no_pattern_op then
            None, t
         else
            let p, t = dest_patt t in
               Some p, t

   let dest_let t =
      let rec dest t =
         if Opname.eq (opname_of_term t) patt_in_op then
            [], one_subterm "dest_let" t
         else
            let p, t = dest_patt t in
            let pl, e = dest t in
               p :: pl, e
      in
      let pe, el = two_subterms t in
      let pl, e = dest pe in
      let el = List.map dest_expr (dest_olist el) in
         if List.length pl <> List.length el then
            raise (Failure "Filter_ocaml.dest_let: pattern count mismatch")
         else
            List.combine pl el, e

   let dest_fix t =
      let rec dest_exprs t =
         if Opname.eq (opname_of_term t) patt_in_op then
            [], one_subterm "dest_fix" t
         else
            let e, t = two_subterms_opname patt_fix_arg_op t in
            let es, e' = dest_exprs t in
            (dest_expr e) :: es, e'
      and dest_patts t =
         let p, t = dest_patt t in
         if Opname.eq (opname_of_term t) patt_fix_arg_op then
            let es, e = dest_exprs t in
            [p], es, e
         else
            let t = one_subterm_opname patt_fix_and_op t in
            let ps, es, e = dest_patts t in
            p :: ps, es, e
      in
         let ps, es, e = dest_patts (one_subterm "dest_fix" t) in
         (List.combine ps es), e

   let dest_class_type_infos t =
      let loc = dest_loc "dest_class_type_infos" t in
      let s, sl, b, t = four_subterms t in
      let prm = List.map dest_string_opt_bool_opt (dest_olist sl) in
          { ciLoc = loc;
            ciNam = Ploc.VaVal (dest_string s);
            ciPrm = loc, Ploc.VaVal (List.map (fun (s, b) -> Ploc.VaVal s, b) prm);
            ciVir = Ploc.VaVal (dest_bool b);
            ciExp = dest_ct t
          }

   let dest_fun_aux =
      let dest_pwe t =
         let p, e = dest_patt t in
            if Opname.eq (opname_of_term e) patt_with_op then
               let w, e = two_subterms e in
                  p, Some (dest_expr w), dest_expr e
            else
               p, None, dest_expr (one_subterm "dest_fun_aux" e)
      in
      let rec dest t =
         let op = opname_of_term t in
            if Opname.eq op patt_fail_op then
               []
            else if Opname.eq op patt_if_op then
               [dest_pwe (one_subterm "dest_fun_aux" t)]
            else if Opname.eq op patt_ifelse_op then
               let pe, pel = two_subterms t in
                  dest_pwe pe :: dest pel
            else
               raise (Failure "Filter_ocaml.dest_fun_aux")
      in
         dest

   let dest_fun_vala_aux pwel =
      List.map (fun (p, w, e) -> p, Ploc.VaVal w, e) (dest_fun_aux pwel)

   let dest_lab_aux =
      let dest_poe t =
         let p, oe = dest_patt t in
         let oe = dest_opt dest_expr oe in
               p, Ploc.VaVal oe
      in
      let rec dest t =
         let op = opname_of_term t in
            if Opname.eq op patt_fail_op then
               []
            else if Opname.eq op patt_if_op then
               [dest_poe (one_subterm "dest_lab_aux" t)]
            else if Opname.eq op patt_ifelse_op then
               let pe, pel = two_subterms t in
                  dest_poe pe :: dest pel
            else
               raise (Failure "Filter_ocaml.dest_lab_aux")
      in
         dest

   let dest_joinclause t =
      let tl = dest_olist t in
         List.map (fun t ->
            let loc = dest_loc "dest_joinclause" t in
            let jcll = one_subterm "dest_joinclause" t in
            let jcll =
               List.map (fun t ->
                  let loc1 = dest_loc "dest_def_str_1" t in
                  let jcl, e = two_subterms t in
                  let e = dest_expr e in
                  let jcl =
                     List.map (fun t ->
                        let loc2, s = dest_loc_string "dest_def_str_2" t in
                        let po = one_subterm "dest_def_str_3" t in
                        let po, _ = dest_patt_opt po in
                           loc2, (loc2, Ploc.VaVal s), Ploc.VaVal po) (dest_olist jcll)
                  in
                    (loc1, Ploc.VaVal jcl, e)) (dest_olist jcll)
            in
               { jcLoc = loc; jcVal = Ploc.VaVal jcll }) tl

   let dest_patt_triple t =
      let p1, t = dest_patt (one_subterm "dest_patt_triple" t) in
      let p2, t = dest_patt (one_subterm "dest_patt_triple" t) in
         p1, p2, one_subterm "dest_patt_triple" t

   let expr_string_op =
      let dest_string_expr t =
         let _loc, s = dest_loc_string "dest_string_expr" t in
            <:expr< $str:s$ >>
      in add_expr "string" dest_string_expr

   let expr_seq_op =
      let dest_seq_expr t =
         let _loc = dest_loc "dest_seq_expr" t in
         let el = dest_olist (one_subterm "dest_seq_expr" t) in
            <:expr< do { $list:List.map dest_expr el$ } >>
      in add_expr "sequence" dest_seq_expr

   let dest_sigloc t =
      dest_sig (one_subterm "dest_sigloc" t), dest_loc "dest_sigloc" t

   let dest_strloc t =
      dest_str (one_subterm "dest_strloc" t), dest_loc "dest_strloc" t

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

   let class_type_infos_op = mk_ocaml_op "class_type_infos"

   (*
    * Expressions.
    *)
   let rec mk_expr =
      let expr_char_op =
         let dest_char_expr t =
            let _loc, s = dest_loc_string "dest_char_expr" t in
               <:expr< $chr:s$ >>
         in add_expr "char" dest_char_expr
      and expr_float_op =
         let dest_float_expr t =
            let _loc, s = dest_loc_string "dest_float_expr" t in
               <:expr< $flo:s$ >>
         in add_expr "float" dest_float_expr
      and expr_int_op =
         let dest_int_expr t =
            let _loc, i = dest_loc_int "dest_int_expr" t in
               <:expr< $int:i$ >>
         in add_expr "int" dest_int_expr
      and expr_native_int_op =
         let dest_native_int_expr t =
            let _loc, i = dest_loc_int "dest_native_int_expr" t in
               <:expr< $nativeint:i$ >>
         in add_expr "native_int" dest_native_int_expr
      and expr_int32_op =
         let dest_int32_expr t =
            let _loc, i = dest_loc_int "dest_int32_expr" t in
               <:expr< $int32:i$ >>
         in add_expr "int32" dest_int32_expr
      and expr_int64_op =
         let dest_int64_expr t =
            let _loc, i = dest_loc_int "dest_int64_expr" t in
               <:expr< $int64:i$ >>
         in add_expr "int64" dest_int64_expr
      and expr_asr_op =
         let dest_asr_expr t =
            let _loc = dest_loc "dest_asr_expr" t in
            let e = one_subterm "dest_asr_expr" t in
               <:expr< assert $dest_expr e$ >>
         in add_expr "assert" dest_asr_expr
      and expr_lid_op =
         let dest_lid_expr t =
            let _loc = dest_loc "dest_lid_expr" t in
               <:expr< $lid:dest_var t$ >>
         in add_expr "lid" dest_lid_expr
      and expr_local_module_op =
         let dest_local_module_expr t =
            let _loc = dest_loc "dest_local_module_expr" t in
            let s, me, e = three_subterms t in
               <:expr< let module $uid:dest_string s$ = $dest_me me$ in $dest_expr e$ >>
            (* ExLmd (loc,
                      Ploc.VaVal (dest_string s),
                      dest_me me,
                      dest_expr e) *)
         in add_expr "local_module" dest_local_module_expr
      and expr_uid_op =
         let dest_uid_expr t =
            let _loc = dest_loc "dest_uid_expr" t in
               <:expr< $uid:dest_var t$ >>
         in add_expr "uid" dest_uid_expr
      and expr_proj_op =
         let dest_proj_expr t =
            let _loc = dest_loc "dest_proj_expr" t in
            let e1, e2 = two_subterms t in
               <:expr< $dest_expr e1$ . $dest_expr e2$ >>
         in add_expr "proj" dest_proj_expr
      and expr_anti_op =
         let dest_anti_expr t =
            let _loc = dest_loc "dest_ant_expr" t in
            let e = one_subterm "dest_anti_expr" t in
               <:expr< $anti: dest_expr e$ >>
         in add_expr "anti" dest_anti_expr
      and expr_apply_op =
         let dest_apply_expr t =
            let _loc = dest_loc "dest_apply_expr" t in
            let e1, e2 = two_subterms t in
               <:expr< $dest_expr e1$ $dest_expr e2$ >>
         in add_expr "apply" dest_apply_expr
      and expr_array_subscript_op =
         let dest_array_subscript_expr t =
            let _loc = dest_loc "dest_array_subscript" t in
            let e1, e2 = two_subterms t in
               <:expr< $dest_expr e1$ .( $dest_expr e2$ ) >>
         in add_expr "array_subscript" dest_array_subscript_expr
      and expr_array_op =
         let dest_array_expr t =
            let _loc = dest_loc "dest_array_expr" t in
            let el = List.map dest_expr (dest_olist (one_subterm "dest_array_expr" t)) in
               <:expr< [| $list:el$ |] >>
         in add_expr "array" dest_array_expr
      and expr_assign_op =
         let dest_assign_expr t =
            let _loc = dest_loc "dest_assign_expr" t in
            let e1, e2 = two_subterms t in
               <:expr< $dest_expr e1$ := $dest_expr e2$ >>
         in add_expr "assign" dest_assign_expr
      and expr_coerce_class_op =
         let dest_coerce_class_expr t =
            let loc = dest_loc "dest_coerce_class_expr" t in
            let e, s, t = three_subterms t in
               ExCoe (loc, dest_expr e, dest_opt dest_type s, dest_type t)
         in add_expr "coerce_class" dest_coerce_class_expr
      and expr_upto_op =
         let dest_upto_expr t =
            let _loc = dest_loc "dest_upto_expr" t in
            let e1, e2, v, e3 = dest_dep0_dep0_dep1_any_term t in
            let el = dest_olist e3 in
               <:expr< for $string_of_symbol v$ = $dest_expr e1$ $to:true$ $dest_expr e2$ do { $list: List.map dest_expr el$ } >>
         in add_expr "for_upto" dest_upto_expr
      and expr_downto_op =
         let dest_downto_expr t =
            let _loc = dest_loc "dest_downto_expr" t in
            let e1, e2, v, e3 = dest_dep0_dep0_dep1_any_term t in
            let el = dest_olist e3 in
               <:expr< for $string_of_symbol v$ = $dest_expr e1$ $to:false$ $dest_expr e2$ do { $list: List.map dest_expr el$ } >>
         in add_expr "for_downto" dest_downto_expr
      and expr_if_op =
         let dest_if_expr t =
            let _loc = dest_loc "dest_if_expr" t in
            let e1, e2, e3 = three_subterms t in
               <:expr< if $dest_expr e1$ then $dest_expr e2$ else $dest_expr e3$ >>
         in add_expr "ifthenelse" dest_if_expr
      and expr_laz_op =
         let dest_laz_expr t =
            let _loc = dest_loc "dest_laz_expr" t in
            let e = one_subterm "dest_laz_expr" t in
               <:expr< lazy $dest_expr e$ >>
         in add_expr "lazy" dest_laz_expr
      and expr_new_op =
         let dest_new_expr t =
            let _loc = dest_loc "dest_new_expr" t in
               <:expr< new $list:List.map dest_string (dest_olist t)$ >>
         in add_expr "new" dest_new_expr
      and expr_obj_op =
         let dest_obj_expr t =
            let loc = dest_loc "dest_obj_expr" t in
            let po, t = dest_patt_opt t in
            let cfl = List.map dest_cf (dest_olist t) in
               ExObj(loc, Ploc.VaVal po, Ploc.VaVal cfl)
         in add_expr "obj" dest_obj_expr
      and expr_stream_op =
         let dest_se t =
            let s, e = two_subterms t in
               dest_string s, dest_expr e
         in let dest_stream_expr t =
            let _loc = dest_loc "dest_stream_expr" t in
            let sel = dest_olist (one_subterm "dest_stream_expr" t) in
               <:expr< {< $list: List.map dest_se sel$ >} >>
         in add_expr "stream" dest_stream_expr
      and expr_record_op =
         let dest_record_expr t =
            let loc = dest_loc "dest_record_expr" t in
            let pel, eo = two_subterms t in
            let pel = dest_olist pel in
            let pel = List.map dest_ident_pe pel in
            let eo = dest_expr_opt eo in
               MLast.ExRec (loc, Ploc.VaVal pel, eo)
         in add_expr "record" dest_record_expr
      and expr_select_op =
         let dest_select_expr t =
            let _loc = dest_loc "dest_select_expr" t in
            let e, s = two_subterms t in
               <:expr< $dest_expr e$ # $dest_string s$ >>
         in add_expr "select" dest_select_expr
      and expr_string_subscript_op =
         let dest_string_subscript_expr t =
            let _loc = dest_loc "dest_string_subscript_expr" t in
            let e1, e2 = two_subterms t in
               <:expr< $dest_expr e1$ .[ $dest_expr e2$ ] >>
         in add_expr "string_subscript" dest_string_subscript_expr
      and expr_tuple_op =
         let dest_tuple_expr t =
            let _loc = dest_loc "dest_tuple_expr" t in
            let el = dest_olist (one_subterm "dest_tuple_expr" t) in
               <:expr< ( $list: List.map dest_expr el$ ) >>
         in add_expr "tuple" dest_tuple_expr
      and expr_cast_op =
         let dest_cast_expr t =
            let _loc = dest_loc "dest_cast_expr" t in
            let e, t = two_subterms t in
               <:expr< ( $dest_expr e$ : $dest_type t$ ) >>
         in add_expr "cast" dest_cast_expr
      and expr_while_op =
         let dest_while_expr t =
            let _loc = dest_loc "dest_while_expr" t in
            let e, el = two_subterms t in
               <:expr< while $dest_expr e$ do { $list: List.map dest_expr (dest_olist el)$ } >>
         in add_expr "while" dest_while_expr
      and expr_bigarray_element_op =
         let dest_bigarray_element_expr t =
            let _loc = dest_loc "dest_bigarray_element_expr" t in
            let e, el = two_subterms t in
            let el = List.map dest_expr (dest_olist el) in
               <:expr< $dest_expr e$ .{ $list:el$ } >>
         in add_expr "bigarray_element" dest_bigarray_element_expr
      and expr_vrn_op =
         let dest_vrn_expr t =
            let loc, s = dest_loc_string "dest_vrn_expr" t in
               MLast.ExVrn (loc, Ploc.VaVal s)
         in add_expr "vrn" dest_vrn_expr
      and expr_olb_op =
         let dest_olb_expr t =
            let loc = dest_loc "dest_olb_expr" t in
            let p, t = dest_patt (one_subterm "dest_olb_expr" t) in
            let eo = dest_expr_opt t in
               MLast.ExOlb (loc, p, Ploc.VaVal eo)
         in add_expr "olb" dest_olb_expr
      and expr_jdf_op =
         let dest_jdf_expr t =
            let loc = dest_loc "dest_jdf_expr" t in
            let e, jc = two_subterms t in
            let jc = dest_joinclause jc in
               MLast.ExJdf (loc, Ploc.VaVal jc, dest_expr e)
         in add_expr "jdf" dest_jdf_expr
      and expr_lop_op =
         let dest_lop_expr t =
            let _loc = dest_loc "dest_lop_expr" t in
            let me, e = two_subterms t in
               MLast.ExLop (_loc, dest_me me, dest_expr e)
         in add_expr "lop_expr" dest_lop_expr
      and expr_par_op =
         let dest_par_expr t =
            let _loc = dest_loc "dest_par_expr" t in
            let e1, e2 = two_subterms t in
               MLast.ExPar (_loc, dest_expr e1, dest_expr e2)
         in add_expr "par_expr" dest_par_expr
      and expr_pck_op =
         let dest_pck_expr t =
            let _loc = dest_loc "dest_pck_expr" t in
            let me, mto = two_subterms t in
               MLast.ExPck (_loc, dest_me me, dest_opt dest_mt mto)
         in add_expr "pck_expr" dest_pck_expr
      and expr_rpl_op =
         let dest_rpl_expr t =
            let _loc = dest_loc "dest_rpl_expr" t in
            let eo, ls = two_subterms t in
            let loc2, s = dest_loc_string "dest_rpl_expr" ls in
               MLast.ExRpl (_loc, Ploc.VaVal (dest_opt dest_expr eo), Ploc.VaVal (loc2, Ploc.VaVal s))
         in add_expr "rpl_expr" dest_rpl_expr
      and expr_spw_op =
         let dest_spw_expr t =
            let _loc = dest_loc "dest_spw_expr" t in
            let e = one_subterm "dest_spw_expr" t in
               MLast.ExSpw (_loc, dest_expr e)
         in add_expr "spw" dest_spw_expr
       and expr_xtr_op =
         let dest_xtr_expr t =
            let loc = dest_loc "dest_xtr_expr" t in
            let s, eo = two_subterms t in
               ExXtr (loc, dest_string s, dest_opt (fun e -> Ploc.VaVal (dest_expr e)) eo)
         in add_expr "expr_xtr" dest_xtr_expr
      (*
       * Compute a hash value from the struct.
       * vars is a list of the bound variables.
       *)
      in fun vars expr ->
         let loc = loc_of_expr expr in
            match expr with
               (<:expr< $e1$ . $e2$ >>) ->
                  mk_simple_term expr_proj_op loc [mk_expr vars e1; mk_expr vars e2]
             | (<:expr< $anti: e$ >>) ->
                  mk_simple_term expr_anti_op loc [mk_expr vars e]
             | (<:expr< $e1$ $e2$ >>) ->
                  mk_simple_term expr_apply_op loc [mk_expr vars e1; mk_expr vars e2]
             | (<:expr< assert $e$ >>) ->
                  mk_simple_term expr_asr_op loc [mk_expr vars e]
             | (<:expr< $e1$ .( $e2$ ) >>) ->
                  mk_simple_term expr_array_subscript_op loc [mk_expr vars e1; mk_expr vars e2]
             | (<:expr< [| $list:el$ |] >>) ->
                  mk_simple_term expr_array_op loc [mk_olist_term (List.map (mk_expr vars) el)]
             | (<:expr< $e1$ := $e2$ >>) ->
                  mk_simple_term expr_assign_op loc [mk_expr vars e1; mk_expr vars e2]
             | (<:expr< $chr:c$ >>) ->
                  mk_loc_string expr_char_op loc c
             | (<:expr< $flo:s$ >>) ->
                  mk_loc_string expr_float_op loc s
             | (<:expr< for $s$ = $e1$ $to:b$ $e2$ do { $list:el$ } >>) ->
                  let op = if b then expr_upto_op else expr_downto_op in
                  let op_loc = mk_op_loc op loc in
                  let el' = mk_olist_term (List.map (mk_expr (s :: vars)) el) in
                      mk_dep0_dep0_dep1_any_term op_loc (mk_expr vars e1) (mk_expr vars e2) (Lm_symbol.add s) el'
             | (<:expr< fun [ $list:pwel$ ] >>) ->
                  mk_fun vars loc pwel
             | (<:expr< if $e1$ then $e2$ else $e3$ >>) ->
                  mk_simple_term expr_if_op loc [mk_expr vars e1;
                                                 mk_expr vars e2;
                                                 mk_expr vars e3]
             | (<:expr< $int:s$ >>) ->
                  mk_loc_int expr_int_op loc s
             | (<:expr< $nativeint:s$ >>) ->
                  mk_loc_int expr_native_int_op loc s
             | (<:expr< $int32:s$ >>) ->
                  mk_loc_int expr_int32_op loc s
             | (<:expr< $int64:s$ >>) ->
                  mk_loc_int expr_int64_op loc s
             | (<:expr< lazy $e$ >>) ->
                  mk_simple_term expr_laz_op loc [mk_expr vars e]
             | (<:expr< let rec $list:pel$ in $e$ >>) ->
                  mk_fix vars loc pel e
             | (<:expr< let $list:pel$ in $e$ >>) ->
                  mk_let vars loc pel e
             | (<:expr< $lid:s$ >>) ->
                  mk_var expr_lid_op vars loc s
             | (<:expr< let module $uid:s$ = $me$ in $e$ >>) ->
                  mk_simple_term expr_local_module_op loc [mk_string_term expr_local_module_op s;
                                                           mk_module_expr vars me;
                                                           mk_expr vars e]
             | (<:expr< match $e$ with [ $list:pwel$ ] >>) ->
                  mk_match vars loc pwel e
             | (<:expr< new $list: sl$ >>) ->
                  mk_simple_term expr_new_op loc [mk_olist_term (List.map (mk_string expr_new_op) sl)]
             | (<:expr:< object $opt:po$ $list:cfl$ end >>) ->
                  mk_simple_term expr_obj_op loc [mk_patt_opt _loc [] po (mk_cf_list cfl)]
             | (<:expr< {< $list:sel$ >} >>) ->
                  mk_simple_term expr_stream_op loc (List.map (mk_se vars) sel)
             | ExRec (_, Ploc.VaVal pel, eo) (* <:expr< { $list:eel$ } >> *) ->
                  mk_simple_term expr_record_op loc [mk_olist_term (List.map (mk_ident_pe vars) pel);
                                                     mk_expr_opt vars eo]
             | (<:expr< do { $list:el$ } >>) ->
                  mk_simple_term expr_seq_op loc [mk_olist_term (List.map (mk_expr vars) el)]
             | (<:expr< $e$ # $i$ >>) ->
                  mk_simple_term expr_select_op loc [mk_expr vars e; mk_string expr_string_op i]
             | (<:expr< $e1$ .[ $e2$ ] >>) ->
                  mk_simple_term expr_string_subscript_op loc [mk_expr vars e1; mk_expr vars e2]
             | (<:expr< $str:s$ >>) ->
                  mk_loc_string expr_string_op loc s
             | (<:expr< try $e$ with [ $list:pwel$ ] >>) ->
                  mk_try vars loc pwel e
             | (<:expr< ( $list:el$ ) >>) ->
                  mk_simple_term expr_tuple_op loc [mk_olist_term (List.map (mk_expr vars) el)]
             | (<:expr< ( $e$ : $t$ ) >>) ->
                  mk_simple_term expr_cast_op loc [mk_expr vars e; mk_type t]
             | (<:expr< $uid:s$ >>) ->
                  mk_var expr_uid_op vars loc s
             | (<:expr< while $e$ do { $list:el$ } >>) ->
                  mk_simple_term expr_while_op loc [mk_expr vars e; mk_olist_term (List.map (mk_expr vars) el)]
             | MLast.ExVrn (_, Ploc.VaVal s) ->
                  mk_simple_named_term expr_vrn_op loc s []
             | MLast.ExLab (loc', Ploc.VaVal poel) ->
                  mk_lab_expr vars loc poel
             | MLast.ExOlb (loc', p, Ploc.VaVal eo) ->
                  mk_simple_term expr_olb_op loc [mk_patt vars p (fun vars -> mk_expr_opt vars eo)]
             | MLast.ExCoe (l, e, ot, t) ->
                  mk_simple_term expr_coerce_class_op loc [mk_expr vars e; mk_opt mk_type ot; mk_type t]
             | (<:expr< $e$ .{ $list:el$ } >>) ->
                  mk_simple_term expr_bigarray_element_op loc [mk_expr vars e; mk_olist_term (List.map (mk_expr vars) el)]
             | MLast.ExJdf (_, Ploc.VaVal jcl, e) ->
                  mk_simple_term expr_jdf_op loc [mk_expr vars e; mk_olist_term (List.map (mk_joinclause vars) jcl)]
             | MLast.ExLop (_, me, e) ->
                  mk_simple_term expr_lop_op loc [mk_module_expr vars me; mk_expr vars e]
             | MLast.ExPar (_, e1, e2) ->
                  mk_simple_term expr_par_op loc [mk_expr vars e1; mk_expr vars e2]
             | MLast.ExPck (_, me, mto) ->
                  mk_simple_term expr_pck_op loc [mk_module_expr vars me; mk_opt mk_module_type mto]
             | MLast.ExRpl (_, Ploc.VaVal eo, Ploc.VaVal (loc2, Ploc.VaVal s)) ->
                  let ls = mk_loc_string expr_rpl_string_op (num_of_loc loc2) s in
                    mk_simple_term expr_rpl_op loc [mk_opt (mk_expr vars) eo; ls]
             | MLast.ExSpw (_, e) ->
                  mk_simple_term expr_spw_op loc [mk_expr vars e]
             | ExXtr (loc, s, eo) ->
                  mk_simple_named_term expr_xtr_op (num_of_loc loc) s [mk_opt (fun e -> (mk_expr vars (dest_vala "ExXtr" e))) eo]
             | MLast.ExArr (_, Ploc.VaAnt _)
             | MLast.ExChr (_, Ploc.VaAnt _)
             | MLast.ExFlo (_, Ploc.VaAnt _)
             | MLast.ExFor (_, _, _, _, _, Ploc.VaAnt _)
             | MLast.ExFor (_, _, _, _, Ploc.VaAnt _, _)
             | MLast.ExFor (_, Ploc.VaAnt _, _, _, _, _)
             | MLast.ExFun (_, Ploc.VaAnt _)
             | MLast.ExInt (_, Ploc.VaAnt _, _)
             | MLast.ExInt (_, Ploc.VaVal _, _)
             | MLast.ExLet (_, _, Ploc.VaAnt _, _)
             | MLast.ExLet (_, Ploc.VaAnt _, _, _)
             | MLast.ExLid (_, Ploc.VaAnt _)
             | MLast.ExLmd (_, Ploc.VaAnt _, _, _)
             | MLast.ExMat (_, _, Ploc.VaAnt _)
             | MLast.ExNew (_, Ploc.VaAnt _)
             | MLast.ExOvr (_, Ploc.VaAnt _)
             | MLast.ExRec (_, Ploc.VaAnt _, _)
             | MLast.ExSeq (_, Ploc.VaAnt _)
             | MLast.ExSnd (_, _, Ploc.VaAnt _)
             | MLast.ExStr (_, Ploc.VaAnt _)
             | MLast.ExTry (_, _, Ploc.VaAnt _)
             | MLast.ExTup (_, Ploc.VaAnt _)
             | MLast.ExUid (_, Ploc.VaAnt _)
             | MLast.ExWhi (_, _, Ploc.VaAnt _)
             | MLast.ExVrn (_, Ploc.VaAnt _)
             | MLast.ExLab (_, Ploc.VaAnt _)
             | MLast.ExObj (_, _, Ploc.VaAnt _)
             | MLast.ExObj (_, Ploc.VaAnt _, _)
             | MLast.ExOlb (_, _, Ploc.VaAnt _)
             | MLast.ExBae (_, _, Ploc.VaAnt _)
             | MLast.ExJdf (_, Ploc.VaAnt _, _)
             | MLast.ExRpl (_, Ploc.VaAnt _, _)
             | MLast.ExRpl (_, _, Ploc.VaAnt _)
             | MLast.ExRpl (_, _, Ploc.VaVal (_, Ploc.VaAnt _)) | _ ->
                  raise (RefineError ("mk_expr", StringError "antiquotations are not supported"))

   (*
    * Patterns.
    *)
   and mk_patt =
      let patt_tuple_arg_op             = mk_ocaml_op "patt_tuple_arg"
      and patt_tuple_end_op 		= mk_ocaml_op "patt_tuple_end"
      and patt_array_arg_op             = mk_ocaml_op "patt_array_arg"
      and patt_array_end_op 		= mk_ocaml_op "patt_array_end"
      and patt_as_arg_op                = mk_ocaml_op "patt_as_arg"
      and patt_as_end_op                = mk_ocaml_op "patt_as_end"
      and patt_choice_arg_op 		= mk_ocaml_op "patt_choice_arg"
      and patt_choice_end_op 		= mk_ocaml_op "patt_choice_end"
      and patt_range_arg_op 		= mk_ocaml_op "patt_range_arg"
      and patt_range_end_op 		= mk_ocaml_op "patt_range_end"
      and patt_proj_arg_op 		= mk_ocaml_op "patt_proj_arg"
      and patt_proj_end_op 		= mk_ocaml_op "patt_proj_end"
      and patt_apply_arg_op 		= mk_ocaml_op "patt_apply_arg"
      and patt_apply_end_op 		= mk_ocaml_op "patt_apply_end"
      in let patt_int_op =
         let dest_int_patt t =
            let _loc, i = dest_loc_int "dest_int_patt" t in
               <:patt< $int:i$ >>, one_subterm "dest_int_patt" t
         in add_patt "patt_int" dest_int_patt
      and patt_native_int_op =
         let dest_native_int_patt t =
            let loc, i = dest_loc_int "dest_native_int_patt" t in
               PaInt (loc, Ploc.VaVal i, "n"), one_subterm "dest_native_int_patt" t
         in add_patt "patt_native_int" dest_native_int_patt
      and patt_int32_op =
         let dest_int32_patt t =
            let loc, i = dest_loc_int "dest_int32_patt" t in
               PaInt (loc, Ploc.VaVal i, "l"), one_subterm "dest_int32_patt" t
         in add_patt "patt_int32" dest_int32_patt
      and patt_int64_op =
         let dest_int64_patt t =
            let loc, i = dest_loc_int "dest_int64_patt" t in
               PaInt (loc, Ploc.VaVal i, "L"), one_subterm "dest_int64_patt" t
         in add_patt "patt_int64" dest_int64_patt
      and patt_float_op =
         let dest_float_patt t =
            let _loc, s, t = dest_loc_string_term "dest_float_patt" t in
               <:patt< $flo:s$ >>, t
         in add_patt "patt_float" dest_float_patt
      and patt_string_op =
         let dest_string_patt t =
            let _loc, s, t = dest_loc_string_term "dest_string_patt" t in
               <:patt< $str:s$ >>, t
         in add_patt "patt_string" dest_string_patt
      and patt_char_op =
         let dest_char_patt t =
            let _loc, s, t = dest_loc_string_term "dest_char_patt" t in
               <:patt< $chr:s$ >>, t
         in add_patt "patt_char" dest_char_patt
      and patt_uid_op =
         let dest_patt_id t =
            if is_var_term t then
               dest_var t
            else
               dest_string_param t
         in
         let dest_uid_patt t =
         let _loc = dest_loc "dest_uid_patt" t in
         let p, t = two_subterms t in
            <:patt< $uid:dest_patt_id p$ >>, t
         in add_patt "patt_uid" dest_uid_patt
      and patt_ty_lid_op =
         let dest_patt_id t =
            if is_var_term t then
               dest_var t
            else
               dest_string_param t
         in
         let dest_ty_lid_patt t =
            let _loc = dest_loc "dest_uid_patt" t in
            let p, t = two_subterms t in
               <:patt< (type $lid:dest_patt_id p$) >>, t
         in add_patt "patt_ty_lid" dest_ty_lid_patt
      and patt_var_op =
         let dest_var_patt t =
            let _loc = dest_loc "dest_var_patt" t in
            let v, t = dest_dep1_term t in
               <:patt< $lid:string_of_symbol v$ >>, t
         in add_patt "patt_var" dest_var_patt
      and patt_proj_op =
         let dest_proj_patt t =
            let _loc = dest_loc "dest_proj_patt" t in
            let p1, p2, t = dest_patt_triple t in
               <:patt< $p1$ . $p2$ >>, t
         in add_patt "patt_proj" dest_proj_patt
      and patt_as_op =
         let dest_as_patt t =
            let _loc = dest_loc "dest_as_patt" t in
            let p1, p2, t = dest_patt_triple t in
               <:patt< ( $p1$ as $p2$ ) >>, t
         in add_patt "patt_as" dest_as_patt
      and patt_wildcard_op =
         let dest_wildcard_patt t =
            let _loc, t = dest_loc_term "dest_wildcard_expr" t in
               <:patt< _ >>, t
         in add_patt "patt_wildcard" dest_wildcard_patt
      and patt_apply_op =
         let dest_apply_patt t =
            let _loc = dest_loc "dest_apply_patt" t in
            let p1, p2, t = dest_patt_triple t in
               <:patt< $p1$ $p2$ >>, t
         in add_patt "patt_apply" dest_apply_patt
      and patt_choice_op =
         let dest_choice_patt t =
            let _loc = dest_loc "dest_choice_patt" t in
            let p1, p2, t = dest_patt_triple t in
               <:patt< $p1$ | $p2$ >>, t
         in add_patt "patt_choice" dest_choice_patt
      and patt_range_op =
         let dest_range_patt t =
            let _loc = dest_loc "dest_range_expr" t in
            let p1, p2, t = dest_patt_triple t in
               <:patt< $p1$ .. $p2$ >>, t
         in add_patt "patt_range" dest_range_patt
      and patt_tuple_op =
         let dest_tuple_patt t =
            let _loc = dest_loc "dest_tuple_patt" t in
            let rec dest_tuple t =
               if Opname.eq (opname_of_term t) patt_tuple_end_op then
                  [], one_subterm "dest_tuple_patt" t
               else
                  let p, t = dest_patt (one_subterm "dest_tuple_patt" t) in
                  let l, t = dest_tuple t in
                     p :: l, t
            in
            let pl, t = dest_tuple t in
               <:patt< ( $list: pl$ ) >>, t
         in add_patt "patt_tuple" dest_tuple_patt
      and patt_array_op =
         let dest_array_patt t =
            let _loc = dest_loc "dest_array_patt" t in
            let rec dest_array t =
               if Opname.eq (opname_of_term t) patt_array_end_op then
                  [], one_subterm "dest_array_patt" t
               else
                  let p, t = dest_patt (one_subterm "dest_array_patt" t) in
                  let l, t = dest_array t in
                     p :: l, t
            in
            let pl, t =
               let t' = one_subterm "dest_array_patt" t in
               if Opname.eq (opname_of_term t') patt_array_end_op then
                  [], one_subterm "dest_array_patt" t'
               else
                  dest_array t
            in
               <:patt< [| $list: pl$ |] >>, t
         in add_patt "patt_array" dest_array_patt
      and patt_cast_op =
         let dest_cast_patt t =
            let _loc = dest_loc "dest_cast_patt" t in
            let p, t = two_subterms t in
            let p, t' = dest_patt p in
               <:patt< ( $p$ : $dest_type t$ ) >>, t'
         in add_patt "patt_cast" dest_cast_patt
      and patt_vrn_op =
         let dest_vrn_patt t =
            let loc, s = dest_loc_string "dest_vrn_patt" t in
               MLast.PaVrn (loc, Ploc.VaVal s), t
         in add_patt "patt_vrn" dest_vrn_patt
      and patt_olb_op =
         let dest_olb_patt t =
            let _loc = dest_loc "dest_olb_patt" t in
            let p, oe = two_subterms t in
            let p, t = dest_patt p in
            let oe = dest_expr_opt oe in
               MLast.PaOlb (_loc, p, Ploc.VaVal oe), t
         in add_patt "patt_olb" dest_olb_patt
      and patt_unp_op =
         let dest_unp_patt t =
            let _loc, s = dest_loc_string "dest_unp_patt" t in
            let mt, t = two_subterms t in
            let mto = dest_opt dest_mt mt in
            let patt = match mto with
                          Some mt -> <:patt< (module $uid:s$ :  $mt$) >>
                        | None    -> <:patt< (module $uid:s$) >> in
               patt, t
         in add_patt "patt_unp" dest_unp_patt
(* XXX
      and patt_olb_none_op =
         let dest_olb_none_patt t =
            let _loc, s = dest_loc_string "dest_olb_none_patt" t in
               <:patt< ? $s$ >>, one_subterm "dest_olb_none_patt" t
         in add_patt "patt_olb_none" dest_olb_none_patt
*)
      and patt_lazy_op =
         let dest_lazy_patt t =
            let _loc = dest_loc "dest_lazy_patt" t in
            let p = one_subterm "patt_lazy_op" t in
            let p, t = dest_patt p in
               <:patt< lazy $p$ >>, t
         in add_patt "patt_lazy" dest_lazy_patt
      and patt_typ_op =
         let dest_typ_patt t =
            let _loc = dest_loc "dest_typ_patt" t in
            let sl, t = two_subterms t in
            let sl = dest_sl sl in
               <:patt< # $sl$ >>, t
         in add_patt "patt_typ" dest_typ_patt
       and patt_xtr_op =
         let dest_xtr_patt t =
            let loc = dest_loc "dest_xtr_patt" t in
            let s, po = two_subterms t in
            let po, t = dest_patt_opt po in
            let po = match po with Some p -> Some (Ploc.VaVal p) | None -> None in
               PaXtr (loc, dest_string s, po), t
         in add_patt "patt_xtr" dest_xtr_patt
      in fun vars patt tailf ->
         let loc = loc_of_patt patt in
            match patt with
               (<:patt< $p1$ . $p2$ >>) ->
                  mk_patt_triple vars loc patt_proj_op patt_proj_arg_op patt_proj_end_op p1 p2 tailf
             | (<:patt< ( $p1$ as $p2$ ) >>) ->
                  mk_patt_triple vars loc patt_as_op patt_as_arg_op patt_as_end_op p1 p2 tailf
             | (<:patt< _ >>) ->
                  mk_simple_term patt_wildcard_op loc [tailf vars]
             | (<:patt< $p1$ $p2$ >>) ->
                  mk_patt_triple vars loc patt_apply_op patt_apply_arg_op patt_apply_end_op p1 p2 tailf
             | (<:patt< [| $list:pl$ |] >>) ->
                  mk_patt_list vars loc patt_array_op patt_array_arg_op patt_array_end_op pl tailf
             | (<:patt< $chr:c$ >>) ->
                  mk_loc_string_term patt_char_op loc c (tailf vars)
             | (<:patt< $int:s$ >>) ->
                  mk_loc_int_term patt_int_op loc s (tailf vars)
             | (<:patt< $nativeint:s$ >>) ->
                  mk_loc_int_term patt_native_int_op loc s (tailf vars)
             | (<:patt< $int32:s$ >>) ->
                  mk_loc_int_term patt_int32_op loc s (tailf vars)
             | (<:patt< $int64:s$ >>) ->
                  mk_loc_int_term patt_int64_op loc s (tailf vars)
             | (<:patt< $flo:s$ >>) ->
                  mk_loc_string_term patt_float_op loc s (tailf vars)
             | (<:patt< $lid:v$ >>) ->
                  (* This is a binding occurrence *)
                  mk_patt_var patt_var_op loc (Lm_symbol.add v) (tailf (v :: vars))
             | (<:patt< $p1$ | $p2$ >>) ->
                  mk_patt_triple vars loc patt_choice_op patt_choice_arg_op patt_choice_end_op p1 p2 tailf
             | (<:patt< $p1$ .. $p2$ >>) ->
                  mk_patt_triple vars loc patt_range_op patt_range_arg_op patt_range_end_op p1 p2 tailf
             | (<:patt< { $list:ppl$ } >>) ->
                  mk_patt_record vars loc ppl tailf
             | (<:patt< $str:s$ >>) ->
                  mk_loc_string_term patt_string_op loc s (tailf vars)
             | (<:patt< ( $list:pl$ ) >>) ->
                  mk_patt_list vars loc patt_tuple_op patt_tuple_arg_op patt_tuple_end_op pl tailf
             | (<:patt< ( $p$ : $t'$ ) >>) ->
                  mk_simple_term patt_cast_op loc [mk_patt vars p tailf; mk_type t']
             | (<:patt< $uid:s$ >>) ->
                  mk_var_term patt_uid_op vars loc s (tailf vars)
             | (<:patt< (type $lid:s$) >>) ->
                  mk_var_term patt_ty_lid_op vars loc s (tailf vars)
             | (<:patt< $anti: p$ >>) ->
                  Stdpp.raise_with_loc (MLast.loc_of_patt patt) (Failure "Filter_ocaml.mk_patt: encountered PaAnt")
             | (<:patt< `$s$ >>) ->
                  mk_simple_named_term patt_vrn_op loc s [tailf vars]
             | PaLab (_loc, Ploc.VaVal ppol) ->
                  mk_patt_lab vars loc ppol tailf
             | (<:patt< ?{ $p$ $opt:oe$ } >>) ->
                  mk_simple_term patt_olb_op loc [mk_patt vars p tailf; mk_expr_opt vars oe]
             | (<:patt< # $sl$ >>) ->
                  mk_simple_term patt_typ_op loc [mk_string_list sl]
             | (<:patt< lazy $p$ >>) ->
                  mk_simple_term patt_lazy_op loc [mk_patt vars p tailf]
             | (<:patt< (module $uid:s$) >>) ->
                  mk_simple_named_term patt_unp_op loc s [mk_opt mk_module_type None; tailf vars]
             | (<:patt< (module $uid:s$ :  $mt$) >>) ->
                  mk_simple_named_term patt_unp_op loc s [mk_opt mk_module_type (Some mt); tailf vars]
             | PaXtr (_, s, po) ->
                  mk_simple_named_term patt_xtr_op loc s [mk_patt_opt_vala loc vars po tailf]
             | PaArr (_, Ploc.VaAnt _)
             | PaChr (_, Ploc.VaAnt _)
             | PaInt _
             | PaFlo (_, Ploc.VaAnt _)
             | PaLid (_, Ploc.VaAnt _)
             | PaRec (_, Ploc.VaAnt _)
             | PaStr (_, Ploc.VaAnt _)
             | PaTup (_, Ploc.VaAnt _)
             | PaUid (_, Ploc.VaAnt _)
             | PaVrn (_, Ploc.VaAnt _)
             | PaOlb (_, _, Ploc.VaAnt _)
             | PaTyp (_, Ploc.VaAnt _)
             | PaLab (_, Ploc.VaAnt _)
             | PaNty (_, Ploc.VaAnt _)
             | PaUnp (_, Ploc.VaAnt _, _) | _ ->
                  raise (RefineError ("mk_patt", StringError "antiquotations are not supported"))

   and mk_patt_opt_loc loc vars patt tailf =
      match patt with
        Some patt ->
           mk_simple_term pattern_op loc [mk_patt vars patt tailf]
       | None ->
           mk_simple_term no_pattern_op loc [tailf vars]

   and mk_patt_opt loc vars patt tailf =
      mk_patt_opt_loc (num_of_loc loc) vars patt tailf

   and mk_patt_opt_vala loc vars patt tailf =
      match patt with
        Some (Ploc.VaVal patt) ->
           mk_simple_term pattern_op loc [mk_patt vars patt tailf]
       | None ->
           mk_simple_term no_pattern_op loc [tailf vars]
       | Some (Ploc.VaAnt _) ->
           raise (RefineError ("mk_patt_opt_vala", StringError "antiquotations are not supported"))

   and mk_patt_triple vars loc op1 op2 op3 p1 p2 tailf =
      let tailf vars = mk_simple_term op3 loc [tailf vars] in
      let tailf vars = mk_simple_term op2 loc [mk_patt vars p2 tailf] in
         mk_simple_term op1 loc [mk_patt vars p1 tailf]

   and mk_patt_record =
      let patt_record_proj_op = mk_ocaml_op "patt_record_proj"
      and patt_record_end_op = mk_ocaml_op "patt_record_end"
      in let patt_record_op =
         let dest_record_patt t =
            let _loc = dest_loc "dest_record_patt" t in
            let rec dest_record t =
               if Opname.eq (opname_of_term t) patt_record_end_op then
                  [], one_subterm "dest_record_patt" t
               else
                  let n, p = two_subterms t in
                  let p, t = dest_patt p in
                  let l, t = dest_record t in
                     (patt_of_expr_ident (dest_expr n), p) :: l, t
            in
            let ppl, t = dest_record (one_subterm "dest_record_patt" t) in
               <:patt< { $list: ppl$ } >>, t
         in add_patt "patt_record" dest_record_patt
      in fun vars loc ppl tailf ->
         let tailf vars = mk_simple_term patt_record_end_op loc [tailf vars] in
         let rec make ppl vars =
            match ppl with
               (p1, p2)::ppl ->
                  mk_simple_term patt_record_proj_op loc [mk_expr vars (expr_of_patt_ident p1);
                                                          mk_patt vars p2 (make ppl)]
             | [] ->
                  tailf vars
         in
            mk_simple_term patt_record_op loc [make ppl vars]

   and mk_patt_lab =
      let patt_lab_proj_op = mk_ocaml_op "patt_lab_proj"
      and patt_lab_end_op = mk_ocaml_op "patt_lab_end"
      in let patt_lab_op =
         let dest_lab_patt t =
            let _loc = dest_loc "dest_lab_patt" t in
            let rec dest_lab t =
               if Opname.eq (opname_of_term t) patt_lab_end_op then
                  [], one_subterm "dest_lab_patt" t
               else
                  let n, p = two_subterms t in
                  let p, t = dest_patt p in
                  let l, t = dest_lab t in
                     (patt_of_expr_ident (dest_expr n), p) :: l, t
            in
            let ppl, t = dest_lab (one_subterm "dest_lab_patt" t) in
               <:patt< { $list: ppl$ } >>, t
         in add_patt "patt_lab" dest_lab_patt
      in fun vars loc ppol tailf ->
         let tailf vars = mk_simple_term patt_lab_end_op loc [tailf vars] in
         let rec make ppl vars =
            match ppol with
               (p, Ploc.VaVal po)::ppol ->
                  mk_simple_term patt_lab_proj_op loc [mk_expr vars (expr_of_patt_ident p);
                                                       mk_patt_opt_loc loc vars po (make ppol)]
             | (_, Ploc.VaAnt _) :: _ ->
                  raise (RefineError ("mk_patt_lab", StringError "antiquotations are not supported"))
             | [] ->
                  tailf vars
         in
            mk_simple_term patt_lab_op loc [make ppol vars]

   and mk_patt_list vars loc op1 op2 op3 pl tailf =
      let tailf vars =
         mk_simple_term op3 loc [tailf vars]
      in
      let rec make pl vars =
         match pl with
            p::pl ->
               let tailf' vars =
                  if pl = [] then
                     tailf vars
                  else
                     mk_simple_term op2 loc [make pl vars]
               in
                  mk_patt vars p tailf'
          | [] ->
               tailf vars
      in
         mk_simple_term op1 loc [make pl vars]

   (*
    * Types.
    *)
   and mk_type =
      let type_lid_op =
         let dest_lid_type t =
            let _loc = dest_loc "dest_lid_type" t in
               <:ctyp< $lid:dest_var t$ >>
         in add_type "type_lid" dest_lid_type
      and type_uid_op =
         let dest_uid_type t =
            let _loc = dest_loc "dest_uid_type" t in
               <:ctyp< $uid:dest_var t$ >>
         in add_type "type_uid" dest_uid_type
      and type_proj_op =
         let dest_proj_type t =
            let _loc = dest_loc "dest_proj_type" t in
            let t1, t2 = two_subterms t in
               <:ctyp< $dest_type t1$ . $dest_type t2$ >>
         in add_type "type_proj" dest_proj_type
      and type_as_op =
         let dest_as_type t =
            let _loc = dest_loc "dest_as_type" t in
            let t1, t2 = two_subterms t in
               <:ctyp< $dest_type t1$ as $dest_type t2$ >>
         in add_type "type_as" dest_as_type
      and type_wildcard_op =
         let dest_wildcard_type t =
            let _loc = dest_loc "dest_wildcard_type" t in
               <:ctyp< _ >>
         in add_type "type_wildcard" dest_wildcard_type
      and type_apply_op =
         let dest_apply_type t =
            let _loc = dest_loc "dest_apply_type" t in
            let t1, t2 = two_subterms t in
               <:ctyp< $dest_type t1$ $dest_type t2$ >>
         in add_type "type_apply" dest_apply_type
      and type_fun_op =
         let dest_fun_type t =
            let _loc = dest_loc "dest_fun_type" t in
            let t1, t2 = two_subterms t in
               <:ctyp< $dest_type t1$ -> $dest_type t2$ >>
         in add_type "type_fun" dest_fun_type
      and type_class_id_op =
         let dest_class_id_type t =
            let _loc = dest_loc "dest_class_id_type" t in
               <:ctyp< # $list:List.map dest_string (dest_olist (one_subterm "dest_class_id_type" t))$ >>
         in add_type "type_class_id" dest_class_id_type
      and type_param_op =
         let dest_param_type t =
            let _loc, s = dest_loc_string "dest_param_type" t in
               <:ctyp< '$s$ >>
         in add_type "type_param" dest_param_type
      and type_equal_op =
         let dest_equal_type t =
            let _loc = dest_loc "dest_equal_type" t in
            let t1, t2 = two_subterms t in
               <:ctyp< $dest_type t1$ == $dest_type t2$ >>
         in add_type "type_equal" dest_equal_type
      and type_object_tt_op =
         let dest_object_tt_type t =
            let _loc = dest_loc "dest_object_tt_type" t in
            let stl = dest_olist (one_subterm "dest_object_tt_type" t) in
               <:ctyp< < $list: List.map dest_st stl$ .. > >>
         in add_type "type_object_tt" dest_object_tt_type
      and type_object_ff_op =
         let dest_object_ff_type t =
            let _loc = dest_loc "dest_object_ff_type" t in
            let stl = dest_olist (one_subterm "dest_object_ff_type" t) in
               <:ctyp< < $list: List.map dest_st stl$ > >>
         in add_type "type_object_ff" dest_object_ff_type
      and type_record_op (* XXX , type_pvt_record_op *) =
         let dest_sbt t =
            let l, s = dest_loc_string "dest_sbt" t in
            let b, t = two_subterms t in
               l, s, dest_bool b, dest_type t
         in let dest_record_type t =
            let _loc = dest_loc "dest_record_type" t in
            let sbtl = dest_olist (one_subterm "dest_record_type" t) in
               <:ctyp< { $list: List.map dest_sbt sbtl$ } >>
(* XXX
         in let dest_pvt_record_type t =
            let _loc = dest_loc "dest_pvt_record_type" t in
            let sbtl = dest_olist (one_subterm "dest_pvt_record_type" t) in
               <:ctyp< private { $list: List.map dest_sbt sbtl$ } >>
*)
         in add_type "type_record" dest_record_type (* XXX , add_type "type_pvt_record" dest_pvt_record_type *)
      and type_list_op (* XXX , type_pvt_list_op *) =
         let dest_stl t =
            let l, s, t = dest_loc_string_term "dest_stl" t in
               l, s, List.map dest_type (dest_olist t)
         in let dest_list_type t =
            let _loc = dest_loc "dest_list_type" t in
            let stll = dest_olist (one_subterm "dest_list_type" t) in
            let stll = List.map (fun t ->
               let l, s, tl = dest_stl t in
               l, Ploc.VaVal s, Ploc.VaVal tl, None) stll
            in
               <:ctyp< [ $list: stll$ ] >>
(* XXX
         in let dest_pvt_list_type t =
            let _loc = dest_loc "dest_pvt_list_type" t in
            let stll = dest_olist (one_subterm "dest_pvt_list_type" t) in
               <:ctyp< private [ $list: List.map dest_stl stll$ ] >>
*)
         in add_type "type_list" dest_list_type (* XXX , add_type "type_pvt_list" dest_pvt_list_type *)
      and type_prod_op =
         let dest_prod_type t =
            let _loc = dest_loc "dest_prod_type" t in
            let tl = dest_olist (one_subterm "dest_prod_type" t) in
               <:ctyp< ( $list: List.map dest_type tl$ ) >>
         in add_type "type_prod" dest_prod_type
      and type_pol_op =
         let dest_pol_type t =
            let _loc = dest_loc "type_pol_op" t in
            let strs, ct = two_subterms t in
            let ls = List.map dest_string (dest_olist strs) in
            let t = dest_type ct in
               <:ctyp< ! $list:ls$ . $t$ >>
         in add_type "type_pol" dest_pol_type
      and type_vrn_op =
         let dest_sbtl t =
            let s, b, tl = three_subterms t in
            let s = dest_string s in
            let b = dest_bool b in
            let tl = List.map dest_type (dest_olist tl) in
               s, b, tl
         in let dest_rf loc t =
            let op = opname_of_term t in
               if Opname.eq op row_field_tag_op then
                  let s, b, tl = dest_sbtl t in
                     PvTag (loc, Ploc.VaVal s, Ploc.VaVal b, Ploc.VaVal tl)
               else if Opname.eq op row_field_inh_op then
                  PvInh (loc, dest_type (one_subterm "dest_rf" t))
               else
                  raise (Invalid_argument "dest_rf")
         in let dest_vrn_type t =
            let loc = dest_loc "dest_vrn_type" t in
            let sbtll, bsloo = two_subterms t in
            let sbtll = dest_olist sbtll in
            let sbtll = List.map (dest_rf loc) sbtll in
            let bsloo = dest_opt (dest_opt (fun t -> Ploc.VaVal (dest_sl t))) bsloo in
               MLast.TyVrn (loc, Ploc.VaVal sbtll, bsloo)
         in add_type "type_vrn" dest_vrn_type
      and type_olb_op =
         let dest_olb_type t =
            let loc, s = dest_loc_string "dest_olb_type" t in
            let t = one_subterm "dest_olb_type" t in
               MLast.TyOlb (loc, Ploc.VaVal s, dest_type t)
         in add_type "type_olb" dest_olb_type
      and type_lab_op =
         let dest_lab_type t =
            let loc, s = dest_loc_string "dest_lab_type" t in
            let t = one_subterm "dest_lab_type" t in
               MLast.TyLab (loc, Ploc.VaVal s, dest_type t)
         in add_type "type_lab" dest_lab_type
      in fun t ->
         let loc = loc_of_ctyp t in
            match t with
               (<:ctyp< $t1$ . $t2$ >>) ->
                  mk_simple_term type_proj_op loc [mk_type t1; mk_type t2]
             | (<:ctyp< $t1$ as $t2$ >>) ->
                  mk_simple_term type_as_op loc [mk_type t1; mk_type t2]
             | (<:ctyp< _ >>) ->
                  mk_simple_term type_wildcard_op loc []
             | (<:ctyp< $t1$ $t2$ >>) ->
                  mk_simple_term type_apply_op loc [mk_type t1; mk_type t2]
             | (<:ctyp< $t1$ -> $t2$ >>) ->
                  mk_simple_term type_fun_op loc [mk_type t1; mk_type t2]
             | (<:ctyp< # $list:i$ >>) ->
                  mk_simple_term type_class_id_op loc [mk_olist_term (List.map (mk_string type_class_id_op) i)]
             | (<:ctyp< $lid:s$ >>) ->
                  mk_var type_lid_op [] loc s
             | (<:ctyp< '$s$ >>) ->
                  mk_loc_string type_param_op loc s
             | (<:ctyp< $t1$ == $t2$ >>) ->
                  mk_simple_term type_equal_op loc [mk_type t1; mk_type t2]
             | (<:ctyp< < $list:stl$ $opt:b$ > >>) ->
                  let op = if b then type_object_tt_op else type_object_ff_op in
                     mk_simple_term op loc (List.map mk_st stl)
(* XXX
             | (<:ctyp< private { $list:sbtl$ } >>) ->
                  mk_simple_term type_pvt_record_op loc [mk_olist_term (List.map mk_sbt sbtl)]
 *)
             | (<:ctyp< { $list:sbtl$ } >>) ->
                  mk_simple_term type_record_op loc [mk_olist_term (List.map mk_sbt sbtl)]
(* XXX
             | (<:ctyp< private [ $list:stll$ ] >>) ->
                  mk_simple_term type_pvt_list_op loc [mk_olist_term (List.map mk_stl stll)]
 *)
             | (<:ctyp< [ $list:stll$ ] >>) ->
                  mk_simple_term type_list_op loc [mk_olist_term (List.map mk_stl stll)]
             | (<:ctyp< ( $list:tl$ ) >>) ->
                  mk_simple_term type_prod_op loc [mk_olist_term (List.map mk_type tl)]
             | (<:ctyp< $uid:s$ >>) ->
                  mk_var type_uid_op [] loc s
             | MLast.TyVrn (_, Ploc.VaVal sbtll, sloo) ->
                  mk_simple_term type_vrn_op loc [mk_olist_term (List.map mk_rf sbtll);
                                                  mk_opt (mk_opt (mk_vala "TyVrn" mk_string_list)) sloo]
             | MLast.TyOlb (_, Ploc.VaVal s, t) ->
                  mk_simple_named_term type_olb_op loc s [mk_type t]
             | MLast.TyLab (_, Ploc.VaVal s, t) ->
                  mk_simple_named_term type_lab_op loc s [mk_type t]
             | MLast.TyPol (_, Ploc.VaVal strs, t) when true ->
                  mk_simple_term type_pol_op loc [mk_olist_term (List.map (mk_string type_class_id_op) strs); mk_type t]
             | t ->
                  (*
                   * XXX: TODO: Once OCaml 3.08 compatibility is no longer required:
                   *   - Fill in the appropriate TyPrv entry here
                   *   - Remove the "when true" from the above case (it's there to shut up the "unused case"
                   *     warning when compiling under 3.08
                   *)
                  Stdpp.raise_with_loc (MLast. loc_of_ctyp t) (Invalid_argument "You are using a feature that was added in OCaml 3.09.
MetaPRL does not support this yet in order to remain compatible with OCaml 3.08")

   (*
    * Signatures.
    *)
   and mk_sig_item =
      let sig_class_sig_op =
         let dest_class_sig_sig t =
            let loc = dest_loc "dest_class_sig_sig" t in
            let ctl = dest_olist (one_subterm "dest_class_sig_sig" t) in
         (*
               <:sig_item< class $list: List.map dest_class_type ctl$ >>
         *)
               SgCls (loc, Ploc.VaVal (List.map dest_class_type_infos ctl))
         in add_sig "sig_class_sig" dest_class_sig_sig
      and sig_class_type_op =
         let dest_class_sig_type t =
            let loc = dest_loc "dest_class_sig_type" t in
            let ctl = dest_olist (one_subterm "dest_class_sig_type" t) in
         (*
               <:sig_item< class $list: List.map dest_class_type ctl$ >>
         *)
               SgClt (loc, Ploc.VaVal (List.map dest_class_type_infos ctl))
         in add_sig "sig_class_type" dest_class_sig_type
      and sig_subsig_op =
         let dest_subsig_sig t =
            let _loc = dest_loc "dest_subsig_sig" t in
            let sl = dest_olist (one_subterm "dest_subsig_sig" t) in
               <:sig_item< declare $list: List.map dest_sig sl$ end >>
         in add_sig "sig_subsig" dest_subsig_sig
      and sig_exception_op =
         let dest_exception_sig t =
            let _loc, s = dest_loc_string "dest_exception_sig" t in
            let tl = dest_olist (one_subterm "dest_exception_sig" t) in
               <:sig_item< exception $s$ of $list: List.map dest_type tl$ >>
         in add_sig "sig_exception" dest_exception_sig
      and sig_external_op =
         let dest_external_sig t =
            let _loc, s = dest_loc_string "dest_external_sig" t in
               match dest_olist (one_subterm "dest_external_sig" t) with
                  t :: sl ->
                     <:sig_item< external $s$ : $dest_type t$ = $list: List.map dest_string sl$ >>
                | _ ->
                    raise_format_error "external requires a name and a type" t
         in add_sig "sig_external" dest_external_sig
      and sig_inc_op =
         let dest_inc_sig t =
            let loc = dest_loc "dest_inc_sig" t in
            let mt = one_subterm "dest_inc_sig" t in
               SgInc (loc, dest_mt mt)
         in add_sig "sig_inc" dest_inc_sig
      and sig_module_op =
         let dest_module_sig t =
            let _loc, s = dest_loc_string "dest_module_sig" t in
            let mt = one_subterm "dest_module_sig" t in
               <:sig_item< module $s$ : $dest_mt mt$ >>
         in add_sig "sig_module" dest_module_sig
      and sig_module_type_op =
         let dest_module_type_sig t =
            let _loc, s = dest_loc_string "dest_module_type_sig" t in
            let mt = one_subterm "dest_module_type_sig" t in
               <:sig_item< module type $s$ = $dest_mt mt$ >>
         in add_sig "sig_module_type" dest_module_type_sig
      and sig_open_op =
         let dest_open_sig t =
            let _loc = dest_loc "dest_open_sig" t in
            let sl = dest_olist (one_subterm "dest_open_sig" t) in
               <:sig_item< open $List.map dest_string sl$ >>
         in add_sig "sig_open" dest_open_sig
      and sig_type_op =
         let dest_type_sig t =
            let _loc = dest_loc "dest_type_sig" t in
            let tdl = dest_olist (one_subterm "dest_type_sig" t) in
               <:sig_item< type $list: List.map dest_tdl tdl$ >>
         in add_sig "sig_type" dest_type_sig
      and sig_value_op =
         let dest_value_sig t =
            let _loc, s = dest_loc_string "dest_value_sig" t in
            let t = one_subterm "dest_value_sig" t in
               <:sig_item< value $s$ : $dest_type t$ >>
         in add_sig "sig_value" dest_value_sig
      and sig_dir_op =
         let dest_dir_sig t =
            let loc, s = dest_loc_string "dest_dir_sig" t in
            let eo = one_subterm "dest_dir_sig" t in
            let eo = dest_expr_opt eo in
               MLast.SgDir (loc, Ploc.VaVal s, Ploc.VaVal eo)
         in add_sig "sig_dir" dest_dir_sig
      and sig_mod_op =
         let dest_mod_sig t =
            let _loc = dest_loc "dest_recmod_sig" t in
            let b, smtl = two_subterms t in
            let smtl = dest_olist smtl in
               <:sig_item< module $flag:dest_bool b$ $list:List.map dest_smt smtl$ >>
               (* SgMod (loc, Ploc.VaVal (dest_bool b), Ploc.VaVal (List.map dest_smt smtl)) *)
         in add_sig "sig_mod" dest_mod_sig
      and sig_use_op =
         let dest_use_sig t =
            let loc, s = dest_loc_string "dest_use_sig" t in
            let sigll = dest_olist (one_subterm "dest_use_sig" t) in
               SgUse (loc, Ploc.VaVal s, Ploc.VaVal (List.map dest_sigloc sigll))
         in add_sig "sig_use" dest_use_sig
       and sig_xtr_op =
         let dest_xtr_sig t =
            let loc = dest_loc "dest_xtr_sig" t in
            let s, sgo = two_subterms t in
               SgXtr (loc, dest_string s, dest_opt (fun sg -> Ploc.VaVal (dest_sig sg)) sgo)
         in add_sig "sig_xtr" dest_xtr_sig
   in fun si ->
         let loc = loc_of_sig_item si in
            match si with
               SgCls (_, Ploc.VaVal ctl) ->
                  mk_simple_term sig_class_sig_op loc (List.map mk_class_type_infos ctl)
             | SgClt (_, Ploc.VaVal ctl) ->
                  mk_simple_term sig_class_type_op loc (List.map mk_class_type_infos ctl)
             | (<:sig_item< declare $list:sil$ end >>) ->
                  mk_simple_term sig_subsig_op loc (List.map mk_sig_item sil)
             | (<:sig_item< exception $s$ of $list:tl$ >>) ->
                  mk_simple_named_term sig_exception_op loc s [mk_olist_term (List.map mk_type tl)]
             | (<:sig_item< external $s$ : $t$ = $list:sl$ >>) ->
                  mk_simple_named_term sig_external_op loc s
                     (mk_type t :: List.map mk_simple_string sl)
             | SgInc (_, mt) ->
                  mk_simple_term sig_inc_op loc [mk_module_type mt]
             | (<:sig_item< module $s$ : $mt$ >>) ->
                  mk_simple_named_term sig_module_op loc s [mk_module_type mt]
             | (<:sig_item< module type $s$ = $mt$ >>) ->
                  mk_simple_named_term sig_module_type_op loc s [mk_module_type mt]
             | (<:sig_item< open $sl$ >>) ->
                  mk_simple_term sig_open_op loc [mk_olist_term (List.map mk_simple_string sl)]
             | (<:sig_item< type $list:tdl$ >>) ->
                  mk_simple_term sig_type_op loc [mk_olist_term (List.map mk_tdl tdl)]
             | (<:sig_item< value $s$ : $t$ >>) ->
                  mk_simple_named_term sig_value_op loc s [mk_type t]
             | SgDir (_, Ploc.VaVal s, Ploc.VaVal eo) ->
                  mk_simple_named_term sig_dir_op loc s [mk_expr_opt [] eo]
             | (<:sig_item< module $flag:b$ $list:smtl$ >>) ->
                  mk_simple_term sig_mod_op loc [mk_bool b; mk_olist_term (List.map mk_smt smtl)]
             | SgUse (_, Ploc.VaVal s, Ploc.VaVal sigll) ->
                  mk_simple_named_term sig_use_op loc s [mk_olist_term (List.map mk_sigloc sigll)]
             | SgXtr (loc, s, sgo) ->
                  mk_simple_named_term sig_xtr_op (num_of_loc loc) s [mk_opt (fun sg -> (mk_sig_item (dest_vala "SgXtr" sg))) sgo]
             | SgCls (_, Ploc.VaAnt _)
             | SgClt (_, Ploc.VaAnt _)
             | SgDcl (_, Ploc.VaAnt _)
             | SgExc (_, _, Ploc.VaAnt _)
             | SgExc (_, Ploc.VaAnt _, _)
             | SgExt (_, _, _, Ploc.VaAnt _)
             | SgExt (_, Ploc.VaAnt _, _, _)
             | SgOpn (_, Ploc.VaAnt _)
             | SgTyp (_, Ploc.VaAnt _)
             | SgMty (_, Ploc.VaAnt _, _)
             | SgMod (_, _, Ploc.VaAnt _)
             | SgMod (_, Ploc.VaAnt _, _)
             | SgVal (_, Ploc.VaAnt _, _)
             | SgDir (_, _, Ploc.VaAnt _)
             | SgDir (_, Ploc.VaAnt _, _)
             | SgUse (_, _, Ploc.VaAnt _)
             | SgUse (_, Ploc.VaAnt _, _) ->
                  raise (RefineError ("mk_st", StringError "antiquotations are not supported"))

   (*
    * Structure items.
    *)
   and mk_str_item =
      let str_class_str_op =
         let dest_class_expr_infos t =
            let loc = dest_loc "dest_class_expr_infos" t in
            let s, sl, b, t = four_subterms t in
                { ciLoc = loc;
                  ciNam = Ploc.VaVal (dest_string s);
                  ciPrm = loc, Ploc.VaVal (List.map dest_sbb (dest_olist sl));
                  ciVir = Ploc.VaVal (dest_bool b);
                  ciExp = dest_ce t
                }
         in let dest_class_str_str t =
            let loc = dest_loc "dest_class_str_str" t in
            let cdl = dest_olist (one_subterm "dest_class_str_str" t) in
         (*
               <:str_item< class $list: List.map dest_class cdl$ >>
         *)
               StCls (loc, Ploc.VaVal (List.map dest_class_expr_infos cdl))
         in add_str "str_class_str" dest_class_str_str
      and str_class_type_op =
         let dest_class_str_type t =
            let loc = dest_loc "dest_class_str_type" t in
            let cdl = dest_olist (one_subterm "dest_class_str_type" t) in
         (*
               <:str_item< class $list: List.map dest_class cdl$ >>
         *)
               StClt (loc, Ploc.VaVal (List.map dest_class_type_infos cdl))
         in add_str "str_class_type" dest_class_str_type
      and str_substruct_op =
         let dest_substruct_str t =
            let _loc = dest_loc "dest_substruct_str" t in
            let stl = subterms_of_term t in
               <:str_item< declare $list: List.map dest_str stl$ end >>
         in add_str "str_substruct" dest_substruct_str
      and str_exception_op =
         let dest_exception_str t =
            let _loc, s = dest_loc_string "dest_exception_str" t in
            let tl = dest_olist (one_subterm "dest_exception_str" t) in
               <:str_item< exception $s$ of $list: List.map dest_type tl$ >>
         in add_str "str_exception" dest_exception_str
      and str_expr_op =
         let dest_expr_str t =
            let _loc = dest_loc "dest_expr_str" t in
               <:str_item< $exp: dest_expr (one_subterm "dest_expr_str" t)$ >>
         in add_str "str_expr" dest_expr_str
      and str_external_op =
         let dest_external_str t =
            let _loc, s = dest_loc_string "dest_external_str" t in
               match dest_olist (one_subterm "dest_external_str" t) with
                  t :: sl ->
                     <:str_item< external $s$ : $dest_type t$ = $list: List.map dest_string sl$ >>
                | _ ->
                     raise_format_error "external requires a name and type" t
         in add_str "str_external" dest_external_str
      and str_module_op =
         let dest_module_str t =
            let _loc, s = dest_loc_string "dest_module_str" t in
            let me = one_subterm "dest_module_str" t in
               <:str_item< module $s$ = $dest_me me$ >>
         in add_str "str_module" dest_module_str
      and str_module_type_op =
         let dest_module_type_str t =
            let _loc, s = dest_loc_string "dest_module_type_str" t in
            let mt = one_subterm "dest_module_type_str" t in
               <:str_item< module type $s$ = $dest_mt mt$ >>
         in add_str "str_module_type" dest_module_type_str
      and str_open_op =
         let dest_open_str t =
            let _loc = dest_loc "dest_open_str" t in
            let sl = dest_olist (one_subterm "dest_open_str" t) in
               <:str_item< open $List.map dest_string sl$ >>
         in add_str "str_open" dest_open_str
      and str_inc_op =
         let dest_inc_str t =
            let loc = dest_loc "dest_inc_str" t in
            let me = one_subterm "dest_inc_str" t in
            let me = dest_me me in
               MLast.StInc (loc, me)
         in add_str "str_inc" dest_inc_str
      and str_type_op =
         let dest_type_str t =
            let _loc = dest_loc "dest_type_str" t in
            let tdl = dest_olist (one_subterm "dest_type_str" t) in
               <:str_item< type $list: List.map dest_tdl tdl$ >>
         in add_str "str_type" dest_type_str
      and str_dir_op =
         let dest_dir_str t =
            let loc, s = dest_loc_string "dest_dir_str" t in
            let eo = one_subterm "dest_dir_str" t in
            let eo = dest_expr_opt eo in
               MLast.StDir (loc, Ploc.VaVal s, Ploc.VaVal eo)
         in add_str "str_dir" dest_dir_str
      and str_exc_op =
         let dest_exc_str t =
            let loc, s = dest_loc_string "dest_exc_str" t in
            let tl, sl = two_subterms t in
            let tl = List.map dest_type (dest_olist tl) in
            let sl = List.map dest_string (dest_olist sl) in
               MLast.StExc (loc, Ploc.VaVal s, Ploc.VaVal tl, Ploc.VaVal sl)
         in add_str "str_exc" dest_exc_str
      and str_mod_op =
         let dest_mod_str t =
            let loc = dest_loc "dest_recmod_str" t in
            let b, smel = two_subterms t in
            let smel = dest_olist smel in
               StMod (loc, Ploc.VaVal (dest_bool b), Ploc.VaVal (List.map dest_sme smel))
         in add_str "str_mod" dest_mod_str
      and str_use_op =
         let dest_use_str t =
            let loc, s = dest_loc_string "dest_use_str" t in
            let strll = dest_olist (one_subterm "dest_use_str" t) in
               StUse (loc, Ploc.VaVal s, Ploc.VaVal (List.map dest_strloc strll))
         in add_str "str_use" dest_use_str
       and str_xtr_op =
         let dest_xtr_st t =
            let loc = dest_loc "dest_xtr_st" t in
            let s, sto = two_subterms t in
               StXtr (loc, dest_string s, dest_opt (fun st -> Ploc.VaVal (dest_str st)) sto)
         in add_str "st_xtr" dest_xtr_st
      and str_def_op =
         (* TODO[jyh]: fix this.  I don't understand what a join clause is *)
         let dest_def_str t =
            let loc = dest_loc "dest_def_str" t in
            let jcll = one_subterm "dest_def_str" t in
            let jcll =
               List.map (fun t ->
                  let jcll = one_subterm "dest_def_str" t in
                  let jcll = dest_olist jcll in
                  let jcll =
                     List.map (fun t ->
                        let loc1 = dest_loc "dest_def_str_1" t in
                        let jcl, e = two_subterms t in
                        let e = dest_expr e in
                        let jcl =
                           List.map (fun t ->
                              let loc2, s = dest_loc_string "dest_def_str_2" t in
                              let po = one_subterm "dest_def_str_3" t in
                              let po, _ = dest_patt_opt po in
                                 loc2, (loc2, Ploc.VaVal s), Ploc.VaVal po) (dest_olist jcl)
                        in
                          (loc1, Ploc.VaVal jcl, e)) jcll
                  in
                     { jcLoc = loc; jcVal = Ploc.VaVal jcll }) (dest_olist jcll)
             in
               StDef (loc, Ploc.VaVal jcll)
         in add_str "str_def" dest_def_str
      in fun vars si ->
         let loc = loc_of_str_item si in
            match si with
               (<:str_item< class $list:cdl$ >>) ->
                  mk_simple_term str_class_str_op loc (List.map (mk_class_expr_infos vars) cdl)
             | (<:str_item< class type $list:cdl$ >>) ->
                  mk_simple_term str_class_type_op loc (List.map mk_class_type_infos cdl)
             | (<:str_item< declare $list:stl$ end >>) ->
                  mk_simple_term str_substruct_op loc (List.map (mk_str_item vars) stl)
             | (<:str_item< exception $s$ of $list:tl$ >>) ->
                  mk_simple_named_term str_exception_op loc s [mk_olist_term (List.map mk_type tl)]
             | (<:str_item< $exp:e$ >>) ->
                  mk_simple_term str_expr_op loc [mk_expr [] e]
             | (<:str_item< external $s$ : $t$ = $list:sl$ >>) ->
                  mk_simple_named_term str_external_op loc s
                     (mk_type t :: List.map mk_simple_string sl)
             | (<:str_item< module $s$ = $me$ >>) ->
                  mk_simple_named_term str_module_op loc s [mk_module_expr vars me]
             | (<:str_item< module type $s$ = $mt$ >>) ->
                  mk_simple_named_term str_module_type_op loc s [mk_module_type mt]
             | (<:str_item< open $sl$ >>) ->
                  mk_simple_term str_open_op loc [mk_olist_term (List.map mk_simple_string sl)]
             | (<:str_item< type $list:tdl$ >>) ->
                  mk_simple_term str_type_op loc [mk_olist_term (List.map mk_tdl tdl)]
             | (<:str_item< value $opt:b$ $list:pel$ >>) -> mk_str_fix loc b pel
             | StInc (_, me) ->
                  mk_simple_term str_inc_op loc [mk_module_expr vars me]
             | StDir (_, Ploc.VaVal s, Ploc.VaVal eo) ->
                  mk_simple_named_term str_dir_op loc s [mk_expr_opt vars eo]
             | StExc (_, Ploc.VaVal s, Ploc.VaVal tl, Ploc.VaVal sl) ->
                  mk_simple_named_term str_exc_op loc s [mk_olist_term (List.map mk_type tl);
                                                         mk_string_list sl]
             | StMod (_, Ploc.VaVal b, Ploc.VaVal smel) ->
                  mk_simple_term str_mod_op loc [mk_bool b; mk_olist_term (List.map (mk_sme vars) smel)]
             | StUse (_, Ploc.VaVal s, Ploc.VaVal strll) ->
                  mk_simple_named_term str_use_op loc s [mk_olist_term (List.map (mk_strloc vars) strll)]
             | StDef (loc, Ploc.VaVal jcl) ->
                  mk_simple_term str_def_op (num_of_loc loc) [mk_olist_term (List.map (mk_joinclause vars) jcl)]
             | StXtr (loc, s, sto) ->
                  mk_simple_named_term str_xtr_op (num_of_loc loc) s [mk_opt (fun st -> (mk_str_item vars (dest_vala "StXtr" st))) sto]
             | StCls (_, Ploc.VaAnt _)
             | StClt (_, Ploc.VaAnt _)
             | StDcl (_, Ploc.VaAnt _)
             | StExt (_, _, _, Ploc.VaAnt _)
             | StExt (_, Ploc.VaAnt _, _, _)
             | StOpn (_, Ploc.VaAnt _)
             | StTyp (_, _, Ploc.VaAnt _)
             | StVal (_, _, Ploc.VaAnt _)
             | StVal (_, Ploc.VaAnt _, _)
             | StDir (_, _, Ploc.VaAnt _)
             | StDir (_, Ploc.VaAnt _, _)
             | StExc (_, _, _, Ploc.VaAnt _)
             | StExc (_, _, Ploc.VaAnt _, _)
             | StExc (_, Ploc.VaAnt _, _, _)
             | StMty (_, Ploc.VaAnt _, _)
             | StMod (_, _, Ploc.VaAnt _)
             | StMod (_, Ploc.VaAnt _, _)
             | StUse (_, _, Ploc.VaAnt _)
             | StUse (_, Ploc.VaAnt _, _)
             | StDef (_, Ploc.VaAnt _ ) | _ ->
                  raise (RefineError ("mk_st", StringError "antiquotations are not supported"))


   and mk_joinclause =
     fun vars jc ->
        match jc with
           { jcLoc = loc; jcVal = Ploc.VaVal jcll } ->
              let jcll = List.map (function (loc1, Ploc.VaVal jc, e) ->
                 let jcl =
                    List.map (function (loc2, (loc3, Ploc.VaVal s), Ploc.VaVal po) ->
                        let p = mk_patt_opt loc [] po (fun _ -> mk_simple_term jc_op (num_of_loc loc2) []) in
                          mk_simple_named_term jc_op (num_of_loc loc2) s [p]
                     | (_, (_, Ploc.VaAnt _), _)
                     | (_, _, Ploc.VaAnt _) ->
                          raise (RefineError ("mk_joinclause", StringError "antiquotations are not supported"))) jc
                 in
                    mk_simple_term jcl_op (num_of_loc loc1) [mk_olist_term jcl; mk_expr vars e]
               | (_, Ploc.VaAnt _, _) ->
                  raise (RefineError ("mk_joinclause", StringError "antiquotations are not supported"))) jcll
              in
                  mk_simple_term joinclause_op (num_of_loc loc) [mk_olist_term jcll]
        | { jcLoc = _; jcVal = Ploc.VaAnt _ } ->
           raise (RefineError ("mk_joinclause", StringError "antiquotations are not supported"))

   (*
    * Module types.
    *)
   and mk_module_type =
      let mt_lid_op =
         let dest_lid_mt t =
            let _loc = dest_loc "dest_lid_mt" t in
               <:module_type< $lid:dest_var t$ >>
         in add_mt "mt_lid" dest_lid_mt
      and mt_uid_op =
         let dest_uid_mt t =
            let _loc = dest_loc "dest_uid_mt" t in
               <:module_type< $uid:dest_var t$ >>
         in add_mt "mt_uid" dest_uid_mt
      and mt_proj_op =
         let dest_proj_mt t =
            let _loc = dest_loc "dest_proj_mt" t in
            let mt1, mt2 = two_subterms t in
               <:module_type< $dest_mt mt1$ . $dest_mt mt2$ >>
         in add_mt "mt_proj" dest_proj_mt
      and mt_apply_op =
         let dest_apply_mt t =
            let _loc = dest_loc "dest_apply_mt" t in
            let mt1, mt2 = two_subterms t in
               <:module_type< $dest_mt mt1$ $dest_mt mt2$ >>
         in add_mt "mt_apply" dest_apply_mt
      and mt_functor_op =
         let dest_functor_mt t =
            let _loc = dest_loc "dest_functor_mt" t in
            let v, mt1, mt2 = dest_dep0_dep1_any_term t in
               <:module_type< functor ($string_of_symbol v$ : $dest_mt mt1$) -> $dest_mt mt2$ >>
         in add_mt "mt_functor" dest_functor_mt
      and mt_quot_op =
         let dest_quot_mt t =
            let _loc = dest_loc "dest_quot_mt" t in
               <:module_type< ' $dest_var t$ >>
         in add_mt "mt_quot" dest_quot_mt
      and mt_sig_op =
         let dest_sig_mt t =
            let _loc = dest_loc "dest_sig_mt" t in
            let sil = dest_olist (one_subterm "dest_sig_mt" t) in
               <:module_type< sig $list: List.map dest_sig sil$ end >>
         in add_mt "mt_sig" dest_sig_mt
      and mt_type_with_op =
         let dest_with_mt t =
            let _loc = dest_loc "dest_with_mt" t in
               let mt, wcl = two_subterms t in
               let wcl = dest_olist wcl in
                  <:module_type< $dest_mt mt$ with $list: List.map dest_wc wcl$ >>
         in add_mt "mt_type_with" dest_with_mt
      and mt_tyo_op =
         let dest_tyo_mt t =
            let _loc = dest_loc "dest_tyo_mt" t in
               let me = one_subterm "mt_tyo_op" t in
                  <:module_type< module type of $dest_me me$ >>
         in add_mt "mt_tyo" dest_tyo_mt
       and mt_xtr_op =
         let dest_xtr_mt t =
            let loc = dest_loc "dest_xtr_mt" t in
            let s, mto = two_subterms t in
               MtXtr (loc, dest_string s, dest_opt (fun mt -> Ploc.VaVal (dest_mt mt)) mto)
         in add_mt "mt_xtr" dest_xtr_mt
     in fun mt ->
         let loc = loc_of_module_type mt in
            match mt with
               (<:module_type< $mt1$ . $mt2$ >>) ->
                  mk_simple_term mt_proj_op loc [mk_module_type mt1; mk_module_type mt2]
             | (<:module_type< $mt1$ $mt2$ >>) ->
                  mk_simple_term mt_apply_op loc [mk_module_type mt1; mk_module_type mt2]
             | (<:module_type< functor ( $s$ : $mt1$ ) -> $mt2$ >>) ->
                  let op_loc = mk_op_loc mt_functor_op loc in
                     mk_dep0_dep1_any_term op_loc (Lm_symbol.add s) (mk_module_type mt1) (mk_module_type mt2)
             | (<:module_type< $lid:i$ >>) ->
                  mk_var mt_lid_op [] loc i
             | (<:module_type< ' $i$ >>) ->
                  mk_var mt_quot_op [] loc i
             | (<:module_type< sig $list:sil$ end >>) ->
                  mk_simple_term mt_sig_op loc [mk_olist_term (List.map mk_sig_item sil)]
             | (<:module_type< $uid:i$ >>) ->
                  mk_var mt_uid_op [] loc i
             | (<:module_type< $mt$ with $list:wcl$ >>) ->
                  mk_simple_term mt_type_with_op loc
                     [mk_module_type mt; mk_olist_term (List.map mk_wc wcl)]
             | MtTyo (loc, me) ->
                  mk_simple_term mt_tyo_op (num_of_loc loc) [mk_module_expr [] me]
             | MtXtr (loc, s, mto) ->
                  mk_simple_named_term mt_xtr_op (num_of_loc loc) s [mk_opt (fun mt -> (mk_module_type (dest_vala "MtXtr" mt))) mto]
             | MtFun (_, Ploc.VaAnt _, _)
             | MtLid (_, Ploc.VaAnt _)
             | MtQuo (_, Ploc.VaAnt _)
             | MtSig (_, Ploc.VaAnt _)
             | MtUid (_, Ploc.VaAnt _)
             | MtWit (_, _, Ploc.VaAnt _) | _ ->
                   raise (RefineError ("mk_wc", StringError "antiquotations are not supported"))

   and mk_wc =
      let wc_type_op =
         let dest_type_wc t =
            let loc = dest_loc "dest_type_wc" t in
            let sl1, sl2, b, t = four_subterms t in
            let sl1' = List.map dest_string (dest_olist sl1) in
            let sl2' = List.map dest_sbb (dest_olist sl2) in
               WcTyp (loc, Ploc.VaVal sl1', Ploc.VaVal sl2', Ploc.VaVal (dest_bool b), dest_type t)
         in add_wc "wc_type" dest_type_wc
      and wc_tys_op =
         let dest_tys_wc t =
            let loc = dest_loc "dest_tys_wc" t in
            let sl1, sl2, t = three_subterms t in
            let sl1' = List.map dest_string (dest_olist sl1) in
            let sl2' = List.map dest_sbb (dest_olist sl2) in
               WcTys (loc, Ploc.VaVal sl1', Ploc.VaVal sl2', dest_type t)
         in add_wc "wc_tys" dest_tys_wc
      and wc_module_op =
         let dest_module_wc t =
            let loc = dest_loc "dest_module_wc" t in
            let sl1, mt = two_subterms t in
               WcMod (loc, Ploc.VaVal (List.map dest_string (dest_olist sl1)), dest_me mt)
         in add_wc "wc_module" dest_module_wc
      and wc_mos_op =
         let dest_mos_wc t =
            let loc = dest_loc "dest_mos_wc" t in
            let sl, me = two_subterms t in
               WcMos (loc, Ploc.VaVal (List.map dest_string (dest_olist sl)), dest_me me)
         in add_wc "wc_mos" dest_mos_wc
      in function
         WcTyp (loc, Ploc.VaVal sl1, Ploc.VaVal sl2, Ploc.VaVal b, t) ->
            let loc = num_of_loc loc in
            let sl1' = mk_olist_term (List.map mk_simple_string sl1) in
            let sl2' = mk_olist_term (List.map mk_sbb sl2) in
            let b = mk_bool b in
              mk_simple_term wc_type_op loc [sl1'; sl2'; b; mk_type t]
       | WcTys (loc, Ploc.VaVal sl1, Ploc.VaVal sl2, t) ->
            let loc = num_of_loc loc in
            let sl1' = mk_olist_term (List.map mk_simple_string sl1) in
            let sl2' = mk_olist_term (List.map mk_sbb sl2) in
              mk_simple_term wc_tys_op loc [sl1'; sl2'; mk_type t]
       | WcMod (loc, Ploc.VaVal sl1, mt) ->
            let loc = num_of_loc loc in
            let sl1' = mk_olist_term (List.map mk_simple_string sl1) in
              mk_simple_term wc_module_op loc [sl1'; mk_module_expr [] mt]
       | WcMos (loc, Ploc.VaVal sl, me) ->
            let loc = num_of_loc loc in
            let sl = mk_olist_term (List.map mk_simple_string sl) in
              mk_simple_term wc_mos_op loc [sl; mk_module_expr [] me]
       | WcTyp (_, _, _, Ploc.VaAnt _, _)
       | WcTyp (_, _, Ploc.VaAnt _, _, _)
       | WcTyp (_, Ploc.VaAnt _, _, _, _)
       | WcTys (_, _, Ploc.VaAnt _, _)
       | WcTys (_, Ploc.VaAnt _, _, _)
       | WcMod (_, Ploc.VaAnt _, _)
       | WcMos (_, Ploc.VaAnt _, _) ->
             raise (RefineError ("mk_wc", StringError "antiquotations are not supported"))

   (*
    * Module expressions.
    *)
   and mk_module_expr =
      let me_uid_op =
         let dest_uid_me t =
            let _loc = dest_loc "dest_uid_me" t in
               <:module_expr< $uid:dest_var t$ >>
         in add_me "me_uid" dest_uid_me
      and me_proj_op =
         let dest_proj_me t =
            let _loc = dest_loc "dest_proj_me" t in
            let me1, me2 = two_subterms t in
               <:module_expr< $dest_me me1$ . $dest_me me2$ >>
         in add_me "me_proj" dest_proj_me
      and me_apply_op =
         let dest_apply_me t =
            let _loc = dest_loc "dest_apply_me" t in
            let me1, me2 = two_subterms t in
               <:module_expr< $dest_me me1$ $dest_me me2$ >>
         in add_me "me_apply" dest_apply_me
      and me_functor_op =
         let dest_functor_me t =
            let _loc = dest_loc "dest_functor_me" t in
            let v, mt, me = dest_dep0_dep1_any_term t in
               <:module_expr< functor ($string_of_symbol v$ : $dest_mt mt$ ) -> $dest_me me$ >>
         in add_me "me_functor" dest_functor_me
      and me_struct_op =
         let dest_struct_me t =
            let _loc = dest_loc "dest_struct_me" t in
            let stl = subterms_of_term t in
               <:module_expr< struct $list: List.map dest_str stl$ end >>
         in add_me "me_struct" dest_struct_me
      and me_cast_op =
         let dest_cast_me t =
            let _loc = dest_loc "dest_cast_me" t in
            let me, mt = two_subterms t in
               <:module_expr< ( $dest_me me$ : $dest_mt mt$) >>
         in add_me "me_cast" dest_cast_me
      and me_unp_op =
         let dest_unp_me t =
            let _loc = dest_loc "dest_cast_me" t in
            let e, mt = two_subterms t in
               <:module_expr< (value $dest_expr e$ : $dest_mt mt$) >>
         in add_me "me_unp" dest_unp_me
      and me_xtr_op =
         let dest_xtr_me t =
            let loc = dest_loc "dest_xtr_me" t in
            let s, meo = two_subterms t in
               MeXtr (loc, dest_string s, dest_opt (fun me -> Ploc.VaVal (dest_me me)) meo)
         in add_me "me_xtr" dest_xtr_me
      in fun vars me ->
         let loc = loc_of_module_expr me in
            match me with
               (<:module_expr< $me1$ . $me2$ >>) ->
                  mk_simple_term me_proj_op loc [mk_module_expr vars me1;
                                                 mk_module_expr vars me2]
             | (<:module_expr< $me1$ $me2$ >>) ->
                  mk_simple_term me_apply_op loc [mk_module_expr vars me1;
                                                  mk_module_expr vars me2]
             | (<:module_expr< functor ( $s$ : $mt$ ) -> $me$ >>) ->
                     mk_dep0_dep1_any_term (mk_op_loc me_functor_op loc) (Lm_symbol.add s)
                        (mk_module_type mt) (mk_module_expr vars me)
             | (<:module_expr< struct $list:sil$ end >>) ->
                  mk_simple_term me_struct_op loc (List.map (mk_str_item vars) sil)
             | (<:module_expr< ( $me$ : $mt$) >>) ->
                  mk_simple_term me_cast_op loc [mk_module_expr vars me;
                                                 mk_module_type mt]
             | (<:module_expr< $uid:i$ >>) ->
                  mk_var me_uid_op [] loc i
             | MeUnp (loc, e, mto) ->
                  mk_simple_term me_unp_op (num_of_loc loc) [mk_expr vars e; mk_opt mk_module_type mto]
             | MeXtr (loc, s, meo) ->
                  mk_simple_named_term me_xtr_op (num_of_loc loc) s [mk_opt (fun me -> (mk_module_expr vars (dest_vala "MeXtr" me))) meo]
             | MeFun (_, Ploc.VaAnt _, _)
             | MeStr (_, Ploc.VaAnt _)
             | MeUid (_, Ploc.VaAnt _) | _ ->
                   raise (RefineError ("mk_module_expr", StringError "antiquotations are not supported"))


   and mk_class_type_infos = function
      { ciLoc = loc;
        ciNam = Ploc.VaVal s;
        ciPrm = _, Ploc.VaVal sl;
        ciVir = Ploc.VaVal b;
        ciExp = t
      } ->
      mk_simple_named_term class_type_infos_op (num_of_loc loc) s
         [ mk_olist_term (List.map mk_sbb sl);
           mk_bool b;
           mk_ct t
         ]
    | { ciNam = Ploc.VaAnt _; _ }
    | { ciPrm = _, Ploc.VaAnt _; _ }
    | { ciVir = Ploc.VaAnt _; _ } ->
            raise (RefineError ("mk_class_type_infos", StringError "antiquotations are not supported"))

   and mk_class_expr_infos vars = function
     { ciLoc = loc;
       ciNam = Ploc.VaVal s;
       ciPrm = _, Ploc.VaVal sl;
       ciVir = Ploc.VaVal b;
       ciExp = t
     } ->
      mk_simple_named_term class_type_infos_op (num_of_loc loc) s
         [ mk_olist_term (List.map mk_sbb sl);
           mk_bool b;
           mk_ce vars t
         ]
    | { ciNam = Ploc.VaAnt _; _ }
    | { ciPrm = _, Ploc.VaAnt _; _ }
    | { ciVir = Ploc.VaAnt _; _ } ->
            raise (RefineError ("mk_class_expr_infos", StringError "antiquotations are not supported"))

   (*
    * Class expressions.
    *)
   and mk_ce =
      let ce_app_op =
         let dest_app_ce t =
            let loc = dest_loc "dest_app_ce" t in
            let ce, e = two_subterms t in
               CeApp (loc, dest_ce ce, dest_expr e)
         in add_ce "class_expr_app" dest_app_ce
      and ce_con_op =
         let dest_con_ce t =
            let loc = dest_loc "dest_con_ce" t in
            let sl, tl = two_subterms t in
               CeCon (loc,
                      Ploc.VaVal (List.map dest_string (dest_olist sl)),
                      Ploc.VaVal (List.map dest_type (dest_olist tl)))
         in add_ce "class_expr_con" dest_con_ce
      and ce_fun_op =
         let dest_fun_ce t =
            let loc = dest_loc "dest_fun_ce" t in
            let p, ce = dest_patt (one_subterm "dest_fun_ce" t) in
               CeFun (loc, p, dest_ce ce)
         in add_ce "class_expr_fun" dest_fun_ce
      and ce_let_op =
         let dest_let_ce t =
            let loc = dest_loc "dest_let_ce" t in
            let b, t = two_subterms t in
            let b = dest_bool b in
            let pel, ce =
               if b then
                  dest_fix t
               else
                  dest_let t
            in
               CeLet (loc, Ploc.VaVal b, Ploc.VaVal pel, dest_ce ce)
         in add_ce "class_expr_let" dest_let_ce
      and ce_str_op =
         let dest_str_ce t =
            let loc = dest_loc "dest_str_ce" t in
            let p, cfl = dest_patt_opt (one_subterm "dest_str_ce" t) in
               CeStr (loc, Ploc.VaVal p, Ploc.VaVal (List.map dest_cf (dest_olist cfl)))
         in add_ce "class_expr_str" dest_str_ce
      and ce_tyc_op =
         let dest_tyc_ce t =
            let loc = dest_loc "dest_tyc_ce" t in
            let ce, ct = two_subterms t in
               CeTyc (loc, dest_ce ce, dest_ct ct)
         in add_ce "class_expr_tyc" dest_tyc_ce
      and ce_xtr_op =
         let dest_xtr_ce t =
            let loc = dest_loc "dest_xtr_ce" t in
            let s, ceo = two_subterms t in
               CeXtr (loc, dest_string s, dest_opt (fun ce -> Ploc.VaVal (dest_ce ce)) ceo)
         in add_ce "class_expr_xtr" dest_xtr_ce
      in fun vars -> function
         MLast.CeApp (loc, ce, e) ->
            mk_simple_term ce_app_op (num_of_loc loc) (**)
               [mk_ce vars ce;
                mk_expr vars e]
       | MLast.CeCon (loc, Ploc.VaVal sl, Ploc.VaVal tl) ->
            mk_simple_term ce_con_op (num_of_loc loc) (**)
               [mk_olist_term (List.map (mk_string ce_con_op) sl);
                mk_olist_term (List.map mk_type tl)]
       | MLast.CeFun (loc, p, ce) ->
            mk_simple_term ce_fun_op (num_of_loc loc) (**)
               [mk_patt vars p (fun vars -> mk_ce vars ce)]
       | MLast.CeLet (loc, Ploc.VaVal b, Ploc.VaVal pel, ce) ->
            mk_simple_term ce_let_op (num_of_loc loc) (**)
               [(if b then
                   mk_fix_tail
                else
                   mk_let_tail) vars (num_of_loc loc) pel (fun vars -> mk_ce vars ce)]
       | MLast.CeStr (loc, Ploc.VaVal p, Ploc.VaVal cfl) ->
            mk_simple_term ce_str_op (num_of_loc loc) (**)
               [mk_patt_opt loc vars p (mk_cf_list cfl)]
       | MLast.CeTyc (loc, ce, ct) ->
            mk_simple_term ce_tyc_op (num_of_loc loc) (**)
               [mk_ce vars ce;
                mk_ct ct]
       | MLast.CeXtr (loc, s, ceo) ->
            mk_simple_named_term ce_xtr_op (num_of_loc loc) s [mk_opt (fun ce -> (mk_ce vars (dest_vala "CeXtr" ce))) ceo]
       | MLast.CeCon (_, _, Ploc.VaAnt _)
       | MLast.CeCon (_, Ploc.VaAnt _, _)
       | MLast.CeLet (_, _, Ploc.VaAnt _, _)
       | MLast.CeLet (_, Ploc.VaAnt _, _, _)
       | MLast.CeStr (_, _, Ploc.VaAnt _)
       | MLast.CeStr (_, Ploc.VaAnt _, _) ->
            raise (RefineError ("mk_ce", StringError "antiquotations are not supported"))

   (*
    * Class types.
    *)
   and mk_ct =
      let ct_con_op =
         let dest_con_ct t =
            let loc = dest_loc "dest_con_ct" t in
            let ct, tl = two_subterms t in
               CtCon (loc,
                      dest_ct ct,
                      Ploc.VaVal (List.map dest_type (dest_olist tl)))
         in add_ct "class_type_con" dest_con_ct
      and ct_acc_op =
         let dest_acc_ct t =
            let loc = dest_loc "dest_acc_ct" t in
            let ct1, ct2 = two_subterms t in
               CtAcc (loc, dest_ct ct1, dest_ct ct2)
         in add_ct "class_type_acc" dest_acc_ct
      and ct_app_op =
         let dest_app_ct t =
            let loc = dest_loc "dest_app_ct" t in
            let ct1, ct2 = two_subterms t in
               CtAcc (loc, dest_ct ct1, dest_ct ct2)
         in add_ct "class_type_app" dest_app_ct
      and ct_ide_op =
         let dest_ide_ct t =
            let loc, s = dest_loc_string "dest_ide_ct" t in
               CtIde (loc, Ploc.VaVal s)
         in add_ct "class_type_ide" dest_ide_ct
      and ct_fun_op =
         let dest_fun_ct t =
            let loc = dest_loc "dest_fun_ct" t in
            let t, ct = two_subterms t in
               CtFun (loc, dest_type t, dest_ct ct)
         in add_ct "class_type_fun" dest_fun_ct
      and ct_sig_op =
         let dest_sig_ct t =
            let loc = dest_loc "dest_sig_ct" t in
            let t, ctfl = two_subterms t in
               CtSig (loc, Ploc.VaVal (dest_opt dest_type t), Ploc.VaVal (List.map dest_ctf (dest_olist ctfl)))
         in add_ct "class_type_sig" dest_sig_ct
      and ct_xtr_op =
         let dest_xtr_ct t =
            let loc = dest_loc "dest_xtr_ct" t in
            let s, cto = two_subterms t in
               CtXtr (loc, dest_string s, dest_opt (fun ct -> Ploc.VaVal (dest_ct ct)) cto)
         in add_ct "class_type_xtr" dest_xtr_ct
      in function
         CtCon (loc, ct, Ploc.VaVal tl) ->
            mk_simple_term ct_con_op (num_of_loc loc) (**)
               [mk_ct ct;
                mk_olist_term (List.map mk_type tl)]
       | CtFun (loc, t, ct) ->
            mk_simple_term ct_fun_op (num_of_loc loc) [mk_type t; mk_ct ct]
       | CtSig (loc, Ploc.VaVal t, Ploc.VaVal ctfl) ->
            mk_simple_term ct_sig_op (num_of_loc loc) (**)
               [mk_type_opt t; mk_olist_term (List.map mk_ctf ctfl)]
       | CtAcc (loc, ct1, ct2) ->
            mk_simple_term ct_acc_op (num_of_loc loc) [mk_ct ct1; mk_ct ct2]
       | CtApp (loc, ct1, ct2) ->
            mk_simple_term ct_app_op (num_of_loc loc) [mk_ct ct1; mk_ct ct2]
       | CtIde (loc, Ploc.VaVal s) ->
            mk_simple_named_term ct_ide_op (num_of_loc loc) s []
       | CtXtr (loc, s, cto) ->
            mk_simple_named_term ct_xtr_op (num_of_loc loc) s [mk_opt (fun ct -> (mk_ct (dest_vala "CtXtr" ct))) cto]
       | CtCon (_, _, Ploc.VaAnt _)
       | CtSig (_, _, Ploc.VaAnt _)
       | CtSig (_, Ploc.VaAnt _, _)
       | CtIde (_, Ploc.VaAnt _) ->
            raise (RefineError ("mk_ctf", StringError "antiquotations are not supported"))

   and mk_ctf =
      let ctf_ctr_op =
         let dest_ctr_ctf t =
            let loc = dest_loc "dest_ctr_ctf" t in
            let s, t = two_subterms t in
               CgCtr (loc, dest_type s, dest_type t)
         in add_ctf "class_type_ctr" dest_ctr_ctf
      and ctf_dcl_op =
         let dest_dcl_ctf t =
            let loc = dest_loc "dest_dcl_ctf" t in
            let t = one_subterm "dest_dcl_ctf" t in
            let t = List.map dest_ctf (dest_olist t) in
               CgDcl (loc, Ploc.VaVal t)
         in add_ctf "class_type_ctf" dest_dcl_ctf
      and ctf_inh_op =
         let dest_inh_ctf t =
            let loc = dest_loc "dest_inh_ctf" t in
            let t = one_subterm "dest_inh_ctf" t in
               CgInh (loc, dest_ct t)
         in add_ctf "class_type_inh" dest_inh_ctf
      and ctf_mth_op =
         let dest_mth_ctf t =
            let loc = dest_loc "dest_mth_ctf" t in
            let s, b, t = three_subterms t in
               CgMth (loc, Ploc.VaVal (dest_bool b), Ploc.VaVal (dest_string s), dest_type t)
         in add_ctf "class_type_mth" dest_mth_ctf
      and ctf_val_op =
         let dest_val_ctf t =
            let loc = dest_loc "dest_val_ctf" t in
            let s, b, t = three_subterms t in
               CgVal (loc, Ploc.VaVal (dest_bool b), Ploc.VaVal (dest_string s), dest_type t)
         in add_ctf "class_type_val" dest_val_ctf
      and ctf_vir_op =
         let dest_vir_ctf t =
            let loc = dest_loc "dest_vir_ctf" t in
            let s, b, t = three_subterms t in
               CgVir (loc, Ploc.VaVal (dest_bool b), Ploc.VaVal (dest_string s), dest_type t)
         in add_ctf "class_type_vir" dest_vir_ctf
      in function
         CgCtr (loc, s, t) ->
            mk_simple_term ctf_ctr_op (num_of_loc loc) [mk_type s; mk_type t]
       | CgDcl (loc, Ploc.VaVal t) ->
            mk_simple_term ctf_dcl_op (num_of_loc loc) [mk_olist_term (List.map mk_ctf t)]
       | CgInh (loc, ct) ->
            mk_simple_term ctf_inh_op (num_of_loc loc) [mk_ct ct]
       | CgMth (loc, Ploc.VaVal b, Ploc.VaVal s, t) ->
            mk_simple_term ctf_mth_op (num_of_loc loc) [mk_simple_string s; mk_bool b; mk_type t]
       | CgVal (loc, Ploc.VaVal b, Ploc.VaVal s, t) ->
            mk_simple_term ctf_val_op (num_of_loc loc) [mk_simple_string s; mk_bool b; mk_type t]
       | CgVir (loc, Ploc.VaVal b, Ploc.VaVal s, t) ->
            mk_simple_term ctf_vir_op (num_of_loc loc) [mk_simple_string s; mk_bool b; mk_type t]
       | CgDcl (_, Ploc.VaAnt _)
       | CgMth (_, _, Ploc.VaAnt _, _)
       | CgMth (_, Ploc.VaAnt _, _, _)
       | CgVal (_, _, Ploc.VaAnt _, _)
       | CgVal (_, Ploc.VaAnt _, _, _)
       | CgVir (_, _, Ploc.VaAnt _, _)
       | CgVir (_, Ploc.VaAnt _, _, _) ->
            raise (RefineError ("mk_ctf", StringError "antiquotations are not supported"))

   and mk_cf =
      let cf_ctr_op =
         let dest_ctr_cf t =
            let loc = dest_loc "dest_ctr_cf" t in
            let s, t = two_subterms t in
               CrCtr (loc, dest_type s, dest_type t)
         in add_cf "class_ctr" dest_ctr_cf
      and cf_dcl_op =
         let dest_dcl_cf t =
            let loc = dest_loc "dest_dcl_cf" t in
            let t = one_subterm "dest_dcl_cf" t in
            let t = List.map dest_cf (dest_olist t) in
               CrDcl (loc, Ploc.VaVal t)
         in add_cf "class_dcl" dest_dcl_cf
      and cf_inh_op =
         let dest_inh_cf t =
            let loc = dest_loc "dest_inh_cf" t in
            let ce, so = two_subterms t in
               CrInh (loc, dest_ce ce, Ploc.VaVal (dest_opt dest_string so))
         in add_cf "class_inh" dest_inh_cf
      and cf_ini_op =
         let dest_ini_cf t =
            let loc = dest_loc "dest_ini_cf" t in
            let e = one_subterm "dest_ini_cf" t in
               CrIni (loc, dest_expr e)
         in add_cf "class_ini" dest_ini_cf
      and cf_mth_op =
         let dest_mth_cf t =
            let loc = dest_loc "dest_mth_cf" t in
            let s, b1, b2, e, t = five_subterms t in
               CrMth (loc, Ploc.VaVal (dest_bool b1), Ploc.VaVal (dest_bool b2), Ploc.VaVal (dest_string s), Ploc.VaVal (dest_opt dest_type t), dest_expr e)
         in add_cf "class_mth" dest_mth_cf
      and cf_val_op =
         let dest_val_cf t =
            let loc = dest_loc "dest_val_cf" t in
            let s, b1, b2, e = four_subterms t in
               CrVal (loc, Ploc.VaVal (dest_bool b1), Ploc.VaVal (dest_bool b2), Ploc.VaVal (dest_string s), dest_expr e)
         in add_cf "class_val" dest_val_cf
      and cf_vir_op =
         let dest_vir_cf t =
            let loc = dest_loc "dest_vir_cf" t in
            let s, b, t = three_subterms t in
               CrVir (loc, Ploc.VaVal (dest_bool b), Ploc.VaVal (dest_string s), dest_type t)
         in add_cf "class_vir" dest_vir_cf
      and cf_vav_op =
         let dest_vav_cf t =
            let loc = dest_loc "dest_vav_cf" t in
            let s, b, t = three_subterms t in
               CrVav (loc, Ploc.VaVal (dest_bool b), Ploc.VaVal (dest_string s), dest_type t)
         in add_cf "class_vav" dest_vav_cf
      in fun vars -> function
         CrCtr (loc, s, t) ->
            let loc = num_of_loc loc in
               mk_simple_term cf_ctr_op loc [mk_type s; mk_type t]
       | CrDcl (loc, Ploc.VaVal t) ->
            let loc = num_of_loc loc in
               mk_simple_term cf_dcl_op loc [mk_cf_list t vars]
       | CrInh (loc, ce, Ploc.VaVal so) ->
            let loc = num_of_loc loc in
               mk_simple_term cf_inh_op loc [mk_ce vars ce; mk_string_opt expr_string_op so]
       | CrIni (loc, e) ->
            let loc = num_of_loc loc in
               mk_simple_term cf_ini_op loc [mk_expr vars e]
       | CrMth (loc, Ploc.VaVal b1, Ploc.VaVal b2, Ploc.VaVal s, Ploc.VaVal t, e) ->
            let loc = num_of_loc loc in
               mk_simple_term cf_mth_op loc (**)
                  [mk_simple_string s; mk_bool b1; mk_bool b2; mk_expr vars e; mk_opt mk_type t]
       | CrVal (loc, Ploc.VaVal b1, Ploc.VaVal b2, Ploc.VaVal s, e) ->
            let loc = num_of_loc loc in
               mk_simple_term cf_val_op loc [mk_simple_string s; mk_bool b1; mk_bool b2; mk_expr vars e]
       | CrVir (loc, Ploc.VaVal b, Ploc.VaVal s, t) ->
            let loc = num_of_loc loc in
               mk_simple_term cf_vir_op loc [mk_simple_string s; mk_bool b; mk_type t]
       | CrVav (loc, Ploc.VaVal b, Ploc.VaVal s, t) ->
            let loc = num_of_loc loc in
               mk_simple_term cf_vav_op loc [mk_simple_string s; mk_bool b; mk_type t]
       | CrDcl (_, Ploc.VaAnt _)
       | CrInh (_, _, Ploc.VaAnt _)
       | CrMth (_, _, _,  _, Ploc.VaAnt _, _)
       | CrMth (_, _, _, Ploc.VaAnt _, _, _)
       | CrMth (_, _, Ploc.VaAnt _, _, _, _)
       | CrMth (_, Ploc.VaAnt _, _, _, _, _)
       | CrVal (_, _, _, Ploc.VaAnt _, _)
       | CrVal (_, _, Ploc.VaAnt _, _, _)
       | CrVal (_, Ploc.VaAnt _, _, _, _)
       | CrVir (_, _, Ploc.VaAnt _, _)
       | CrVir (_, Ploc.VaAnt _, _, _)
       | CrVav (_, _, Ploc.VaAnt _, _)
       | CrVav (_, Ploc.VaAnt _, _, _) ->
            raise (RefineError ("mk_cf", StringError "antiquotations are not supported"))

   and mk_cf_list cfl vars =
      mk_olist_term (List.map (mk_cf vars) cfl)

   (*
    * Make a fix expression.
    *)
   and mk_fix_aux vars loc pel tailf =
      let pl, el = List.split pel in
      let pl = List.rev pl in
      let el = List.rev el in
      let rec tailf' el vars =
         match el with
           e::el ->
              mk_simple_term patt_fix_arg_op loc [mk_expr vars e; tailf' el vars]
          | [] ->
              tailf vars
      in
      let rec make pl vars =
         match pl with
            p::pl ->
               let tailf'' vars =
                  if pl = [] then
                     tailf' el vars
                  else
                     mk_simple_term patt_fix_and_op loc [make pl vars]
               in
                  mk_patt vars p tailf''
          | [] ->
             tailf' el vars
      in
         make pl vars

   and mk_fix_tail =
      let expr_fix_op =
         let dest_fix_expr t =
            let _loc = dest_loc "dest_fix_expr" t in
            let pel, e = dest_fix t in
               <:expr< let rec $list: pel$ in $dest_expr e$ >>
         in add_expr "fix" dest_fix_expr
      in fun vars loc pel tailf ->
         let tailf vars =
            mk_simple_term patt_in_op loc [tailf vars]
         in
            mk_simple_term expr_fix_op loc [mk_fix_aux vars loc pel tailf]

   and mk_fix vars loc pel e =
      mk_fix_tail vars loc pel (fun vars -> mk_expr vars e)

   and mk_let_tail =
      let patt_and_op = mk_ocaml_op "patt_and"
      and expr_let_op =
         let dest_let_expr t =
            let _loc = dest_loc "dest_let_expr" t in
            let pel, e = dest_let t in
               <:expr< let $list: pel$ in $dest_expr e$ >>
         in add_expr "let" dest_let_expr
      in fun vars loc pel tailf ->
         let pl, el = List.split pel in
         let el = List.map (mk_expr vars) el in
         let tailf vars =
            mk_simple_term patt_in_op loc [tailf vars]
         in
         let rec make pl vars =
            match pl with
               p::pl ->
                  let tailf' vars =
                     if pl = [] then
                        tailf vars
                     else
                        mk_simple_term patt_and_op loc [make pl vars]
                  in
                     mk_patt vars p tailf'
            | [] ->
               tailf vars
         in
            mk_simple_term expr_let_op loc [make pl vars; mk_olist_term el]

   and mk_let vars loc pel e =
      mk_let_tail vars loc pel (fun vars -> mk_expr vars e)

   and mk_str_fix =
      let patt_done_op = mk_ocaml_op "patt_done"
      in let str_fix_op =
         let dest_fix_str t =
            let _loc = dest_loc "dest_fix_str" t in
            let rec dest_exprs t =
               if Opname.eq (opname_of_term t) patt_done_op then
                  []
               else
                  let e, t = two_subterms_opname patt_fix_arg_op t in
                  (dest_expr e) :: (dest_exprs t)
            and dest_patts t =
               let p, t = dest_patt t in
               if Opname.eq (opname_of_term t) patt_fix_arg_op then
                  [p], dest_exprs t
               else
                  let t = one_subterm_opname patt_fix_and_op t in
                  let ps, es = dest_patts t in
                  p :: ps, es
            in
            let ps, es = dest_patts (one_subterm "dest_fix_str" t) in
               <:str_item< value rec $list: List.combine ps es$ >>
         in add_str "str_fix" dest_fix_str
      and str_let_op =
         let dest_let_str t =
            let _loc = dest_loc "dest_let_str" t in
            let dest t =
               let p, e = two_subterms t in
               let p, _ = dest_patt p in
               let e = dest_expr e in
                  p, e
            in
            let lets = dest_olist (one_subterm "dest_let_str" t) in
            let pel = List.map dest lets in
               <:str_item< value $list: pel$ >>
         in add_str "str_let" dest_let_str
      in fun loc b pel ->
         if b then
            let tailf vars =
               mk_simple_term patt_done_op loc []
            in
               mk_simple_term str_fix_op loc [mk_fix_aux [] loc pel tailf]
         else
            let make (p, e) =
               let tailf vars =
                  mk_simple_term patt_done_op loc []
               in
               let p = mk_patt [] p tailf in
               let e = mk_expr [] e in
                  mk_simple_term str_let_op loc [p; e]
            in
            let tl = List.map make pel in
            let t = mk_olist_term tl in
               mk_simple_term str_let_op loc [t]

   and mk_fun_aux =
      let patt_body_op = mk_ocaml_op "patt_body"
      in fun vars loc pwel ->
         let make_pwe (p, w, e) =
            let tailf vars =
               match w with
                  Some w ->
                     mk_simple_term patt_with_op loc [mk_expr vars w; mk_expr vars e]
                | None ->
                     mk_simple_term patt_body_op loc [mk_expr vars e]
            in
               mk_patt vars p tailf
         in
         let rec make = function
            [pwe] ->
               mk_simple_term patt_if_op loc [make_pwe pwe]
          | pwe :: t ->
               mk_simple_term patt_ifelse_op loc [make_pwe pwe; make t]
          | [] ->
               mk_simple_term patt_fail_op loc []
         in
            make pwel

   and mk_fun_vala_aux =
     fun vars loc pwel ->
        let pwel = List.map (fun (p, w, e) -> p, dest_vala "mk_fun_vala_aux" w, e) pwel in
        mk_fun_aux vars loc pwel

   and mk_fun =
      let expr_fun_op =
         let dest_fun_expr t =
            let _loc = dest_loc "dest_fun_expr" t in
            let pwel = dest_fun_vala_aux (one_subterm "dest_fun_expr" t) in
               <:expr< fun [ $list: pwel$ ] >>
         in add_expr "fun" dest_fun_expr
      in fun vars loc pwel ->
         mk_simple_term expr_fun_op loc [mk_fun_vala_aux vars loc pwel]

   and mk_match =
      let expr_match_op =
         let dest_match_expr t =
            let _loc = dest_loc "dest_match_expr" t in
            let pwel, e = two_subterms t in
            let pwel = dest_fun_vala_aux pwel in
               <:expr< match $dest_expr e$ with [ $list: pwel$ ] >>
         in add_expr "match" dest_match_expr
      in fun vars loc pwel e ->
         mk_simple_term expr_match_op loc [mk_fun_vala_aux vars loc pwel; mk_expr vars e]

   and mk_try =
      let expr_try_op =
         let dest_try_expr t =
            let _loc = dest_loc "dest_try_expr" t in
            let pwel, e = two_subterms t in
            let pwel = dest_fun_vala_aux pwel in
               <:expr< try $dest_expr e$ with [ $list: pwel$ ] >>
         in add_expr "try" dest_try_expr
      in fun vars loc pwel e ->
         mk_simple_term expr_try_op loc [mk_fun_vala_aux vars loc pwel; mk_expr vars e]

   and mk_lab_expr =
      let expr_lab_op =
         let dest_lab_expr t =
            let _loc = dest_loc "dest_lab_expr" t in
            let poel = one_subterm "dest_lab_expr" t in
            let poel = dest_lab_aux poel in
               ExLab (_loc, Ploc.VaVal poel)
         in add_expr "lab" dest_lab_expr
      in fun vars loc poel ->
         let make_poe = function
            (p, Ploc.VaVal oe) ->
                mk_patt vars p (fun vars -> mk_opt (mk_expr vars) oe)
          | (_, Ploc.VaAnt _) ->
                raise (RefineError ("mk_lab_expr", StringError "antiquotations are not supported"))
         in
         let rec make = function
            [poe] ->
               mk_simple_term patt_if_op loc [make_poe poe]
          | poe :: t ->
               mk_simple_term patt_ifelse_op loc [make_poe poe; make t]
          | [] ->
               mk_simple_term patt_fail_op loc []
         in
            mk_simple_term expr_lab_op loc [make poel]

   (*
    * Combined forms.
    *)
   and mk_expr_opt vars x = mk_opt (mk_expr vars) x

   and mk_type_opt x = mk_opt mk_type x

   and mk_se =
      let se_op = mk_ocaml_op "se"
      in fun vars (s, e) ->
         ToTerm.Term.mk_simple_term se_op [mk_simple_string s; mk_expr vars e]

   and mk_ident_pe =
      let ee_op = mk_ocaml_op "ee"
      in fun vars (p, e) ->
         ToTerm.Term.mk_simple_term ee_op [mk_expr vars (expr_of_patt_ident p); mk_expr vars e]

   and mk_st =
      let st_op = mk_ocaml_op "st"
      in fun (s, t) ->
         ToTerm.Term.mk_simple_term st_op [mk_simple_string s; mk_type t]

   and mk_smt =
      let smt_op = mk_ocaml_op "smt"
      in fun (s, mt) ->
         ToTerm.Term.mk_simple_term smt_op [mk_vala "mk_smt" (mk_opt (mk_vala "mk_smt" mk_simple_string)) s; mk_module_type mt]

   and mk_sme =
      let sme_op = mk_ocaml_op "sme"
      in fun vars (s, me) ->
         ToTerm.Term.mk_simple_term sme_op [mk_vala "mk_sme" (mk_opt (mk_vala "mk_sme" mk_simple_string)) s; mk_module_expr vars me]

   and mk_sbt =
      let sbt_op = mk_ocaml_op "sbt"
      in fun (l, s, b, t) ->
         let l = num_of_loc l in
            mk_simple_named_term sbt_op l s [mk_bool b; mk_type t]

   and mk_rf rf =
      match rf with
         PvTag (_, Ploc.VaVal s, Ploc.VaVal b, Ploc.VaVal tl) ->
            ToTerm.Term.mk_simple_term row_field_tag_op [mk_simple_string s; mk_bool b; mk_olist_term (List.map mk_type tl)]
       | PvInh (_, t) ->
            ToTerm.Term.mk_simple_term row_field_inh_op [mk_type t]
       | PvTag (_, Ploc.VaAnt _, _, _)
       | PvTag (_, _, Ploc.VaAnt _, _)
       | PvTag (_, _, _, Ploc.VaAnt _) ->
            raise (RefineError ("mk_rf", StringError "antiquotations are not supported"))

   and mk_stl =
      let stl_op =  mk_ocaml_op "stl"
      in fun (l, s, tl, t) ->
         mk_simple_named_term stl_op (num_of_loc l) (dest_vala "mk_stl" s) [mk_olist_term (List.map mk_type (dest_vala "mk_smt" tl)); mk_opt mk_type t]

   and mk_tc =
      let tc_op = mk_ocaml_op "tc"
      in fun (t1, t2) ->
         ToTerm.Term.mk_simple_term tc_op [mk_type t1; mk_type t2]

   and mk_tdl =
      let tdl_op = mk_ocaml_op "tdl"
      in function { tdNam = Ploc.VaVal (l, Ploc.VaVal s);
                    tdPrm = Ploc.VaVal sl;
                    tdPrv = Ploc.VaVal b;
                    tdDef = t;
                    tdCon = Ploc.VaVal tl
             } ->
         ToTerm.Term.mk_simple_term tdl_op [mk_loc_string tdl_op (num_of_loc l) s;
                                            mk_olist_term (List.map mk_sbb sl);
                                            mk_type t;
                                            mk_olist_term (List.map mk_tc tl) ]
      | { tdNam = Ploc.VaVal (_, Ploc.VaAnt _); _ }
      | { tdNam = Ploc.VaAnt _; _ }
      | { tdPrm = Ploc.VaAnt _; _ }
      | { tdPrv = Ploc.VaAnt _; _ }
      | { tdCon = Ploc.VaAnt _; _ } ->
            raise (RefineError ("mk_tdl", StringError "antiquotations are not supported"))

   and mk_sbb =
      let sbb_op = mk_ocaml_op "sbb"
      in function (Ploc.VaVal so, bo) ->
         let b1, b2 = match bo with Some b -> true, b | None -> false, false in
         ToTerm.Term.mk_simple_term sbb_op [mk_opt mk_simple_string so; mk_bool b1; mk_bool b2]
       | (Ploc.VaAnt _, _) ->
            raise (RefineError ("mk_sbb", StringError "antiquotations are not supported"))


(* unused
   and mk_bsl =
      let bsl_op = mk_ocaml_op "bsl"
      in fun (b, sl) ->
         let sl = mk_olist_term (List.map mk_simple_string sl) in
            ToTerm.Term.mk_simple_term bsl_op [mk_bool b; sl]
*)

   and mk_sigloc =
      let sigloc_op = mk_ocaml_op "sigloc"
      in fun (sg, loc) ->
         mk_simple_term sigloc_op (num_of_loc loc) [mk_sig_item sg]

   and mk_strloc =
      let strloc_op = mk_ocaml_op "strloc"
      in fun vars (str, loc) ->
         mk_simple_term strloc_op (num_of_loc loc) [mk_str_item vars str]

   (************************************************************************
    * EXPORTS                                                              *
    ************************************************************************)

   (*
    * Default functions.
    *)
   let dest_loc = dest_loc "external"
   let dest_loc_string = dest_loc_string "external"
   let dest_loc_int = dest_loc_int "external"

   (*
    * Some default terms to return on error.
    *)
   let _loc = dummy_loc
   let def_str_item = StDcl (_loc, Ploc.VaVal [])

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

   let resource_str_of_term resourse_op t =
      (*
       * XXX: HACK: In ASCII IO format <= 1.0.12 and Term IO format <= 1.0.9,
       * resource_str was just an expression. Once we no longer have old files,
       * there will be no need to pass the resourse_op parameter and test it.
       *)
      if is_dep0_dep0_dep0_term resourse_op t then
         let inp, outp, expr = three_subterms t in {
            res_input = dest_type inp;
            res_output = dest_type outp;
            res_body = dest_expr expr;
         }
      else {
         res_input = <:ctyp< Filter_ocaml.fake_ctyp_made_for_old_file_format_compatibility >>;
         res_output = <:ctyp< Filter_ocaml.fake_ctyp_made_for_old_file_format_compatibility >>;
         res_body = dest_expr t
      }

end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
