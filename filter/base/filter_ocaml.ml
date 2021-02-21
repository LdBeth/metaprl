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

(* open MLast *)
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
   open ToTerm.RefineError

   module SimplePrint = Simple_print.MakeSimplePrint (ToTerm)
   module TermCopy2 = Term_copy2_weak.TermCopy2Weak (Refiner.Refiner) (ToTerm)

   (************************************************************************
    * BASIC TERM OPERATIONS                                                *
    ************************************************************************)

   let mk_caml_op =
      let tbl = Hashtbl.create 31 in
      let ocaml_op = mk_opname "Ocaml" nil_opname in
         (fun s ->
               if Hashtbl.mem tbl s then invalid_arg ("Filter_ocaml.mk_ocaml_op: " ^ s ^ " already exists");
               Hashtbl.add tbl s ();
               mk_opname s ocaml_op)
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


   (************************************************************************
    * OCaml lists.
    *)

   (*
    * Lists.
    *)
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
*)

   (************************************************************************
    * TERM DESTRUCTORS                                                     *
    ************************************************************************)

   (*
    * Standard term ops.
    *)
   let some_op           = mk_caml_op "some"
   let none_op           = mk_caml_op "none"

(*
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
      mk_any_term (mk_op_loc_name opname loc name) subterms *)

   (*
    * Optional term.
    *)
   let mk_opt f = function
      None ->
         ToTerm.Term.mk_simple_term none_op []
    | Some t ->
         ToTerm.Term.mk_simple_term some_op [f t]
(*

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
      mk_loc_string_aux opname loc s [t] *)

   let mk_string opname s =
      let p1 = make_param (String s) in
      let op = mk_op opname [p1] in
         mk_term op []

 (*  let mk_string_opt op =
      mk_opt (mk_string op) *)


(*
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
*)
   (*
    * Variables are enclosed in terms that mark
    * the variable type.
    *
    * If the var is bound, then we produce a real var,
    * Otherwise, we produce a string to lookup from the environment.
    *)
   let mk_var_aux opname vars s l =
      let v =
         (* XXX HACK: internal "_$" vars are always vars *)
         if List.mem s vars || (s.[0] = '_' && s.[1] = '$') then
            mk_var_term (Lm_symbol.add s)
         else
            mk_string_term opname s
      in
         mk_any_term (mk_op opname []) (v :: l)

   let mk_var opname vars s =
      mk_var_aux opname vars s []

(*
   let mk_var_term opname vars loc s t =
      mk_var_aux opname vars loc s [t]

   let mk_patt_var opname loc s t =
      let op = mk_op_loc opname loc in
      let bterm = mk_bterm [s] t in
         mk_term op [bterm] *)

   (* let class_type_infos_op = mk_ocaml_op "class_type_infos" *)

   let mk_simple_named_term opname name subterms =
      mk_any_term (mk_op opname [make_param (String name)]) subterms

   let stub_opname = mk_opname "Ocaml_stub" nil_opname
   let sig_opname = mk_opname "Ocaml_stub_sig" nil_opname
   let str_opname = mk_opname "Ocaml_stub_str" nil_opname
   let ctyp_opname = mk_opname "Ocaml_stub_ctyp" nil_opname
   let mk_stub_term opname = mk_any_term (mk_op opname []) []
   let stub_term = mk_stub_term stub_opname

   let onil_opname = mk_caml_op "onil"
   let onil_term = mk_term (mk_op onil_opname []) []

   let ocons_opname = mk_caml_op "ocons"
   let ocons_op = mk_op ocons_opname []

   let rec mk_olist_term f = function
      h::t ->
         mk_term ocons_op [mk_simple_bterm (f h); mk_simple_bterm (mk_olist_term f t)]
    | [] ->
         onil_term

   let expr_string_op = mk_caml_op "string"
   let mk_simple_string =
      mk_string expr_string_op

   let ident_op = mk_caml_op "ident"
   let mk_ident_term = mk_string_term ident_op

   let mk_longid_term longid =
      let rec aux () = function
         (<:extended_longident:< $longid:x$ . $uid:uid$ >>) ->
            Printf.sprintf "%a.%s" aux x uid
       | (<:extended_longident:< $longid:x1$ ( $longid:x2$ ) >>) ->
            Printf.sprintf "%a(%a)" aux x1 aux x2
       | (<:extended_longident< $uid:s$ >>) ->
            s
       | _ -> invalid_arg "Filter_ocaml.mk_longid_term"
      in mk_ident_term (aux () longid)

  (* let mk_patt = function
      (<:patt< $lid:s$ >>) ->
         mk_ident_term s
    | _ -> stub_term *)

   let rec mk_lab_expr =
      let poe_op = mk_caml_op "poe"
      in fun vars ->
         let make_poe = function
            (<:patt< $lid:s$ >>, <:vala< oe >>) ->
               mk_simple_named_term poe_op s [mk_expr_opt vars oe]
          | _ ->
               raise (RefineError ("mk_lab_expr", StringError "antiquotations are not supported"))
         in mk_olist_term make_poe

   and mk_expr_opt vars x = mk_opt (mk_expr vars) x

   and mk_expr =
      let expr_apply_op = mk_caml_op "apply" in
      let expr_array_op = mk_caml_op "array" in
      let expr_lab_op = mk_caml_op "lab" in
      let expr_lid_op = mk_caml_op "lid" in
      let expr_uid_op = mk_caml_op "uid" in
      let expr_tuple_op = mk_caml_op "tuple"
      in fun vars -> function
         (<:expr< $e1$ $e2$ >>) ->
            mk_simple_term expr_apply_op [mk_expr vars e1; mk_expr vars e2]
       | (<:expr< [| $list:el$ |] >>) ->
            mk_simple_term expr_array_op [mk_olist_term (mk_expr vars) el]
       | (<:expr< ~{$list:lpe$} >>) ->
            mk_simple_term expr_lab_op [mk_lab_expr vars lpe]
       | (<:expr< $lid:s$ >>) ->
            mk_var expr_lid_op vars s
       | (<:expr< $uid:s$ >>) ->
            mk_var expr_uid_op vars s
       | (<:expr< $longid:x$ >>) ->
            mk_longid_term x
       | (<:expr< ( $list:el$ ) >>) ->
            mk_simple_term expr_tuple_op [mk_olist_term (mk_expr vars) el]
       | _ -> stub_term

   let rec mk_type =
      let type_apply_op = mk_caml_op "type_apply" in
      let type_fun_op = mk_caml_op "type_fun" in
      let type_equal_op = mk_caml_op "type_equal" in
      let type_prod_op = mk_caml_op "type_prod" in
      let type_olb_op = mk_caml_op "type_olb" in
      let type_lab_op = mk_caml_op "type_lab"
      in function
         (<:ctyp< $lid:s$ >>) ->
            mk_ident_term s
       | (<:ctyp< $longid:_$ . $lid:s$ >>) ->
            mk_ident_term s
       | (<:ctyp< $t1$ $t2$ >>) ->
            mk_simple_term type_apply_op [mk_type t1; mk_type t2]
       | (<:ctyp< $t1$ -> $t2$ >>) ->
            mk_simple_term type_fun_op [mk_type t1; mk_type t2]
       | (<:ctyp< $t1$ == $priv:_$ $t2$ >>) ->
            mk_simple_term type_equal_op [mk_type t1; mk_type t2]
       | (<:ctyp< ( $list:tl$ ) >>) ->
            mk_simple_term type_prod_op [mk_olist_term mk_type tl]
       | (<:ctyp< ?$s$: $t$ >>) ->
            mk_simple_named_term type_olb_op s [mk_type t]
       | (<:ctyp< ~$s$: $t$ >>) ->
            mk_simple_named_term type_lab_op s [mk_type t]
       | _ -> mk_stub_term ctyp_opname

   let rec mk_sig_item =
      let sig_subsig_op = mk_caml_op "sig_subsig" in
      let sig_open_op = mk_caml_op "sig_open" in
      let sig_value_op = mk_caml_op "sig_value"
      in function
         (<:sig_item< declare $list:lsi$ end >>) ->
            mk_simple_term sig_subsig_op (List.map mk_sig_item lsi)
       | (<:sig_item< open $longid:id$ $itemattrs:_$>>) ->
            mk_simple_term sig_open_op [mk_longid_term id]
       | (<:sig_item< value $s$ : $t$ >>) ->
            mk_simple_named_term sig_value_op s [mk_type t]
       | _ -> mk_stub_term sig_opname

(*
   let mk_me_item me =
      let rec aux () = function
         (<:module_expr< $me1$ . $me2$ >>) ->
            Printf.sprintf "%a.%a" aux me1 aux me2
       | (<:module_expr< $me1$ $me2$ >>) ->
            Printf.sprintf "%a(%a)" aux me1 aux me2
       | (<:module_expr< $uid:s$ >>) ->
            s
       | _ -> "#< UNKNOWN >"
      in mk_ident_term (aux () me)
*)
   let mk_str_item =
      let str_external_op = mk_caml_op "str_ext"
      in fun vars -> function
         (<:str_item< external $s$ : $t$ = $list:ls$ $itemattrs:_$ >>) ->
            mk_simple_named_term str_external_op s (mk_type t :: List.map mk_simple_string ls)
       | _ -> mk_stub_term str_opname


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
   let def_str_item = <:str_item< declare $list:[]$ end >>

   (* TODO: these are going to be removed *)
   let dest_expr (_ : ToTerm.TermType.term) = <:expr< $lid: "stub"$ >>
   (* let dest_patt (a : ToTerm.TermType.term) = <:patt< $lid: "stub"$ >>, a *)
   let dest_type (_ : ToTerm.TermType.term) = <:ctyp< $lid: "stub"$ >>
   let dest_sig  (_ : ToTerm.TermType.term) = raise (Failure "sig")
   let dest_str  (_ : ToTerm.TermType.term) = raise (Failure "str")
   let dest_mt   (_ : ToTerm.TermType.term) = raise (Failure "mt")
   let dest_me   (_ : ToTerm.TermType.term) = raise (Failure "me")

   (* let mk_sig_item _ = mk_stub_term sig_opname *)
   (* let mk_str_item _ _ = mk_stub_term str_opname *)
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
   (* let patt_of_term             = wrap_error "patt_of_term" dest_patt *)
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
   (* let term_of_patt = mk_patt *)
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
