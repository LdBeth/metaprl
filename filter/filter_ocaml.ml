(*
 * Convert between terms and ocaml asts.
 *)

open MLast

open Opname
open Term

(*
 * All errors pass through here.
 *)
exception FormatError of string * term

(************************************************************************
 * BASIC TERM OPERATIONS						*
 ************************************************************************)

(*
 * OCaml operators.
 *)
let mk_ocaml_op s = mk_opname s nil_opname

(************************************************************************
 * TERM DESTRUCTORS							*
 ************************************************************************)

(*
 * Standard term ops.
 *)
let some_op = mk_ocaml_op "some"
let none_op = mk_ocaml_op "none"
let true_op = mk_ocaml_op "true"
let false_op = mk_ocaml_op "false"
let list_op  = mk_ocaml_op "list"

let cons_op = mk_ocaml_op "cons"
let nil_op = mk_ocaml_op "nil"

(*
 * Loc has two integer describing character offsets.
 * Ignore remaining params.
 *)
let dest_loc t =
   let { term_op = op } = dest_term t in
      match dest_op op with
         { op_params = p1 :: p2 :: _ } ->
            begin
               match dest_param p1, dest_param p2 with
                  Number start, Number finish ->
                     start, finish
                | _ ->
                     raise (FormatError ("dest_loc: needs two numbers", t))
            end
       | _ ->
            raise (FormatError ("dest_loc: need at least two parameters", t))

(*
 * Location and string take exactly three params.
 *)
let dest_loc_string t =
   let { term_op = op } = dest_term t in
   let { op_params = params } = dest_op op in
      match List.map dest_param params with
         [Number start; Number finish; String s] ->
            (start, finish), s
       | _ ->
            raise (FormatError ("dest_loc_string: needs two numbers and a string", t))

(*
 * Optional argument.
 *)
let dest_opt f =
   let dest t =
      let op = opname_of_term t in
         if op == none_op then
            None
         else
            Some (f (one_subterm t))
   in
      dest

(*
 * Strings.
 *)
let dest_string =
   dest_string_param

let dest_string_opt =
   dest_opt dest_string

(*
 * Variables are wrapped.
 *)
let dest_var t =
   Term.dest_var (one_subterm t)

(*
 * Integers are also wrapped.
 *)
let dest_loc_int t =
   let { term_op = op } = dest_term t in
   let { op_params = params } = dest_op op in
      match List.map dest_param params with
         [Number start; Number finish; Number i] ->
            (start, finish), string_of_int i
       | _ ->
            raise (FormatError ("dest_loc_int: needs three numbers", t))

(*
 * For looking up destructors in a hashtable.
 *)
let dest_tbl code table t =
   try (Hashtbl.find table (opname_of_term t)) t with
      Not_found ->
         raise (FormatError ("Filter_ocaml.dest_" ^ code ^ " : unrecognized opname", t))
    | TermMatch (_, t, s) ->
         raise (FormatError ("Filter_ocaml.dest_" ^ code ^ " : " ^ s, t))


let add_tbl table name f =
   let opname = mk_ocaml_op name in
      Hashtbl.add table opname f;
      opname

(*
 * Hashtables for destructing terms.
 *)
let expr_table = Hashtbl.create 17
let patt_table = Hashtbl.create 17
let type_table = Hashtbl.create 17
let sig_table  = Hashtbl.create 17
let str_table  = Hashtbl.create 17
let mt_able    = Hashtbl.create 17
let me_table   = Hashtbl.create 17
let wc_table   = Hashtbl.create 5
let ctf_table  = Hashtbl.create 17
let cf_table   = Hashtbl.create 17

(*
 * Expressions.
 *)
let rec dest_proj_expr t =
   let loc = dest_loc t in
   let e1, e2 = two_subterms t in
      <:expr< $dest_expr e1$ . $dest_expr e2$ >>

and dest_apply_expr t =
   let loc = dest_loc t in
   let e1, e2 = two_subterms t in
      <:expr< $dest_expr e1$ $dest_expr e2$ >>

and dest_array_subscript_expr t =
   let loc = dest_loc t in
   let e1, e2 = two_subterms t in
      <:expr< $dest_expr e1$ .( $dest_expr e2$ ) >>

and dest_array_expr t =
   let loc = dest_loc t in
   let el = List.map dest_expr (subterms_of_term t) in
      <:expr< [| $list:el$ |] >>

and dest_assign_expr t =
   let loc = dest_loc t in
   let e1, e2 = two_subterms t in
      <:expr< $dest_expr e1$ := $dest_expr e2$ >>

and dest_char_expr t =
   let loc, s = dest_loc_string t in
      if String.length s = 0 then
         raise (FormatError ("dest_char_expr: string is empty", t));
      <:expr< $chr:s.[0]$ >>

and dest_coerce_class_expr t =
   let loc = dest_loc t in
   let e, t = two_subterms t in
      <:expr< ( $dest_expr e$ :> $dest_type t$ ) >>

and dest_float_expr t =
   let loc = dest_loc t in
      <:expr< $flo: dest_string (one_subterm t)$ >>

and dest_upto_expr t =
   let loc = dest_loc t in
   let e1, e2, v, e3 = dest_dep0_dep0_dep1_any_term t in
   let el = subterms_of_term e3 in
      <:expr< for $v$ = $dest_expr e1$ $to:true$ $dest_expr e2$ do $list: List.map dest_expr el$ done >>

and dest_downto_expr t =
   let loc = dest_loc t in
   let e1, e2, v, e3 = dest_dep0_dep0_dep1_any_term t in
   let el = subterms_of_term e3 in
      <:expr< for $v$ = $dest_expr e1$ $to:false$ $dest_expr e2$ do $list: List.map dest_expr el$ done >>

and dest_fun_expr t =
   let loc = dest_loc t in
   let pwel = subterms_of_term t in
      <:expr< fun [ $list: List.map dest_pwe pwel$ ] >>

and dest_if_expr t =
   let loc = dest_loc t in
   let e1, e2, e3 = three_subterms t in
      <:expr< if $dest_expr e1$ then $dest_expr e2$ else $dest_expr e3$ >>

and dest_int_expr t =
   let loc, i = dest_loc_int t in
      <:expr< $int:i$ >>

and dest_let_expr t =
   let loc = dest_loc t in
   let sub = subterms_of_term t in
   let pe, e = List_util.split_last sub in
      <:expr< let $rec:false$ $list: List.map dest_pe pe$ in $dest_expr e$ >>

and dest_letrec_expr t =
   let loc = dest_loc t in
   let sub = subterms_of_term t in
   let pe, e = List_util.split_last sub in
      <:expr< let $rec:true$ $list: List.map dest_pe pe$ in $dest_expr e$ >>

and dest_lid_expr t =
   let loc = dest_loc t in
      <:expr< $lid:dest_var t$ >>

and dest_match_expr t =
   let loc = dest_loc t in
      match subterms_of_term t with
         e :: pwel ->
            <:expr< match $dest_expr e$ with [ $list: List.map dest_pwe pwel$ ] >>
       | [] ->
           raise (FormatError ("match needs an argument", t))

and dest_new_expr t =
   let loc = dest_loc t in
      <:expr< new $dest_expr (one_subterm t)$ >>

and dest_stream_expr t =
   let loc = dest_loc t in
   let sel = subterms_of_term t in
      <:expr< {< $list: List.map dest_se sel$ >} >>

and dest_record_expr t =
   let loc = dest_loc t in
   let eel = subterms_of_term t in
      <:expr< { $list: List.map dest_ee eel$ } >>

and dest_seq_expr t =
   let loc = dest_loc t in
   let el = subterms_of_term t in
   let el', e = List_util.split_last el in
      <:expr< do $list:List.map dest_expr el'$ return $dest_expr e$ >>

and dest_select_expr t =
   let loc = dest_loc t in
   let e, s = two_subterms t in
      <:expr< $dest_expr e$ # $dest_string s$ >>

and dest_string_subscript_expr t =
   let loc = dest_loc t in
   let e1, e2 = two_subterms t in
      <:expr< $dest_expr e1$ .[ $dest_expr e2$ ] >>

and dest_string_expr t =
   let loc, s = dest_loc_string t in
      <:expr< $str:s$ >>

and dest_try_expr t =
   let loc = dest_loc t in
      match subterms_of_term t with
         e :: pwel ->
   	    <:expr< try $dest_expr e$ with [ $list: List.map dest_pwe pwel$ ] >>
       | [] ->
            raise (FormatError ("try needs an argument", t))

and dest_tuple_expr t =
   let loc = dest_loc t in
   let el = subterms_of_term t in
      <:expr< ( $list: List.map dest_expr el$ ) >>

and dest_cast_expr t =
   let loc = dest_loc t in
   let e, t = two_subterms t in
      <:expr< ( $dest_expr e$ : $dest_type t$ ) >>

and dest_uid_expr t =
   let loc = dest_loc t in
      <:expr< $uid:dest_var t$ >>

and dest_while_expr t =
   let loc = dest_loc t in
   let e, el = two_subterms t in
      <:expr< while $dest_expr e$ do $list: List.map dest_expr (subterms_of_term el)$ done >>

(*
 * Patterns.
 *)
and dest_proj_patt t =
   let loc = dest_loc t in
   let p1, p2 = two_subterms t in
      <:patt< $dest_patt p1$ . $dest_patt p2$ >>

and dest_as_patt t =
   let loc = dest_loc t in
   let p1, p2 = two_subterms t in
      <:patt< ( $dest_patt p1$ as $dest_patt p2$ ) >>

and dest_wildcard_patt t =
   let loc = dest_loc t in
      <:patt< _ >>

and dest_apply_patt t =
   let loc = dest_loc t in
   let p1, p2 = two_subterms t in
      <:patt< $dest_patt p1$ $dest_patt p2$ >>

and dest_char_patt t =
   let loc, s = dest_loc_string t in
      if String.length s = 0 then
         raise (FormatError ("dest_char_patt: string needs at least one char", t));
      <:patt< $chr:s.[0]$ >>

and dest_int_patt t =
   let loc, i = dest_loc_int t in
      <:patt< $int:i$ >>

and dest_lid_patt t =
   let loc = dest_loc t in
      <:patt< $lid:dest_var t$ >>

and dest_choice_patt t =
   let loc = dest_loc t in
   let p1, p2 = two_subterms t in
      <:patt< $dest_patt p1$ | $dest_patt p2$ >>

and dest_range_patt t =
   let loc = dest_loc t in
   let p1, p2 = two_subterms t in
      <:patt< $dest_patt p1$ .. $dest_patt p2$ >>

and dest_record_patt t =
   let loc = dest_loc t in
   let ppl = subterms_of_term t in
      <:patt< { $list: List.map dest_pp ppl$ } >>

and dest_string_patt t =
   let loc, s = dest_loc_string t in
      <:patt< $str:s$ >>

and dest_tuple_patt t =
   let loc = dest_loc t in
   let pl = subterms_of_term t in
      <:patt< ( $list: List.map dest_patt pl$ ) >>

and dest_cast_patt t =
   let loc = dest_loc t in
   let p, t = two_subterms t in
      <:patt< ( $dest_patt p$ : $dest_type t$ ) >>

and dest_uid_patt t =
   let loc = dest_loc t in
      <:patt< $uid:dest_var t$ >>

(*
 * Types.
 *)
and dest_proj_type t =
   let loc = dest_loc t in
   let t1, t2 = two_subterms t in
      <:ctyp< $dest_type t1$ . $dest_type t2$ >>

and dest_as_type t =
   let loc = dest_loc t in
   let t1, t2 = two_subterms t in
      <:ctyp< $dest_type t1$ as $dest_type t2$ >>

and dest_wildcard_type t =
   let loc = dest_loc t in
      <:ctyp< _ >>

and dest_apply_type t =
   let loc = dest_loc t in
   let t1, t2 = two_subterms t in
      <:ctyp< $dest_type t1$ $dest_type t2$ >>

and dest_fun_type t =
   let loc = dest_loc t in
   let t1, t2 = two_subterms t in
      <:ctyp< $dest_type t1$ -> $dest_type t2$ >>

and dest_class_id_type t =
   let loc = dest_loc t in
      <:ctyp< # $dest_type t$ >>

and dest_lid_type t =
   let loc = dest_loc t in
      <:ctyp< $lid:dest_var t$ >>

and dest_param_type t =
   let loc, s = dest_loc_string t in
      <:ctyp< '$s$ >>

and dest_equal_type t =
   let loc = dest_loc t in
   let t1, t2 = two_subterms t in
      <:ctyp< $dest_type t1$ == $dest_type t2$ >>

and dest_object_tt_type t =
   let loc = dest_loc t in
   let stl = subterms_of_term t in
      <:ctyp< < $list: List.map dest_st stl$ $dd:true$ > >>

and dest_object_ff_type t =
   let loc = dest_loc t in
   let stl = subterms_of_term t in
      <:ctyp< < $list: List.map dest_st stl$ $dd:false$ > >>

and dest_record_type t =
   let loc = dest_loc t in
   let sbtl = subterms_of_term t in
      <:ctyp< { $list: List.map dest_sbt sbtl$ } >>

and dest_list_type t =
   let loc = dest_loc t in
   let stll = subterms_of_term t in
      <:ctyp< [ $list: List.map dest_stl stll$ ] >>

and dest_prod_type t =
   let loc = dest_loc t in
   let tl = subterms_of_term t in
      <:ctyp< ( $list: List.map dest_type tl$ ) >>

and dest_uid_type t =
   let loc = dest_loc t in
      <:ctyp< $uid:dest_var t$ >>

(*
 * Signatures.
 *)
and dest_class_sig t =
   let loc = dest_loc t in
   let ctl = subterms_of_term t in
      <:sig_item< class $list: List.map dest_class_type ctl$ >>

and dest_subsig_sig t =
   let loc = dest_loc t in
   let sl = subterms_of_term t in
      <:sig_item< declare $list: List.map dest_sig sl$ end >>

and dest_exception_sig t =
   let loc, s = dest_loc_string t in
   let tl = subterms_of_term t in
      <:sig_item< exception $s$ of $list: List.map dest_type tl$ >>

and dest_external_sig t =
   let loc, s = dest_loc_string t in
      match subterms_of_term t with
         t :: sl ->
            <:sig_item< external $s$ : $dest_type t$ = $list: List.map dest_string sl$ >>
       | _ ->
           raise (FormatError ("external requires a name and a type", t))

and dest_module_sig t =
   let loc, s = dest_loc_string t in
   let mt = one_subterm t in
      <:sig_item< module $s$ : $dest_mt mt$ >>

and dest_module_type_sig t =
   let loc, s = dest_loc_string t in
   let mt = one_subterm t in
      <:sig_item< module type $s$ = $dest_mt mt$ >>

and dest_open_sig t =
   let loc = dest_loc t in
   let sl = subterms_of_term t in
      <:sig_item< open $List.map dest_string sl$ >>

and dest_type_sig t =
   let loc = dest_loc t in
   let ssltl = subterms_of_term t in
      <:sig_item< type $list: List.map dest_sslt ssltl$ >>

and dest_value_sig t =
   let loc, s = dest_loc_string t in
   let t = one_subterm t in
      <:sig_item< value $s$ : $dest_type t$ >>

(*
 * Structure items.
 *)
and dest_class_str t =
   let loc = dest_loc t in
   let cdl = subterms_of_term t in
      <:str_item< class $list: List.map dest_class cdl$ >>

and dest_substruct_str t =
   let loc = dest_loc t in
   let stl = subterms_of_term t in
      <:str_item< declare $list: List.map dest_str stl$ end >>

and dest_exception_str t =
   let loc, s = dest_loc_string t in
   let tl = subterms_of_term t in
      <:str_item< exception $s$ of $list: List.map dest_type tl$ >>

and dest_expr_str t =
   let loc = dest_loc t in
      <:str_item< $exp: dest_expr (one_subterm t)$ >>

and dest_external_str t =
   let loc, s = dest_loc_string t in
      match subterms_of_term t with
         t :: sl ->
            <:str_item< external $s$ : $dest_type t$ = $list: List.map dest_string sl$ >>
       | _ ->
            raise (FormatError ("external requires a name and type", t))

and dest_module_str t =
   let loc, s = dest_loc_string t in
   let me = one_subterm t in
      <:str_item< module $s$ = $dest_me me$ >>

and dest_module_type_str t =
   let loc, s = dest_loc_string t in
   let mt = one_subterm t in
      <:str_item< module type $s$ = $dest_mt mt$ >>

and dest_open_str t =
   let loc = dest_loc t in
   let sl = subterms_of_term t in
      <:str_item< open $List.map dest_string sl$ >>

and dest_type_str t =
   let loc = dest_loc t in
   let ssltl = subterms_of_term t in
      <:str_item< type $list: List.map dest_sslt ssltl$ >>

and dest_letrec_str t =
   let loc = dest_loc t in
   let pel = subterms_of_term t in
      <:str_item< value $rec:true$ $list: List.map dest_pe pel$ >>

and dest_let_str t =
   let loc = dest_loc t in
   let pel = subterms_of_term t in
      <:str_item< value $rec:false$ $list: List.map dest_pe pel$ >>

(*
 * Module types.
 *)
and dest_proj_mt t =
   let loc = dest_loc t in
   let mt1, mt2 = two_subterms t in
      <:module_type< $dest_mt mt1$ . $dest_mt mt2$ >>

and dest_apply_mt t =
   let loc = dest_loc t in
   let mt1, mt2 = two_subterms t in
      <:module_type< $dest_mt mt1$ $dest_mt mt2$ >>

and dest_functor_mt t =
   let loc = dest_loc t in
   let v, mt1, mt2 = dest_dep0_dep1_any_term t in
      <:module_type< functor ($v$ : $dest_mt mt1$) -> $dest_mt mt2$ >>

and dest_lid_mt t =
   let loc = dest_loc t in
      <:module_type< $lid:dest_var t$ >>

and dest_sig_mt t =
   let loc = dest_loc t in
   let sil = subterms_of_term t in
      <:module_type< sig $list: List.map dest_sig sil$ end >>

and dest_uid_mt t =
   let loc = dest_loc t in
      <:module_type< $uid:dest_var t$ >>

and dest_with_mt t =
   let loc = dest_loc t in
      match subterms_of_term t with
         mt :: wcl ->
            <:module_type< $dest_mt mt$ with $list: List.map dest_wc wcl$ >>
       | [] ->
           raise (FormatError ("module \"with\" clause must have type", t))

and dest_type_wc t =
   let loc = dest_loc t in
   let sl1, sl2, t = three_subterms t in
   let sl1' = List.map dest_string (subterms_of_term sl1) in
   let sl2' = List.map dest_string (subterms_of_term sl2) in
      WcTyp (loc, sl1', sl2', dest_type t)

and dest_module_wc t =
   let loc = dest_loc t in
   let sl1, mt = two_subterms t in
      WcMod (loc, List.map dest_string (subterms_of_term sl1), dest_mt mt)

(*
 * Module expressions.
 *)
and dest_proj_me t =
   let loc = dest_loc t in
   let me1, me2 = two_subterms t in
      <:module_expr< $dest_me me1$ . $dest_me me2$ >>

and dest_apply_me t =
   let loc = dest_loc t in
   let me1, me2 = two_subterms t in
      <:module_expr< $dest_me me1$ $dest_me me2$ >>

and dest_functor_me t =
   let loc = dest_loc t in
   let v, mt, me = dest_dep0_dep1_any_term t in
      <:module_expr< functor ($v$ : $dest_mt mt$ ) -> $dest_me me$ >>

(*
and dest_lid_me t =
   let loc = dest_loc t in
      <:module_expr< $lid:dest_var t$ >>
*)

and dest_struct_me t =
   let loc = dest_loc t in
   let stl = subterms_of_term t in
      <:module_expr< struct $list: List.map dest_str stl$ end >>

and dest_cast_me t =
   let loc = dest_loc t in
   let me, mt = two_subterms t in
      <:module_expr< ( $dest_me me$ : $dest_mt mt$) >>

and dest_uid_me t =
   let loc = dest_loc t in
      <:module_expr< $uid:dest_var t$ >>

(*
 * Class type.
 *)
and dest_class_type t =
   let loc = dest_loc t in
      match subterms_of_term t with
         [s; sl; tl1; so; ctfl; b1; b2] ->
             { ctLoc = loc;
               ctNam = dest_string s;
               ctPrm = List.map dest_string (subterms_of_term sl);
               ctArg = List.map dest_type (subterms_of_term tl1);
               ctTyc = dest_string_opt so;
               ctFld = List.map dest_ctf (subterms_of_term ctfl);
               ctVir = dest_bool b1;
               ctCls = dest_bool b2
             }
       | _ ->
             raise (FormatError ("class type format not recognized", t))

and dest_ctr_ctf t =
   let loc = dest_loc t in
   let s, t = two_subterms t in
      CtCtr (loc, dest_string s, dest_type t)

and dest_inh_ctf t =
   let loc = dest_loc t in
   let t = one_subterm t in
      CtInh (loc, dest_type t)

and dest_mth_ctf t =
   let loc = dest_loc t in
   let s, t = two_subterms t in
      CtMth (loc, dest_string s, dest_type t)

and dest_val_ctf t =
   let loc = dest_loc t in
   let s, b1, b2, ot = four_subterms t in
      CtVal (loc, dest_string s, dest_bool b1, dest_bool b2, dest_type_opt ot)

and dest_vir_ctf t =
   let loc = dest_loc t in
   let s, t = two_subterms t in
      CtVir (loc, dest_string s, dest_type t)

(*
 * Classes.
 *)
and dest_class t =
   let loc = dest_loc t in
      match subterms_of_term t with
         [s; sl1; pl1; so1; so2; cfl; b1; b2] ->
             { cdLoc = loc;
               cdNam = dest_string s;
               cdPrm = List.map dest_string (subterms_of_term sl1);
               cdArg = List.map dest_patt (subterms_of_term pl1);
               cdSlf = dest_string_opt so1;
               cdTyc = dest_string_opt so2;
               cdFld = List.map dest_cf (subterms_of_term cfl);
               cdVir = dest_bool b1;
               cdCls = dest_bool b2
             }
       | _ ->
             raise (FormatError ("class format not recognized", t))

and dest_ctr_cf t =
   let loc = dest_loc t in
   let s, t = two_subterms t in
      CfCtr (loc, dest_string s, dest_type t)

and dest_inh_cf t =
   let loc = dest_loc t in
   let t, e, so = three_subterms t in
      CfInh (loc, dest_type t, dest_expr e, dest_string_opt so)

and dest_mth_cf t =
   let loc = dest_loc t in
   let s, e = two_subterms t in
      CfMth (loc, dest_string s, dest_expr e)

and dest_val_cf t =
   let loc = dest_loc t in
   let s, b1, b2, eo = four_subterms t in
      CfVal (loc, dest_string s, dest_bool b1, dest_bool b2, dest_expr_opt eo)

and dest_vir_cf t =
   let loc = dest_loc t in
   let s, t = two_subterms t in
      CfVir (loc, dest_string s, dest_type t)

(*
 * Utilities.
 *)
and dest_pwe t =
   let p, wo, e = three_subterms t in
      dest_patt p, dest_expr_opt wo, dest_expr e

and dest_pe t =
   let p, e = two_subterms t in
      dest_patt p, dest_expr e

and dest_se t =
   let s, e = two_subterms t in
      dest_string s, dest_expr e

and dest_ee t =
   let e1, e2 = two_subterms t in
      dest_expr e1, dest_expr e2

and dest_pp t =
   let p1, p2 = two_subterms t in
      dest_patt p1, dest_patt p2

and dest_st t =
   let s, t = two_subterms t in
      dest_string s, dest_type t

and dest_sbt t =
   let s, b, t = three_subterms t in
      dest_string s, dest_bool b, dest_type t

and dest_stl t =
   match subterms_of_term t with
      s :: tl ->
         dest_string s, List.map dest_type tl
    | [] ->
         raise (FormatError ("Filter_ocaml.dest_stl: requires a subterm", t))

and dest_sslt t =
   match subterms_of_term t with
      s :: slt ->
         let sl, t = List_util.split_last slt in
            dest_string s, List.map dest_string sl, dest_type t
   | [] ->
      raise (FormatError ("Filter_ocaml.dest_sslt: requires a subterm", t))

and dest_expr_opt t = dest_opt dest_expr t

and dest_type_opt t = dest_opt dest_type t

and dest_bool t =
   let op = opname_of_term t in
      not (op == false_op)

(*
 * Destruction uses hashtables.
 *)
and dest_expr t = dest_tbl "expr" expr_table t
and dest_patt t = dest_tbl "patt" patt_table t
and dest_type t = dest_tbl "type" type_table t
and dest_sig  t = dest_tbl "sig"  sig_table  t
and dest_str  t = dest_tbl "str"  str_table  t
and dest_mt   t = dest_tbl "mt"   mt_able    t
and dest_me   t = dest_tbl "me"   me_table   t
and dest_wc   t = dest_tbl "wc"   wc_table   t
and dest_ctf  t = dest_tbl "ctf"  ctf_table  t
and dest_cf   t = dest_tbl "cf"   cf_table   t

and add_expr name f = add_tbl expr_table name f
and add_patt name f = add_tbl patt_table name f
and add_type name f = add_tbl type_table name f
and add_sig  name f = add_tbl sig_table  name f
and add_str  name f = add_tbl str_table  name f
and add_mt   name f = add_tbl mt_able    name f
and add_me   name f = add_tbl me_table   name f
and add_wc   name f = add_tbl wc_table   name f
and add_ctf  name f = add_tbl ctf_table  name f
and add_cf   name f = add_tbl cf_table   name f

(************************************************************************
 * OPERATOR NAMES							*
 ************************************************************************)

let expr_char_op		= add_expr "char" 		dest_char_expr
let expr_float_op		= add_expr "float"		dest_float_expr
let expr_int_op			= add_expr "int"		dest_int_expr
let expr_string_op              = add_expr "string"             dest_string_expr
let expr_lid_op			= add_expr "lid"		dest_lid_expr
let expr_uid_op			= add_expr "uid"		dest_uid_expr

let expr_proj_op 		= add_expr "proj"		dest_proj_expr
let expr_apply_op 		= add_expr "apply"		dest_apply_expr
let expr_array_subscript_op 	= add_expr "array_subscript"	dest_array_subscript_expr
let expr_array_op 		= add_expr "array"		dest_array_expr
let expr_assign_op		= add_expr "assign"		dest_assign_expr
let expr_coerce_class_op	= add_expr "coerce_class"	dest_coerce_class_expr
let expr_upto_op		= add_expr "for_upto"		dest_upto_expr
let expr_downto_op		= add_expr "for_downto"		dest_downto_expr
let expr_fun_op			= add_expr "fun"      	        dest_fun_expr
let expr_if_op			= add_expr "if"			dest_if_expr
let expr_letrec_op		= add_expr "letrec"		dest_letrec_expr
let expr_let_op			= add_expr "let"		dest_let_expr
let expr_match_op		= add_expr "match"		dest_match_expr
let expr_new_op			= add_expr "new"		dest_new_expr
let expr_stream_op		= add_expr "stream"		dest_stream_expr
let expr_record_op		= add_expr "record"		dest_record_expr
let expr_seq_op			= add_expr "sequence"		dest_seq_expr
let expr_select_op		= add_expr "select"		dest_select_expr
let expr_string_subscript_op	= add_expr "string_subscript"	dest_string_subscript_expr
let expr_try_op			= add_expr "try"		dest_try_expr
let expr_tuple_op		= add_expr "tuple"		dest_tuple_expr
let expr_cast_op		= add_expr "cast"		dest_cast_expr
let expr_while_op		= add_expr "while"		dest_while_expr

let patt_int_op			= add_patt "patt_int"           dest_int_patt
let patt_string_op		= add_patt "patt_string"        dest_string_patt
let patt_char_op		= add_patt "patt_char"          dest_char_patt
let patt_lid_op			= add_patt "patt_lid"           dest_lid_patt
let patt_uid_op			= add_patt "patt_uid"           dest_uid_patt
let patt_proj_op		= add_patt "patt_proj"          dest_proj_patt
let patt_as_op			= add_patt "patt_as"            dest_as_patt
let patt_wildcard_op		= add_patt "patt_wildcard"      dest_wildcard_patt
let patt_apply_op		= add_patt "patt_apply"         dest_apply_patt
let patt_choice_op		= add_patt "patt_choice"        dest_choice_patt
let patt_range_op		= add_patt "patt_range"         dest_range_patt
let patt_record_op		= add_patt "patt_record"        dest_record_patt
let patt_tuple_op		= add_patt "patt_tuple"         dest_tuple_patt
let patt_cast_op		= add_patt "patt_cast"          dest_cast_patt

let type_lid_op                 = add_type "type_lid"           dest_lid_type
let type_uid_op                 = add_type "type_uid"           dest_uid_type
let type_proj_op		= add_type "type_proj"          dest_proj_type
let type_as_op			= add_type "type_as"            dest_as_type
let type_wildcard_op		= add_type "type_wildcard"      dest_wildcard_type
let type_apply_op		= add_type "type_apply_op"      dest_apply_type
let type_fun_op                 = add_type "type_fun_op"        dest_fun_type
let type_class_id_op		= add_type "type_class_id"      dest_class_id_type
let type_param_op		= add_type "type_param"         dest_param_type
let type_equal_op		= add_type "type_equal"         dest_equal_type
let type_object_tt_op		= add_type "type_object_tt"     dest_object_tt_type
let type_object_ff_op		= add_type "type_object_ff"     dest_object_ff_type
let type_record_op		= add_type "type_record"        dest_record_type
let type_list_op		= add_type "type_list"          dest_list_type
let type_prod_op		= add_type "type_prod"          dest_prod_type

let sig_class_op		= add_sig "sig_class"           dest_class_sig
let sig_subsig_op		= add_sig "sig_subsig"          dest_subsig_sig
let sig_exception_op		= add_sig "sig_exception"       dest_exception_sig
let sig_external_op		= add_sig "sig_external"        dest_external_sig
let sig_module_op		= add_sig "sig_module"          dest_module_sig
let sig_module_type_op		= add_sig "sig_module_type"     dest_module_type_sig
let sig_open_op			= add_sig "sig_open"            dest_open_sig
let sig_type_op			= add_sig "sig_type"            dest_type_sig
let sig_value_op		= add_sig "sig_value"           dest_value_sig

let str_class_op		= add_str "str_class"           dest_class_str
let str_substruct_op		= add_str "str_substruct"       dest_substruct_str
let str_exception_op		= add_str "str_exception"       dest_exception_str
let str_expr_op			= add_str "str_expr"            dest_expr_str
let str_external_op		= add_str "str_external"        dest_external_str
let str_module_op		= add_str "str_module"          dest_module_str
let str_module_type_op		= add_str "str_module_type"     dest_module_type_str
let str_open_op			= add_str "str_open"            dest_open_str
let str_type_op			= add_str "str_type"            dest_type_str
let str_letrec_op		= add_str "str_letrec"          dest_letrec_str
let str_let_op			= add_str "str_let"             dest_let_str

let mt_lid_op			= add_mt "mt_lid"               dest_lid_mt
let mt_uid_op			= add_mt "mt_uid"               dest_uid_mt
let mt_proj_op			= add_mt "mt_proj"              dest_proj_mt
let mt_apply_op                 = add_mt "mt_apply"             dest_apply_mt
let mt_functor_op		= add_mt "mt_functor"           dest_functor_mt
let mt_sig_op			= add_mt "mt_sig"               dest_sig_mt
let mt_type_with_op		= add_mt "mt_type_with"         dest_with_mt

let wc_type_op     		= add_wc "wc_type"              dest_type_wc
let wc_module_op   		= add_wc "wc_module"            dest_module_wc

(* let me_lid_op			= add_me "me_lid"               dest_lid_me *)
let me_uid_op			= add_me "me_uid"               dest_uid_me
let me_proj_op			= add_me "me_proj"              dest_proj_me
let me_apply_op			= add_me "me_apply"             dest_apply_me
let me_functor_op		= add_me "me_functor"           dest_functor_me
let me_struct_op		= add_me "me_struct"            dest_struct_me
let me_cast_op			= add_me "me_cast"              dest_cast_me

let class_type_op		= mk_ocaml_op "class_type"

let ctf_ctr_op                  = add_ctf "class_type_ctr"      dest_ctr_ctf
let ctf_inh_op                  = add_ctf "class_type_inh"      dest_inh_ctf
let ctf_mth_op                  = add_ctf "class_type_mth"      dest_mth_ctf
let ctf_val_op                  = add_ctf "class_type_val"      dest_val_ctf
let ctf_vir_op                  = add_ctf "class_type_vir"      dest_vir_ctf

let class_op			= mk_ocaml_op "class"

let cf_ctr_op                   = add_cf "class_ctr"            dest_ctr_cf
let cf_inh_op                   = add_cf "class_inh"            dest_inh_cf
let cf_mth_op                   = add_cf "class_mth"            dest_mth_cf
let cf_val_op                   = add_cf "class_val"            dest_val_cf
let cf_vir_op                   = add_cf "class_vir"            dest_vir_cf

let pwe_op                      = mk_ocaml_op "pwe"
let pe_op                       = mk_ocaml_op "pe"
let se_op                       = mk_ocaml_op "se"
let ee_op                       = mk_ocaml_op "ee"
let pp_op                       = mk_ocaml_op "pp"
let st_op                       = mk_ocaml_op "st"
let sbt_op                      = mk_ocaml_op "sbt"
let stl_op                      = mk_ocaml_op "stl"
let sslt_op                     = mk_ocaml_op "sslt"

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
      Term.mk_simple_term none_op []
 | Some t ->
      Term.mk_simple_term some_op [f t]

(*
 * String without location.
 *)
let mk_loc_string opname (start, finish) s =
   let p1 = make_param (Number start) in
   let p2 = make_param (Number finish) in
   let p3 = make_param (String s) in
   let op = mk_op opname [p1; p2; p3] in
      mk_term op []

let mk_string opname s =
   let p1 = make_param (String s) in
   let op = mk_op opname [p1] in
      mk_term op []

let mk_string_opt op =
   mk_opt (mk_string op)

let mk_simple_string =
   mk_string expr_string_op

(*
 * Number with location.
 *)
let mk_loc_int opname (start, finish) i =
   let p1 = make_param (Number start) in
   let p2 = make_param (Number finish) in
   let p3 = make_param (Number (int_of_string i)) in
   let op = mk_op opname [p1; p2; p3] in
      mk_term op []

(*
 * List of terms.
 *)
let mk_list_term terms =
   Term.mk_simple_term expr_seq_op terms

(*
 * Variables are enclosed in terms that mark
 * the variable type.
 *)
let mk_var opname loc s =
   mk_any_term (mk_op_loc opname loc) [mk_var_term s]

(*
 * Compute a hash value from the struct.
 *)
let rec mk_expr expr =
   let loc = loc_of_expr expr in
      match expr with
         (<:expr< $e1$ . $e2$ >>) ->
            mk_simple_term expr_proj_op loc [mk_expr e1; mk_expr e2]
       | (<:expr< $e1$ $e2$ >>) ->
            mk_simple_term expr_apply_op loc [mk_expr e1; mk_expr e2]
       | (<:expr< $e1$ .( $e2$ ) >>) ->
            mk_simple_term expr_array_subscript_op loc [mk_expr e1; mk_expr e2]
       | (<:expr< [| $list:el$ |] >>) ->
            mk_simple_term expr_array_op loc (List.map mk_expr el)
       | (<:expr< $e1$ := $e2$ >>) ->
            mk_simple_term expr_assign_op loc [mk_expr e1; mk_expr e2]
       | (<:expr< $chr:c$ >>) ->
            mk_loc_string expr_char_op loc (String.make 1 c)
       | (<:expr< ( $e$ :> $t$ ) >>) ->
            mk_simple_term expr_coerce_class_op loc [mk_expr e; mk_type t]
       | (<:expr< $flo:s$ >>) ->
            mk_loc_string expr_float_op loc s
       | (<:expr< for $s$ = $e1$ $to:b$ $e2$ do $list:el$ done >>) ->
            let op = if b then expr_upto_op else expr_downto_op in
            let op_loc = mk_op_loc op loc in
            let el' = mk_list_term (List.map mk_expr el) in
                mk_dep0_dep0_dep1_any_term op_loc (mk_expr e1) (mk_expr e2) s el'
       | (<:expr< fun [ $list:pwel$ ] >>) ->
            mk_simple_term expr_fun_op loc (List.map mk_pwe pwel)
       | (<:expr< if $e1$ then $e2$ else $e3$ >>) ->
            mk_simple_term expr_if_op loc [mk_expr e1; mk_expr e1; mk_expr e3]
       | (<:expr< $int:s$ >>) ->
            mk_loc_int expr_int_op loc s
       | (<:expr< let $rec:b$ $list:pel$ in $e$ >>) ->
            let op = if b then expr_letrec_op else expr_let_op in
               mk_simple_term op loc ((List.map mk_pe pel) @ [mk_expr e])
       | (<:expr< $lid:s$ >>) ->
            mk_var expr_lid_op loc s
       | (<:expr< match $e$ with [ $list:pwel$ ] >>) ->
            mk_simple_term expr_match_op loc (mk_expr e :: List.map mk_pwe pwel)
       | (<:expr< new $e$ >>) ->
            mk_simple_term expr_new_op loc [mk_expr e]
       | (<:expr< {< $list:sel$ >} >>) ->
            mk_simple_term expr_stream_op loc (List.map mk_se sel)
       | (<:expr< { $list:eel$ } >>) ->
            mk_simple_term expr_record_op loc (List.map mk_ee eel)
       | (<:expr< do $list:el$ return $e$ >>) ->
            mk_simple_term expr_seq_op loc (List.map mk_expr el @ [mk_expr e])
       | (<:expr< $e$ # $i$ >>) ->
            mk_simple_term expr_select_op loc [mk_expr e; mk_string expr_string_op i]
       | (<:expr< $e1$ .[ $e2$ ] >>) ->
            mk_simple_term expr_string_subscript_op loc [mk_expr e1; mk_expr e2]
       | (<:expr< $str:s$ >>) ->
            mk_loc_string expr_string_op loc s
       | (<:expr< try $e$ with [ $list:pwel$ ] >>) ->
            mk_simple_term expr_try_op loc (mk_expr e :: List.map mk_pwe pwel)
       | (<:expr< ( $list:el$ ) >>) ->
            mk_simple_term expr_tuple_op loc (List.map mk_expr el)
       | (<:expr< ( $e$ : $t$ ) >>) ->
            mk_simple_term expr_cast_op loc [mk_expr e; mk_type t]
       | (<:expr< $uid:s$ >>) ->
            mk_var expr_uid_op loc s
       | (<:expr< while $e$ do $list:el$ done >>) ->
            mk_simple_term expr_while_op loc [mk_expr e; mk_list_term (List.map mk_expr el)]
       | MLast.ExAnt (_, e) ->
            Stdpp.raise_with_loc loc (Failure "Filter_ocaml.mk_expr: encountered an ExAnt")
      
and mk_patt patt =
   let loc = loc_of_patt patt in
      match patt with
         (<:patt< $p1$ . $p2$ >>) ->
            mk_simple_term patt_proj_op loc [mk_patt p1; mk_patt p2]
       | (<:patt< ( $p1$ as $p2$ ) >>) ->
            mk_simple_term patt_as_op loc [mk_patt p1; mk_patt p2]
       | (<:patt< _ >>) ->
            mk_simple_term patt_wildcard_op loc []
       | (<:patt< $p1$ $p2$ >>) ->
            mk_simple_term patt_apply_op loc [mk_patt p1; mk_patt p2]
       | (<:patt< $chr:c$ >>) ->
            mk_loc_string patt_char_op loc (String.make 1 c)
       | (<:patt< $int:s$ >>) ->
            mk_loc_int patt_int_op loc s
       | (<:patt< $lid:i$ >>) ->
            mk_var patt_lid_op loc i
       | (<:patt< $p1$ | $p2$ >>) ->
            mk_simple_term patt_choice_op loc [mk_patt p1; mk_patt p2]
       | (<:patt< $p1$ .. $p2$ >>) ->
            mk_simple_term patt_range_op loc [mk_patt p1; mk_patt p2]
       | (<:patt< { $list:ppl$ } >>) ->
            mk_simple_term patt_record_op loc (List.map mk_pp ppl)
       | (<:patt< $str:s$ >>) ->
            mk_loc_string patt_string_op loc s
       | (<:patt< ( $list:pl$ ) >>) ->
            mk_simple_term patt_tuple_op loc (List.map mk_patt pl)
       | (<:patt< ( $p$ : $t$ ) >>) ->
            mk_simple_term patt_cast_op loc [mk_patt p; mk_type t]
       | (<:patt< $uid:s$ >>) ->
            mk_var patt_uid_op loc s
       | MLast.PaAnt (_, p) ->
            Stdpp.raise_with_loc loc (Failure "Filter_ocaml:mk_patt: encountered PaAnt")
      
and mk_type t =
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
       | (<:ctyp< # $i$ >>) ->
            mk_simple_term type_class_id_op loc [mk_type i]
       | (<:ctyp< $lid:s$ >>) ->
            mk_var type_lid_op loc s
       | (<:ctyp< '$s$ >>) ->
            mk_loc_string type_param_op loc s
       | (<:ctyp< $t1$ == $t2$ >>) ->
            mk_simple_term type_equal_op loc [mk_type t1; mk_type t2]
       | (<:ctyp< < $list:stl$ $dd:b$ > >>) ->
            let op = if b then type_object_tt_op else type_object_ff_op in
               mk_simple_term op loc (List.map mk_st stl)
       | (<:ctyp< { $list:sbtl$ } >>) ->
            mk_simple_term type_record_op loc (List.map mk_sbt sbtl)
       | (<:ctyp< [ $list:stll$ ] >>) ->
            mk_simple_term type_list_op loc (List.map mk_stl stll)
       | (<:ctyp< ( $list:tl$ ) >>) ->
            mk_simple_term type_prod_op loc (List.map mk_type tl)
       | (<:ctyp< $uid:s$ >>) ->
            mk_var type_uid_op loc s
      
and mk_sig_item si =
   let loc = loc_of_sig_item si in
      match si with
         (<:sig_item< class $list:ctl$ >>) ->
            mk_simple_term sig_class_op loc (List.map mk_class_type ctl)
       | (<:sig_item< declare $list:sil$ end >>) ->
            mk_simple_term sig_subsig_op loc (List.map mk_sig_item sil)
       | (<:sig_item< exception $s$ of $list:tl$ >>) ->
            mk_simple_named_term sig_exception_op loc s (List.map mk_type tl)
       | (<:sig_item< external $s$ : $t$ = $list:sl$ >>) ->
            mk_simple_named_term sig_external_op loc s (mk_type t :: List.map mk_simple_string sl)
       | (<:sig_item< module $s$ : $mt$ >>) ->
            mk_simple_named_term sig_module_op loc s [mk_module_type mt]
       | (<:sig_item< module type $s$ = $mt$ >>) ->
            mk_simple_named_term sig_module_type_op loc s [mk_module_type mt]
       | (<:sig_item< open $sl$ >>) ->
            mk_simple_term sig_open_op loc (List.map mk_simple_string sl)
       | (<:sig_item< type $list:ssltl$ >>) ->
            mk_simple_term sig_type_op loc (List.map mk_sslt ssltl)
       | (<:sig_item< value $s$ : $t$ >>) ->
            mk_simple_named_term sig_value_op loc s [mk_type t]
      
and mk_str_item si =
   let loc = loc_of_str_item si in
      match si with
         (<:str_item< class $list:cdl$ >>) ->
            mk_simple_term str_class_op loc (List.map mk_class cdl)
       | (<:str_item< declare $list:stl$ end >>) ->
            mk_simple_term str_substruct_op loc (List.map mk_str_item stl)
       | (<:str_item< exception $s$ of $list:tl$ >>) ->
            mk_simple_named_term str_exception_op loc s (List.map mk_type tl)
       | (<:str_item< $exp:e$ >>) ->
            mk_simple_term str_expr_op loc [mk_expr e]
       | (<:str_item< external $s$ : $t$ = $list:sl$ >>) ->
            mk_simple_named_term str_external_op loc s (mk_type t :: List.map mk_simple_string sl)
       | (<:str_item< module $s$ = $me$ >>) ->
            mk_simple_named_term str_module_op loc s [mk_module_expr me]
       | (<:str_item< module type $s$ = $mt$ >>) ->
            mk_simple_named_term str_module_type_op loc s [mk_module_type mt]
       | (<:str_item< open $sl$ >>) ->
            mk_simple_term str_open_op loc (List.map mk_simple_string sl)
       | (<:str_item< type $list:ssltl$ >>) ->
            mk_simple_term str_type_op loc (List.map mk_sslt ssltl)
       | (<:str_item< value $rec:b$ $list:pel$ >>) ->
            let op = if b then str_letrec_op else str_let_op in
      	       mk_simple_term op loc (List.map mk_pe pel)
          
and mk_module_type mt =
   let loc = loc_of_module_type mt in
      match mt with
         (<:module_type< $mt1$ . $mt2$ >>) ->
            mk_simple_term mt_proj_op loc [mk_module_type mt1; mk_module_type mt2]
       | (<:module_type< $mt1$ $mt2$ >>) ->
            mk_simple_term mt_apply_op loc [mk_module_type mt1; mk_module_type mt2]
       | (<:module_type< functor ( $s$ : $mt1$ ) -> $mt2$ >>) ->
            let op_loc = mk_op_loc mt_functor_op loc in
               mk_dep0_dep1_any_term op_loc s (mk_module_type mt1) (mk_module_type mt2)
       | (<:module_type< $lid:i$ >>) ->
            mk_var mt_lid_op loc i
       | (<:module_type< sig $list:sil$ end >>) ->
            mk_simple_term mt_sig_op loc (List.map mk_sig_item sil)
       | (<:module_type< $uid:i$ >>) ->
            mk_var mt_uid_op loc i
       | (<:module_type< $mt$ with $list:wcl$ >>) ->
            mk_simple_term mt_type_with_op loc (mk_module_type mt :: List.map mk_wc wcl)
      
and mk_wc = function
   WcTyp (loc, sl1, sl2, t) ->
      let sl1' = mk_list_term (List.map mk_simple_string sl1) in
      let sl2' = mk_list_term (List.map mk_simple_string sl2) in
         mk_simple_term wc_type_op loc [sl1'; sl2'; mk_type t]
 | WcMod (loc, sl1, mt) ->
      let sl1' = mk_list_term (List.map mk_simple_string sl1) in
         mk_simple_term wc_module_op loc [sl1'; mk_module_type mt]

and mk_module_expr me =
   let loc = loc_of_module_expr me in
      match me with
         (<:module_expr< $me1$ . $me2$ >>) ->
            mk_simple_term me_proj_op loc [mk_module_expr me1; mk_module_expr me2]
       | (<:module_expr< $me1$ $me2$ >>) ->
            mk_simple_term me_apply_op loc [mk_module_expr me1; mk_module_expr me2]
       | (<:module_expr< functor ( $s$ : $mt$ ) -> $me$ >>) ->
            let op_loc = mk_op_loc me_functor_op loc in
               mk_dep0_dep1_any_term op_loc s (mk_module_type mt) (mk_module_expr me)
(*
       | (<:module_expr< $lid:i$ >>) ->
            mk_var me_lid_op loc i
*)
       | (<:module_expr< struct $list:sil$ end >>) ->
            mk_simple_term me_struct_op loc (List.map mk_str_item sil)
       | (<:module_expr< ( $me$ : $mt$) >>) ->
            mk_simple_term me_cast_op loc [mk_module_expr me; mk_module_type mt]
       | (<:module_expr< $uid:i$ >>) ->
            mk_var me_uid_op loc i
      
and mk_class_type
  { ctLoc = loc;
    ctNam = s;
    ctPrm = sl;
    ctArg = tl;
    ctTyc = so;
    ctFld = ctfl;
    ctVir = b1;
    ctCls = b2 } =
   mk_simple_named_term class_type_op loc s
	[ mk_list_term (List.map mk_simple_string sl);
	  mk_list_term (List.map mk_type tl);
          mk_string_opt expr_string_op so;
 	  mk_list_term (List.map mk_ctf ctfl);
	  mk_bool b1;
	  mk_bool b2]

and mk_ctf = function
   CtCtr (loc, s, t) ->
      mk_simple_term ctf_ctr_op loc [mk_simple_string s; mk_type t]
 | CtInh (loc, t) ->
      mk_simple_term ctf_inh_op loc [mk_type t]
 | CtMth (loc, s, t) ->
      mk_simple_term ctf_mth_op loc [mk_simple_string s; mk_type t]
 | CtVal (loc, s, b1, b2, ot) ->
      mk_simple_term ctf_val_op loc [mk_simple_string s; mk_bool b1; mk_bool b2; mk_type_opt ot]
 | CtVir (loc, s, t) ->
      mk_simple_term ctf_vir_op loc [mk_simple_string s; mk_type t]

and mk_class
  { cdLoc = loc;
    cdNam = s;
    cdPrm = sl1;
    cdArg = pl1;
    cdSlf = so1;
    cdTyc = so2;
    cdFld = cfl;
    cdVir = b1;
    cdCls = b2 } =
   mk_simple_named_term class_op loc s
	[ mk_list_term (List.map mk_simple_string sl1);
	  mk_list_term (List.map mk_patt pl1);
          mk_string_opt expr_string_op so1;
	  mk_string_opt expr_string_op so2;
 	  mk_list_term (List.map mk_cf cfl);
	  mk_bool b1;
	  mk_bool b2]

and mk_cf = function
   CfCtr (loc, s, t) ->
      mk_simple_term cf_ctr_op loc [mk_simple_string s; mk_type t]
 | CfInh (loc, t, e, so) ->
      mk_simple_term cf_inh_op loc [mk_type t; mk_expr e; mk_string_opt expr_string_op so]
 | CfMth (loc, s, e) ->
      mk_simple_term cf_mth_op loc [mk_simple_string s; mk_expr e]
 | CfVal (loc, s, b1, b2, eo) ->
      mk_simple_term cf_val_op loc [mk_simple_string s; mk_bool b1; mk_bool b2; mk_expr_opt eo]
 | CfVir (loc, s, t) ->
      mk_simple_term cf_vir_op loc [mk_simple_string s; mk_type t]

(*
 * Combined forms.
 *)
and mk_expr_opt x = mk_opt mk_expr x

and mk_type_opt x = mk_opt mk_type x

and mk_pwe (patt, with_expr, expr) =
   Term.mk_simple_term pwe_op [mk_patt patt; mk_expr_opt with_expr; mk_expr expr]

and mk_pe (patt, expr) =
   Term.mk_simple_term pe_op [mk_patt patt; mk_expr expr]

and mk_se (s, e) =
   Term.mk_simple_term se_op [mk_simple_string s; mk_expr e]

and mk_ee (e1, e2) =
   Term.mk_simple_term ee_op [mk_expr e1; mk_expr e2]

and mk_pp (p1, p2) =
   Term.mk_simple_term pp_op [mk_patt p1; mk_patt p2]

and mk_st (s, t) =
   Term.mk_simple_term st_op [mk_simple_string s; mk_type t]

and mk_sbt (s, b, t) =
   Term.mk_simple_term sbt_op [mk_simple_string s; mk_bool b; mk_type t]

and mk_stl (s, tl) =
   Term.mk_simple_term stl_op (mk_simple_string s :: List.map mk_type tl)

and mk_sslt (s, sl, t) =
   Term.mk_simple_term sslt_op (mk_simple_string s :: (List.map mk_simple_string sl @ [ mk_type t ]))

and mk_bool flag =
   Term.mk_simple_term (if flag then true_op else false_op) []

(************************************************************************
 * EXPORTS                                                              *
 ************************************************************************)

(*
 * Terms to MLAst.
 * FormatError (reason, term that failed)
 *)
let expr_of_term = dest_expr
let patt_of_term = dest_patt
let type_of_term = dest_type
let sig_item_of_term = dest_sig
let str_item_of_term = dest_str
let module_type_of_term = dest_mt
let module_expr_of_term = dest_me
let class_of_term = dest_class

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
let term_of_class = mk_class

(*
 * $Log$
 * Revision 1.2  1998/01/27 23:03:33  jyh
 * Ocaml 1.07.
 *
 * Revision 1.1  1997/09/12 17:21:36  jyh
 * Added MLast <-> term conversion.
 * Splitting filter_parse into two phases:
 *    1. Compile into Filter_summary
 *    2. Compile Filter_summary into code.
 *
 * Revision 1.2  1997/08/06 16:17:29  jyh
 * This is an ocaml version with subtyping, type inference,
 * d and eqcd tactics.  It is a basic system, but not debugged.
 *
 * Revision 1.1  1997/04/28 15:50:53  jyh
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
