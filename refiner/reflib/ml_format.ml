(*
 * Module for printing terms to an ML module.
 *
 *)

open Printf
open Debug
open Opname
open Refiner.Refiner.Term
open Refiner.Refiner.TermMeta
open Ml_format_sig

(*
 * Show the file loading.
 *)
let _ =
   if !debug_load then
      eprintf "Loading Ml_format%t" eflush

module FormatTerm =
struct
   (************************************************************************
    * HASHING                                                              *
    ************************************************************************)
   
   (*
    * This code also serves as a common subexpression eliminator.
    * Hash tables for each of the different types.
    *)
   let (opname_tbl : (opname, string) Hashtbl.t) = Hashtbl.create 97
   let (level_tbl : (level_exp, string) Hashtbl.t) = Hashtbl.create 17
   let (param_tbl : (param, string) Hashtbl.t) = Hashtbl.create 17
   let (operator_tbl : (operator, string) Hashtbl.t) = Hashtbl.create 97
   let (term_tbl : (term, string) Hashtbl.t) = Hashtbl.create 97
   let (bterm_tbl : (bound_term, string) Hashtbl.t) = Hashtbl.create 97
   
   (*
    * Reset tables.
    *)
   let clear_tbls () =
      Hashtbl.clear opname_tbl;
      Hashtbl.clear level_tbl;
      Hashtbl.clear param_tbl;
      Hashtbl.clear operator_tbl;
      Hashtbl.clear term_tbl;
      Hashtbl.clear bterm_tbl
   
   (*
    * Interfaces.
    *)
   let find_opname opname =
      try Some (Hashtbl.find opname_tbl opname) with
         Not_found -> None
   
   let add_opname opname v =
      Hashtbl.add opname_tbl opname v
   
   let find_level_exp level =
      try Some (Hashtbl.find level_tbl level) with
         Not_found -> None
   
   let add_level_exp level v =
      Hashtbl.add level_tbl level v
   
   let find_param param =
      try Some (Hashtbl.find param_tbl param) with
         Not_found -> None
   
   let add_param param v =
      Hashtbl.add param_tbl param v
   
   let find_operator op =
      try Some (Hashtbl.find operator_tbl op) with
         Not_found -> None
   
   let add_operator op v =
      Hashtbl.add operator_tbl op v
   
   let find_term term =
      try Some (Hashtbl.find term_tbl term) with
         Not_found -> None
   
   let add_term term v =
      Hashtbl.add term_tbl term v
   
   let find_bterm bterm =
      try Some (Hashtbl.find bterm_tbl bterm) with
         Not_found -> None
   
   let add_bterm bterm v =
      Hashtbl.add bterm_tbl bterm v
   
   (************************************************************************
    * GLOBAL VALUES                                                        *
    ************************************************************************)
   
   (*
    * Printer variables.
    *)
   let resetv, newv =
      let vindex = ref 0 in
         (function () -> vindex := 0),
         (function () -> vindex := !vindex + 1; "v" ^ (string_of_int !vindex))
   
   (*
    * Variables.
    *)
   let term_name = ["Refiner"; "Refiner"; "Term"]
   let make_level_var_expr = ML_Module_Var (term_name @ ["make_level_var"])
   let le_var_expr = ML_Module_Var (term_name @ ["le_var"])
   let le_offset_expr = ML_Module_Var (term_name @ ["le_offset"])
   let make_level_expr = ML_Module_Var (term_name @ ["make_level"])
   let le_const_expr = ML_Module_Var (term_name @ ["le_const"])
   let le_vars_expr = ML_Module_Var (term_name @ ["le_vars"])
   let make_param_expr = ML_Module_Var (term_name @ ["make_param"])
   let number_expr = ML_Module_Var (term_name @ ["Number"])
   let string_expr = ML_Module_Var (term_name @ ["String"])
   let token_expr = ML_Module_Var (term_name @ ["Token"])
   let level_expr = ML_Module_Var (term_name @ ["Level"])
   let var_expr = ML_Module_Var (term_name @ ["Var"])
   let mnumber_expr = ML_Module_Var (term_name @ ["MNumber"])
   let mstring_expr = ML_Module_Var (term_name @ ["MString"])
   let mtoken_expr = ML_Module_Var (term_name @ ["MToken"])
   let mlevel_expr = ML_Module_Var (term_name @ ["MLevel"])
   let mvar_expr = ML_Module_Var (term_name @ ["MVar"])
   let msum_expr = ML_Module_Var (term_name @ ["MSum"])
   let mdiff_expr = ML_Module_Var (term_name @ ["MDiff"])
   let mproduct_expr = ML_Module_Var (term_name @ ["MProduct"])
   let mquotient_expr = ML_Module_Var (term_name @ ["MQuotient"])
   let mrem_expr = ML_Module_Var (term_name @ ["MRem"])
   let mless_than_expr = ML_Module_Var (term_name @ ["MLessThan"])
   let mequal_expr = ML_Module_Var (term_name @ ["MEqual"])
   let mnot_equal_expr = ML_Module_Var (term_name @ ["MNotEqual"])
   let make_op_expr = ML_Module_Var (term_name @ ["make_op"])
   let op_name_expr = ML_Module_Var (term_name @ ["op_name"])
   let op_params_expr = ML_Module_Var (term_name @ ["op_params"])
   let make_bterm_expr = ML_Module_Var (term_name @ ["make_bterm"])
   let bvars_expr = ML_Module_Var (term_name @ ["bvars"])
   let bterm_expr = ML_Module_Var (term_name @ ["bterm"])
   let mk_var_term_expr = ML_Module_Var (term_name @ ["mk_var_term"])
   let make_term_expr = ML_Module_Var (term_name @ ["make_term"])
   let term_op_expr = ML_Module_Var (term_name @ ["term_op"])
   let term_terms_expr = ML_Module_Var (term_name @ ["term_terms"])
   
   let opname_name = "Opname"
   let make_opname_expr = ML_Module_Var [opname_name; "make_opname"]

   let term_util_name = ["Refiner"; "Refiner"; "TermMeta"]
   let meta_theorem_expr = ML_Module_Var (term_util_name @ ["MetaTheorem"])
   let meta_implies_expr = ML_Module_Var (term_util_name @ ["MetaImplies"])
   let meta_function_expr = ML_Module_Var (term_util_name @ ["MetaFunction"])
   let meta_iff_expr = ML_Module_Var (term_util_name @ ["MetaIff"])

   (************************************************************************
    * PRINTERS                                                             *
    ************************************************************************)
   
   (* "File" operation *)
   let push ofile pair =
      ofile := pair :: !ofile
   
   (* New file *)
   let create () =
      ref []
   
   (* Extract the file *)
   let get ofile body =
      let rec construct = function
         (name, expr)::t ->
            ML_Let (name, expr, construct t)
       | [] ->
            body
      in
         construct (List.rev !ofile)
   
   (* Level expression *)
   let print_level_exp' ofile l =
      match dest_level l with
         { le_const = c; le_vars = vars } ->
            let print_level_var lv =
               match dest_level_var lv with
                  { le_var = v; le_offset = o } ->
                     let v' = newv () in
                     let le = ML_Apply [make_level_var_expr;
                                        ML_Record [le_var_expr, ML_String v;
                                                   le_offset_expr, ML_Int o]]
                     in
                        push ofile (v', le);
                        v'
            in
            let vs = List.map (function v -> ML_Var (print_level_var v)) vars in
            let v = newv() in
            let le = ML_Apply [make_level_expr;
                               ML_Record [le_const_expr, ML_Int c;
                                          le_vars_expr, ML_List vs]]
            in
               push ofile (v, le);
               v
   
   let print_level_exp ofile l =
      match find_level_exp l with
         Some v -> v
       | None ->
            let v = print_level_exp' ofile l in
               add_level_exp l v;
               v
   
   (* General parameter *)
   let rec print_param' ofile p =
      let print_string_param v name str =
         let param = ML_Apply [make_param_expr; ML_Apply [name; ML_String str]] in
            push ofile (v, param);
            v
      in
      let print_num_param v name n =
         let param = ML_Apply [make_param_expr; ML_Apply [name; ML_Num n]] in
            push ofile (v, param);
            v
      in
      let print_le_param v name l =
         let param = ML_Apply [make_param_expr; ML_Apply [name; ML_Var l]] in
            push ofile (v, param);
            v
      in
      let print_pair_param v name a b =
         let param = ML_Apply [make_param_expr; ML_Apply [name; ML_Tuple [ML_Var a; ML_Var b]]] in
            push ofile (v, param);
            v
      in
         match dest_param p with
            Number n -> print_num_param (newv ()) number_expr n
          | String s -> print_string_param (newv ()) string_expr s
          | Token t -> print_string_param (newv ()) token_expr t
          | Level l ->
               let v1 = print_level_exp ofile l in
                  print_le_param (newv ()) level_expr v1
          | Var v -> print_string_param (newv ()) var_expr v
          | MNumber v -> print_string_param (newv ()) mnumber_expr v
          | MString v -> print_string_param (newv ()) mstring_expr v
          | MToken v -> print_string_param (newv ()) mtoken_expr v
          | MLevel v -> print_string_param (newv ()) mlevel_expr v
          | MVar v -> print_string_param (newv ()) mvar_expr v
          | MSum (a, b) ->
               let v1 = print_param ofile a in
               let v2 = print_param ofile b in
               let v = newv () in
                  print_pair_param v msum_expr v1 v2
          | MDiff (a, b) ->
               let v1 = print_param ofile a in
               let v2 = print_param ofile b in
               let v = newv () in
                  print_pair_param v mdiff_expr v1 v2
          | MProduct (a, b) ->
               let v1 = print_param ofile a in
               let v2 = print_param ofile b in
               let v = newv () in
                  print_pair_param v mproduct_expr v1 v2
          | MQuotient (a, b) ->
               let v1 = print_param ofile a in
               let v2 = print_param ofile b in
               let v = newv () in
                  print_pair_param v mquotient_expr v1 v2
          | MRem (a, b) ->
               let v1 = print_param ofile a in
               let v2 = print_param ofile b in
               let v = newv () in
                  print_pair_param v mrem_expr v1 v2
   
          | MLessThan (a, b) ->
               let v1 = print_param ofile a in
               let v2 = print_param ofile b in
               let v = newv () in
                  print_pair_param v mless_than_expr v1 v2
   
          | MEqual (a, b) ->
               let v1 = print_param ofile a in
               let v2 = print_param ofile b in
               let v = newv () in
                  print_pair_param v mequal_expr v1 v2
   
          | MNotEqual (a, b) ->
               let v1 = print_param ofile a in
               let v2 = print_param ofile b in
               let v = newv () in
                  print_pair_param v mnot_equal_expr v1 v2
   
          | ObId _ ->
               (* We don't allow these fancy terms *)
               raise (Failure "Ml_format.print_param: can't do term with ObId parameter")

          | ParamList _ ->
               (* We don't allow these fancy terms *)
               raise (Failure "Ml_format.print_param: can't do term with ParamList parameter")
   
   and print_param ofile p =
      match find_param p with
         Some v -> v
       | None ->
            let v = print_param' ofile p in
               add_param p v;
               v
   
   (*
    * Print an operator.
    *)
   let print_opname' ofile name =
      let v = newv() in
      let names = List.map (function s -> ML_String s) (dest_opname name) in
      let opname = ML_Apply [make_opname_expr; ML_List names] in
         push ofile (v, opname);
         v
   
   let print_opname ofile name =
      match find_opname name with
         Some v -> v
       | None ->
            let v = print_opname' ofile name in
               add_opname name v;
               v
   
   (*
    * Print a general operator.
    *)
   let print_operator' ofile op =
      match dest_op op with
         { op_name = opname; op_params = params } ->
            let opv = print_opname ofile opname in
            let pv = List.map (function p -> ML_Var (print_param ofile p)) params in
            let v = newv() in
            let operator = ML_Apply [make_op_expr;
                                     ML_Record [op_name_expr, ML_Var opv;
                                                op_params_expr, ML_List pv]]
            in
               push ofile (v, operator);
               v
   
   let print_operator ofile op =
      match find_operator op with
         Some v -> v
       | None ->
            let v = print_operator' ofile op in
               add_operator op v;
               v
   
   (*
    * Print "bterms".
    *)
   let rec print_bterm' ofile bterm =
      match dest_bterm bterm with
         { bvars = vars; bterm = term } ->
            let bv = print_term ofile term in
            let v = newv() in
            let vars' = List.map (function v -> ML_String v) vars in
            let bterm = ML_Apply [make_bterm_expr;
                                  ML_Record [bvars_expr, ML_List vars';
                                             bterm_expr, ML_Var bv]]
            in
               push ofile (v, bterm);
               v
   
   (*
    * Top level print function.
    *)
   and print_term' ofile term =
      (* Special cases *)
      if is_var_term term then
         let var = dest_var term in
         let v = newv () in
         let term = ML_Apply [mk_var_term_expr; ML_String var] in
            push ofile (v, term);
            v
      else
         match dest_term term with
            { term_op = op; term_terms = bterms } ->
               let opv = print_operator ofile op in
               let vb = List.map (function t -> ML_Var (print_bterm ofile t)) bterms in
               let v = newv () in
               let term = ML_Apply [make_term_expr;
                                    ML_Record [term_op_expr, ML_Var opv;
                                               term_terms_expr, ML_List vb]]
               in
                  push ofile (v, term);
                  v
   
   (*
    * Cached.
    *)
   and print_bterm ofile bterm =
      match find_bterm bterm with
         Some v -> v
       | None ->
            let v = print_bterm' ofile bterm in
               add_bterm bterm v;
               v
   
   and print_term ofile term =
      match find_term term with
         Some v -> v
       | None ->
            let v = print_term' ofile term in
               add_term term v;
               v
   
   (*
    * Meta term.
    *)
   let rec print_mterm ofile = function
      MetaTheorem t ->
         let v = newv () in
         let a = print_term ofile t in
         let mterm = ML_Apply [meta_theorem_expr; ML_Var a] in
            push ofile (v, mterm);
            v
    | MetaImplies (a, b) ->
         let v = newv () in
         let va = print_mterm ofile a in
         let vb = print_mterm ofile b in
         let mterm = ML_Apply [meta_implies_expr; ML_Tuple [ML_Var va; ML_Var vb]] in
            push ofile (v, mterm);
            v
    | MetaFunction (v, a, b) ->
         let v = newv () in
         let va = print_mterm ofile a in
         let vb = print_mterm ofile b in
         let mterm = ML_Apply [meta_function_expr; ML_Tuple [ML_Var va; ML_Var vb]] in
            push ofile (v, mterm);
            v
    | MetaIff (a, b) ->
         let v = newv () in
         let va = print_mterm ofile a in
         let vb = print_mterm ofile b in
         let mterm = ML_Apply [meta_iff_expr; ML_Tuple [ML_Var va; ML_Var vb]] in
            push ofile (v, mterm);
            v
   
   (************************************************************************
    * FILE INTERFACE                                                       *
    ************************************************************************)
   
   (*
    * Interface.
    *)
   let format_term term =
      (* Separate lines *)
      resetv ();
      clear_tbls ();
      let ofile = create () in
      let v = print_term ofile term in
         get ofile (ML_Var v)
   
   (*
    * Meta terms.
    *)
   let format_mterm mterm =
      (* Environment *)
      resetv ();
      clear_tbls ();
   
      (* Print output *)
      let ofile = create () in
      let v = print_mterm ofile mterm in
         get ofile (ML_Var v)
   
   (*
    * List of terms.
    *)
   let format_term_list l =
      (* Clear tables *)
      resetv ();
      clear_tbls ();
      
      (* Print them *)
      let ofile = create () in
      let vars = List.map (function t -> ML_Var (print_term ofile t)) l in
         ML_List vars
end
   
(*
 * $Log$
 * Revision 1.1  1998/05/28 15:00:52  jyh
 * Partitioned refiner into subdirectories.
 *
 * Revision 1.6  1998/05/28 13:47:05  jyh
 * Updated the editor to use new Refiner structure.
 * ITT needs dform names.
 *
 * Revision 1.5  1998/05/27 15:13:46  jyh
 * Functorized the refiner over the Term module.
 *
 * Revision 1.4  1998/04/24 02:42:39  jyh
 * Added more extensive debugging capabilities.
 *
 * Revision 1.3  1998/03/20 22:16:18  eli
 * Eli: Changed integer parameters to Num.num's.
 *
 * Revision 1.2  1997/09/12 17:21:43  jyh
 * Added MLast <-> term conversion.
 * Splitting filter_parse into two phases:
 *    1. Compile into Filter_summary
 *    2. Compile Filter_summary into code.
 *
 * Revision 1.1  1997/04/29 16:16:45  jyh
 * Modified ml_format.
 *
 * Revision 1.8  1996/05/21 02:13:56  jyh
 * This is a semi-working version before Wisconsin vacation.
 *
 * Revision 1.7  1996/04/07 18:24:48  jyh
 * This is an intermediate commit while adjusting the dforms.
 * We intend that dform printers just return a list of terms.
 *
 * Revision 1.6  1996/03/25 20:50:41  jyh
 * Intermediate commit while modifying grammer.  Restricting
 * ML hooks to condition terms.
 *
 * Revision 1.5  1996/03/05 19:48:30  jyh
 * Preliminary version with logical framework.
 *
 * Revision 1.4  1996/02/25 15:16:15  jyh
 * This is a partial checkin as filterModule is being developed.
 * After the meta-logical framework is developed, sequent.* will go away.
 *
 * Revision 1.3  1996/02/18 23:32:28  jyh
 * Changin Format module to more Nuprl-like format.
 *
 * Revision 1.2  1996/02/13 21:32:22  jyh
 * This is an intermediate checkin while matching is being added to the rewriter.
 *
 * Revision 1.1  1996/02/10 20:19:54  jyh
 * Initial checkin of filter (prlcomp).
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner.run"
 * End:
 * -*-
 *)
