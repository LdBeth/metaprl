(*
 * Preprocessor.
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
 * Author: Eli Barzilay
 * eli@cs.cornell.edu
 *)


(*
 * Additional syntax provided by this file:
 * ========================================
 *
 * DEFINE SYMBOL
 * UNDEFINE SYMBOL
 * UNDEF SYMBOL
 *   Define and undefine a symbol (UNDEF is the same as UNDEFINE to mimic CPP).
 *   Also adds "-DSYM" & "-USYM" command-line options for these.
 *
 * DEFINE SYMBOL = <expr>
 *   Same as above, but also define SYMBOL as an argumentless macro.
 *   Also makes "-DSYM=<expr>" available this.
 *
 * IFDEF SYMBOL THEN ...
 * IFDEF SYMBOL THEN ...
 * IFNDEF SYMBOL THEN ... ELSE ...
 * IFNDEF SYMBOL THEN ... ELSE ...
 *   Works for top-level structure items and for expressions.
 *
 * DEFEXPRMACRO MAC ARG... = <expr>
 *   Expand MAC ARG... using <expr> as a template.
 *
 * DEFMACRO MAC ARG... = <expr>
 *   A shortcut for the previous.
 *
 * DEFPATTMACRO MAC ARG.. = <patt>
 *   The same but for patterns.
 *
 * DEFEXPRPATTMACRO MAC ARG... = <expr&patt>
 *   Defines two macro types, <expr&patt> should be parsed as both.
 *
 * LETMACRO MAC ARG... = <expr> IN <expr>
 *   Defines and uses a local macro, it will be expanded as soon as seen,
 *   before expansion of global macros.
 *
 * CONCAT SYM1 SYM2
 *   The result of this macro will be the symbol made out of the concatenation
 *   of SYM1 and SYM2, it's type (LIDENT or UIDENT) depends on SYM1.
 *
 * INCLUDE <string>
 *   Parse the given file name at this point.
 *   Also adds a "-Idir" command-line option for include search directories.
 *
 * Note:
 *   The definition mechanism is mimicked using empty expression macros, also,
 *   when a symbols is UNDEFINEd, both expr and patt macros are deleted.
 *   This makes the keywords a bit messy in an attempt to be a bit CPP-like...
 *)


open Pcaml
open MLast



(*****************************************************************************)
(* General code walker *)

(*
 * Use global variables for the functions instead of transferring many
 * function arguments or using a big tuple.
 * The main entry point for this Codewalk.walkwith, call it with the processing
 * functions and a new loc field value wrapped by Some, or with None, then
 * further apply it to an initial function and finally, the object to walk
 * over.
 * Applying the loc function all over might not be the best thing, but adding
 * 200 lines of code wouldn't look too good as well ("typed" languages... Ha!).
 *)

module Codewalk = struct

   let expr_fun = ref None
   let patt_fun = ref None
   let loc      = ref (fun x -> x)

   let map = List.map

   let rec walkwith exprF pattF locN func x =
      let exprO = !expr_fun
      and pattO = !patt_fun
      and locO  = !loc
      in
         expr_fun := exprF;
         patt_fun := pattF;
         loc := (match locN with None -> fun x -> x | Some l -> fun _ -> l);
         let result = func x in
            expr_fun := exprO;
            patt_fun := pattO;
            loc      := locO;
            result

   and changeloc loc =
      walkwith None None (Some loc)

   and patt_patt (x, y) =
      (patt x, patt y)

   and patt_expr (x, y) =
      (patt x, expr y)

   and string_ctyp (x, y) =
      (x, ctyp y)

   and string_bool_ctyp (x, y, z) =
      (x, y, ctyp z)

   and string_ctyplist (x, y) =
      (x, map ctyp y)

   and string_stringlist_ctyp (x, y, z) =
      (x, y, ctyp z)

   and string_expr (x, y) =
      (x, expr y)

   and expr_expr (x, y) =
      (expr x, expr y)

   and patt_expropt_expr (x, y, z) =
      (patt x, expropt y, expr z)

   and expropt x =
      match x with
       | Some x -> Some (expr x)
       | None   -> None

   and ctypopt x =
      match x with
       | Some x -> Some (ctyp x)
       | None   -> None

   and pattopt x =
      match x with
       | Some x -> Some (patt x)
       | None   -> None

   and ctyp x =
      match x with
       | TyAcc (l, x, y)    -> TyAcc (!loc l, ctyp x, ctyp y)
       | TyAli (l, x, y)    -> TyAli (!loc l, ctyp x, ctyp y)
       | TyAny (l)          -> TyAny (!loc l)
       | TyApp (l, x, y)    -> TyApp (!loc l, ctyp x, ctyp y)
       | TyArr (l, x, y)    -> TyArr (!loc l, ctyp x, ctyp y)
       | TyCls (l, x)       -> TyCls (!loc l, x)
       | TyLid (l, x)       -> TyLid (!loc l, x)
       | TyMan (l, x, y)    -> TyMan (!loc l, ctyp x, ctyp y)
       | TyObj (l, x, y)    -> TyObj (!loc l, map string_ctyp x, y)
       | TyQuo (l, x)       -> TyQuo (!loc l, x)
       | TyRec (l, x)       -> TyRec (!loc l, map string_bool_ctyp x)
       | TySum (l, x)       -> TySum (!loc l, map string_ctyplist x)
       | TyTup (l, x)       -> TyTup (!loc l, map ctyp x)
       | TyUid (l, x)       -> TyUid (!loc l, x)
       | TyXnd (l, s, x)    -> TyXnd (!loc l, s, ctyp x)

   and patt x =
      let x = match !patt_fun with None -> x | Some f -> f x in
      match x with
       | PaAcc (l, x, y)    -> PaAcc (!loc l, patt x, patt y)
       | PaAli (l, x, y)    -> PaAli (!loc l, patt x, patt y)
       | PaAnt (l, x)       -> PaAnt (!loc l, patt x)
       | PaAny (l)          -> PaAny (!loc l)
       | PaApp (l, x, y)    -> PaApp (!loc l, patt x, patt y)
       | PaArr (l, x)       -> PaArr (!loc l, map patt x)
       | PaChr (l, x)       -> PaChr (!loc l, x)
       | PaInt (l, x)       -> PaInt (!loc l, x)
       | PaLid (l, x)       -> PaLid (!loc l, x)
       | PaOrp (l, x, y)    -> PaOrp (!loc l, patt x, patt y)
       | PaRng (l, x, y)    -> PaRng (!loc l, patt x, patt y)
       | PaRec (l, x)       -> PaRec (!loc l, map patt_patt x)
       | PaStr (l, x)       -> PaStr (!loc l, x)
       | PaTup (l, x)       -> PaTup (!loc l, map patt x)
       | PaTyc (l, x, y)    -> PaTyc (!loc l, patt x, ctyp y)
       | PaUid (l, x)       -> PaUid (!loc l, x)
       | PaXnd (l, s, x)    -> PaXnd (!loc l, s, patt x)

   and class_type_infos x =
      match x with
         { ciLoc = v; ciVir = w; ciPrm = x; ciNam = y; ciExp = z } ->
            { ciLoc = v; ciVir = w; ciPrm = x; ciNam = y;
              ciExp = class_type z }

   and class_expr_infos x =
      match x with
         { ciLoc = v; ciVir = w; ciPrm = x; ciNam = y; ciExp = z } ->
            { ciLoc = v; ciVir = w; ciPrm = x; ciNam = y;
              ciExp = class_expr z }

   and expr x =
      let x = match !expr_fun with None -> x | Some f -> f x in
      match x with
       | ExAcc (l, x, y)    -> ExAcc (!loc l, expr x, expr y)
       | ExAnt (l, x)       -> ExAnt (!loc l, expr x)
       | ExApp (l, x, y)    -> ExApp (!loc l, expr x, expr y)
       | ExAre (l, x, y)    -> ExAre (!loc l, expr x, expr y)
       | ExArr (l, x)       -> ExArr (!loc l, map expr x)
       | ExAss (l, x, y)    -> ExAss (!loc l, expr x, expr y)
       | ExChr (l, x)       -> ExChr (!loc l, x)
       | ExCoe (l, x, y)    -> ExCoe (!loc l, expr x, ctyp y)
       | ExFlo (l, x)       -> ExFlo (!loc l, x)
       | ExFor (l, x, y, z, v, w) ->
                               ExFor (!loc l, x, expr y, expr z, v, map expr w)
       | ExFun (l, x)       -> ExFun (!loc l, map patt_expropt_expr x)
       | ExIfe (l, x, y, z) -> ExIfe (!loc l, expr x, expr y, expr z)
       | ExInt (l, x)       -> ExInt (!loc l, x)
       | ExLet (l, x, y, z) -> ExLet (!loc l, x, map patt_expr y, expr z)
       | ExLid (l, x)       -> ExLid (!loc l, x)
       | ExLmd (l, x, y, z) -> ExLmd (!loc l, x, module_expr y, expr z)
       | ExMat (l, x, y)    -> ExMat (!loc l, expr x, map patt_expropt_expr y)
       | ExNew (l, x)       -> ExNew (!loc l, x)
       | ExOvr (l, x)       -> ExOvr (!loc l, map string_expr x)
       | ExRec (l, x, y)    -> ExRec (!loc l, map expr_expr x, expropt y)
       | ExSeq (l, x, y)    -> ExSeq (!loc l, map expr x, expr y)
       | ExSnd (l, x, y)    -> ExSnd (!loc l, expr x, y)
       | ExSte (l, x, y)    -> ExSte (!loc l, expr x, expr y)
       | ExStr (l, x)       -> ExStr (!loc l, x)
       | ExTry (l, x, y)    -> ExTry (!loc l, expr x, map patt_expropt_expr y)
       | ExTup (l, x)       -> ExTup (!loc l, map expr x)
       | ExTyc (l, x, y)    -> ExTyc (!loc l, expr x, ctyp y)
       | ExUid (l, x)       -> ExUid (!loc l, x)
       | ExWhi (l, x, y)    -> ExWhi (!loc l, expr x, map expr y)
       | ExXnd (l, s, x)    -> ExXnd (!loc l, s, expr x)

   and module_type x =
      match x with
       | MtAcc (l, x, y)    -> MtAcc (!loc l, module_type x, module_type y)
       | MtApp (l, x, y)    -> MtApp (!loc l, module_type x, module_type y)
       | MtFun (l, x, y, z) -> MtFun (!loc l, x, module_type y, module_type z)
       | MtLid (l, x)       -> MtLid (!loc l, x)
       | MtSig (l, x)       -> MtSig (!loc l, map sig_item x)
       | MtUid (l, x)       -> MtUid (!loc l, x)
       | MtWit (l, x, y)    -> MtWit (!loc l, module_type x, map with_constr y)

   and sig_item x =
      match x with
       | SgCls (l, x)       -> SgCls (!loc l, map class_type_infos x)
       | SgClt (l, x)       -> SgClt (!loc l, map class_type_infos x)
       | SgDcl (l, x)       -> SgDcl (!loc l, map sig_item x)
       | SgExc (l, x, y)    -> SgExc (!loc l, x, map ctyp y)
       | SgExt (l, x, y, z) -> SgExt (!loc l, x, ctyp y, z)
       | SgInc (l, x)       -> SgInc (!loc l, module_type x)
       | SgMod (l, x, y)    -> SgMod (!loc l, x, module_type y)
       | SgMty (l, x, y)    -> SgMty (!loc l, x, module_type y)
       | SgOpn (l, x)       -> SgOpn (!loc l, x)
       | SgTyp (l, x)       -> SgTyp (!loc l, map string_stringlist_ctyp x)
       | SgVal (l, x, y)    -> SgVal (!loc l, x, ctyp y)

   and with_constr x =
      match x with
       | WcTyp (l, x, y, z) -> WcTyp (!loc l, x, y, ctyp z)
       | WcMod (l, x, y)    -> WcMod (!loc l, x, module_type y)

   and module_expr x =
      match x with
       | MeAcc (l, x, y)    -> MeAcc (!loc l, module_expr x, module_expr y)
       | MeApp (l, x, y)    -> MeApp (!loc l, module_expr x, module_expr y)
       | MeFun (l, x, y, z) -> MeFun (!loc l, x, module_type y, module_expr z)
       | MeStr (l, x)       -> MeStr (!loc l, map str_item x)
       | MeTyc (l, x, y)    -> MeTyc (!loc l, module_expr x, module_type y)
       | MeUid (l, x)       -> MeUid (!loc l, x)

   and str_item x =
      match x with
       | StCls (l, x)       -> StCls (!loc l, map class_expr_infos x)
       | StClt (l, x)       -> StClt (!loc l, map class_type_infos x)
       | StDcl (l, x)       -> StDcl (!loc l, map str_item x)
       | StExc (l, x, y)    -> StExc (!loc l, x, map ctyp y)
       | StExp (l, x)       -> StExp (!loc l, expr x)
       | StExt (l, x, y, z) -> StExt (!loc l, x, ctyp y, z)
       | StMod (l, x, y)    -> StMod (!loc l, x, module_expr y)
       | StMty (l, x, y)    -> StMty (!loc l, x, module_type y)
       | StOpn (l, x)       -> StOpn (!loc l, x)
       | StTyp (l, x)       -> StTyp (!loc l, map string_stringlist_ctyp x)
       | StVal (l, x, y)    -> StVal (!loc l, x, map patt_expr y)

   and class_type x =
      match x with
       | CtCon (l, x, y)    -> CtCon (!loc l, x, map ctyp y)
       | CtFun (l, x, y)    -> CtFun (!loc l, ctyp x, class_type y)
       | CtSig (l, x, y)    -> CtSig (!loc l, ctypopt x, map class_sig_item y)
       | CtXnd (l, s, x)    -> CtXnd (!loc l, s, class_type x)

   and class_sig_item x =
      match x with
       | CgCtr (l, x, y)    -> CgCtr (!loc l, ctyp x, ctyp y)
       | CgInh (l, x)       -> CgInh (!loc l, class_type x)
       | CgMth (l, x, y, z) -> CgMth (!loc l, x, y, ctyp z)
       | CgVal (l, x, y, z) -> CgVal (!loc l, x, y, ctyp z)
       | CgVir (l, x, y, z) -> CgVir (!loc l, x, y, ctyp z)

   and class_expr x =
      match x with
       | CeApp (l, x, y)    -> CeApp (!loc l, class_expr x, map expr y)
       | CeCon (l, x, y)    -> CeCon (!loc l, x, map ctyp y)
       | CeFun (l, x, y)    -> CeFun (!loc l, patt x, class_expr y)
       | CeLet (l, x, y, z) -> CeLet (!loc l, x, map patt_expr y, class_expr z)
       | CeStr (l, x, y)    -> CeStr (!loc l, pattopt x, map class_str_item y)
       | CeTyc (l, x, y)    -> CeTyc (!loc l, class_expr x, class_type y)
       | CeXnd (l, s, x)    -> CeXnd (!loc l, s, class_expr x)

   and class_str_item x =
      match x with
       | CrCtr (l, x, y)    -> CrCtr (!loc l, ctyp x, ctyp y)
       | CrInh (l, x, y)    -> CrInh (!loc l, class_expr x, y)
       | CrIni (l, x)       -> CrIni (!loc l, expr x)
       | CrMth (l, x, y, z) -> CrMth (!loc l, x, y, expr z)
       | CrVal (l, x, y, z) -> CrVal (!loc l, x, y, expr z)
       | CrVir (l, x, y, z) -> CrVir (!loc l, x, y, ctyp z)

   and ast_list f ast_list =
      map (fun (ast, l) -> (f ast, !loc l)) ast_list

end



(*****************************************************************************)
(* Macro processing *)

(*
 * A macro is a function that takes a list of exprs and returns an expr.
 * There are several useful utilities here:
 *   add_simple_expr_macro name arg-list body
 *   add_simple_patt_macro name arg-list body
 *     takes a list of variable names (strings) and an expr/patt and returns
 *     a simple template macro
 *   undefine name
 *     delete macros with this name in both lists
 *   append_exprs expr expr-list
 *   append_patts expr expr-list
 *     when expr-list = e1, e2, ... -- returns the expr/patt (expr e1 e2...)
 *   macro_error msg
 *     prints an error msg with the current macro name and its location
 *)

(* These are association lists of name and expander pairs. *)
(* expr_macros is also used for strings that are DEFINEd - they are defined as
 * macros with an () expression body expander. *)
let expr_macros = ref []
let patt_macros = ref []

let add_expr_macro name expander =
   expr_macros := (name, expander) :: !expr_macros

let add_patt_macro name expander =
   patt_macros := (name, expander) :: !patt_macros

let undefine_macro x =
   expr_macros :=
      List.fold_right (fun y l -> if fst y = x then l else y::l) !expr_macros [];
   patt_macros :=
      List.fold_right (fun y l -> if fst y = x then l else y::l) !patt_macros []

let is_defined x =
   List.exists (fun y -> fst y = x) !expr_macros ||
   List.exists (fun y -> fst y = x) !patt_macros

(* This is for errors and locs while expanding macros. *)
let current_macname_loc = ref ("", 0, 0)

(* Report an error, prints the macro name and its position. *)
let macro_error msg =
   match !current_macname_loc with (name, b, e) ->
      Printf.eprintf "While expanding <%s> in \"%s\" at %d-%d: %s.\n"
         name !input_file b e msg;
      exit 1

(* Return current expanded macro location, for new constructions. *)
let current_loc () =
   match !current_macname_loc with (_, a, b) -> (a, b)

(* append_exprs expr [e1; e2; ...] --> "expr e1 e2..." *)
let rec append_exprs expr = function
 | [] -> expr
 | e::exprs ->
      let loc = loc_of_expr expr in
         append_exprs <:expr< $expr$ $e$ >> exprs

(* append_patts patt [p1; p2; ...] --> "patt p1 p2..." *)
let rec append_patts patt = function
 | [] -> patt
 | p::patts ->
      let loc = loc_of_patt patt in
         append_patts <:patt< $patt$ $p$ >> patts

let _ =
   add_expr_macro "CONCAT"
      (function
        | [] | [_] -> macro_error "expecting two identifiers"
        | x :: y :: exprs ->
             let loc = (fst (current_loc ())), (snd (loc_of_expr y)) in
             let id =
                match x, y with
                 | <:expr< $lid:x$ >>, <:expr< $lid:y$ >> -> <:expr<$lid:x^y$>>
                 | <:expr< $lid:x$ >>, <:expr< $uid:y$ >> -> <:expr<$lid:x^y$>>
                 | <:expr< $uid:x$ >>, <:expr< $lid:y$ >> -> <:expr<$uid:x^y$>>
                 | <:expr< $uid:x$ >>, <:expr< $uid:y$ >> -> <:expr<$uid:x^y$>>
                 | _ -> macro_error
                         "expected two identifiers, got some other expression"
             in
                append_exprs <:expr< $id$ >> exprs)

let _ =
   add_patt_macro "CONCAT"
      (function
          [] | [_] -> macro_error "expecting two identifiers"
        | x :: y :: patts ->
             let loc = (fst (current_loc ())), (snd (loc_of_patt y)) in
             let id =
                match x, y with
                 | <:patt< $lid:x$ >>, <:patt< $lid:y$ >> -> <:patt<$lid:x^y$>>
                 | <:patt< $lid:x$ >>, <:patt< $uid:y$ >> -> <:patt<$lid:x^y$>>
                 | <:patt< $uid:x$ >>, <:patt< $lid:y$ >> -> <:patt<$uid:x^y$>>
                 | <:patt< $uid:x$ >>, <:patt< $uid:y$ >> -> <:patt<$uid:x^y$>>
                 | _ -> macro_error
                         "expected two identifiers, got some other expression"
             in
                append_patts <:patt< $id$ >> patts)

(*
 * Get a pattern/expression "MAC x y z...",
 * Find a macro and its arguments, then apply it;
 * return the original syntax if no macro was found.
 *)

let rec apply_expr_macros expr =
   let rec aux expr =
      match expr with
       | <:expr< $e1$ $e2$ >> ->
            let (name_mac, args) = aux e1 in (name_mac, e2 :: args)
       | <:expr< $uid:str$ >> ->
            ((str, List.assoc str !expr_macros), [])
       | _ -> raise Not_found
   in
   try let ((name, mac), exprs) = (aux expr) in
      current_macname_loc := (let (b,e) = loc_of_expr expr in (name, b, e));
      Codewalk.changeloc (loc_of_expr expr) Codewalk.expr
         (substitute_expr_macros (mac (List.rev exprs)))
   with Not_found -> expr

and apply_patt_macros patt =
   let rec aux patt =
      match patt with
       | <:patt< $p1$ $p2$ >> ->
            let (name_mac, args) = aux p1 in (name_mac, p2 :: args)
       | <:patt< $uid:str$ >> -> ((str, List.assoc str !patt_macros), [])
       | _ -> raise Not_found
   in
   try let ((name, mac), patts) = (aux patt) in
      current_macname_loc := (let (b,e) = loc_of_patt patt in (name, b, e));
      Codewalk.changeloc (loc_of_patt patt) Codewalk.patt
         (substitute_patt_macros (mac (List.rev patts)))
   with Not_found -> patt

(*
 * The next two functions create simple template macros;
 * these are expanded by temporarily binding the formal arguments to the actual
 * arguments and using the same substitution code again.
 *)

and add_simple_expr_macro name args body =
   add_expr_macro name
      (fun exprs ->
          let rec aux = function
           | (a::args, e::exprs) ->
                (* Combine macro list *)
                add_simple_expr_macro a [] e;
                aux (args, exprs)
           | ([], exprs) ->
                (* Do the substitution, append extra exprs *)
                append_exprs (substitute_expr_macros body) exprs
           | (args, _) ->
                (* Not enough expressions *)
                macro_error "not enough arguments for simple macro";
          in
          let old_macros = !expr_macros in
             expr_macros := [];
             let result = aux (args, exprs) in
                expr_macros := old_macros;
                result)

and add_simple_patt_macro name args body =
   add_patt_macro name
      (fun patts ->
          let rec aux = function
           | (a::args, e::patts) ->
                (* Combine macro list *)
                add_simple_patt_macro a [] e;
                aux (args, patts)
           | ([], patts) ->
                (* Do the substitution, append extra patts *)
                append_patts (substitute_patt_macros body) patts
           | (args, _) ->
                (* Not enough expressions *)
                macro_error "not enough arguments for simple macro";
          in
          let old_macros = !patt_macros in
             patt_macros := [];
             let result = aux (args, patts) in
                patt_macros := old_macros;
                result)

(* The actual processing uses code-walk *)

(*
 * This is cute but won't work in a let-rec...
 * and macros_codewalk =
 *    Codewalk.walkwith
 *       (Some apply_expr_macros)
 *       (Some apply_patt_macros)
 *       None
 *)

and substitute_expr_macros x =
   Codewalk.walkwith
      (Some apply_expr_macros)
      (Some apply_patt_macros)
      None
      Codewalk.expr x

and substitute_patt_macros x =
   Codewalk.walkwith
      (Some apply_expr_macros)
      (Some apply_patt_macros)
      None
      Codewalk.patt x

and substitute_str_item_macros x =
   Codewalk.walkwith
      (Some apply_expr_macros)
      (Some apply_patt_macros)
      None
      Codewalk.str_item x



(*****************************************************************************)
(* Utilities *)

(* Parse a string as an expr - wrap it in parens to make sure it all parses. *)
let parse_expr str =
   let str = "(" ^ str ^ ")" in
   let stream = Stream.of_string str in
      try Grammar.Entry.parse expr stream
      with exc ->
         Format.set_formatter_out_channel stderr;
         Format.open_vbox 0;
         let exc =
            match exc with
             | Stdpp.Exc_located (bp, ep) exc ->
                  Printf.eprintf
                     "When processing -D for %s, at chars %d-%d of \"%s\"\n"
                     !Pcaml.input_file bp ep str;
                  exc
             | _ -> exc
         in
            Pcaml.report_error exc;
            Format.close_box ();
            Format.print_newline ();
            exit 2

(* Define str, or if it "X=Y", then define X with Y parsed as an expression *)
let define_str str =
   try let i = String.index str '=' in
          add_simple_expr_macro (String.sub str 0 i) []
             (parse_expr (String.sub str (i+1) ((String.length str)-i-1)))
   with Not_found ->
      add_simple_expr_macro str [] (let loc = (0,0) in <:expr< () >>)

(* This function turns simple patterns to expressions for exprpatt macros. *)
let rec patt2expr patt =
   let loc = loc_of_patt patt in
      match patt with
       | <:patt< $chr:c$ >> -> <:expr< $chr:c$ >>
       | <:patt< $str:s$ >> -> <:expr< $str:s$ >>
       | <:patt< $int:s$ >> -> <:expr< $int:s$ >>
       | <:patt< $lid:i$ >> -> <:expr< $lid:i$ >>
       | <:patt< $uid:s$ >> -> <:expr< $uid:s$ >>
       | <:patt< $p1$ . $p2$ >> ->
            let e1 = patt2expr p1 and e2 = patt2expr p2
            in <:expr< $e1$ . $e2$ >>
       | <:patt< $p1$ $p2$ >> ->
            let e1 = patt2expr p1 and e2 = patt2expr p2
            in <:expr< $e1$ $e2$ >>
       | <:patt< ( $list:pl$ ) >> ->
            let el = List.map patt2expr pl in
               <:expr< ( $list:el$ ) >>
       | <:patt< { $list:ppl$ } >> ->
            let eel =
               List.map
                  (fun (p1, p2) -> ((patt2expr p1), (patt2expr p2)))
                  ppl
            in <:expr< { $list:eel$ } >>
       | patt -> let (b, e) = loc in
            Printf.eprintf
               "could not convert pattern to expression at %d-%d.\n" b e;
            exit 1

(* This is a list of directories to search for INCLUDE statements. *)
let include_dirs = ref ["./"]

(* Add something to the above, make sure it ends with a slash. *)
let add_include_dir str =
   if str <> "" then
      let str = if String.get str ((String.length str)-1) = '/'
                then str else str ^ "/" in
         include_dirs := !include_dirs @ [str]



(*****************************************************************************)
(* Syntax extensions *)

type str_item_or_def =
   | MaStr of str_item                    (* normal str_item *)
   | MaDfe of string * string list * expr (* an expr macro def. statement *)
   | MaDfp of string * string list * patt (* a patt macro def. statement *)
   | MaDfb of string * string list * patt (* both patt&expr (should convert) *)
   | MaUnd of string                      (* a macro undefine statement *)
   | MaIfd of string * str_item_or_def list * str_item_or_def list
                                          (* a conditional str_item *)
   | MaLst of str_item_or_def list        (* str_items to scan *)
   | MaInc of string                      (* a file to include *)

let handle_macstuff loc =
   let rec aux macstuff =
      let nothing = <:str_item< declare end >> in
      match macstuff with
       | MaStr si  -> si
       | MaDfe n a e -> add_simple_expr_macro n a e; nothing
       | MaDfp n a p -> add_simple_patt_macro n a p; nothing
       | MaDfb n a p -> add_simple_expr_macro n a (patt2expr p);
                        add_simple_patt_macro n a p; nothing
       | MaUnd x -> undefine_macro x; nothing
       | MaIfd x e1 e2 -> aux (MaLst (if is_defined x then e1 else e2))
       | MaLst l ->
            (match List.filter (fun x -> x<>nothing) (List.map aux l)
             with
              | []   -> nothing
              | [si] -> si
              | sis  -> <:str_item< declare $list:sis$ end >>)
       | MaInc file ->
            (* This is copied from camlp4/argl.ml's 'process'. *)
            let file =
               try (List.find (fun dir -> Sys.file_exists (dir ^ file))
                              !include_dirs)
                   ^ file
               with Not_found -> file in
            let old_name = !input_file in
            let ic       = open_in_bin file in
            let clear()  = close_in ic in
            let cs       = Stream.of_channel ic in
               input_file := file;
               let phr = try Grammar.Entry.parse implem cs
                         with x -> clear(); raise x
               in
                  clear();
                  (* Note: input_file isn't restored above when on an error. *)
                  input_file := old_name;
                  StDcl (loc, List.map fst phr)
   in
      aux

EXTEND
   GLOBAL: expr str_item;
   str_item: FIRST
      [[ x = macstuff -> handle_macstuff loc x
       | (* This is where macros are expanded *)
         si = NEXT -> substitute_str_item_macros si
       ]];
   macstuff:
      [[ "IFDEF"; c = UIDENT; "THEN"; e1 = LIST0 str_item_macstuff;
         "ELSE"; e2 = LIST0 str_item_macstuff; "ENDIF" ->
            MaIfd c e1 e2
       | "IFDEF"; c = UIDENT; "THEN"; e1 = LIST0 str_item_macstuff; "ENDIF" ->
            MaIfd c e1 []
       | "IFNDEF"; c = UIDENT; "THEN"; e1 = LIST0 str_item_macstuff;
         "ELSE"; e2 = LIST0 str_item_macstuff; "ENDIF" ->
            MaIfd c e2 e1
       | "IFNDEF"; c = UIDENT; "THEN"; e1 = LIST0 str_item_macstuff; "ENDIF"->
            MaIfd c [] e1
       | "DEFINE"; c = UIDENT ->
            MaDfe c [] <:expr< () >>
       | "DEFINE"; name = UIDENT; "="; body = expr ->
            (* can only be used for argument-less macros because of camlp4. *)
            MaDfe name [] body
       | ["UNDEFINE" | "UNDEF"]; c = UIDENT ->
            (* UNDEF is the same as UNDEFINE to mimic CPP *)
            MaUnd c
       | [ "DEFEXPRMACRO" | "DEFMACRO" ];
         name = UIDENT; args = LIST0 UIDENT; "="; body = expr ->
            MaDfe name args body
       | "DEFPATTMACRO";
         name = UIDENT; args = LIST0 UIDENT; "="; body = patt ->
            MaDfp name args body
       | "DEFEXPRPATTMACRO";
         name = UIDENT; args = LIST0 UIDENT; "="; body = patt ->
         (* The above only forces body to be parsed as a pattern, then it is
          * parsed as an expr as well and an error is raised if this fails. *)
            MaDfb name args body
       | "INCLUDE"; file = STRING ->
            MaInc file
       ]];
   str_item_macstuff:
      [[ d = macstuff -> d
       | sis = str_item -> MaStr sis
       ]];
   expr: LEVEL "expr1"
      [[ "IFDEF"; c = UIDENT; "THEN"; e1 = expr; "ELSE"; e2 = expr; "ENDIF" ->
            if is_defined c then e1 else e2
       | "IFDEF"; c = UIDENT; "THEN"; e1 = expr; "ENDIF" ->
            if is_defined c then e1 else <:expr< () >>
       | "IFNDEF"; c = UIDENT; "THEN"; e1 = expr; "ELSE"; e2 = expr; "ENDIF" ->
            if is_defined c then e2 else e1
       | "IFNDEF"; c = UIDENT; "THEN"; e1 = expr; "ENDIF" ->
            if is_defined c then <:expr< () >> else e1
       | "LETMACRO"; name = UIDENT; args = LIST0 UIDENT; "="; body = expr;
         "IN"; e = expr ->
            let old_macros = !expr_macros in
               expr_macros := [];
               add_simple_expr_macro name args body;
               let result = substitute_expr_macros e in
                  expr_macros := old_macros;
                  result
       ]];
END



(*****************************************************************************)
(* Command line options *)

let _ =
   add_option "-D" (Arg.String define_str)
      "<string>   Define for IFDEF's." ;
   add_option "-U" (Arg.String undefine_macro)
      "<string>   Undefine for IFDEF's.";
   add_option "-I" (Arg.String add_include_dir)
      "<string>   Add a path for INCLUDE." ;
