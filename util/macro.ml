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
 *   Same as above, but also define SYMBOL as an argumentless macro, will also
 *   try to define a pattern macro etc if possible.
 *   Also makes "-DSYM=..." available this.
 *
 * IFDEF SYMBOL THEN ...
 * IFDEF SYMBOL THEN ...
 * IFNDEF SYMBOL THEN ... ELSE ...
 * IFNDEF SYMBOL THEN ... ELSE ...
 *   Works for top-level structure items and for expressions.
 *
 * DEFMACRO MAC ARG... = <expr>
 *   Similar to DEFINE (try to convert expr to other types), but allows
 *   arguments.
 *
 * DEFEXPRMACRO MAC ARG... = <expr>
 *   Expand MAC ARG... using <expr> as a template.
 *
 * DEFPATTMACRO MAC ARG.. = <patt>
 *   The same but for patterns.
 *
 * LETMACRO MAC ARG... = <expr> IN <expr>
 *   Defines and uses a local macro, it will be expanded as soon as seen,
 *   before expansion of global macros.  Works like DEFINE.
 *
 * CONCAT SYM1 SYM2
 *   The result of this macro will be the symbol made out of the concatenation
 *   of SYM1 and SYM2, it's type (LIDENT or UIDENT) depends on SYM1.
 *
 * NOTHING
 *   This is a special expression or pattern that will be eliminated in a final
 *   scan, *after* macro expansion; currently it works only in applications and
 *   curried arguments.
 *   Note that care should be taken when using this, for example, in cases
 *   where it cannot be eliminated, a bogus "!NOTHING" identifier will be left
 *   instead, also, since infix operators are translated into function
 *   applications, something like "NOTHING+1" will leave you with "(+) 1" which
 *   is a function (so if this is wanted, specific care should be taken).
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
   let ctyp_fun = ref None
   let mexp_fun = ref None
   let mtyp_fun = ref None
   let loc      = ref (fun x -> x)

   let map = List.map

   let rec walkwith exprF pattF ctypF mexpF mtypF locN func x =
      let exprO = !expr_fun
      and pattO = !patt_fun
      and ctypO = !ctyp_fun
      and mexpO = !mexp_fun
      and mtypO = !mtyp_fun
      and locO  = !loc
      in
         expr_fun := exprF;
         patt_fun := pattF;
         ctyp_fun := ctypF;
         mexp_fun := mexpF;
         mtyp_fun := mtypF;
         loc := (match locN with None -> fun x -> x | Some l -> fun _ -> l);
         let result = func x in
            expr_fun := exprO;
            patt_fun := pattO;
            ctyp_fun := ctypO;
            mexp_fun := mexpO;
            mtyp_fun := mtypO;
            loc      := locO;
            result

   and changeloc loc =
      walkwith None None None None None (Some loc)

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
      let newx = (match !ctyp_fun with None -> x | Some f -> f x) in
      if x <> newx then ctyp newx else
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
      let newx = (match !patt_fun with None -> x | Some f -> f x) in
      if x <> newx then patt newx else
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
      let newx = match !expr_fun with None -> x | Some f -> f x in
      if x <> newx then expr newx else
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
      let newx = match !mtyp_fun with None -> x | Some f -> f x in
      if x <> newx then module_type newx else
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
      let newx = match !mexp_fun with None -> x | Some f -> f x in
      if x <> newx then module_expr newx else
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
let ctyp_macros = ref []
let mexp_macros = ref []
let mtyp_macros = ref []

let add_expr_macro name expander = expr_macros := (name,expander)::!expr_macros
let add_patt_macro name expander = patt_macros := (name,expander)::!patt_macros
let add_ctyp_macro name expander = ctyp_macros := (name,expander)::!ctyp_macros
let add_mexp_macro name expander = mexp_macros := (name,expander)::!mexp_macros
let add_mtyp_macro name expander = mtyp_macros := (name,expander)::!mtyp_macros

let undefine_macro x =
   let aux lp =
      lp := List.fold_right (fun y l -> if fst y = x then l else y::l) !lp []
   in
      aux expr_macros;
      aux patt_macros;
      aux ctyp_macros;
      aux mexp_macros;
      aux mtyp_macros

let is_defined x =
   let aux lp = List.exists (fun y -> fst y = x) !lp in
      aux expr_macros ||
      aux patt_macros ||
      aux ctyp_macros ||
      aux mexp_macros ||
      aux mtyp_macros

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

(* append syntaxes using application: X [Y1; Y2; ...] --> "X Y1 Y2..." *)
let rec append_exprs expr = function
 | [] -> expr
 | e::exprs -> let loc = loc_of_expr expr in
                  append_exprs <:expr< $expr$ $e$ >> exprs
let rec append_patts patt = function
 | [] -> patt
 | p::patts -> let loc = loc_of_patt patt in
                  append_patts <:patt< $patt$ $p$ >> patts
let rec append_ctyps ctyp = function
 | [] -> ctyp
 | t::ctyps -> let loc = loc_of_ctyp ctyp in
                  append_ctyps <:ctyp< $ctyp$ $t$ >> ctyps
let rec append_mexps mexp = function
 | [] -> mexp
 | e::mexps -> let loc = loc_of_module_expr mexp in
                  append_mexps <:module_expr< $mexp$ $e$ >> mexps
let rec append_mtyps mtyp = function
 | [] -> mtyp
 | t::mtyps -> let loc = loc_of_module_type mtyp in
                  append_mtyps <:module_type< $mtyp$ $t$ >> mtyps

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
                append_exprs id exprs)

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
                append_patts id patts)

let _ =
   add_ctyp_macro "CONCAT"
      (function
          [] | [_] -> macro_error "expecting two identifiers"
        | x :: y :: ctyps ->
             let loc = (fst (current_loc ())), (snd (loc_of_ctyp y)) in
             let id =
                match x, y with
                 | <:ctyp< $lid:x$ >>, <:ctyp< $lid:y$ >> -> <:ctyp<$lid:x^y$>>
                 | <:ctyp< $lid:x$ >>, <:ctyp< $uid:y$ >> -> <:ctyp<$lid:x^y$>>
                 | <:ctyp< $uid:x$ >>, <:ctyp< $lid:y$ >> -> <:ctyp<$uid:x^y$>>
                 | <:ctyp< $uid:x$ >>, <:ctyp< $uid:y$ >> -> <:ctyp<$uid:x^y$>>
                 | _ -> macro_error
                         "expected two identifiers, got some other expression"
             in
                append_ctyps id ctyps)

let _ =
   add_mexp_macro "CONCAT"
      (function
          [] | [_] -> macro_error "expecting two identifiers"
        | x :: y :: mexps ->
             let loc = (fst (current_loc ())), (snd (loc_of_module_expr y)) in
             let id =
                match x, y with
                 | <:module_expr< $uid:x$ >>, <:module_expr< $uid:y$ >> ->
                      <:module_expr<$uid:x^y$>>
                 | _ -> macro_error
                         "expected two identifiers, got some other expression"
             in
                append_mexps id mexps)

let _ =
   add_mtyp_macro "CONCAT"
      (function
          [] | [_] -> macro_error "expecting two identifiers"
        | x :: y :: mtyps ->
             let loc = (fst (current_loc ())), (snd (loc_of_module_type y)) in
             let id =
                match x, y with
                 | <:module_type< $lid:x$ >>, <:module_type< $lid:y$ >> ->
                      <:module_type<$lid:x^y$>>
                 | <:module_type< $lid:x$ >>, <:module_type< $uid:y$ >> ->
                      <:module_type<$lid:x^y$>>
                 | <:module_type< $uid:x$ >>, <:module_type< $lid:y$ >> ->
                      <:module_type<$uid:x^y$>>
                 | <:module_type< $uid:x$ >>, <:module_type< $uid:y$ >> ->
                      <:module_type<$uid:x^y$>>
                 | _ -> macro_error
                         "expected two identifiers, got some other expression"
             in
                append_mtyps id mtyps)

(*
 * Get a pattern/expression "MAC x y z...",
 * Return a name MAC and a list of args [X; Y; ...] if it fits Not_found otw.
 *)

let rec flat_expr = function
 | <:expr< $x$ $y$ >> -> let (name, args) = flat_expr x in (name, y::args)
 | <:expr< $uid:str$ >> -> (str, [])
 | _ -> raise Not_found
let rec flat_patt = function
 | <:patt< $x$ $y$ >> -> let (name, args) = flat_patt x in (name, y::args)
 | <:patt< $uid:str$ >> -> (str, [])
 | _ -> raise Not_found
let rec flat_ctyp = function
 | <:ctyp< $x$ $y$ >> -> let (name, args) = flat_ctyp x in (name, y::args)
 | <:ctyp< $uid:str$ >> -> (str, [])
 | _ -> raise Not_found
let rec flat_mexp = function
 | <:module_expr< $x$ $y$ >> -> let (name,args) = flat_mexp x in (name,y::args)
 | <:module_expr< $uid:str$ >> -> (str, [])
 | _ -> raise Not_found
let rec flat_mtyp = function
 | <:module_type< $x$ $y$ >> -> let (name,args) = flat_mtyp x in (name,y::args)
 | <:module_type< $uid:str$ >> -> (str, [])
 | _ -> raise Not_found

let apply_macros flatter codewalker locator macros x =
   try let name, args = flatter x in
       let mac = List.assoc name macros in
       let loc = locator x in
          current_macname_loc := (let (b,e) = loc in (name, b, e));
          Codewalk.changeloc loc codewalker (mac (List.rev args))
   with Not_found -> x

(* The actual processing uses code-walk *)
let macros_codewalk f =
   Codewalk.walkwith
      (Some (apply_macros flat_expr Codewalk.expr loc_of_expr !expr_macros))
      (Some (apply_macros flat_patt Codewalk.patt loc_of_patt !patt_macros))
      (Some (apply_macros flat_ctyp Codewalk.ctyp loc_of_ctyp !ctyp_macros))
      (Some (apply_macros flat_mexp Codewalk.module_expr loc_of_module_expr
                          !mexp_macros))
      (Some (apply_macros flat_mtyp Codewalk.module_type loc_of_module_type
                          !mtyp_macros))
      None
      f

(*
 * The next two functions create simple template macros;
 * these are expanded by temporarily binding the formal arguments to the actual
 * arguments and using the same substitution code again.
 *)

let rec add_simple_expr_macro name args body =
   add_expr_macro name
      (fun exprs ->
          let rec aux = function
           | (a::args, e::exprs) ->
                (* Combine macro list *)
                add_simple_expr_macro a [] e;
                aux (args, exprs)
           | ([], exprs) ->
                (* Do the substitution, append extra exprs *)
                append_exprs (macros_codewalk Codewalk.expr body) exprs
           | (args, _) ->
                macro_error "not enough arguments for simple macro";
          in
          let old_macros = !expr_macros in
             expr_macros := [];
             let result = aux (args, exprs) in
                expr_macros := old_macros;
                result)

let rec add_simple_patt_macro name args body =
   add_patt_macro name
      (fun patts ->
          let rec aux = function
           | (a::args, p::patts) ->
                (* Combine macro list *)
                add_simple_patt_macro a [] p;
                aux (args, patts)
           | ([], patts) ->
                (* Do the substitution, append extra patts *)
                append_patts (macros_codewalk Codewalk.patt body) patts
           | (args, _) ->
                macro_error "not enough arguments for simple macro";
          in
          let old_macros = !patt_macros in
             patt_macros := [];
             let result = aux (args, patts) in
                patt_macros := old_macros;
                result)

let rec add_simple_ctyp_macro name args body =
   add_ctyp_macro name
      (fun ctyps ->
          let rec aux = function
           | (a::args, ct::ctyp) ->
                (* Combine macro list *)
                add_simple_ctyp_macro a [] ct;
                aux (args, ctyps)
           | ([], ctyps) ->
                (* Do the substitution, append extra ctyps *)
                append_ctyps (macros_codewalk Codewalk.ctyp body) ctyps
           | (args, _) ->
                macro_error "not enough arguments for simple macro";
          in
          let old_macros = !ctyp_macros in
             ctyp_macros := [];
             let result = aux (args, ctyps) in
                ctyp_macros := old_macros;
                result)

let rec add_simple_mexp_macro name args body =
   add_mexp_macro name
      (fun mexps ->
          let rec aux = function
           | (a::args, me::mexp) ->
                (* Combine macro list *)
                add_simple_mexp_macro a [] me;
                aux (args, mexps)
           | ([], mexps) ->
                (* Do the substitution, append extra mexps *)
                append_mexps (macros_codewalk Codewalk.module_expr body) mexps
           | (args, _) ->
                macro_error "not enough arguments for simple macro";
          in
          let old_macros = !mexp_macros in
             mexp_macros := [];
             let result = aux (args, mexps) in
                mexp_macros := old_macros;
                result)

let rec add_simple_mtyp_macro name args body =
   add_mtyp_macro name
      (fun mtyps ->
          let rec aux = function
           | (a::args, mt::mtyp) ->
                (* Combine macro list *)
                add_simple_mtyp_macro a [] mt;
                aux (args, mtyps)
           | ([], mtyps) ->
                (* Do the substitution, append extra mtyps *)
                append_mtyps (macros_codewalk Codewalk.module_type body) mtyps
           | (args, _) ->
                macro_error "not enough arguments for simple macro";
          in
          let old_macros = !mtyp_macros in
             mtyp_macros := [];
             let result = aux (args, mtyps) in
                mtyp_macros := old_macros;
                result)



(*****************************************************************************)
(* Utilities *)

let remove_nothings f =
   Codewalk.walkwith
      (Some (fun expr -> match expr with
              | <:expr< $e1$ $lid:"!NOTHING"$ >> -> e1
              | <:expr< $lid:"!NOTHING"$ $e2$ >> -> e2
              | <:expr< fun $x$ $lid:"!NOTHING"$ -> $b$ >> ->
                   let loc = loc_of_expr expr in <:expr< fun $x$ -> $b$ >>
              | <:expr< fun $lid:"!NOTHING"$ $x$ -> $b$ >> ->
                   let loc = loc_of_expr expr in <:expr< fun $x$ -> $b$ >>
              | x -> x))
      (Some (fun patt -> match patt with
              | <:patt< $p1$ $lid:"!NOTHING"$ >> -> p1
              | <:patt< $lid:"!NOTHING"$ $p2$ >> -> p2
              | x -> x))
      (Some (fun ctyp -> match ctyp with
              | <:ctyp< $ct1$ $lid:"!NOTHING"$ >> -> ct1
              | <:ctyp< $lid:"!NOTHING"$ $ct2$ >> -> ct2
              | x -> x))
      (Some (fun mexp -> match mexp with
              | <:module_expr< $me1$ $uid:"!NOTHING"$ >> -> me1
              | <:module_expr< $uid:"!NOTHING"$ $me2$ >> -> me2
              | x -> x))
      (Some (fun mtyp -> match mtyp with
              | <:module_type< $mt1$ $uid:"!NOTHING"$ >> -> mt1
              | <:module_type< $uid:"!NOTHING"$ $mt2$ >> -> mt2
              | x -> x))
      None
      f

(* Define str; if it "X=Y", then define X with Y parsed as possible macros *)
let define_str str =
   try begin
      let i = String.index str '=' in
      (* wrap it in parens to make sure it all parses. *)
      let name = String.sub str 0 i in
      let pstr = "("^ (String.sub str (i+1) ((String.length str)-i-1)) ^")" in
      let parsed = ref false in
         begin try
            add_simple_expr_macro
               name [] (Grammar.Entry.parse expr (Stream.of_string pstr));
            parsed := true;
         with _ -> () end;
         begin try
            add_simple_patt_macro
               name [] (Grammar.Entry.parse patt (Stream.of_string pstr));
            parsed := true;
         with _ -> () end;
         begin try
            add_simple_ctyp_macro
               name [] (Grammar.Entry.parse ctyp (Stream.of_string pstr));
            parsed := true;
         with _ -> () end;
         begin try
            add_simple_mexp_macro
               name []
               (Grammar.Entry.parse module_expr (Stream.of_string pstr));
            parsed := true;
         with _ -> () end;
         begin try
            add_simple_mtyp_macro
               name []
               (Grammar.Entry.parse module_type (Stream.of_string pstr));
            parsed := true;
         with _ -> () end;
         if not !parsed then begin
            Printf.eprintf "Couldn't parse -D flag definition: \"%s\"\n" str;
            exit 2;
         end
   end with Not_found ->
      add_simple_expr_macro str [] (let loc = (0,0) in <:expr< () >>)

(* This function turns simple expressions to patterns. *)
let rec expr2patt expr =
   let loc = loc_of_expr expr in
      match expr with
       | <:expr< $chr:c$ >> -> <:patt< $chr:c$ >>
       | <:expr< $str:s$ >> -> <:patt< $str:s$ >>
       | <:expr< $int:s$ >> -> <:patt< $int:s$ >>
       | <:expr< $lid:i$ >> -> <:patt< $lid:i$ >>
       | <:expr< $uid:s$ >> -> <:patt< $uid:s$ >>
       | <:expr< $e1$ . $e2$ >> ->
            let p1 = expr2patt e1 and p2 = expr2patt e2
            in <:patt< $p1$ . $p2$ >>
       | <:expr< $e1$ $e2$ >> ->
            let p1 = expr2patt e1 and p2 = expr2patt e2
            in <:patt< $p1$ $p2$ >>
       | <:expr< ( $list:el$ ) >> ->
            let pl = List.map expr2patt el in
               <:patt< ( $list:pl$ ) >>
       | <:expr< { $list:eel$ } >> ->
            let ppl =
               List.map
                  (fun (e1, e2) -> ((expr2patt e1), (expr2patt e2)))
                  eel
            in <:patt< { $list:ppl$ } >>
       | _ -> failwith "could not convert expression to pattern."

(* This function turns simple expressions to patterns. *)
let rec expr2ctyp expr =
   let loc = loc_of_expr expr in
      match expr with
       | <:expr< $lid:i$ >> -> <:ctyp< $lid:i$ >>
       | <:expr< $uid:s$ >> -> <:ctyp< $uid:s$ >>
       | <:expr< $e1$ . $e2$ >> ->
            let ct1 = expr2ctyp e1 and ct2 = expr2ctyp e2
            in <:ctyp< $ct1$ . $ct2$ >>
       | <:expr< $e1$ $e2$ >> ->
            let ct1 = expr2ctyp e1 and ct2 = expr2ctyp e2
            in <:ctyp< $ct1$ $ct2$ >>
       | <:expr< ( $list:el$ ) >> ->
            let ctl = List.map expr2ctyp el in
               <:ctyp< ( $list:ctl$ ) >>
       | _ -> failwith "could not convert expression to type."

(* This function turns simple expressions to patterns. *)
let rec expr2mexp expr =
   let loc = loc_of_expr expr in
      match expr with
       | <:expr< $uid:s$ >> -> <:module_expr< $uid:s$ >>
       | <:expr< $e1$ . $e2$ >> ->
            let me1 = expr2mexp e1 and me2 = expr2mexp e2
            in <:module_expr< $me1$ . $me2$ >>
       | <:expr< $e1$ $e2$ >> ->
            let me1 = expr2mexp e1 and me2 = expr2mexp e2
            in <:module_expr< $me1$ $me2$ >>
       | _ -> failwith "could not convert expression to module expression."

(* This function turns simple expressions to patterns. *)
let rec expr2mtyp expr =
   let loc = loc_of_expr expr in
      match expr with
       | <:expr< $lid:i$ >> -> <:module_type< $lid:i$ >>
       | <:expr< $uid:s$ >> -> <:module_type< $uid:s$ >>
       | <:expr< $e1$ . $e2$ >> ->
            let mt1 = expr2mtyp e1 and mt2 = expr2mtyp e2
            in <:module_type< $mt1$ . $mt2$ >>
       | <:expr< $e1$ $e2$ >> ->
            let mt1 = expr2mtyp e1 and mt2 = expr2mtyp e2
            in <:module_type< $mt1$ $mt2$ >>
       | _ -> failwith "could not convert expression to module expression."

(* This is a list of directories to search for INCLUDE statements. *)
let include_dirs = ref ["./"]

(* Add something to the above, make sure it ends with a slash. *)
let add_include_dir str =
   if str <> "" then
      let str = if String.get str ((String.length str)-1) = '/'
                then str else str ^ "/" in
         include_dirs := !include_dirs @ [str]

let read_file file pa =
   (* This is copied from camlp4/argl.ml's 'process'. *)
   let file =
      try (List.find (fun dir -> Sys.file_exists (dir ^ file)) !include_dirs)
          ^ file
      with Not_found -> file in
   let old_name = !input_file in
   let ic       = open_in_bin file in
   let clear()  = close_in ic in
   let cs       = Stream.of_channel ic in
      input_file := file;
      let phr = try Grammar.Entry.parse pa cs with x -> clear(); raise x
      in
         clear();
         (* Note: input_file isn't restored above when on an error. *)
         input_file := old_name;
         phr



(*****************************************************************************)
(* Syntax extensions *)

type str_item_or_def =
   | MaStr of str_item                    (* normal str_item *)
   | MaSig of sig_item                    (* normal sig_item *)
   | MaDfe of string * string list * expr (* an expr macro def. statement *)
   | MaDfp of string * string list * patt (* a patt macro def. statement *)
   | MaDfa of string * string list * expr (* all types (should convert expr) *)
   | MaUnd of string                      (* a macro undefine statement *)
   | MaIfd of string * str_item_or_def list * str_item_or_def list
                                          (* a conditional str_item *)
   | MaLst of str_item_or_def list        (* str_items to scan *)
   | MaInc of string                      (* a file to include *)

let handle_macstuff_str loc =
   let rec aux macstuff =
      let nothing = <:str_item< declare end >> in
      match macstuff with
       | MaStr si -> si
       | MaSig _ -> failwith "Something bad happened."
       | MaDfe n a e -> add_simple_expr_macro n a e; nothing
       | MaDfp n a p -> add_simple_patt_macro n a p; nothing
       | MaDfa n a e -> add_simple_expr_macro n a e;
            (try add_simple_patt_macro n a (expr2patt e) with _ -> ());
            (try add_simple_ctyp_macro n a (expr2ctyp e) with _ -> ());
            (try add_simple_mexp_macro n a (expr2mexp e) with _ -> ());
            (try add_simple_mtyp_macro n a (expr2mtyp e) with _ -> ());
            nothing
       | MaUnd x -> undefine_macro x; nothing
       | MaIfd x l1 l2 -> aux (MaLst (if is_defined x then l1 else l2))
       | MaLst l ->
            (match List.filter (fun x -> x<>nothing) (List.map aux l)
             with
              | []   -> nothing
              | [si] -> si
              | sis  -> <:str_item< declare $list:sis$ end >>)
       | MaInc file -> StDcl (loc,
                              List.map fst
                                 (Codewalk.changeloc
                                     loc
                                     (Codewalk.ast_list Codewalk.str_item)
                                     (read_file file implem)))
   in
      aux

let handle_macstuff_sig loc =
   let rec aux macstuff =
      let nothing = <:sig_item< declare end >> in
      match macstuff with
       | MaSig si -> si
       | MaStr _ -> failwith "Something bad happened."
       | MaDfe n a e -> add_simple_expr_macro n a e; nothing
       | MaDfp n a p -> add_simple_patt_macro n a p; nothing
       | MaDfa n a e -> add_simple_expr_macro n a e;
            (try add_simple_patt_macro n a (expr2patt e) with _ -> ());
            (try add_simple_ctyp_macro n a (expr2ctyp e) with _ -> ());
            (try add_simple_mexp_macro n a (expr2mexp e) with _ -> ());
            (try add_simple_mtyp_macro n a (expr2mtyp e) with _ -> ());
            nothing
       | MaUnd x -> undefine_macro x; nothing
       | MaIfd x l1 l2 -> aux (MaLst (if is_defined x then l1 else l2))
       | MaLst l ->
            (match List.filter (fun x -> x<>nothing) (List.map aux l)
             with
              | []   -> nothing
              | [si] -> si
              | sis  -> <:sig_item< declare $list:sis$ end >>)
       | MaInc file -> SgDcl (loc,
                              List.map fst
                                 (Codewalk.changeloc
                                     loc
                                     (Codewalk.ast_list Codewalk.sig_item)
                                     (read_file file interf)))
   in
      aux

EXTEND
   GLOBAL: str_item sig_item expr patt ctyp module_expr module_type;
   (* Note: macstuff is macro definitions etc, in these cases, there is no
    * substitutions done because we want them as they are. *)
   str_item: FIRST
      [[ x = macstuff_str -> (* macro directives etc *)
            handle_macstuff_str loc x
       | si = NEXT -> (* expand macros etc *)
            remove_nothings Codewalk.str_item
                            (macros_codewalk Codewalk.str_item si)
       ]];
   sig_item: FIRST
      [[ x = macstuff_sig -> (* macro directives etc *)
            handle_macstuff_sig loc x
       | si = NEXT -> (* expand macros etc *)
            remove_nothings Codewalk.sig_item
                            (macros_codewalk Codewalk.sig_item si)
       ]];
   macstuff_str:
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
       | x = macstuff_shared -> x
       ]];
   macstuff_sig:
      [[ "IFDEF"; c = UIDENT; "THEN"; e1 = LIST0 sig_item_macstuff;
         "ELSE"; e2 = LIST0 sig_item_macstuff; "ENDIF" ->
            MaIfd c e1 e2
       | "IFDEF"; c = UIDENT; "THEN"; e1 = LIST0 sig_item_macstuff; "ENDIF" ->
            MaIfd c e1 []
       | "IFNDEF"; c = UIDENT; "THEN"; e1 = LIST0 sig_item_macstuff;
         "ELSE"; e2 = LIST0 sig_item_macstuff; "ENDIF" ->
            MaIfd c e2 e1
       | "IFNDEF"; c = UIDENT; "THEN"; e1 = LIST0 sig_item_macstuff; "ENDIF"->
            MaIfd c [] e1
       | x = macstuff_shared -> x
       ]];
   macstuff_shared:
      [[ "DEFINE"; c = UIDENT ->
            MaDfe c [] <:expr< () >>
       | "DEFINE"; name = UIDENT; "="; body = expr ->
            (* can only be used for argument-less macros because of camlp4. *)
            MaDfa name [] body
       | ["UNDEFINE" | "UNDEF"]; c = UIDENT ->
            (* UNDEF is the same as UNDEFINE to mimic CPP *)
            MaUnd c
       | "DEFMACRO";     name= UIDENT; args = LIST0 UIDENT; "="; body = expr ->
            MaDfa name args body
       | "DEFEXPRMACRO"; name= UIDENT; args = LIST0 UIDENT; "="; body = expr ->
            MaDfe name args body
       | "DEFPATTMACRO"; name= UIDENT; args = LIST0 UIDENT; "="; body = patt ->
            MaDfp name args body
       | "INCLUDE"; file = STRING ->
            MaInc file
       ]];
   str_item_macstuff:
      [[ d = macstuff_str -> d
       | sis = str_item -> MaStr sis
       ]];
   sig_item_macstuff:
      [[ d = macstuff_sig -> d
       | sis = sig_item -> MaSig sis
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
               let result = macros_codewalk Codewalk.expr e in
                  expr_macros := old_macros;
                  result
       ]];
   expr: LEVEL "simple" [[ "NOTHING" -> <:expr< $lid:"!NOTHING"$ >> ]];
   patt: LEVEL "simple" [[ "NOTHING" -> <:patt< $lid:"!NOTHING"$ >> ]];
   ctyp: LEVEL "simple" [[ "NOTHING" -> <:ctyp< $lid:"!NOTHING"$ >> ]];
   module_expr: [[ "NOTHING" -> <:module_expr< $uid:"!NOTHING"$ >> ]];
   module_type: [[ "NOTHING" -> <:module_type< $uid:"!NOTHING"$ >> ]];
END



(*****************************************************************************)
(* Command line options *)

let _ =
   add_option "-D" (Arg.String define_str)
      "<string>   Define for IFDEF's." ;
   add_option "-U" (Arg.String undefine_macro)
      "<string>   Undefine for IFDEF's.";
   add_option "-I" (Arg.String add_include_dir)
      "<string>   Add a path for INCLUDE."
