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

open Pcaml
open MLast
open Printf


(******************************************************************************)
(* DEFINE / UNDEFINE extension *)

let list_remove x l =
   List.fold_right (fun e l -> if e = x then l else e::l) l []

let defined = ref []
let define   x = defined := x :: !defined
let undefine x = defined := list_remove x !defined

type str_item_or_def =
   SdStr of str_item | SdDef of string | SdUnd of string | SdNop

EXTEND
   GLOBAL: expr str_item;
   expr: LEVEL "top"
      [[ "IFDEF"; c = UIDENT; "THEN"; e1 = expr; "ELSE"; e2 = expr ->
            if List.mem c !defined then e1 else e2 ]];
   str_item: FIRST
      [[ x = def_undef ->
            match x with
              SdStr si -> si
            | SdDef x -> define x;   <:str_item< declare end >>
            | SdUnd x -> undefine x; <:str_item< declare end >>
            | SdNop   ->             <:str_item< declare end >> ]];
   def_undef:
      [[ "IFDEF"; c = UIDENT; "THEN"; e1 = str_item_def_undef;
         "ELSE"; e2 = str_item_def_undef ->
            if List.mem c !defined then e1 else e2
       | "IFDEF"; c = UIDENT; "THEN"; e1 = str_item_def_undef ->
            if List.mem c !defined then e1 else SdNop
       | "DEFINE"; c = UIDENT -> SdDef c
       | "UNDEFINE"; c = UIDENT -> SdUnd c ]];
   str_item_def_undef:
      [[ d = def_undef -> d
       | si = Pcaml.str_item -> SdStr si ]];
END

let _ =
   add_option "-D" (Arg.String define)   "<string>   Define for ifdef instruction." ;
   add_option "-U" (Arg.String undefine) "<string>   Undefine for ifdef instruction."


(******************************************************************************)
(* DEFMACRO extensions *)

EXTEND
   GLOBAL: str_item expr;
   (*
    * Transform: "DEF...MACRO MAC ARG... = body"  -->  "let !MAC (ARG...) = body"
    * Note: the name & arguments are uids, but the expression formed uses lids
    * since str_item won't parse them otherwise.
    * The forms are DEFMACRO/DEFEXPRMACRO for expression macros,
    *               DEFPATTMACRO for pattern macros
    *           and DEFEXPRPATTMACRO for both (the body should be parsable as both)
    *)
   str_item: FIRST
      [[ "DEFEXPRMACRO"; name = UIDENT; args = LIST0 UIDENT; "="; body = expr ->
            let args = List.map (fun arg -> <:patt< $lid:arg$ >>) args in
            let args = <:patt< ($list:args$) >> in
            let name = "!" ^ name in
               <:str_item< value $lid:name$ = fun $args$ -> $body$ >>
       | "DEFMACRO"; name = UIDENT; args = LIST0 UIDENT; "="; body = expr ->
            let args = List.map (fun arg -> <:patt< $lid:arg$ >>) args in
            let args = <:patt< ($list:args$) >> in
            let name = "!" ^ name in
               <:str_item< value $lid:name$ = fun $args$ -> $body$ >>
       (*
        * Note: the transformation of pattern macros is a little different so they
        * wouldn't be mistaken as expression macros.
        *)
       | "DEFPATTMACRO"; name = UIDENT; args = LIST0 UIDENT; "="; body = patt ->
            let args = List.map (fun arg -> <:patt< $lid:arg$ >>) args in
            let args = <:patt< ($list:args$) >> in
            let name = "!" ^ name in
               <:str_item< value $lid:name$ = match pattmac with $body$ -> fun $args$ -> () >>
       | "DEFEXPRPATTMACRO"; name = UIDENT; args = LIST0 UIDENT; "="; body = patt ->
            let args = List.map (fun arg -> <:patt< $lid:arg$ >>) args in
            let args = <:patt< ($list:args$) >> in
            let name = "!" ^ name in
               <:str_item< value $lid:name$ = match exprpattmac with $body$ -> fun $args$ -> () >>
       ]];
END


(******************************************************************************)
(* Macro postprocessing *)

(*
 * A macro is a function that takes a list of exprs and returns an expr.
 * There are several useful utilities:
 *   make_simple_expr_macro arg-list body
 *   make_simple_patt_macro arg-list body
 *     takes a list of variable names (string) and an expr/patt and returns
 *     a simple template macro
 *   append_exprs expr expr-list
 *   append_patts expr expr-list
 *     when expr-list = e1, e2,... -- returns the expr/patt (expr e1 e2...)
 *   macro_error msg
 *     prints an error msg with the current macro name and its location
 *)

(* This function turns simple patterns to expressions for exprpatt macros. *)
let rec patt_to_expr patt =
   let loc = loc_of_patt patt in
      match patt with
         <:patt< $chr:c$ >> -> <:expr< $chr:c$ >>
       | <:patt< $str:s$ >> -> <:expr< $str:s$ >>
       | <:patt< $int:s$ >> -> <:expr< $int:s$ >>
       | <:patt< $lid:i$ >> -> <:expr< $lid:i$ >>
       | <:patt< $uid:s$ >> -> <:expr< $uid:s$ >>
       | <:patt< $p1$ . $p2$ >> ->
            let e1 = patt_to_expr p1 and e2 = patt_to_expr p2
            in <:expr< $e1$ . $e2$ >>
       | <:patt< $p1$ $p2$ >> ->
            let e1 = patt_to_expr p1 and e2 = patt_to_expr p2
            in <:expr< $e1$ $e2$ >>
       | <:patt< ( $list:pl$ ) >> ->
            let el = List.map patt_to_expr pl in
               <:expr< ( $list:el$ ) >>
       | <:patt< { $list:ppl$ } >> ->
            let eel =
               List.map (fun (p1, p2) -> ((patt_to_expr p1), (patt_to_expr p2)))
                        ppl
            in <:expr< { $list:eel$ } >>
       | patt -> let (b, e) = loc in
            eprintf "could not convert pattern to expression at %d-%d.\n" b e;
            exit 1

(* This is for errors and locs while expanding macros. *)
let current_macname_loc = ref ("", 0, 0)

(* Report an error, prints the macro name and its position. *)
let macro_error msg =
   match !current_macname_loc with (name, b, e) ->
      eprintf "While expanding \"%s\" at %d-%d: %s.\n"
         name b e msg;
      exit 1

(* Return current expanded macro location, for new constructions. *)
let current_loc () =
   match !current_macname_loc with (_, a, b) -> (a, b)

let expr_macros = ref []
let patt_macros = ref []

let add_expr_macro name expander =
   expr_macros := (name, expander) :: !expr_macros

let add_patt_macro name expander =
   patt_macros := (name, expander) :: !patt_macros

(* append_exprs expr [e1; e2; ...] --> "expr e1 e2..." *)
let rec append_exprs expr = function
   [] -> expr
 | e::exprs ->
      let loc = loc_of_expr expr in
         append_exprs <:expr< $expr$ $e$ >> exprs

(* append_patts patt [p1; p2; ...] --> "patt p1 p2..." *)
let rec append_patts patt = function
   [] -> patt
 | p::patts ->
      let loc = loc_of_patt patt in
         append_patts <:patt< $patt$ $p$ >> patts

let _ =
   add_expr_macro "CONCAT"
      (function
          [] | [_] -> macro_error "expecting two identifiers"
        | x :: y :: exprs ->
             let loc = (fst (current_loc ())), (snd (loc_of_expr y)) in
             let id = match x, y with
                <:expr< $lid:x$ >>, <:expr< $lid:y$ >> -> <:expr< $lid:x^y$ >>
              | <:expr< $lid:x$ >>, <:expr< $uid:y$ >> -> <:expr< $lid:x^y$ >>
              | <:expr< $uid:x$ >>, <:expr< $lid:y$ >> -> <:expr< $uid:x^y$ >>
              | <:expr< $uid:x$ >>, <:expr< $uid:y$ >> -> <:expr< $uid:x^y$ >>
              | _ -> macro_error "expected two identifiers, got some other expression"
             in
                append_exprs <:expr< $id$ >> exprs)

let _ =
   add_patt_macro "CONCAT"
      (function
          [] | [_] -> macro_error "expecting two identifiers"
        | x :: y :: patts ->
             let loc = (fst (current_loc ())), (snd (loc_of_patt y)) in
             let id = match x, y with
                <:patt< $lid:x$ >>, <:patt< $lid:y$ >> -> <:patt< $lid:x^y$ >>
              | <:patt< $lid:x$ >>, <:patt< $uid:y$ >> -> <:patt< $lid:x^y$ >>
              | <:patt< $uid:x$ >>, <:patt< $lid:y$ >> -> <:patt< $uid:x^y$ >>
              | <:patt< $uid:x$ >>, <:patt< $uid:y$ >> -> <:patt< $uid:x^y$ >>
              | _ -> macro_error "expected two identifiers, got some other expression"
             in
                append_patts <:patt< $id$ >> patts)

(*
 * Get an expression "x y z...",
 * return a pair of vars/body of macro, and the flat list of expressions.
 *)
let flat_mac_expr expr =
   let rec aux expr =
      match expr with
         <:expr< $e1$ $e2$ >> ->
            let (name_mac, args) = aux e1 in (name_mac, e2 :: args)
       | <:expr< $uid:str$ >> -> ((str, List.assoc str !expr_macros), [])
       | _ -> raise Not_found
   in
   let ((name, mac), exprs) = (aux expr) in
      (name, mac, List.rev exprs)

(*
 * Get a pattern "x y z...",
 * return a pair of vars/body of macro, and the flat list of patterns.
 *)
let flat_mac_patt patt =
   let rec aux patt =
      match patt with
         <:patt< $p1$ $p2$ >> ->
            let (name_mac, args) = aux p1 in (name_mac, p2 :: args)
       | <:patt< $uid:str$ >> -> ((str, List.assoc str !patt_macros), [])
       | _ -> raise Not_found
   in
   let ((name, mac), patts) = (aux patt) in
      (name, mac, List.rev patts)

let rec make_simple_expr_macro args body exprs =
   let rec aux = function
      (a::args, e::exprs) ->
         (* Combine macro list *)
         add_expr_macro a (make_simple_expr_macro [] e);
         aux (args, exprs)
    | ([], exprs) ->
         (* Do the substitution, append extra exprs *)
         append_exprs (process_expr body) exprs
    | (args, _) ->
         (* Not enough expressions *)
         macro_error "not enough arguments for simple macro";
   in
   let old_macros = !expr_macros in
      expr_macros := [];
      let result = aux (args, exprs) in
         expr_macros := old_macros;
         result

and make_simple_patt_macro args body patts =
   let rec aux = function
      (a::args, e::patts) ->
         (* Combine macro list *)
         add_patt_macro a (make_simple_patt_macro [] e);
         aux (args, patts)
    | ([], patts) ->
         (* Do the substitution, append extra patts *)
         append_patts (process_patt body) patts
    | (args, _) ->
         (* Not enough expressions *)
         macro_error "not enough arguments for simple macro";
   in
   let old_macros = !patt_macros in
      patt_macros := [];
      let result = aux (args, patts) in
         patt_macros := old_macros;
         result

and substitute_expr_macros expr =
   try match flat_mac_expr expr with (name, mac, exprs) ->
      current_macname_loc := (let (b,e) = loc_of_expr expr in (name, b, e));
      (* Must call process again *)
      process_expr (mac exprs)
   with Not_found -> expr

and substitute_patt_macros patt =
   try match flat_mac_patt patt with (name, mac, patts) ->
      current_macname_loc := (let (b,e) = loc_of_patt patt in (name, b, e));
      (* Must call process again *)
      process_patt (mac patts)
   with Not_found -> patt


(* Postprocessing by code-walk *)

and process_patt_patt (x, y) =
   (process_patt x, process_patt y)

and process_patt_expr (x, y) =
   (process_patt x, process_expr y)

and process_string_ctyp (x, y) =
   (x, process_ctyp y)

and process_string_bool_ctyp (x, y, z) =
   (x, y, process_ctyp z)

and process_string_ctyplist (x, y) =
   (x, List.map process_ctyp y)

and process_string_stringlist_ctyp (x, y, z) =
   (x, y, process_ctyp z)

and process_string_expr (x, y) =
   (x, process_expr y)

and process_expr_expr (x, y) =
   (process_expr x, process_expr y)

and process_patt_expropt_expr (x, y, z) =
   (process_patt x, process_expropt y, process_expr z)

and process_expropt x =
   match x with
      Some x -> Some (process_expr x)
    | None   -> None

and process_ctypopt x =
   match x with
      Some x -> Some (process_ctyp x)
    | None   -> None

and process_pattopt x =
   match x with
      Some x -> Some (process_patt x)
    | None   -> None

and process_ctyp x =
   match x with
      TyAcc (loc, x, y)    -> TyAcc (loc, process_ctyp x, process_ctyp y)
    | TyAli (loc, x, y)    -> TyAli (loc, process_ctyp x, process_ctyp y)
    | TyAny (loc)          -> TyAny (loc)
    | TyApp (loc, x, y)    -> TyApp (loc, process_ctyp x, process_ctyp y)
    | TyArr (loc, x, y)    -> TyArr (loc, process_ctyp x, process_ctyp y)
    | TyCls (loc, x)       -> TyCls (loc, x)
    | TyLid (loc, x)       -> TyLid (loc, x)
    | TyMan (loc, x, y)    -> TyMan (loc, process_ctyp x, process_ctyp y)
    | TyObj (loc, x, y)    -> TyObj (loc, List.map process_string_ctyp x, y)
    | TyQuo (loc, x)       -> TyQuo (loc, x)
    | TyRec (loc, x)       -> TyRec (loc, List.map process_string_bool_ctyp x)
    | TySum (loc, x)       -> TySum (loc, List.map process_string_ctyplist x)
    | TyTup (loc, x)       -> TyTup (loc, List.map process_ctyp x)
    | TyUid (loc, x)       -> TyUid (loc, x)
    | TyXnd (loc, s, x)    -> TyXnd (loc, s, List.map process_ctyp x)

and process_patt x =
   let x = substitute_patt_macros x in
   match x with
      PaAcc (loc, x, y)    -> PaAcc (loc, process_patt x, process_patt y)
    | PaAli (loc, x, y)    -> PaAli (loc, process_patt x, process_patt y)
    | PaAnt (loc, x)       -> PaAnt (loc, process_patt x)
    | PaAny (loc)          -> PaAny (loc)
    | PaApp (loc, x, y)    -> PaApp (loc, process_patt x, process_patt y)
    | PaArr (loc, x)       -> PaArr (loc, List.map process_patt x)
    | PaChr (loc, x)       -> PaChr (loc, x)
    | PaInt (loc, x)       -> PaInt (loc, x)
    | PaLid (loc, x)       -> PaLid (loc, x)
    | PaOrp (loc, x, y)    -> PaOrp (loc, process_patt x, process_patt y)
    | PaRng (loc, x, y)    -> PaRng (loc, process_patt x, process_patt y)
    | PaRec (loc, x)       -> PaRec (loc, List.map process_patt_patt x)
    | PaStr (loc, x)       -> PaStr (loc, x)
    | PaTup (loc, x)       -> PaTup (loc, List.map process_patt x)
    | PaTyc (loc, x, y)    -> PaTyc (loc, process_patt x, process_ctyp y)
    | PaUid (loc, x)       -> PaUid (loc, x)
    | PaXnd (loc, s, x)    -> PaXnd (loc, s, process_patt x)

and process_class_type_infos x =
   match x with
      { ciLoc = v; ciVir = w; ciPrm = x; ciNam = y; ciExp = z } ->
         { ciLoc = v; ciVir = w; ciPrm = x; ciNam = y; ciExp = process_class_type z }

and process_class_expr_infos x =
   match x with
      { ciLoc = v; ciVir = w; ciPrm = x; ciNam = y; ciExp = z } ->
         { ciLoc = v; ciVir = w; ciPrm = x; ciNam = y; ciExp = process_class_expr z }

and process_expr x =
   let x = substitute_expr_macros x in
   match x with
      ExAcc (loc, x, y)    -> ExAcc (loc, process_expr x, process_expr y)
    | ExAnt (loc, x)       -> ExAnt (loc, process_expr x)
    | ExApp (loc, x, y)    -> ExApp (loc, process_expr x, process_expr y)
    | ExAre (loc, x, y)    -> ExAre (loc, process_expr x, process_expr y)
    | ExArr (loc, x)       -> ExArr (loc, List.map process_expr x)
    | ExAss (loc, x, y)    -> ExAss (loc, process_expr x, process_expr y)
    | ExChr (loc, x)       -> ExChr (loc, x)
    | ExCoe (loc, x, y)    -> ExCoe (loc, process_expr x, process_ctyp y)
    | ExFlo (loc, x)       -> ExFlo (loc, x)
    | ExFor (loc, x, y, z, v, w) -> ExFor (loc, x, process_expr y, process_expr z, v, List.map process_expr w)
    | ExFun (loc, x)       -> ExFun (loc, List.map process_patt_expropt_expr x)
    | ExIfe (loc, x, y, z) -> ExIfe (loc, process_expr x, process_expr y, process_expr z)
    | ExInt (loc, x)       -> ExInt (loc, x)
    | ExLet (loc, x, y, z) -> ExLet (loc, x, List.map process_patt_expr y, process_expr z)
    | ExLid (loc, x)       -> ExLid (loc, x)
    | ExLmd (loc, x, y, z) -> ExLmd (loc, x, process_module_expr y, process_expr z)
    | ExMat (loc, x, y)    -> ExMat (loc, process_expr x, List.map process_patt_expropt_expr y)
    | ExNew (loc, x)       -> ExNew (loc, x)
    | ExOvr (loc, x)       -> ExOvr (loc, List.map process_string_expr x)
    | ExRec (loc, x, y)    -> ExRec (loc, List.map process_expr_expr x, process_expropt y)
    | ExSeq (loc, x, y)    -> ExSeq (loc, List.map process_expr x, process_expr y)
    | ExSnd (loc, x, y)    -> ExSnd (loc, process_expr x, y)
    | ExSte (loc, x, y)    -> ExSte (loc, process_expr x, process_expr y)
    | ExStr (loc, x)       -> ExStr (loc, x)
    | ExTry (loc, x, y)    -> ExTry (loc, process_expr x, List.map process_patt_expropt_expr y)
    | ExTup (loc, x)       -> ExTup (loc, List.map process_expr x)
    | ExTyc (loc, x, y)    -> ExTyc (loc, process_expr x, process_ctyp y)
    | ExUid (loc, x)       -> ExUid (loc, x)
    | ExWhi (loc, x, y)    -> ExWhi (loc, process_expr x, List.map process_expr y)
    | ExXnd (loc, s, x)    -> ExXnd (loc, s, process_expr x)

and process_module_type x =
   match x with
      MtAcc (loc, x, y)    -> MtAcc (loc, process_module_type x, process_module_type y)
    | MtApp (loc, x, y)    -> MtApp (loc, process_module_type x, process_module_type y)
    | MtFun (loc, x, y, z) -> MtFun (loc, x, process_module_type y, process_module_type z)
    | MtLid (loc, x)       -> MtLid (loc, x)
    | MtSig (loc, x)       -> MtSig (loc, List.map process_sig_item x)
    | MtUid (loc, x)       -> MtUid (loc, x)
    | MtWit (loc, x, y)    -> MtWit (loc, process_module_type x, List.map process_with_constr y)

and process_sig_item x =
   match x with
      SgCls (loc, x)       -> SgCls (loc, List.map process_class_type_infos x)
    | SgClt (loc, x)       -> SgClt (loc, List.map process_class_type_infos x)
    | SgDcl (loc, x)       -> SgDcl (loc, List.map process_sig_item x)
    | SgExc (loc, x, y)    -> SgExc (loc, x, List.map process_ctyp y)
    | SgExt (loc, x, y, z) -> SgExt (loc, x, process_ctyp y, z)
    | SgInc (loc, x)       -> SgInc (loc, process_module_type x)
    | SgMod (loc, x, y)    -> SgMod (loc, x, process_module_type y)
    | SgMty (loc, x, y)    -> SgMty (loc, x, process_module_type y)
    | SgOpn (loc, x)       -> SgOpn (loc, x)
    | SgTyp (loc, x)       -> SgTyp (loc, List.map process_string_stringlist_ctyp x)
    | SgVal (loc, x, y)    -> SgVal (loc, x, process_ctyp y)

and process_with_constr x =
   match x with
      WcTyp (loc, x, y, z) -> WcTyp (loc, x, y, process_ctyp z)
    | WcMod (loc, x, y)    -> WcMod (loc, x, process_module_type y)

and process_module_expr x =
   match x with
      MeAcc (loc, x, y)    -> MeAcc (loc, process_module_expr x, process_module_expr y)
    | MeApp (loc, x, y)    -> MeApp (loc, process_module_expr x, process_module_expr y)
    | MeFun (loc, x, y, z) -> MeFun (loc, x, process_module_type y, process_module_expr z)
    | MeStr (loc, x)       -> MeStr (loc, List.map process_str_item x)
    | MeTyc (loc, x, y)    -> MeTyc (loc, process_module_expr x, process_module_type y)
    | MeUid (loc, x)       -> MeUid (loc, x)

and process_str_item x =
   match x with
      StVal ( loc, _, _ ) -> begin
         match x with
          |  <:str_item< value $lid:mac$ = fun ($list:args$) -> $body$ >>
               when String.get mac 0 = '!' ->
                  let mac = String.sub mac 1 (String.length mac - 1) in
                  let args = List.map
                                (fun (arg) ->
                                    match arg with
                                       <:patt< $lid:arg$ >> -> arg
                                     | _ -> raise (Failure "Something bad happened"))
                                args
                  in
                     add_expr_macro mac (make_simple_expr_macro args body);
                     <:str_item< declare end >>
          | <:str_item< value $lid:mac$ = match $lid:macid$ with $body$ -> fun ($list:args$) -> () >>
               when String.get mac 0 = '!' ->
                  let mac = String.sub mac 1 (String.length mac - 1) in
                  let args = List.map
                                (fun (arg) ->
                                    match arg with
                                       <:patt< $lid:arg$ >> -> arg
                                     | _ -> raise (Failure "Something bad happened"))
                                args
                  in begin
                     match macid with
                        "pattmac" ->
                           add_patt_macro mac (make_simple_patt_macro args body);
                           <:str_item< declare end >>
                      | "exprpattmac" ->
                           add_expr_macro mac (make_simple_expr_macro args (patt_to_expr body));
                           add_patt_macro mac (make_simple_patt_macro args body);
                           <:str_item< declare end >>
                      | _ -> raise (Failure "Something bad happened")
                  end
          | StVal (loc, x, y) -> StVal (loc, x, List.map process_patt_expr y)
          | _ -> raise (Failure "Something bad happened")
      end
    | StCls (loc, x)       -> StCls (loc, List.map process_class_expr_infos x)
    | StClt (loc, x)       -> StClt (loc, List.map process_class_type_infos x)
    | StDcl (loc, x)       -> StDcl (loc, List.map process_str_item x)
    | StExc (loc, x, y)    -> StExc (loc, x, List.map process_ctyp y)
    | StExp (loc, x)       -> StExp (loc, process_expr x)
    | StExt (loc, x, y, z) -> StExt (loc, x, process_ctyp y, z)
    | StMod (loc, x, y)    -> StMod (loc, x, process_module_expr y)
    | StMty (loc, x, y)    -> StMty (loc, x, process_module_type y)
    | StOpn (loc, x)       -> StOpn (loc, x)
    | StTyp (loc, x)       -> StTyp (loc, List.map process_string_stringlist_ctyp x)

and process_class_type x =
   match x with
      CtCon (loc, x, y)    -> CtCon (loc, x, List.map process_ctyp y)
    | CtFun (loc, x, y)    -> CtFun (loc, process_ctyp x, process_class_type y)
    | CtSig (loc, x, y)    -> CtSig (loc, process_ctypopt x, List.map process_class_sig_item y)
    | CtXnd (loc, s, x)    -> CtXnd (loc, s, process_class_type x)

and process_class_sig_item x =
   match x with
      CgCtr (loc, x, y)    -> CgCtr (loc, process_ctyp x, process_ctyp y)
    | CgInh (loc, x)       -> CgInh (loc, process_class_type x)
    | CgMth (loc, x, y, z) -> CgMth (loc, x, y, process_ctyp z)
    | CgVal (loc, x, y, z) -> CgVal (loc, x, y, process_ctyp z)
    | CgVir (loc, x, y, z) -> CgVir (loc, x, y, process_ctyp z)

and process_class_expr x =
   match x with
      CeApp (loc, x, y)    -> CeApp (loc, process_class_expr x, List.map process_expr y)
    | CeCon (loc, x, y)    -> CeCon (loc, x, List.map process_ctyp y)
    | CeFun (loc, x, y)    -> CeFun (loc, process_patt x, process_class_expr y)
    | CeLet (loc, x, y, z) -> CeLet (loc, x, List.map process_patt_expr y, process_class_expr z)
    | CeStr (loc, x, y)    -> CeStr (loc, process_pattopt x, List.map process_class_str_item y)
    | CeTyc (loc, x, y)    -> CeTyc (loc, process_class_expr x, process_class_type y)
    | CeXnd (loc, s, x)    -> CeXnd (loc, s, process_class_expr x)

and process_class_str_item x =
   match x with
      CrCtr (loc, x, y)    -> CrCtr (loc, process_ctyp x, process_ctyp y)
    | CrInh (loc, x, y)    -> CrInh (loc, process_class_expr x, y)
    | CrIni (loc, x)       -> CrIni (loc, process_expr x)
    | CrMth (loc, x, y, z) -> CrMth (loc, x, y, process_expr z)
    | CrVal (loc, x, y, z) -> CrVal (loc, x, y, process_expr z)
    | CrVir (loc, x, y, z) -> CrVir (loc, x, y, process_ctyp z)

and process_ast_list f ast_list =
   List.map (fun (ast, loc) -> (f ast, loc)) ast_list

(* Install preprocessor before the current printer. *)
let old_print_interf = !print_interf
let old_print_implem = !print_implem
let interf ast_list = old_print_interf (process_ast_list process_sig_item ast_list)
let implem ast_list = old_print_implem (process_ast_list process_str_item ast_list)
let _ = print_interf := interf ; print_implem := implem

