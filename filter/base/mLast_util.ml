(*
 * Handle commenting.
 * We parse a file, and scan in the comments and their
 * locations.  then we pass through the terms, and match their
 * locations with the comments.
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
 * Author: Jason Hickey
 * jyh@cs.cornell.edu
 *)

open Lm_debug
open Lm_printf

open MLast

(*
 * Show the file loading.
 *)
let _ =
   show_loading "Loading MLast_util%t"

(*
 * Iteration functions.
 *)
type 'a fold =
   { fold_expr             : 'a -> MLast.expr -> 'a;
     fold_patt             : 'a -> MLast.patt -> 'a;
     fold_type             : 'a -> MLast.ctyp -> 'a;
     fold_sig_item         : 'a -> MLast.sig_item -> 'a;
     fold_str_item         : 'a -> MLast.str_item -> 'a;
     fold_module_expr      : 'a -> MLast.module_expr -> 'a;
     fold_module_type      : 'a -> MLast.module_type -> 'a;
     fold_with_constr      : 'a -> MLast.with_constr -> 'a;
     fold_class_type_infos : 'a -> MLast.class_type MLast.class_infos -> 'a;
     fold_class_expr_infos : 'a -> MLast.class_expr MLast.class_infos -> 'a;
     fold_class_expr       : 'a -> MLast.class_expr -> 'a;
     fold_class_type       : 'a -> MLast.class_type -> 'a;
     fold_class_str_item   : 'a -> MLast.class_str_item -> 'a;
     fold_class_sig_item   : 'a -> MLast.class_sig_item -> 'a
   }

(*
 * Compute a hash value from the struct.
 *)
let rec fold_expr iter x expr =
   let x = iter.fold_expr x expr in
      match expr with
         (<:expr< $e1$ . $e2$ >>)
       | (<:expr< $e1$ $e2$ >>)
       | (<:expr< $e1$ .( $e2$ ) >>)
       | (<:expr< $e1$ := $e2$ >>)
       | (<:expr< $e1$ .[ $e2$ ] >>) ->
            fold_expr iter (fold_expr iter x e1) e2
       | (<:expr< $anti: e$ >>)
       | (<:expr< ~ $_$ : $e$ >>)
       | (<:expr< ? $_$ : $e$ >>)
       | (<:expr< $e$ # $_$ >>)
       | (<:expr< lazy $e$ >>) ->
            fold_expr iter x e
       | (<:expr< [| $list:el$ |] >>)
       | (<:expr< do { $list:el$ } >>)
       | (<:expr< ( $list:el$ ) >>) ->
            List.fold_left (fold_expr iter) x el
       | (<:expr< $chr:_$ >>)
       | (<:expr< $flo:_$ >>)
       | (<:expr< $int:_$ >>)
       | (<:expr< $lid:_$ >>)
       | (<:expr< new $list:_$ >>)
       | (<:expr< $str:_$ >>)
       | (<:expr< $uid:_$ >>)
       | (<:expr< ` $_$ >>) ->
            x
       | (<:expr< ( $e$ :> $t$ ) >>)
       | (<:expr< ( $e$ : $t$ ) >>) ->
            fold_type iter (fold_expr iter x e) t
       | (<:expr< for $s$ = $e1$ $to:b$ $e2$ do { $list:el$ } >>) ->
            List.fold_left (fold_expr iter) (fold_expr iter (fold_expr iter x e1) e2) el
       | (<:expr< fun [ $list:pwel$ ] >>) ->
            List.fold_left (fold_pwe iter) x pwel
       | (<:expr< if $e1$ then $e2$ else $e3$ >>) ->
           fold_expr iter (fold_expr iter (fold_expr iter x e1) e2) e3
       | (<:expr< let $rec:_$ $list:pel$ in $e$ >>) ->
            fold_expr iter (List.fold_left (fold_pe iter) x pel) e
       | MLast.ExLmd (_, s, me, e) ->
            fold_module_expr iter (fold_expr iter x e) me
       | (<:expr< match $e$ with [ $list:pwel$ ] >>)
       | (<:expr< try $e$ with [ $list:pwel$ ] >>) ->
            List.fold_left (fold_pwe iter) (fold_expr iter x e) pwel
       | (<:expr< {< $list:sel$ >} >>) ->
            List.fold_left (fold_se iter) x sel
(*
       | (<:expr< { $list:eel$ } >>) ->
*)
       | MLast.ExRec (_, eel, eo) ->
            List.fold_left (fold_pe iter) (fold_expr_opt iter x eo) eel
       | (<:expr< while $e$ do { $list:el$ } >>) ->
            List.fold_left (fold_expr iter) (fold_expr iter x e) el
       | MLast.ExCoe (_, e, ot, t) ->
            fold_type iter (fold_type_opt iter (fold_expr iter x e) ot) t

and fold_patt iter x patt =
   let x = iter.fold_patt x patt in
      match patt with
         (<:patt< $p1$ . $p2$ >>)
       | (<:patt< ( $p1$ as $p2$ ) >>)
       | (<:patt< $p1$ $p2$ >>)
       | (<:patt< $p1$ | $p2$ >>)
       | (<:patt< $p1$ .. $p2$ >>) ->
            fold_patt iter (fold_patt iter x p1) p2
       | (<:patt< $anti: p$ >>)
       | (<:patt< ~ $_$ : $p$ >>) ->
(* 3.02
       | (<:patt< ? $_$ : $p$ >>)
       | MLast.PaXnd (_, _, p) ->
 *)
            fold_patt iter x p
       | (<:patt< _ >>)
       | (<:patt< $chr:_$ >>)
       | (<:patt< $flo:_$ >>)
       | (<:patt< $int:_$ >>)
       | (<:patt< $lid:_$ >>)
       | (<:patt< $str:_$ >>)
       | (<:patt< $uid:_$ >>)
       | (<:patt< ` $_$ >>)
       | MLast.PaTyp _ ->
            x
       | (<:patt< [| $list: pl$ |] >>)
       | (<:patt< ( $list:pl$ ) >>) ->
            List.fold_left (fold_patt iter) x pl
       | (<:patt< ? $i$ : ( $p$ = $e$ ) >>) ->
            fold_expr iter (fold_patt iter x p) e
       | (<:patt< { $list:ppl$ } >>) ->
            List.fold_left (fold_pp iter) x ppl
       | (<:patt< ( $p$ : $t$ ) >>) ->
            fold_type iter (fold_patt iter x p) t
       | MLast.PaOlb (_, _, p, eo) ->
            fold_expr_opt iter (fold_patt iter x p) eo

and fold_type iter x t =
   let x = iter.fold_type x t in
      match t with
         (<:ctyp< $t1$ . $t2$ >>)
       | (<:ctyp< $t1$ as $t2$ >>)
       | (<:ctyp< $t1$ $t2$ >>)
       | (<:ctyp< $t1$ -> $t2$ >>)
       | (<:ctyp< $t1$ == $t2$ >>) ->
            fold_type iter (fold_type iter x t1) t2
       | (<:ctyp< _ >>)
       | (<:ctyp< # $list:_$ >>)
       | (<:ctyp< $lid:_$ >>)
       | (<:ctyp< '$_$ >>)
       | (<:ctyp< $uid:_$ >>) ->
            x
       | (<:ctyp< ~ $_$ : $t$ >>)
       | (<:ctyp< ? $_$ : $t$ >>)
       | MLast.TyPol (_, _, t ) ->
            fold_type iter x t
       | (<:ctyp< < $list:stl$ $_$ > >>) ->
            List.fold_left (fold_st iter) x stl
       | (<:ctyp< { $list:sbtl$ } >>) ->
            List.fold_left (fold_sbt iter) x sbtl
       | (<:ctyp< [ $list:stll$ ] >>) ->
            List.fold_left (fold_stl iter) x stll
       | (<:ctyp< ( $list:tl$ ) >>) ->
            List.fold_left (fold_type iter) x tl
       | MLast.TyVrn (_, sbtll, _) ->
            List.fold_left (fold_sbtl iter) x sbtll

and fold_sig_item iter x si =
   let x = iter.fold_sig_item x si in
      match si with
         (<:sig_item< class $list:ctl$ >>)
       | (<:sig_item< class type $list:ctl$ >>) ->
            List.fold_left (fold_class_type_infos iter) x ctl
       | (<:sig_item< declare $list:sil$ end >>) ->
            List.fold_left (fold_sig_item iter) x sil
       | (<:sig_item< # $n$ $opt:dp$ >>) ->
            fold_expr_opt iter x dp
       | (<:sig_item< exception $s$ of $list:tl$ >>) ->
            List.fold_left (fold_type iter) x tl
       | (<:sig_item< external $_$ : $t$ = $list:_$ >>)
       | (<:sig_item< value $_$ : $t$ >>) ->
            fold_type iter x t
       | (<:sig_item< include $mt$ >>)
       | (<:sig_item< module $_$ : $mt$ >>)
       | (<:sig_item< module type $_$ = $mt$ >>) ->
            fold_module_type iter x mt
       | (<:sig_item< open $_$ >>) ->
            x
       | (<:sig_item< type $list:tdl$ >>) ->
            List.fold_left (fold_tdl iter) x tdl

and fold_str_item iter x si =
   let x = iter.fold_str_item x si in
      match si with
         (<:str_item< class $list:cdl$ >>) ->
            List.fold_left (fold_class_expr_infos iter) x cdl
       | (<:str_item< class type $list:cdl$ >>) ->
            List.fold_left (fold_class_type_infos iter) x cdl
       | (<:str_item< declare $list:stl$ end >>) ->
            List.fold_left (fold_str_item iter) x stl
       | (<:str_item< # $n$ $opt:dp$ >>) ->
            fold_expr_opt iter x dp
       | (<:str_item< exception $s$ of $list:tl$ >>) ->
            List.fold_left (fold_type iter) x tl
       | (<:str_item< $exp:e$ >>) ->
            fold_expr iter x e
       | (<:str_item< external $s$ : $t$ = $list:sl$ >>) ->
            fold_type iter x t
       | (<:str_item< include $mt$ >>) ->
            fold_module_expr iter x mt
       | (<:str_item< module $s$ = $me$ >>) ->
            fold_module_expr iter x me
       | (<:str_item< module type $s$ = $mt$ >>) ->
            fold_module_type iter x mt
       | (<:str_item< open $sl$ >>) ->
            x
       | (<:str_item< type $list:tdl$ >>) ->
            List.fold_left (fold_tdl iter) x tdl
       | (<:str_item< value $rec:b$ $list:pel$ >>) ->
            List.fold_left (fold_pe iter) x pel
       | MLast.StExc (_, _, tl, _) ->
            List.fold_left (fold_type iter) x tl

and fold_module_type iter x mt =
   let x = iter.fold_module_type x mt in
      match mt with
         (<:module_type< $mt1$ . $mt2$ >>)
       | (<:module_type< $mt1$ $mt2$ >>)
       | (<:module_type< functor ( $_$ : $mt1$ ) -> $mt2$ >>) ->
            fold_module_type iter (fold_module_type iter x mt1) mt2
       | (<:module_type< $lid:_$ >>)
       | (<:module_type< ' $_$ >>)
       | (<:module_type< $uid:_$ >>) ->
            x
       | (<:module_type< sig $list:sil$ end >>) ->
            List.fold_left (fold_sig_item iter) x sil
       | (<:module_type< $mt$ with $list:wcl$ >>) ->
            List.fold_left (fold_with_constr iter) (fold_module_type iter x mt) wcl

and fold_with_constr iter x wc =
   let x = iter.fold_with_constr x wc in
      match wc with
         MLast.WcTyp (loc, sl1, sl2, t) ->
           fold_type iter x t
       | MLast.WcMod (loc, sl1, mt) ->
           fold_module_expr iter x mt

and fold_module_expr iter x me =
   let x = iter.fold_module_expr x me in
      match me with
         (<:module_expr< $me1$ . $me2$ >>)
       | (<:module_expr< $me1$ $me2$ >>) ->
            fold_module_expr iter (fold_module_expr iter x me1) me2
       | (<:module_expr< functor ( $s$ : $mt$ ) -> $me$ >>) ->
            fold_module_expr iter (fold_module_type iter x mt) me
(*
       | (<:module_expr< $lid:i$ >>) ->
            x
*)
       | (<:module_expr< struct $list:sil$ end >>) ->
            List.fold_left (fold_str_item iter) x sil
       | (<:module_expr< ( $me$ : $mt$) >>) ->
            fold_module_type iter (fold_module_expr iter x me) mt
       | (<:module_expr< $uid:_$ >>) ->
            x

and fold_class_type_infos iter x
  ({ ciLoc = loc;
     ciNam = s;
     ciPrm = sl;
     ciVir = b1;
     ciExp = t } as ct) =
   let x = iter.fold_class_type_infos x ct in
      fold_class_type iter x t

and fold_class_expr_infos iter x
  ({ ciLoc = loc;
     ciNam = s;
     ciPrm = sl;
     ciVir = b1;
     ciExp = e } as ct) =
   let x = iter.fold_class_expr_infos x ct in
      fold_class_expr iter x e

and fold_class_type iter x ct =
   let x = iter.fold_class_type x ct in
      match ct with
         MLast.CtCon (_, _, tl) ->
            List.fold_left (fold_type iter) x tl
       | MLast.CtFun (_, t, ct) ->
            fold_class_type iter (fold_type iter x t) ct
       | MLast.CtSig (_, t, ctfl) ->
            List.fold_left (fold_class_sig_item iter) (fold_type_opt iter x t) ctfl
(* 3.02
       | MLast.CtXnd (_, _, ct) ->
            fold_class_type iter x ct
 *)

and fold_class_sig_item iter x ctf =
   let x = iter.fold_class_sig_item x ctf in
      match ctf with
         CgCtr (_, t1, t2) ->
            fold_type iter (fold_type iter x t1) t2
       | CgDcl (_, tl) ->
            List.fold_left (fold_class_sig_item iter) x tl
       | CgMth (_, _, _, t)
       | CgVal (_, _, _, t)
       | CgVir (_, _, _, t) ->
            fold_type iter x t
       | CgInh (_, ct) ->
            fold_class_type iter x ct

and fold_class_expr iter x ce =
   let x = iter.fold_class_expr x ce in
      match ce with
         MLast.CeApp (_, ce, e) ->
            fold_expr iter (fold_class_expr iter x ce) e
       | MLast.CeCon (_, sl, tl) ->
            List.fold_left (fold_type iter) x tl
       | MLast.CeFun (_, p, ce) ->
            fold_class_expr iter (fold_patt iter x p) ce
       | MLast.CeLet (_, b, pel, ce) ->
            List.fold_left (fold_pe iter) (fold_class_expr iter x ce) pel
       | MLast.CeStr (_, p, cfl) ->
            List.fold_left (fold_class_str_item iter) (fold_patt_opt iter x p) cfl
       | MLast.CeTyc (_, ce, ct) ->
            fold_class_type iter (fold_class_expr iter x ce) ct
(* 3.02
       | MLast.CeXnd (_, _, ce) ->
            fold_class_expr iter x ce
 *)

and fold_class_str_item iter x cf =
   let x = iter.fold_class_str_item x cf in
      match cf with
         CrCtr (_, _, t)
       | CrVir (_, _, _, t) ->
            fold_type iter x t
       | CrDcl (_, tl) ->
            List.fold_left (fold_class_str_item iter) x tl
       | CrInh (_, ce, _) ->
            fold_class_expr iter x ce
       | CrIni (_, e)
       | CrVal (_, _, _, e) ->
            fold_expr iter x e
       | CrMth (_, _, _, e, co) ->
            fold_expr iter (fold_type_opt iter x co) e

(*
 * Combined forms.
 *)
and fold_expr_opt iter x = function
   Some e -> fold_expr iter x e
 | None -> x

and fold_type_opt iter x = function
   Some e -> fold_type iter x e
 | None -> x

and fold_patt_opt iter x = function
   Some p -> fold_patt iter x p
 | None -> x

and fold_pwe iter x (patt, with_expr, expr) =
   fold_expr iter (fold_expr_opt iter (fold_patt iter x patt) with_expr) expr

and fold_pe iter x (patt, expr) =
   fold_expr iter (fold_patt iter x patt) expr

and fold_se iter x (s, e) =
   fold_expr iter x e

and fold_pp iter x (p1, p2) =
   fold_patt iter (fold_patt iter x p1) p2

and fold_st iter x (s, t) =
   fold_type iter x t

and fold_sbt iter x (_, s, b, t) =
   fold_type iter x t

and fold_stl iter x (_, s, tl) =
   List.fold_left (fold_type iter) x tl

and fold_sbtl iter x rf =
   match rf with
      RfTag (_, _, tl) ->
         List.fold_left (fold_type iter) x tl
    | RfInh t ->
         fold_type iter x t

and fold_tt iter x (t1, t2) =
   fold_type iter (fold_type iter x t1) t2

and fold_tdl iter x (s, sl, t, tl) =
   List.fold_left (fold_tt iter) (fold_type iter x t) tl

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
