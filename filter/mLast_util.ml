(*
 * Handle commenting.
 * We parse a file, and scan in the comments and their
 * locations.  then we pass through the terms, and match their
 * locations with the comments.
 *
 * ----------------------------------------------------------------
 *
 * This file is part of Nuprl-Light, a modular, higher order
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

open Printf
open Nl_debug

open MLast

(*
 * Show the file loading.
 *)
let _ =
   if !debug_load then
      eprintf "Loading MLast_util%t" eflush


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
     fold_class_field      : 'a -> MLast.class_field -> 'a;
     fold_class_type       : 'a -> MLast.class_type -> 'a;
     fold_class_type_field : 'a -> MLast.class_type_field -> 'a
   }

(*
 * Compute a hash value from the struct.
 *)
let rec fold_expr iter x expr =
   let x = iter.fold_expr x expr in
      match expr with
         (<:expr< $e1$ . $e2$ >>) ->
            fold_expr iter (fold_expr iter x e1) e2
       | (<:expr< $anti: e$ >>) ->
            fold_expr iter x e
       | (<:expr< $e1$ $e2$ >>) ->
            fold_expr iter (fold_expr iter x e1) e2
       | (<:expr< $e1$ .( $e2$ ) >>) ->
            fold_expr iter (fold_expr iter x e1) e2
       | (<:expr< [| $list:el$ |] >>) ->
            List.fold_left (fold_expr iter) x el
       | (<:expr< $e1$ := $e2$ >>) ->
            fold_expr iter (fold_expr iter x e1) e2
       | (<:expr< $chr:c$ >>) ->
            x
(*
       | (<:expr< ( $e$ :> $t$ ) >>) ->
*)
       | MLast.ExCoe (_, e, t) ->
            fold_type iter (fold_expr iter x e) t
       | (<:expr< $flo:s$ >>) ->
            x
       | (<:expr< for $s$ = $e1$ $to:b$ $e2$ do $list:el$ done >>) ->
            List.fold_left (fold_expr iter) (fold_expr iter (fold_expr iter x e1) e2) el
       | (<:expr< fun [ $list:pwel$ ] >>) ->
            List.fold_left (fold_pwe iter) x pwel
       | (<:expr< if $e1$ then $e2$ else $e3$ >>) ->
           fold_expr iter (fold_expr iter (fold_expr iter x e1) e2) e3
       | (<:expr< $int:s$ >>) ->
            x
       | (<:expr< let $rec:b$ $list:pel$ in $e$ >>) ->
            fold_expr iter (List.fold_left (fold_pe iter) x pel) e
       | (<:expr< $lid:s$ >>) ->
            x
       | MLast.ExLmd (_, s, me, e) ->
            fold_module_expr iter (fold_expr iter x e) me
       | (<:expr< match $e$ with [ $list:pwel$ ] >>) ->
            List.fold_left (fold_pwe iter) (fold_expr iter x e) pwel
(*
       | (<:expr< new $e$ >>) ->
*)
       | MLast.ExNew (_, _) ->
            x
(*
       | (<:expr< {< $list:sel$ >} >>) ->
*)
       | MLast.ExOvr (_, sel) ->
            List.fold_left (fold_se iter) x sel
(*
       | (<:expr< { $list:eel$ } >>) ->
*)
       | MLast.ExRec (_, eel, eo) ->
            List.fold_left (fold_ee iter) (fold_expr_opt iter x eo) eel
       | (<:expr< do $list:el$ return $e$ >>) ->
            fold_expr iter (List.fold_left (fold_expr iter) x el) e
(*
       | (<:expr< $e$ # $i$ >>) ->
*)
       | MLast.ExSnd (_, e, i) ->
            fold_expr iter x e
       | (<:expr< $e1$ .[ $e2$ ] >>) ->
            fold_expr iter (fold_expr iter x e1) e2
       | (<:expr< $str:s$ >>) ->
            x
       | (<:expr< try $e$ with [ $list:pwel$ ] >>) ->
            List.fold_left (fold_pwe iter) (fold_expr iter x e) pwel
       | (<:expr< ( $list:el$ ) >>) ->
            List.fold_left (fold_expr iter) x el
       | (<:expr< ( $e$ : $t$ ) >>) ->
            fold_type iter (fold_expr iter x e) t
       | (<:expr< $uid:s$ >>) ->
            x
       | (<:expr< while $e$ do $list:el$ done >>) ->
            List.fold_left (fold_expr iter) (fold_expr iter x e) el

and fold_patt iter x patt =
   let x = iter.fold_patt x patt in
      match patt with
         (<:patt< $p1$ . $p2$ >>) ->
            fold_patt iter (fold_patt iter x p1) p2
       | (<:patt< $anti: p$ >>) ->
            fold_patt iter x p
       | (<:patt< ( $p1$ as $p2$ ) >>) ->
            fold_patt iter (fold_patt iter x p1) p2
       | (<:patt< _ >>) ->
            x
       | (<:patt< $p1$ $p2$ >>) ->
            fold_patt iter (fold_patt iter x p1) p2
       | (<:patt< [| $list: pl$ |] >>) ->
            List.fold_left (fold_patt iter) x pl
       | (<:patt< $chr:c$ >>) ->
            x
       | (<:patt< $int:s$ >>) ->
            x
       | (<:patt< $lid:i$ >>) ->
            x
       | (<:patt< $p1$ | $p2$ >>) ->
            fold_patt iter (fold_patt iter x p1) p2
       | (<:patt< $p1$ .. $p2$ >>) ->
            fold_patt iter (fold_patt iter x p1) p2
       | (<:patt< { $list:ppl$ } >>) ->
            List.fold_left (fold_pp iter) x ppl
       | (<:patt< $str:s$ >>) ->
            x
       | (<:patt< ( $list:pl$ ) >>) ->
            List.fold_left (fold_patt iter) x pl
       | (<:patt< ( $p$ : $t$ ) >>) ->
            fold_type iter (fold_patt iter x p) t
       | (<:patt< $uid:s$ >>) ->
            x

and fold_type iter x t =
   let x = iter.fold_type x t in
      match t with
         (<:ctyp< $t1$ . $t2$ >>) ->
            fold_type iter (fold_type iter x t1) t2
       | (<:ctyp< $t1$ as $t2$ >>) ->
            fold_type iter (fold_type iter x t1) t2
       | (<:ctyp< _ >>) ->
            x
       | (<:ctyp< $t1$ $t2$ >>) ->
            fold_type iter (fold_type iter x t1) t2
       | (<:ctyp< $t1$ -> $t2$ >>) ->
            fold_type iter (fold_type iter x t1) t2
(*
       | (<:ctyp< # $i$ >>) ->
*)
       | MLast.TyCls (_, i) ->
            x
       | (<:ctyp< $lid:s$ >>) ->
            x
       | (<:ctyp< '$s$ >>) ->
            x
       | (<:ctyp< $t1$ == $t2$ >>) ->
            fold_type iter (fold_type iter x t1) t2
       | (<:ctyp< < $list:stl$ $dd:b$ > >>) ->
            List.fold_left (fold_st iter) x stl
       | (<:ctyp< { $list:sbtl$ } >>) ->
            List.fold_left (fold_sbt iter) x sbtl
       | (<:ctyp< [ $list:stll$ ] >>) ->
            List.fold_left (fold_stl iter) x stll
       | (<:ctyp< ( $list:tl$ ) >>) ->
            List.fold_left (fold_type iter) x tl
       | (<:ctyp< $uid:s$ >>) ->
            x

and fold_sig_item iter x si =
   let x = iter.fold_sig_item x si in
      match si with
(*
         (<:sig_item< class $list:ctl$ >>) ->
*)
         MLast.SgCls (_, ctl) ->
            List.fold_left (fold_class_type_infos iter) x ctl
       | MLast.SgClt (_, ctl) ->
            List.fold_left (fold_class_type_infos iter) x ctl
       | (<:sig_item< declare $list:sil$ end >>) ->
            List.fold_left (fold_sig_item iter) x sil
       | (<:sig_item< exception $s$ of $list:tl$ >>) ->
            List.fold_left (fold_type iter) x tl
       | (<:sig_item< external $s$ : $t$ = $list:sl$ >>) ->
            fold_type iter x t
       | MLast.SgInc (_, mt) ->
            fold_module_type iter x mt
       | (<:sig_item< module $s$ : $mt$ >>) ->
            fold_module_type iter x mt
       | (<:sig_item< module type $s$ = $mt$ >>) ->
            fold_module_type iter x mt
       | (<:sig_item< open $sl$ >>) ->
            x
       | (<:sig_item< type $list:ssltl$ >>) ->
            List.fold_left (fold_sslt iter) x ssltl
       | (<:sig_item< value $s$ : $t$ >>) ->
            fold_type iter x t

and fold_str_item iter x si =
   let x = iter.fold_str_item x si in
      match si with
(*
         (<:str_item< class $list:cdl$ >>) ->
*)
         MLast.StCls (_, cdl) ->
            List.fold_left (fold_class_expr_infos iter) x cdl
       | MLast.StClt (_, cdl) ->
            List.fold_left (fold_class_type_infos iter) x cdl
       | (<:str_item< declare $list:stl$ end >>) ->
            List.fold_left (fold_str_item iter) x stl
       | (<:str_item< exception $s$ of $list:tl$ >>) ->
            List.fold_left (fold_type iter) x tl
       | (<:str_item< $exp:e$ >>) ->
            fold_expr iter x e
       | (<:str_item< external $s$ : $t$ = $list:sl$ >>) ->
            fold_type iter x t
       | (<:str_item< module $s$ = $me$ >>) ->
            fold_module_expr iter x me
       | (<:str_item< module type $s$ = $mt$ >>) ->
            fold_module_type iter x mt
       | (<:str_item< open $sl$ >>) ->
            x
       | (<:str_item< type $list:ssltl$ >>) ->
            List.fold_left (fold_sslt iter) x ssltl
       | (<:str_item< value $rec:b$ $list:pel$ >>) ->
            List.fold_left (fold_pe iter) x pel

and fold_module_type iter x mt =
   let x = iter.fold_module_type x mt in
      match mt with
         (<:module_type< $mt1$ . $mt2$ >>) ->
            fold_module_type iter (fold_module_type iter x mt1) mt2
       | (<:module_type< $mt1$ $mt2$ >>) ->
            fold_module_type iter (fold_module_type iter x mt1) mt2
       | (<:module_type< functor ( $s$ : $mt1$ ) -> $mt2$ >>) ->
            fold_module_type iter (fold_module_type iter x mt1) mt2
       | (<:module_type< $lid:i$ >>) ->
            x
       | (<:module_type< sig $list:sil$ end >>) ->
            List.fold_left (fold_sig_item iter) x sil
       | (<:module_type< $uid:i$ >>) ->
            x
       | (<:module_type< $mt$ with $list:wcl$ >>) ->
            List.fold_left (fold_with_constr iter) (fold_module_type iter x mt) wcl

and fold_with_constr iter x wc =
   let x = iter.fold_with_constr x wc in
      match wc with
         MLast.WcTyp (loc, sl1, sl2, t) ->
           fold_type iter x t
       | MLast.WcMod (loc, sl1, mt) ->
           fold_module_type iter x mt

and fold_module_expr iter x me =
   let x = iter.fold_module_expr x me in
      match me with
         (<:module_expr< $me1$ . $me2$ >>) ->
            fold_module_expr iter (fold_module_expr iter x me1) me2
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
       | (<:module_expr< $uid:i$ >>) ->
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
         MLast.CtCon (_, sl, tl) ->
            List.fold_left (fold_type iter) x tl
       | MLast.CtFun (_, t, ct) ->
            fold_class_type iter (fold_type iter x t) ct
       | MLast.CtSig (_, t, ctfl) ->
            List.fold_left (fold_class_type_field iter) (fold_type iter x t) ctfl

and fold_class_type_field iter x ctf =
   let x = iter.fold_class_type_field x ctf in
      match ctf with
         CiCtr (loc, s, t) ->
            fold_type iter x t
       | CiInh (loc, ct) ->
            fold_class_type iter x ct
       | CiMth (loc, s, b, t) ->
            fold_type iter x t
       | CiVal (loc, s, b, t) ->
            fold_type iter x t
       | CiVir (loc, s, b, t) ->
            fold_type iter x t

and fold_class_expr iter x ce =
   let x = iter.fold_class_expr x ce in
      match ce with
         MLast.CeApp (_, ce, el) ->
            List.fold_left (fold_expr iter) (fold_class_expr iter x ce) el
       | MLast.CeCon (_, sl, tl) ->
            List.fold_left (fold_type iter) x tl
       | MLast.CeFun (_, p, ce) ->
            fold_class_expr iter (fold_patt iter x p) ce
       | MLast.CeLet (_, b, pel, ce) ->
            List.fold_left (fold_pe iter) (fold_class_expr iter x ce) pel
       | MLast.CeStr (_, p, cfl) ->
            List.fold_left (fold_class_field iter) (fold_patt iter x p) cfl
       | MLast.CeTyc (_, ce, ct) ->
            fold_class_type iter (fold_class_expr iter x ce) ct

and fold_class_field iter x cf =
   let x = iter.fold_class_field x cf in
      match cf with
         CfCtr (loc, s, t) ->
            fold_type iter x t
       | CfInh (loc, ce, so) ->
            fold_class_expr iter x ce
       | CfIni (_, e) ->
            fold_expr iter x e
       | CfMth (loc, s, b, e) ->
            fold_expr iter x e
       | CfVal (loc, s, b, e) ->
            fold_expr iter x e
       | CfVir (loc, s, b, t) ->
            fold_type iter x t

(*
 * Combined forms.
 *)
and fold_expr_opt iter x = function
   Some e -> fold_expr iter x e
 | None -> x

and fold_type_opt iter x = function
   Some e -> fold_type iter x e
 | None -> x

and fold_pwe iter x (patt, with_expr, expr) =
   fold_expr iter (fold_expr_opt iter (fold_patt iter x patt) with_expr) expr

and fold_pe iter x (patt, expr) =
   fold_expr iter (fold_patt iter x patt) expr

and fold_se iter x (s, e) =
   fold_expr iter x e

and fold_ee iter x (e1, e2) =
   fold_expr iter (fold_expr iter x e1) e2

and fold_pp iter x (p1, p2) =
   fold_patt iter (fold_patt iter x p1) p2

and fold_st iter x (s, t) =
   fold_type iter x t

and fold_sbt iter x (s, b, t) =
   fold_type iter x t

and fold_stl iter x (s, tl) =
   List.fold_left (fold_type iter) x tl

and fold_sslt iter x (s, sl, t) =
   fold_type iter x t

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
