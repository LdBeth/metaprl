(*
 * Hash functions on caml expressions.
 * We just want to remove location info.
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

open Printf
open Mp_debug

open MLast

(*
 * Show the file loading.
 *)
let _ =
   show_loading "Loading Filter_hash%t"


(*
 * Basic hash functions.
 *)
let hash index i =
   (index lsl 4) lxor (index lsr 4) lxor i

let hash_string index s =
   hash index (Hashtbl.hash s)

let hash_string_opt index = function
   Some s -> hash_string (hash index 0x3a35a593) s
 | None -> hash index 0x4adffe93

let hash_char index c =
   hash index (Char.code c)

let hash_bool index flag =
   hash index (if flag then 0x16ff6a91 else 0x0fad4e6c)

(*
 * Compute a hash value from the struct.
 *)
let rec hash_expr index = function
   (<:expr< $e1$ . $e2$ >>) ->
      hash_expr (hash_expr (hash index 0x36eab5f0) e1) e2
 | (<:expr< $anti: e$ >>) ->
      hash_expr (hash index 0x6757ffcb) e
 | (<:expr< $e1$ $e2$ >>) ->
      hash_expr (hash_expr (hash index 0x0ad11a34) e1) e2
 | (<:expr< $e1$ .( $e2$ ) >>) ->
      hash_expr (hash_expr (hash index 0x71f097ca) e1) e2
 | (<:expr< [| $list:el$ |] >>) ->
      List.fold_left hash_expr (hash index 0x39098b77) el
 | (<:expr< $e1$ := $e2$ >>) ->
      hash_expr (hash_expr (hash index 0x0171ab0d) e1) e2
 | (<:expr< $chr:c$ >>) ->
      hash_string (hash index 0x776fd938) c
 | (<:expr< ( $e$ :> $t$ ) >>) ->
      hash_type (hash_expr (hash index 0x6a8d593f) e) t
 | (<:expr< $flo:s$ >>) ->
      hash_string (hash index 0x1b11445f) s
 | (<:expr< for $s$ = $e1$ $to:b$ $e2$ do { $list:el$ } >>) ->
      List.fold_left hash_expr (hash_string (hash_expr (hash_bool (hash_expr (hash index 0x4ba64ac0) e2) b) e1) s) el
 | (<:expr< fun [ $list:pwel$ ] >>) ->
      List.fold_left hash_pwe (hash index 0x5a60d0dc) pwel
 | (<:expr< if $e1$ then $e2$ else $e3$ >>) ->
      hash_expr (hash_expr (hash_expr (hash index 0x7359fe14) e3) e2) e1
 | (<:expr< $int:s$ >>) ->
      hash_string (hash index 0x1458c565) s
 | (<:expr< ~ $i$ : $e$ >>) ->
      hash_string (hash_expr (hash index 0x18e9bcfa) e) i
 | (<:expr< let $rec:b$ $list:pel$ in $e$ >>) ->
      List.fold_left hash_pe (hash_bool (hash_expr (hash index 0x328a80d8) e) b) pel
 | (<:expr< $lid:s$ >>) ->
      hash_string (hash index 0x089a8dc1) s
 | MLast.ExLmd (_, s, me, e) ->
      hash_string (hash_module_expr (hash_expr (hash index 0x3881be91) e) me) s
 | (<:expr< match $e$ with [ $list:pwel$ ] >>) ->
      List.fold_left hash_pwe (hash_expr (hash index 0x565460ab) e) pwel
 | (<:expr< ? $i$ : $e$ >>) ->
      hash_string (hash_expr (hash index 0xe5a52b6) e) i
(*
 | (<:expr< new $e$ >>) ->
*)
 | MLast.ExNew (_, sl) ->
      List.fold_left hash_string (hash index 0x24e41e2d) sl
 | (<:expr< {< $list:sel$ >} >>) ->
      List.fold_left hash_se (hash index 0x5e034524) sel
(*
 | (<:expr< { $list:eel$ } >>) ->
*)
 | MLast.ExRec (_, eel, eo) ->
      List.fold_left hash_pe (hash_expr_opt (hash index 0x3f143e45) eo) eel
 | (<:expr< do { $list:el$ } >>) ->
      List.fold_left hash_expr (hash index 0x1f9b5a49) el
 | (<:expr< $e$ # $i$ >>) ->
      hash_expr (hash_string (hash index 0x012df1d2) i) e
 | (<:expr< $e1$ .[ $e2$ ] >>) ->
      hash_expr (hash_expr (hash index 0x4d02cac0) e2) e1
 | (<:expr< $str:s$ >>) ->
      hash_string (hash index 0x20ca3051) s
 | (<:expr< try $e$ with [ $list:pwel$ ] >>) ->
      List.fold_left hash_pwe (hash_expr (hash index 0x353ee42f) e) pwel
 | (<:expr< ( $list:el$ ) >>) ->
      List.fold_left hash_expr (hash index 0x49bfad0b) el
 | (<:expr< ( $e$ : $t$ ) >>) ->
      hash_expr (hash_type (hash index 0x206d0588) t) e
 | (<:expr< $uid:s$ >>) ->
      hash_string (hash index 0x27139e6c) s
 | (<:expr< ` $s$ >>) ->
      hash_string (hash index 0x7a5835c) s
 | (<:expr< while $e$ do { $list:el$ } >>) ->
      List.fold_left hash_expr (hash_expr (hash index 0x723d7789) e) el
(* 3.02
 | MLast.ExXnd (loc, s, e) ->
      hash_string (hash_expr (hash index 0x8be2751) e) s
 *)
 | MLast.ExCoe (_, e, ot, t) ->
      hash_type (hash_type_opt (hash_expr (hash index 0x628be91) e) ot) t

and hash_patt index = function
   (<:patt< $p1$ . $p2$ >>) ->
      hash_patt (hash_patt (hash index 0x3abd1a6d) p2) p1
 | (<:patt< $anti: p$ >>) ->
      hash_patt (hash index 0x734a34be) p
 | (<:patt< ( $p1$ as $p2$ ) >>) ->
      hash_patt (hash_patt (hash index 0x5ac10896) p2) p1
 | (<:patt< _ >>) ->
      hash index 0x7880eccb
 | (<:patt< $p1$ $p2$ >>) ->
      hash_patt (hash_patt (hash index 0x7a0d2d66) p2) p1
 | (<:patt< [| $list:pl$ |] >>) ->
      List.fold_left hash_patt (hash index 0x053ba831) pl
 | (<:patt< $chr:c$ >>) ->
      hash_string (hash index 0x00b259e4) c
 | (<:patt< $flo:f$ >>) ->
      hash_string (hash index 0x71f944f6) f
 | (<:patt< $int:s$ >>) ->
      hash_string (hash index 0x32b6925e) s
 | (<:patt< ~ $i$ : $p$ >>) ->
      hash_patt (hash_string (hash index 0x30fb5e6f) i) p
 | (<:patt< $lid:i$ >>) ->
      hash_string (hash index 0x44ed2bf9) i
(* 3.02
 | (<:patt< ? $i$ : $p$ >>) ->
      hash_patt (hash_string (hash index 0x681f0e05) i) p
 | (<:patt< ? $i$ : ( $p$ = $e$ ) >>) ->
      hash_expr (hash_patt (hash_string (hash index 0x18fe22f6) i) p) e
*)
 | (<:patt< $p1$ | $p2$ >>) ->
      hash_patt (hash_patt (hash index 0x72a2f1ae) p2) p1
 | (<:patt< $p1$ .. $p2$ >>) ->
      hash_patt (hash_patt (hash index 0x34283d6b) p2) p1
 | (<:patt< { $list:ppl$ } >>) ->
      List.fold_left hash_pp (hash index 0x3c5d0532) ppl
 | (<:patt< $str:s$ >>) ->
      hash_string (hash index 0x5d304aed) s
 | (<:patt< ( $list:pl$ ) >>) ->
      List.fold_left hash_patt (hash index 0x4f3981cb) pl
 | (<:patt< ( $p$ : $t$ ) >>) ->
      hash_patt (hash_type (hash index 0x08034ff2) t) p
 | (<:patt< $uid:s$ >>) ->
      hash_string (hash index 0x37911bc9) s
 | (<:patt< ` $s$ >>) ->
      hash_string (hash index 0x6aeb916) s
(* 3.02
 | MLast.PaXnd (loc, s, p) ->
      hash_string (hash_patt (hash index 0x6bcc8901) p) s
 *)
 | MLast.PaTyp (_, sl) ->
      List.fold_left hash_string (hash index 0x1be39e21) sl
 | MLast.PaOlb (_, s, p, eo) ->
      hash_string (hash_patt (hash_expr_opt (hash index 0x31663b71) eo) p) s

and hash_type index = function
   (<:ctyp< $t1$ . $t2$ >>) ->
      hash_type (hash_type (hash index 0x42937fdf) t2) t1
 | (<:ctyp< $t1$ as $t2$ >>) ->
      hash_type (hash_type (hash index 0x1c5c1558) t2) t1
 | (<:ctyp< _ >>) ->
      hash index 0x6a1b9ca2
 | (<:ctyp< $t1$ $t2$ >>) ->
      hash_type (hash_type (hash index 0x4b2e0da1) t2) t1
 | (<:ctyp< $t1$ -> $t2$ >>) ->
      hash_type (hash_type (hash index 0x72b07603) t2) t1
 | (<:ctyp< # $list:i$ >>) ->
      hash_string (hash index 0x0effbacf) i
 | (<:ctyp< ~ $i$ : $t$ >>) ->
      hash_type (hash_string (hash index 0x6a06805c) i) t
 | (<:ctyp< $lid:s$ >>) ->
      hash_string (hash index 0x293152c5) s
 | (<:ctyp< ? $i$ : $t$ >>) ->
      hash_type (hash_string (hash index 0x1a6813a8) i) t
 | (<:ctyp< '$s$ >>) ->
      hash_string (hash index 0x31c4b448) s
 | (<:ctyp< $t1$ == $t2$ >>) ->
      hash_type (hash_type (hash index 0x2e9b1519) t2) t1
 | (<:ctyp< < $list:stl$ $b$ > >>) ->
      List.fold_left hash_st (hash_bool (hash index 0x2a5f4498) b) stl
 | (<:ctyp< { $list:sbtl$ } >>) ->
      List.fold_left hash_sbt (hash index 0x7ec77f08) sbtl
 | (<:ctyp< [ $list:stll$ ] >>) ->
      List.fold_left hash_stl (hash index 0x4f65456a) stll
 | (<:ctyp< ( $list:tl$ ) >>) ->
      List.fold_left hash_type (hash index 0x5f9e28c7) tl
 | (<:ctyp< $uid:s$ >>) ->
      hash_string (hash index 0x48872c13) s
 | MLast.TyVrn (_, sbtll, bsloo) ->
      List.fold_left hash_sbtl (hash_string (hash index 0x79ffedd7) bsloo) sbtll
(* 3.02
 | MLast.TyXnd (loc, s, t) ->
      hash_string (hash_type (hash index 0x1677a389) t) s
 *)

and hash_sig_item index = function
   (<:sig_item< class $list:ctl$ >>) ->
      List.fold_left hash_class_type_infos (hash index 0x0629e079) ctl
 | (<:sig_item< class type $list:ctl$ >>) ->
      List.fold_left hash_class_type_infos (hash index 0x704fd0ec) ctl
 | (<:sig_item< declare $list:sil$ end >>) ->
      List.fold_left hash_sig_item index sil
 | (<:sig_item< exception $s$ of $list:tl$ >>) ->
      List.fold_left hash_type (hash_string (hash index 0x6fd24af2) s) tl
 | (<:sig_item< external $s$ : $t$ = $list:sl$ >>) ->
      List.fold_left hash_string (hash_type (hash_string (hash index 0x06b1c733) s) t) sl
 | (<:sig_item< include $mt$ >>) ->
      hash_module_type (hash index 0x283bed1) mt
 | (<:sig_item< module $s$ : $mt$ >>) ->
      hash_module_type (hash (hash_string index s) 0x22e62741) mt
 | (<:sig_item< module type $s$ = $mt$ >>) ->
      hash_module_type (hash_string (hash index 0x3ac4a39d) s) mt
 | (<:sig_item< open $sl$ >>) ->
      List.fold_left hash_string (hash index 0x3890da9c) sl
 | (<:sig_item< type $list:tdl$ >>) ->
      List.fold_left hash_tdl (hash index 0x3c820480) tdl
 | (<:sig_item< value $s$ : $t$ >>) ->
      hash_type (hash_string (hash index 0x2a8f655f) s) t
 | MLast.SgDir (loc, s, eo) ->
      hash_string (hash_expr_opt (hash index 0x5ab39bee) eo) s

and hash_str_item index = function
   (<:str_item< class $list:cdl$ >>) ->
      List.fold_left hash_class_expr_infos (hash index 0x5058b095) cdl
 | (<:str_item< class type $list:cdl$ >>) ->
      List.fold_left hash_class_type_infos (hash index 0x0627e079) cdl
 | (<:str_item< declare $list:stl$ end >>) ->
      List.fold_left hash_str_item index stl
 | (<:str_item< exception $s$ of $list:tl$ >>) ->
      List.fold_left hash_type (hash_string (hash index 0x6172cfc9) s) tl
 | (<:str_item< $exp:e$ >>) ->
      hash_expr (hash index 0x27b41b21) e
 | (<:str_item< external $s$ : $t$ = $list:sl$ >>) ->
      List.fold_left hash_string (hash (hash_type (hash_string index s) t) 0x0294d774) sl
 | (<:str_item< module $s$ = $me$ >>) ->
      hash_module_expr (hash_string (hash index 0x33459068) s) me
 | (<:str_item< module type $s$ = $mt$ >>) ->
      hash_module_type (hash_string (hash index 0x249c92c6) s) mt
 | (<:str_item< open $sl$ >>) ->
      List.fold_left hash_string (hash index 0x048464b8) sl
 | (<:str_item< type $list:tdl$ >>) ->
      List.fold_left hash_tdl (hash index 0x0c9046df) tdl
 | (<:str_item< value $rec:b$ $list:pel$ >>) ->
      List.fold_left hash_pe (if b then 0x2715b1cb else 0x383ff901) pel
 | MLast.StDir (loc, s, eo) ->
      hash_string (hash_expr_opt (hash index 0x371be624) eo) s
 | MLast.StInc (loc, me) ->
      hash_module_expr (hash index 0x17be552b) me
 | MLast.StExc (_, s, tl, sl) ->
      hash_string (List.fold_left hash_type (List.fold_left hash_string (hash index 0x1378b2ef) sl) tl) s

and hash_module_type index = function
   (<:module_type< $mt1$ . $mt2$ >>) ->
      hash_module_type (hash_module_type (hash index 0x622529ad) mt2) mt1
 | (<:module_type< $mt1$ $mt2$ >>) ->
      hash_module_type (hash_module_type (hash index 0x65fc22c6) mt2) mt1
 | (<:module_type< functor ( $s$ : $mt1$ ) -> $mt2$ >>) ->
      hash_string (hash_module_type (hash_module_type (hash index 0x6989bebf) mt2) mt1) s
 | (<:module_type< $lid:i$ >>) ->
      hash_string (hash index 0x54c81b5b) i
 | (<:module_type< sig $list:sil$ end >>) ->
      List.fold_left hash_sig_item (hash index 0x1a246031) sil
 | (<:module_type< $uid:i$ >>) ->
      hash_string (hash index 0x25e6c3f1) i
 | (<:module_type< $mt$ with $list:wcl$ >>) ->
      List.fold_left hash_wc (hash_module_type (hash index 0x31f86649) mt) wcl

and hash_wc index = function
   WcTyp (_, sl1, sl2, t) ->
      List.fold_left hash_string (List.fold_left hash_string (hash_type (hash index 0x695de1fc) t) sl2) sl1
 | WcMod (_, sl1, mt) ->
      List.fold_left hash_string (hash_module_type (hash index 0x2dea13e4) mt) sl1

and hash_module_expr index = function
   (<:module_expr< $me1$ . $me2$ >>) ->
      hash_module_expr (hash_module_expr (hash index 0x69898212) me2) me1
 | (<:module_expr< $me1$ $me2$ >>) ->
      hash_module_expr (hash_module_expr (hash index 0x2bf161dc) me2) me1
 | (<:module_expr< functor ( $s$ : $mt$ ) -> $me$ >>) ->
      hash_string (hash_module_type (hash_module_expr (hash index 0x4a46293c) me) mt) s
(*
 | (<:module_expr< $lid:i$ >>) ->
      hash_string (hash index 0x53a51eb4) i
*)
 | (<:module_expr< struct $list:stl$ end >>) ->
      List.fold_left hash_str_item (hash index 0x771f6f7d) stl
 | (<:module_expr< ( $me$ : $mt$) >>) ->
      hash_module_expr (hash_module_type (hash index 0x3cf69f3f) mt) me
 | (<:module_expr< $uid:i$ >>) ->
      hash_string (hash index 0x62a4d984) i

and hash_class_type_infos index
  { ciNam = s;
    ciPrm = _, sl;
    ciVir = b;
    ciExp = t
  } =
  let index = hash index 0x721a73be in
  let index = hash_string index s in
  let index = List.fold_left hash_string index sl in
  let index = hash_class_type index t in
     index

and hash_class_expr_infos index
  { ciNam = s;
    ciPrm = _, sl;
    ciVir = b;
    ciExp = e
  } =
  let index = hash index 0x721a73bf in
  let index = hash_string index s in
  let index = List.fold_left hash_string index sl in
  let index = hash_class_expr index e in
     index

and hash_class_expr index ce =
  let index = hash index 0x034be1c1 in
     match ce with
         MLast.CeApp (_, ce, el) ->
            hash_expr (hash_class_expr index ce) el
       | MLast.CeCon (_, sl, tl) ->
            List.fold_left hash_string (List.fold_left hash_type index tl) sl
       | MLast.CeFun (_, p, ce) ->
            hash_patt (hash_class_expr index ce) p
       | MLast.CeLet (_, b, pel, ce) ->
            hash_bool (List.fold_left hash_pe (hash_class_expr index ce) pel) b
       | MLast.CeStr (_, p, cfl) ->
            hash_patt_opt (List.fold_left hash_class_str_item index cfl) p
       | MLast.CeTyc (_, ce, ct) ->
            hash_class_expr (hash_class_type index ct) ce
(* 3.02
       | MLast.CeXnd (_, s, ce) ->
            hash_string (hash_class_expr (hash index 0x4475bac) ce) s
 *)

and hash_class_str_item index = function
   CrCtr (_, s, t) ->
      hash_string (hash_type (hash index 0x6ebb5387) t) s
 | CrDcl (_, t) ->
      List.fold_left hash_class_str_item (hash index 0xb4910045) t
 | CrInh (_, ce, so) ->
      hash_class_expr (hash_string_opt (hash index 0x113fee9d) so) ce
 | CrIni (_, e) ->
      hash_expr (hash index 0x73413214) e
 | CrMth (_, s, b, e) ->
      hash_string (hash_expr (hash_bool (hash index 0x4ab006da) b) e) s
 | CrVal (_, s, b, e) ->
      hash_string (hash_bool (hash_expr (hash index 0x6d82d28f) e) b) s
 | CrVir (_, s, b, t) ->
      hash_string (hash_type (hash_bool (hash index 0x60a53407) b) t) s

and hash_class_type index ct =
  let index = hash index 0x7de2ab2c in
     match ct with
        CtCon (_, sl, tl) ->
           List.fold_left hash_string (List.fold_left hash_type index tl) sl
      | CtFun (_, t, ct) ->
           hash_class_type (hash_type index t) ct
      | CtSig (_, t, ctfl) ->
           List.fold_left hash_class_sig_item (hash_type_opt index t) ctfl
(* 3.02
      | CtXnd (_, s, ct) ->
           hash_string (hash_class_type (hash index 0x344b344) ct) s
 *)

and hash_class_sig_item index = function
   CgCtr (_, s, t) ->
      hash_string (hash_type (hash index 0x362be7cd) t) s
 | CgDcl (_, t) ->
      List.fold_left hash_class_sig_item (hash index 0xbe66301f) t
 | CgInh (_, ct) ->
      hash_class_type (hash index 0x1779e662) ct
 | CgMth (_, s, b, t) ->
      hash_string (hash_type (hash_bool (hash index 0x028b2e41) b) t) s
 | CgVal (_, s, b, t) ->
      hash_string (hash_bool (hash_type (hash index 0x3be81fa) t) b) s
 | CgVir (_, s, b, t) ->
      hash_string (hash_type (hash_bool (hash index 0x2730112a) b) t) s

(*
 * Combined forms.
 *)
and hash_expr_opt index = function
   Some expr -> hash_expr (hash index 0x2a4e2fa2) expr
 | None -> hash index 0x3609fea2

and hash_type_opt index = function
   Some t -> hash_type (hash index 0x3bee38a) t
 | None -> hash index 0x32bbe81a

and hash_patt_opt index = function
   Some p -> hash_patt (hash index 0x3be538a) p
 | None -> hash index 0x32ace81a

and hash_pwe index (patt, with_expr, expr) =
   hash_patt (hash_expr_opt (hash_expr (hash index 0x50777efa) expr) with_expr) patt

and hash_pe index (patt, expr) =
   hash_patt (hash_expr (hash index 0x30fff6d5) expr) patt

and hash_se index (s, e) =
   hash_string (hash_expr (hash index 0x70cea23f) e) s

and hash_pp index (p1, p2) =
   hash_patt (hash_patt (hash index 0x1272c69f) p2) p1

and hash_st index (s, t) =
   hash_string (hash_type (hash index 0x241432a7) t) s

and hash_sbt index (_, s, b, t) =
   hash_string (hash_bool (hash_type (hash index 0x1fa3771f) t) b) s

and hash_sbtl index rf =
   match rf with
      RfTag (s, b, tl) ->
         List.fold_left hash_type (hash_bool (hash_string (hash index 0x7497f04c) s) b) tl
    | RfInh t ->
         hash_type (hash index 0x743be12a) t

and hash_stl index (_, s, tl) =
   List.fold_left hash_type (hash_string (hash index 0x7497f04c) s) tl

and hash_typetype index (t1, t2) =
   hash_type (hash_type index t1) t2

and hash_tdl index (s, sl, t, tl) =
   hash_string (List.fold_left hash_string (hash_type (List.fold_left hash_typetype (hash index 0x0a10556d) tl) t) sl) s

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
