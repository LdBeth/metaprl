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
 * Author: Jason Hickey <jyh@cs.cornell.edu>
 * Modified By: Aleksey Nogin <nogin@cs.cornell.edu>
 *)

open Lm_debug
open Lm_printf

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
 * We will relocate everything to 0
 *)
let loc = (0, 0)
let reloc = fun _ -> loc

(*
 * Compute a hash value from the struct.
 *)
let hash_expr index expr =
   hash index (Hashtbl.hash (Pcaml.expr_reloc reloc 0 expr))

let hash_patt index patt =
   hash index (Hashtbl.hash (Pcaml.patt_reloc reloc 0 patt))

let hash_type index ctyp =
   hash_expr index <:expr< ( (assert False) : $ctyp$ ) >>

let rec hash_sig_item index = function
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
 | MLast.StDir (_, s, eo) ->
      hash_string (hash_expr_opt (hash index 0x371be624) eo) s
 | MLast.StInc (_, me) ->
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
 | (<:module_type< ' $i$ >>) ->
      hash_string (hash index 0x3023821b) i
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
      List.fold_left hash_string (hash_module_expr (hash index 0x2dea13e4) mt) sl1

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
      List.fold_left hash_class_str_item (hash index 0x34910045) t
 | CrInh (_, ce, so) ->
      hash_class_expr (hash_string_opt (hash index 0x113fee9d) so) ce
 | CrIni (_, e) ->
      hash_expr (hash index 0x73413214) e
 | CrMth (_, s, b, e, t) ->
      hash_type_opt (hash_string (hash_expr (hash_bool (hash index 0x4ab006da) b) e) s) t
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
      List.fold_left hash_class_sig_item (hash index 0x3e66301f) t
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

and hash_pe index (patt, expr) =
   hash_patt (hash_expr (hash index 0x30fff6d5) expr) patt

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
