(*
 * Hash functions on caml expressions.
 * We just want to remove location info.
 *)

open Printf
open Debug

open MLast

(*
 * Show the file loading.
 *)
let _ =
   if !debug_load then
      eprintf "Loading xyz%t" eflush


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
 | (<:expr< $e1$ $e2$ >>) ->
      hash_expr (hash_expr (hash index 0x0ad11a34) e1) e2
 | (<:expr< $e1$ .( $e2$ ) >>) ->
      hash_expr (hash_expr (hash index 0x71f097ca) e1) e2
 | (<:expr< [| $list:el$ |] >>) ->
      List.fold_left hash_expr (hash index 0x39098b77) el
 | (<:expr< $e1$ := $e2$ >>) ->
      hash_expr (hash_expr (hash index 0x0171ab0d) e1) e2
 | (<:expr< $chr:c$ >>) ->
      hash_char (hash index 0x776fd938) c
 | (<:expr< ( $e$ :> $t$ ) >>) ->
      hash_type (hash_expr (hash index 0x6a8d593f) e) t
 | (<:expr< $flo:s$ >>) ->
      hash_string (hash index 0x1b11445f) s
 | (<:expr< for $s$ = $e1$ $to:b$ $e2$ do $list:el$ done >>) ->
      List.fold_left hash_expr (hash_string (hash_expr (hash_bool (hash_expr (hash index 0x4ba64ac0) e2) b) e1) s) el
 | (<:expr< fun [ $list:pwel$ ] >>) ->
      List.fold_left hash_pwe (hash index 0x5a60d0dc) pwel
 | (<:expr< if $e1$ then $e2$ else $e3$ >>) ->
      hash_expr (hash_expr (hash_expr (hash index 0x7359fe14) e3) e2) e1
 | (<:expr< $int:s$ >>) ->
      hash_string (hash index 0x1458c565) s
 | (<:expr< let $rec:b$ $list:pel$ in $e$ >>) ->
      List.fold_left hash_pe (hash_bool (hash_expr (hash index 0x328a80d8) e) b) pel
 | (<:expr< $lid:s$ >>) ->
      hash_string (hash index 0x089a8dc1) s
 | (<:expr< match $e$ with [ $list:pwel$ ] >>) ->
      List.fold_left hash_pwe (hash_expr (hash index 0x565460ab) e) pwel
 | (<:expr< new $e$ >>) ->
      hash_expr (hash index 0x24e41e2d) e
 | (<:expr< {< $list:sel$ >} >>) ->
      List.fold_left hash_se (hash index 0x5e034524) sel
 | (<:expr< { $list:eel$ } >>) ->
      List.fold_left hash_ee (hash index 0x3f143e45) eel
 | (<:expr< do $list:el$ return $e$ >>) ->
      List.fold_left hash_expr (hash_expr (hash index 0x1f9b5a49) e) el
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
 | (<:expr< while $e$ do $list:el$ done >>) ->
      List.fold_left hash_expr (hash_expr (hash index 0x723d7789) e) el
 | MLast.ExAnt (loc, e) ->
      Stdpp.raise_with_loc loc (Failure "Filter_hash.hash_expr: encountered an ExAnt")

and hash_patt index = function
   (<:patt< $p1$ . $p2$ >>) ->
      hash_patt (hash_patt (hash index 0x3abd1a6d) p2) p1
 | (<:patt< ( $p1$ as $p2$ ) >>) ->
      hash_patt (hash_patt (hash index 0x5ac10896) p2) p1
 | (<:patt< _ >>) ->
      hash index 0x7880eccb
 | (<:patt< $p1$ $p2$ >>) ->
      hash_patt (hash_patt (hash index 0x7a0d2d66) p2) p1
 | (<:patt< $chr:c$ >>) ->
      hash_char (hash index 0x00b259e4) c
 | (<:patt< $int:s$ >>) ->
      hash_string (hash index 0x32b6925e) s
 | (<:patt< $lid:i$ >>) ->
      hash_string (hash index 0x44ed2bf9) i
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
 | MLast.PaAnt (loc, p) ->
      Stdpp.raise_with_loc loc (Failure "Filter_hash:hash_patt: encountered PaAnt")

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
 | (<:ctyp< # $i$ >>) ->
      hash_string (hash index 0x0effbacf) i
 | (<:ctyp< $lid:s$ >>) ->
      hash_string (hash index 0x293152c5) s
 | (<:ctyp< '$s$ >>) ->
      hash_string (hash index 0x31c4b448) s
 | (<:ctyp< $t1$ == $t2$ >>) ->
      hash_type (hash_type (hash index 0x2e9b1519) t2) t1
 | (<:ctyp< < $list:stl$ $dd:b$ > >>) ->
      List.fold_left hash_st (hash_bool (hash index 0x2a5f4498) b) stl
 | (<:ctyp< { $list:sbtl$ } >>) ->
      List.fold_left hash_sbt (hash index 0x7ec77f08) sbtl
 | (<:ctyp< [ $list:stll$ ] >>) ->
      List.fold_left hash_stl (hash index 0x4f65456a) stll
 | (<:ctyp< ( $list:tl$ ) >>) ->
      List.fold_left hash_type (hash index 0x5f9e28c7) tl
 | (<:ctyp< $uid:s$ >>) ->
      hash_string (hash index 0x48872c13) s

and hash_sig_item index = function
   (<:sig_item< class $list:cdl$ >>) ->
      List.fold_left hash_class_type (hash index 0x0629e079) cdl
 | (<:sig_item< declare $list:sil$ end >>) ->
      List.fold_left hash_sig_item index sil
 | (<:sig_item< exception $s$ of $list:tl$ >>) ->
      List.fold_left hash_type (hash_string (hash index 0x6fd24af2) s) tl
 | (<:sig_item< external $s$ : $t$ = $list:sl$ >>) ->
      List.fold_left hash_string (hash_type (hash_string (hash index 0x06b1c733) s) t) sl
 | (<:sig_item< module $s$ : $mt$ >>) ->
      hash_module_type (hash (hash_string index s) 0x22e62741) mt
 | (<:sig_item< module type $s$ = $mt$ >>) ->
      hash_module_type (hash_string (hash index 0x3ac4a39d) s) mt
 | (<:sig_item< open $sl$ >>) ->
      List.fold_left hash_string (hash index 0x3890da9c) sl
 | (<:sig_item< type $list:ssltl$ >>) ->
      List.fold_left hash_sslt (hash index 0x3c820480) ssltl
 | (<:sig_item< value $s$ : $t$ >>) ->
      hash_type (hash_string (hash index 0x2a8f655f) s) t

and hash_str_item index = function
   (<:str_item< class $list:cdl$ >>) ->
      List.fold_left hash_class (hash index 0x0629e079) cdl
 | (<:str_item< declare $list:stl$ end >>) ->
      List.fold_left hash_str_item index stl
 | (<:str_item< exception $s$ of $list:tl$ >>) ->
      List.fold_left hash_type (hash_string (hash index 0x6172cfc9) s) tl
 | (<:str_item< $exp:e$ >>) ->
      hash_expr (hash index 0x3890da9c) e
 | (<:str_item< external $s$ : $t$ = $list:sl$ >>) ->
      List.fold_left hash_string (hash (hash_type (hash_string index s) t) 0x0294d774) sl
 | (<:str_item< module $s$ = $me$ >>) ->
      hash_module_expr (hash_string (hash index 0x33459068) s) me
 | (<:str_item< module type $s$ = $mt$ >>) ->
      hash_module_type (hash_string (hash index 0x249c92c6) s) mt 
 | (<:str_item< open $sl$ >>) ->
      List.fold_left hash_string (hash index 0x048464b8) sl
 | (<:str_item< type $list:ssltl$ >>) ->
      List.fold_left hash_sslt (hash index 0x0c9046df) ssltl
 | (<:str_item< value $rec:b$ $list:pel$ >>) ->
      List.fold_left hash_pe (if b then 0x2715b1cb else 0x383ff901) pel
    
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

and hash_class index
  { cdNam = s;
    cdPrm = sl1;
    cdArg = pl1;
    cdSlf = so1;
    cdTyc = so2;
    cdFld = cfl;
    cdVir = b1;
    cdCls = b2 } =
   let index = hash index 0x2050c242 in
   let index = hash_string index s in
   let index = List.fold_left hash_string index sl1 in
   let index = List.fold_left hash_patt index pl1 in
   let index = hash_string_opt index so1 in
   let index = hash_string_opt index so2 in
   let index = List.fold_left hash_class_field index cfl in
   let index = hash_bool index b1 in
   let index = hash_bool index b2 in
      index

and hash_class_field index = function
   CfCtr (_, s, t) ->
      hash_string (hash_type (hash index 0x6ebb5387) t) s
 | CfInh (_, t, e, s) ->
      hash_type (hash_expr (hash_string_opt (hash index 0x113fee9d) s) e) t
 | CfMth (_, s, e) ->
      hash_string (hash_expr (hash index 0x4ab006da) e) s
 | CfVal (_, s, b1, b2, eo) ->
      hash_string (hash_bool (hash_bool (hash_expr_opt (hash index 0x6d82d28f) eo) b2) b1) s
 | CfVir (_, s, t) ->
      hash_string (hash_type (hash index 0x60a53407) t) s

and hash_class_type index
  { ctNam = name;
    ctPrm = sl1;
    ctArg = pl1;
    ctTyc = so1;
    ctFld = cfl;
    ctVir = b1;
    ctCls = b2
  } =
   let index = hash index 0x7de2ab2c in
   let index = List.fold_left hash_string index sl1 in
   let index = List.fold_left hash_type index pl1 in
   let index = hash_string_opt index so1 in
   let index = List.fold_left hash_class_field_type index cfl in
   let index = hash_bool index b1 in
   let index = hash_bool index b2 in
      index

and hash_class_field_type index = function
   CtCtr (_, s, t) ->
      hash_string (hash_type (hash index 0x362be7cd) t) s
 | CtInh (_, t) ->
      hash_type (hash index 0x1779e662) t
 | CtMth (_, s, t) ->
      hash_string (hash_type (hash index 0x028b2e41) t) s
 | CtVal (_, s, b1, b2, ot) ->
      hash_string (hash_bool (hash_bool (hash_type_opt (hash index 0x3be81fa) ot) b2) b1) s
 | CtVir (_, s, t) ->
      hash_string (hash_type (hash index 0x2730112a) t) s

(*
 * Combined forms.
 *)
and hash_expr_opt index = function
   Some expr -> hash_expr (hash index 0x2a4e2fa2) expr
 | None -> hash index 0x3609fea2

and hash_type_opt index = function
   Some t -> hash_type (hash index 0x3bee38a) t
 | None -> hash index 0x32bbe81a

and hash_pwe index (patt, with_expr, expr) =
   hash_patt (hash_expr_opt (hash_expr (hash index 0x50777efa) expr) with_expr) patt

and hash_pe index (patt, expr) =
   hash_patt (hash_expr (hash index 0x30fff6d5) expr) patt

and hash_se index (s, e) =
   hash_string (hash_expr (hash index 0x70cea23f) e) s

and hash_ee index (e1, e2) =
   hash_expr (hash_expr (hash index 0x7b06e459) e2) e1

and hash_pp index (p1, p2) =
   hash_patt (hash_patt (hash index 0x1272c69f) p2) p1

and hash_st index (s, t) =
   hash_string (hash_type (hash index 0x241432a7) t) s

and hash_sbt index (s, b, t) =
   hash_string (hash_bool (hash_type (hash index 0x1fa3771f) t) b) s

and hash_stl index (s, tl) =
   List.fold_left hash_type (hash_string (hash index 0x7497f04c) s) tl

and hash_sslt index (s, sl, t) =
   hash_string (List.fold_left hash_string (hash_type (hash index 0x0a10556d) t) sl) s

(*
 * $Log$
 * Revision 1.6  1998/04/24 02:41:51  jyh
 * Added more extensive debugging capabilities.
 *
 * Revision 1.5  1998/02/12 23:38:07  jyh
 * Added support for saving intermediate files to the library.
 *
 * Revision 1.3  1998/01/27 23:04:14  jyh
 * Adding OCaml1.07 syntax.
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
