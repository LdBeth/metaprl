(*
 * Handle commenting.
 * We parse a file, and scan in the comments and their
 * locations.  then we pass through the terms, and match their
 * locations with the comments.
 *)

open Printf
open Debug

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
     fold_class            : 'a -> MLast.class_decl -> 'a;
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
       | (<:expr< ( $e$ :> $t$ ) >>) ->
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
       | (<:expr< match $e$ with [ $list:pwel$ ] >>) ->
            List.fold_left (fold_pwe iter) (fold_expr iter x e) pwel
       | (<:expr< new $e$ >>) ->
            fold_expr iter x e
       | (<:expr< {< $list:sel$ >} >>) ->
            List.fold_left (fold_se iter) x sel
       | (<:expr< { $list:eel$ } >>) ->
            List.fold_left (fold_ee iter) x eel
       | (<:expr< do $list:el$ return $e$ >>) ->
            fold_expr iter (List.fold_left (fold_expr iter) x el) e
       | (<:expr< $e$ # $i$ >>) ->
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
       | MLast.ExAnt (loc, e) ->
            Stdpp.raise_with_loc loc (Failure "Filter_ocaml.mk_expr: encountered an ExAnt")

and fold_patt iter x patt =
   let x = iter.fold_patt x patt in
      match patt with
         (<:patt< $p1$ . $p2$ >>) ->
            fold_patt iter (fold_patt iter x p1) p2
       | (<:patt< ( $p1$ as $p2$ ) >>) ->
            fold_patt iter (fold_patt iter x p1) p2
       | (<:patt< _ >>) ->
            x
       | (<:patt< $p1$ $p2$ >>) ->
            fold_patt iter (fold_patt iter x p1) p2
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
       | MLast.PaAnt (loc, p) ->
            Stdpp.raise_with_loc loc (Failure "Filter_ocaml:mk_patt: encountered PaAnt")

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
       | (<:ctyp< # $i$ >>) ->
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
         (<:sig_item< class $list:ctl$ >>) ->
            List.fold_left (fold_class_type iter) x ctl
       | (<:sig_item< declare $list:sil$ end >>) ->
            List.fold_left (fold_sig_item iter) x sil
       | (<:sig_item< exception $s$ of $list:tl$ >>) ->
            List.fold_left (fold_type iter) x tl
       | (<:sig_item< external $s$ : $t$ = $list:sl$ >>) ->
            fold_type iter x t
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
         (<:str_item< class $list:cdl$ >>) ->
            List.fold_left (fold_class iter) x cdl
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

and fold_class_type iter x
  ({ ctLoc = loc;
     ctNam = s;
     ctPrm = sl;
     ctArg = tl;
     ctTyc = so;
     ctFld = ctfl;
     ctVir = b1;
     ctCls = b2 } as ct) =
   let x = iter.fold_class_type x ct in
   let x = List.fold_left (fold_type iter) x tl in
   let x = List.fold_left (fold_class_type_field iter) x ctfl in
      x

and fold_class_type_field iter x ctf =
   let x = iter.fold_class_type_field x ctf in
      match ctf with
         CtCtr (loc, s, t) ->
            fold_type iter x t
       | CtInh (loc, t) ->
            fold_type iter x t
       | CtMth (loc, s, t) ->
            fold_type iter x t
       | CtVal (loc, s, b1, b2, ot) ->
            fold_type_opt iter x ot
       | CtVir (loc, s, t) ->
            fold_type iter x t

and fold_class iter x
  ({ cdLoc = loc;
     cdNam = s;
     cdPrm = sl1;
     cdArg = pl1;
     cdSlf = so1;
     cdTyc = so2;
     cdFld = cfl;
     cdVir = b1;
     cdCls = b2 } as cl) =
   let x = iter.fold_class x cl in
   let x = List.fold_left (fold_patt iter) x pl1 in
   let x = List.fold_left (fold_class_field iter) x cfl in
      x

and fold_class_field iter x cf =
   let x = iter.fold_class_field x cf in
      match cf with
         CfCtr (loc, s, t) ->
            fold_type iter x t
       | CfInh (loc, t, e, so) ->
            fold_expr iter (fold_type iter x t) e
       | CfMth (loc, s, e) ->
            fold_expr iter x e
       | CfVal (loc, s, b1, b2, eo) ->
            fold_expr_opt iter x eo
       | CfVir (loc, s, t) ->
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
 * $Log$
 * Revision 1.5  1998/06/01 13:53:21  jyh
 * Proving twice one is two.
 *
 * Revision 1.4  1998/04/24 19:38:40  jyh
 * Updated debugging.
 *
 * Revision 1.3  1998/04/24 02:42:18  jyh
 * Added more extensive debugging capabilities.
 *
 * Revision 1.2  1998/04/06 19:50:40  jyh
 * Fixed match error in mLast_util.ml
 *
 * Revision 1.1  1998/02/19 17:14:05  jyh
 * Splitting filter_parse.
 *
 *
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
