(*
 * Compute the free vars of a term.
 * This is all the lids.
 *)

open Printf
open Nl_debug

(*
 * Show the file loading.
 *)
let _ =
   if !debug_load then
      eprintf "Loading Free_vars%t" eflush


(*
 * Compute the binding variables.
 *)
let rec patt_bvars bvars = function
   <:patt< $p1$ . $p2$ >> ->
      patt_bvars (patt_bvars bvars p1) p2
 | <:patt< ($p1$ as $p2$) >> ->
      patt_bvars (patt_bvars bvars p1) p2
 | <:patt< _ >> ->
      bvars
 | <:patt< $p1$ $p2$ >> ->
      patt_bvars (patt_bvars bvars p1) p2
 | <:patt< $chr:_$ >> ->
      bvars
 | <:patt< $int:_$ >> ->
      bvars
 | <:patt< $lid:i$ >> ->
      if List.mem i bvars then
         bvars
      else
         i :: bvars
 | <:patt< $p1$ | $p2$ >> ->
      patt_bvars (patt_bvars bvars p1) p2
 | <:patt< $p1$ .. $p2$ >> ->
      patt_bvars (patt_bvars bvars p1) p2
 | <:patt< { $list:ppl$ } >> ->
      List.fold_left (fun bvars (p1, p2) -> patt_bvars (patt_bvars bvars p1) p2) bvars ppl
 | <:patt< $str:_$ >> ->
      bvars
 | <:patt< ( $list:pl$ ) >> ->
      List.fold_left patt_bvars bvars pl
 | <:patt< ( $p$ : $_$ ) >> ->
      patt_bvars bvars p
 | <:patt< $uid:_$ >> ->
      bvars
 | _ ->
      raise (Failure "patt_bvars: pattern not recognized")

let patt_vars patt =
   patt_bvars [] patt

(*
 * Compute free vars of an Ast.expr.
 *)
let free_vars expr =
   let rec free bvars l = function
      <:expr< $lid:v$ >> ->
         if List.mem v bvars or List.mem v l then
            l
         else
            v :: l
    | <:expr< $e1$ . $e2$ >> ->
         free bvars (free bvars l e1) e2
    | <:expr< $e1$ $e2$ >> ->
         free bvars (free bvars l e1) e2
    | <:expr< $e1$ .( $e2$ ) >> ->
         free bvars (free bvars l e1) e2
    | <:expr< [| $list:el$ |] >> ->
         List.fold_left (free bvars) l el
    | <:expr< $e1$ := $e2$ >> ->
         free bvars (free bvars l e1) e2
    | <:expr< $chr:c$ >> ->
         l
(*
    | <:expr< ( $e1$ :> $_$ ) >> ->
*)
    | MLast.ExCoe (_, e, _) ->
         free bvars l e
    | <:expr< $flo:s$ >> ->
         l
    | <:expr< for $v$ = $e1$ $to:_$ $e2$ do $list:el$ done >> ->
         List.fold_left (free (v::bvars)) (free bvars (free bvars l e1) e2) el
    | <:expr< fun [ $list:pwel$ ] >> ->
         free_pwel bvars l pwel
    | <:expr< if $e1$ then $e2$ else $e3$ >> ->
         free bvars (free bvars (free bvars l e1) e2) e3
    | <:expr< $int:s$ >> ->
         l
    | <:expr< let $rec:_$ $list:pel$ in $e$ >> ->
         free_pel bvars l e pel
    | <:expr< match $e$ with [ $list:pwel$ ] >> ->
         free_pwel bvars (free bvars l e) pwel
(*
    | <:expr< new $e$ >> ->
*)
    | MLast.ExNew _ ->
         bvars
    | MLast.ExLmd (_, _, _, e) ->
         free bvars l e
(*
    | <:expr< {< $list:sel$ >} >> ->
*)
    | MLast.ExOvr (_, sel) ->
         List.fold_left (fun l (_, el) -> free bvars l el) l sel
    | <:expr< { $list:eel$ } >> ->
         List.fold_left (fun l (_, el) -> free bvars l el) l eel
    | <:expr< do $list:el$ return $e$ >> ->
         List.fold_left (free bvars) (free bvars l e) el
(*
    | <:expr< $e$ # $_$ >> ->
*)
    | MLast.ExSnd (_, e, _) ->
         free bvars l e
    | <:expr< $e1$ .[ $e2$ ] >> ->
         free bvars (free bvars l e1) e2
    | <:expr< $str:s$ >> ->
         l
    | <:expr< try $e$ with [ $list:pwel$ ] >> ->
         free_pwel bvars (free bvars l e) pwel
    | <:expr< ( $list:el$ ) >> ->
         List.fold_left (free bvars) l el
    | <:expr< ( $e$ : $_$ ) >> ->
         free bvars l e
    | <:expr< $uid:_$ >> ->
         l
    | <:expr< while $e$ do $list:el$ done >> ->
         List.fold_left (free bvars) (free bvars l e) el
    | _ ->
         raise (Failure "free_vars: expression not recognized")
   and free_pwel bvars l = function
      (patt, _, e)::tl ->
          free_pwel bvars (free (patt_bvars bvars patt) l e) tl
     | [] ->
          l
   and free_pel bvars l e = function
      (patt, e)::tl ->
          free_pel (patt_bvars bvars patt) (free bvars l e) e tl
     | [] ->
          free bvars l e
   in
      free [] [] expr

let _ = ()

(*
 * Find some new variable names.
 *)
let new_vars expr l =
   let free = free_vars expr in
   let rec new_var i v =
      let vname = sprintf "%s%d" v i in
         if List.mem vname free then
            new_var (i + 1) v
         else
            vname
   in
      List.map (new_var 0) l

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
