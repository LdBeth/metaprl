(*
 * Build expressions.
 *)

open Printf
open Nl_debug

open Ml_format_sig
open Ml_format

(*
 * Show the file loading.
 *)
let _ =
   if !debug_load then
      eprintf "Loading Filter_ast%t" eflush


(*
 * ML expression converter.
 *)
let build_printed_term loc t =
   let rec build_application = function
      [] ->
         raise (Invalid_argument "build_application")
    | [f] ->
         build f
    | f::a ->
         let a_expr = build_application a in
         let f_expr = build f in
            <:expr< $f_expr$ $a_expr$ >>
   and build_list = function
      h::t ->
         let t_expr = build_list t in
         let h_expr = build h in
            <:expr< [ $h_expr$ :: $t_expr$ ] >>
    | [] ->
         <:expr< [] >>
   and build_var = function
      [h] ->
         if Ctype.is_capitalized h then
            <:expr< $uid:h$ >>
         else
            <:expr< $lid:h$ >>
    | h::t ->
         let mod_expr = <:expr< $uid:h$ >> in
         let name_expr = build_var t in
            <:expr< $mod_expr$ . $name_expr$ >>
    | [] ->
         raise (Invalid_argument "build_application")
   and build = function
      ML_Var v ->
         <:expr< $lid:v$ >>
    | ML_Int i ->
         let i' = string_of_int i in
            <:expr< $int:i'$ >>
    | ML_Num n ->
         <:expr< $uid:"Num"$ . $lid:"num_of_string"$ $str:Nl_num.string_of_num n$ >>
    | ML_String s ->
         <:expr< $str:s$ >>
    | ML_List l ->
         build_list l
    | ML_Apply l ->
         build_application l
    | ML_Tuple l ->
         let exprs = List.map build l in
            <:expr< ( $list:exprs$ ) >>
    | ML_Record l ->
         let fields = List.map (function (name, expr) -> (build name, build expr)) l in
            <:expr< { $list:fields$ } >>
    | ML_Module_Var l ->
         build_var l
    | ML_Let (v, expr, body) ->
         let binding_expr = [<:patt< $lid:v$ >>, build expr] in
         let body_expr = build body in
            <:expr< let $rec:false$ $list:binding_expr$ in $body_expr$ >>
   in
      build t

let build_ml_term loc t =
   build_printed_term loc (FormatTerm.format_term t)

let build_ml_mterm loc mterm =
   build_printed_term loc (FormatTerm.format_mterm mterm)

(*
 * Construct an expression list.
 *)
let list_expr loc f l =
   let rec map = function
      h::t ->
         let hd = f h in
         let tl = map t in
            <:expr< [ $hd$ :: $tl$ ] >>
    | [] ->
         <:expr< [] >>
   in
      map l

(*
 * Construct an expression list.
 *)
let apply_patt loc f l =
   let rec map = function
      [h] ->
         f h
    | h::t ->
         let hd = f h in
         let tl = map t in
            <:patt< $hd$ $tl$ >>
    | [] ->
         raise (Invalid_argument "apply_patt")
   in
      map l

(*
 * Construct an expression list.
 *)
let list_patt loc f l =
   let rec map = function
      [] ->
         <:patt< [] >>
    | h::t ->
         let hd = f h in
         let tl = map t in
            <:patt< [ $hd$ :: $tl$ ] >>
   in
      map l

(*
 * A multiple argument function.
 *)
let fun_expr loc ids body =
   let rec aux = function
      h::t ->
         let patt = <:patt< $lid:h$ >> in
            (<:expr< fun [ $list:[ patt, None, aux t ]$ ] >>)
    | [] ->
         body
   in
      aux ids

let () = ()

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
