doc <:doc<
   @spelling{mptop toplevel}

   @begin[doc]
   @module[Mptop]

   The @tt{Mptop} module defines a simplified OCaml top-loop
   that is used by the @MetaPRL editor to evaluate user input.
   The evaluator handles only a few basic types (for example, for
   strings, numbers, terms, and tactics), and it handles function
   application.  It does not implement more sophisticated OCaml
   expressions such as function definition and pattern matching.
   @end[doc]

   ----------------------------------------------------------------

   @begin[license]

   This file is part of MetaPRL, a modular, higher order
   logical framework that provides a logical programming
   environment for OCaml and other languages.

   See the file doc/index.html for information on Nuprl,
   OCaml, and more information about this system.

   Copyright (C) 1998 Jason Hickey, Cornell University

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License
   as published by the Free Software Foundation; either version 2
   of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

   Author: Jason Hickey @email{jyh@cs.caltech.edu}
   Modified By: Aleksey Nogin @email{nogin@cs.caltech.edu}

   @end[license]
>>

doc <:doc<
   @begin[doc]
   @parents
   @end[doc]
>>
extends Summary
doc <:doc< @docoff >>

open MLast

open Lm_string_set

open Refiner.Refiner.TermType
open Refiner.Refiner.TermAddr
open Refiner.Refiner.TermMan
open Refiner.Refiner.RefineError
open Mp_resource

open Tactic_type.Tacticals
open Tactic_type.Conversionals

open Shell_sig

(* XXX Bootstrapping HACK *)
let _ = Theory.substitute_dforms "comment" "summary"

(************************************************************************
 * TYPES                                                                *
 ************************************************************************)

doc <:doc<
   @begin[doc]
   The valid expression types are given with the following type
   definition.

   @begin[verbatim]
   type top_expr =
      (* Base types *)
      UnitExpr of unit
    | BoolExpr of bool
    | IntExpr of int
    | StringExpr of string
    | TermExpr of term
    | TacticExpr of tactic
    | ConvExpr of conv
    | AddressExpr of address

      (* Untyped tuples and functions *)
    | ListExpr of top_expr list

      (* Common cases are typed *)
    | UnitFunExpr of (unit -> top_expr)
    | BoolFunExpr of (bool -> top_expr)
    | IntFunExpr of (int -> top_expr)
    | StringFunExpr of (string -> top_expr)
    | TermFunExpr of (term -> top_expr)
    | TacticFunExpr of (tactic -> top_expr)
    | IntTacticFunExpr of ((int -> tactic) -> top_expr)
    | ConvFunExpr of (conv -> top_expr)
    | AddressFunExpr of (address -> top_expr)

      (* These functions take lists *)
    | AddrFunExpr of (int list -> top_expr)
    | StringListFunExpr of (string list -> top_expr)
    | TermListFunExpr of (term list -> top_expr)
    | TacticListFunExpr of (tactic list -> top_expr)
    | ConvListFunExpr of (conv list -> top_expr)
   @end[verbatim]
   @end[doc]
>>

type item = string * string * top_expr * top_type

module Table = StringMTable

(*
 * The resource maps strings to values.
 *)
type top_table = (string * top_expr * top_type) Table.t

(************************************************************************
 * IMPLEMENTATION                                                       *
 ************************************************************************)

let add tbl (module_name,name,expr,typ) =
   Table.add tbl name ((String.capitalize module_name),expr,typ)

let add_list = List.fold_left add

let mem = Table.mem

doc <:doc<
   @begin[doc]
   Toplevel values are added to the @Comment!resource[toploop_resource] resource.
   The argument has type @code{string * top_expr}, which includes
   the string name of the value, and it's value.
   @docoff
   @end[doc]
>>
let resource toploop = Functional {
   fp_empty = Table.empty;
   fp_add = add;
   fp_retr = add_list
}

(************************************************************************
 * COMPILING                                                            *
 ************************************************************************)

(*
 * Right now most things are not supported.
 *)
let not_supported loc str =
   Stdpp.raise_with_loc loc (RefineError ("mptop", StringStringError ("operation is not implemented", str)))

let rec mk_proj_expr loc top_expr =
   let rec collect names top_expr =
      match top_expr with
         (<:expr< $uid: name$ . $e2$ >>) ->
            collect (name :: names) e2
       | (<:expr< $lid: v$ >>) ->
            VarProjExpr(names, v)
       | _ ->
            not_supported loc "expr projection"
   in
      collect [] top_expr

(*
 * A tuple of expressions.
 * We only support unit for now.
 *)
and mk_tuple_expr loc = function
   [] ->
      UnitExpr ()
 | _ ->
      not_supported loc "tuple expression"

and mk_expr top_expr =
   let loc = loc_of_expr top_expr in
   let expr =
      match top_expr with
         (<:expr< $e1$ . $e2$ >> as top_expr) ->
            mk_proj_expr loc top_expr
       | (<:expr< $e1$ $e2$ >>) ->
            ApplyExpr (mk_expr e1, mk_expr e2)
       | (<:expr< $e1$ .( $e2$ ) >>) ->
            not_supported loc "array subscript"
       | (<:expr< [| $list:el$ |] >>) ->
            not_supported loc "array"
       | (<:expr< $e1$ := $e2$ >>) ->
            not_supported loc "assignment"
       | (<:expr< $chr:c$ >>) ->
            not_supported loc "char"
       | (<:expr< ( $e$ :> $t$ ) >>) ->
            not_supported loc "class coercion"
       | (<:expr< $flo:s$ >>) ->
            not_supported loc "float"
       | (<:expr< for $s$ = $e1$ $to:b$ $e2$ do { $list:el$ } >>) ->
            not_supported loc "for loop"
       | (<:expr< fun [ $list:pwel$ ] >>) ->
            not_supported loc "fun"
       | (<:expr< if $e1$ then $e2$ else $e3$ >>) ->
            not_supported loc "ifthenelse"
       | (<:expr< $int:s$ >>) ->
            IntExpr (int_of_string s)
       | (<:expr< lazy $_$ >>) ->
            not_supported loc "lazy"
       | (<:expr< let $rec:b$ $list:pel$ in $e$ >>) ->
            not_supported loc "let"
       | (<:expr< $lid:s$ >>)
       | (<:expr< $uid:s$ >>) ->
            VarExpr s
       | MLast.ExLmd _ ->
            not_supported loc "local module"
       | (<:expr< match $e$ with [ $list:pwel$ ] >>) ->
            not_supported loc "match"
       | MLast.ExNew _ ->
            not_supported loc "new"
(*
       | (<:expr< {< $list:sel$ >} >>) ->
*)
       | MLast.ExOvr _ ->
            not_supported loc "stream"
(*
       | (<:expr< { $list:eel$ } >>) ->
*)
       | MLast.ExRec _ ->
            not_supported loc "record"
       | (<:expr< do { $list:el$ } >>) ->
            not_supported loc "do"
(*
       | (<:expr< $e$ # $i$ >>) ->
*)
       | MLast.ExSnd _ ->
            not_supported loc "class projection"
       | (<:expr< $e1$ .[ $e2$ ] >>) ->
            not_supported loc "string subscript"
       | (<:expr< $str:s$ >>) ->
            StringExpr s
       | (<:expr< try $e$ with [ $list:pwel$ ] >>) ->
            not_supported loc "try"
       | (<:expr< ( $list:el$ ) >>) ->
            mk_tuple_expr loc el
       | (<:expr< ( $e$ : $_$ ) >>) ->
            snd (mk_expr e)
       | (<:expr< while $e$ do { $list:el$ } >>) ->
            not_supported loc "while"
       | MLast.ExAnt (_, e) ->
            not_supported loc "ExAnt"
       | MLast.ExVrn _ ->
            not_supported loc "ExVrn"
       | MLast.ExOlb _ ->
            not_supported loc "ExOlb"
       | MLast.ExLab _ ->
            not_supported loc "ExLab"
       | MLast.ExCoe _ ->
            not_supported loc "ExCoe"
   in
      loc, expr

and mk_patt patt =
   let loc = loc_of_patt patt in
      match patt with
         (<:patt< $p1$ . $p2$ >>) ->
            not_supported loc "patt projection"
       | (<:patt< ( $p1$ as $p2$ ) >>) ->
            not_supported loc "patt"
       | (<:patt< _ >>) ->
            not_supported loc "wild pattern"
       | (<:patt< $p1$ $p2$ >>) ->
            not_supported loc "pattern application"
       | (<:patt< [| $list: pl$ |] >>) ->
            not_supported loc "array patterns"
       | (<:patt< $chr:c$ >>) ->
            not_supported loc "pattern char"
       | (<:patt< $int:s$ >>) ->
            not_supported loc "pattern int"
       | (<:patt< $lid:v$ >>) ->
            not_supported loc "pattern var"
       | (<:patt< $p1$ | $p2$ >>) ->
            not_supported loc "pattern choice"
       | (<:patt< $p1$ .. $p2$ >>) ->
            not_supported loc "pattern range"
       | (<:patt< { $list:ppl$ } >>) ->
            not_supported loc "pattern record"
       | (<:patt< $str:s$ >>) ->
            not_supported loc "pattern string"
       | (<:patt< ( $list:pl$ ) >>) ->
            not_supported loc "pattern list"
       | (<:patt< ( $p$ : $t'$ ) >>) ->
            not_supported loc "pattern cast"
       | (<:patt< $uid:s$ >>) ->
            not_supported loc "pattern uid"
       | MLast.PaAnt (_, p) ->
            not_supported loc "pattern PaAnt"
       | MLast.PaVrn _ ->
            not_supported loc "patterm PaVrn"
       | MLast.PaOlb _ ->
            not_supported loc "patterm PaOlb"
       | MLast.PaLab _ ->
            not_supported loc "patterm PaLab"
       | MLast.PaFlo _ ->
            not_supported loc "patterm PaFlo"
       | MLast.PaTyp _ ->
            not_supported loc "patterm PaTyp"

and mk_type t =
   let loc = loc_of_ctyp t in
      match t with
         (<:ctyp< $t1$ . $t2$ >>) ->
            not_supported loc "type projection"
       | (<:ctyp< $t1$ as $t2$ >>) ->
            not_supported loc "type"
       | (<:ctyp< _ >>) ->
            not_supported loc "type wildcard"
       | (<:ctyp< $t1$ $t2$ >>) ->
            not_supported loc "type application"
       | (<:ctyp< $t1$ -> $t2$ >>) ->
            not_supported loc "type function"
(*
       | (<:ctyp< # $i$ >>) ->
*)
       | MLast.TyCls _ ->
            not_supported loc "type method"
       | (<:ctyp< $lid:s$ >>) ->
            not_supported loc "type var"
       | (<:ctyp< '$s$ >>) ->
            not_supported loc "type param"
       | (<:ctyp< $t1$ == $t2$ >>) ->
            not_supported loc "type equality"
(*
       | (<:ctyp< < $list:stl$ $dd:b$ > >>) ->
*)
       | MLast.TyObj (loc, _, _) ->
            not_supported loc "type class"
       | (<:ctyp< { $list:sbtl$ } >>) ->
            not_supported loc "type record"
       | (<:ctyp< [ $list:stll$ ] >>) ->
            not_supported loc "type list"
       | (<:ctyp< ( $list:tl$ ) >>) ->
            not_supported loc "type product"
       | (<:ctyp< $uid:s$ >>) ->
            not_supported loc "type constructor var"
       | MLast.TyPol (_, _, _) ->
            not_supported loc "type constructor Pol"
       | MLast.TyVrn _ ->
            not_supported loc "type constructor Vrn"
       | MLast.TyOlb _ ->
            not_supported loc "type constructor Olb"
       | MLast.TyLab _ ->
            not_supported loc "type constructor Lab"

and mk_sig_item si =
   let loc = loc_of_sig_item si in
      match si with
(*
         (<:sig_item< class $list:ctl$ >>) ->
*)
         MLast.SgCls _
       | MLast.SgClt _ ->
            not_supported loc "sig class"
       | (<:sig_item< declare $list:sil$ end >>) ->
            mk_sig_item (Lm_list_util.last sil)
       | (<:sig_item< exception $s$ of $list:tl$ >>) ->
            not_supported loc "sig exception"
       | (<:sig_item< external $s$ : $t$ = $list:sl$ >>) ->
            not_supported loc "sig external"
       | SgInc (_, mt) ->
            not_supported loc "sig SgInc"
       | (<:sig_item< module $s$ : $mt$ >>) ->
            not_supported loc "sig module"
       | (<:sig_item< module type $s$ = $mt$ >>) ->
            not_supported loc "sig module type"
       | (<:sig_item< open $sl$ >>) ->
            not_supported loc "sig open"
       | (<:sig_item< type $list:tdl$ >>) ->
            not_supported loc "sig type"
       | (<:sig_item< value $s$ : $t$ >>) ->
            not_supported loc "sig value"
       | MLast.SgDir _ ->
            not_supported loc "sig dir"

and mk_str_item si =
   let loc = loc_of_str_item si in
      match si with
         MLast.StCls _
       | MLast.StClt _ ->
            not_supported loc "str class"
       | (<:str_item< declare $list:stl$ end >>) ->
            mk_str_item (Lm_list_util.last stl)
       | (<:str_item< exception $s$ of $list:tl$ >>) ->
            not_supported loc "str exception"
       | (<:str_item< $exp:e$ >>) ->
            mk_expr e
       | (<:str_item< external $s$ : $t$ = $list:sl$ >>) ->
            not_supported loc "str external"
       | (<:str_item< module $s$ = $me$ >>) ->
            not_supported loc "str module"
       | (<:str_item< module type $s$ = $mt$ >>) ->
            not_supported loc "str module type"
       | (<:str_item< open $sl$ >>) ->
            not_supported loc "str module open"
       | (<:str_item< type $list:tdl$ >>) ->
            not_supported loc "str type"
       | (<:str_item< value $rec:b$ $list:pel$ >>) ->
            not_supported loc "str let"
       | MLast.StDir _ ->
            not_supported loc "str dir"
       | MLast.StInc _ ->
            not_supported loc "str include"
       | MLast.StExc _ ->
            not_supported loc "StExc"

and mk_module_type mt =
   let loc = loc_of_module_type mt in
      match mt with
         (<:module_type< $mt1$ . $mt2$ >>) ->
            not_supported loc "module type projection"
       | (<:module_type< $mt1$ $mt2$ >>) ->
            not_supported loc "module type application"
       | (<:module_type< functor ( $s$ : $mt1$ ) -> $mt2$ >>) ->
            not_supported loc "module type functor"
       | (<:module_type< $lid:_$ >>)
       | (<:module_type< ' $_$ >>)
       | (<:module_type< $uid:_$ >>) ->
            not_supported loc "module type var"
       | (<:module_type< sig $list:sil$ end >>) ->
            not_supported loc "module type sig"
       | (<:module_type< $mt$ with $list:wcl$ >>) ->
            not_supported loc "module type constraint"

and mk_wc = function
   WcTyp (loc, sl1, sl2, t) ->
      not_supported loc "with clause type"
 | WcMod (loc, sl1, mt) ->
      not_supported loc "with clause module"

and mk_module_expr me =
   let loc = loc_of_module_expr me in
      match me with
         (<:module_expr< $me1$ . $me2$ >>) ->
            not_supported loc "module top_expr projection"
       | (<:module_expr< $me1$ $me2$ >>) ->
            not_supported loc "module top_expr application"
       | (<:module_expr< functor ( $s$ : $mt$ ) -> $me$ >>) ->
            not_supported loc "module top_expr functor"
       | (<:module_expr< struct $list:sil$ end >>) ->
            not_supported loc "module top_expr struct"
       | (<:module_expr< ( $me$ : $mt$) >>) ->
            not_supported loc "module top_expr type"
       | (<:module_expr< $uid:i$ >>) ->
            not_supported loc "module top_expr id"

(************************************************************************
 * TYPE CHECKING                                                        *
 ************************************************************************)

let type_error loc str =
   Stdpp.raise_with_loc loc (RefineError ("Type error", StringError str))

let rec str_typ = function
   UnitType -> "unit"
 | BoolType -> "bool"
 | IntType -> "int"
 | StringType -> "string"
 | TermType -> "term"
 | TacticType -> "tactic"
 | ConvType -> "conv"
 | AddressType -> "address"
 | ListType t -> (par_str_type t) ^ " list"
 | NilType -> "'a list"
 | ConsType -> "'a -> 'a list -> 'a list"
 | FunType (t1, t2) -> (par_str_type t1) ^ " -> " ^ (str_typ t2)

and par_str_type = function
   (ListType _ | FunType _ | ConsType) as t -> "(" ^ (str_typ t) ^ ")"
 | t -> str_typ t

let find_proj_expr base loc names v =
   let rec search modname v = function
      (modname', top_expr, top_typ) :: _ when modname' = modname ->
         top_expr, top_typ
    | _ :: tl ->
         search modname v tl
    | [] ->
         Stdpp.raise_with_loc loc (RefineError ("mk_proj_expr", StringStringError ("undefined variable", modname ^ "." ^ v)))
   in
      match names with
         [modname] ->
            search modname v (Table.find_all base v)
       | _ ->
            not_supported loc "nested modules"

let subtyp sub sup =
   match sub, sup with
      NilType, ListType _ -> true
    | (ListType IntType | NilType), AddressType -> true
    | _ -> sub = sup

(* Returns the type of the input expression *)
let rec expr_tp base loc = function
   UnitExpr _ -> UnitType
 | BoolExpr _ -> BoolType
 | IntExpr _ -> IntType
 | StringExpr _ -> StringType
 | TermExpr _ -> TermType
 | TacticExpr _ -> TacticType
 | ConvExpr _ -> ConvType
 | AddressExpr _ -> AddressType
 | ListExpr [] -> NilType
 | ListExpr (hd::tl) ->
      let typ = expr_tp base loc hd in
         List.iter (expr_typechk base loc typ) tl;
         ListType typ
 | VarExpr v ->
   begin try
      let _, _, typ = Table.find base v in typ
   with Not_found ->
      Stdpp.raise_with_loc loc (RefineError ("Mptop.mk_var_expr", StringStringError ("undefined variable", v)))
   end
 | VarProjExpr (names,v) ->
      snd (find_proj_expr base loc names v)
 | ApplyExpr (f, (loca, a)) ->
      begin match expr_type base f with
         FunType(t1, t2) ->
            expr_typechk base loca t1 a;
            t2
       | ConsType ->
            let t = ListType(expr_tp base loca a) in
               FunType(t,t)
       | _ ->
            begin match f with
               _, ApplyExpr _ ->
                  type_error loc "Function is applied to too many arguments"
             | _ ->
                  type_error loc "Expression is not a function, it cannot be applied"
            end
      end
 | UnitFunExpr _ | BoolFunExpr _ | IntFunExpr _ | StringFunExpr _ | TermFunExpr _
 | TacticFunExpr _ | IntTacticFunExpr _ | ConvFunExpr _ | AddressFunExpr _
 | IntListFunExpr _ | StringListFunExpr _ | TermListFunExpr _ | TacticListFunExpr _
 | ConvListFunExpr _ | FunExpr _ ->
      Stdpp.raise_with_loc loc (Invalid_argument "Mptop: function expression without an explicit type")

and expr_type base (loc, expr) = expr_tp base loc expr

and expr_typechk base loc typ expr =
   let typ' = expr_tp base loc expr in
      if not (subtyp typ' typ) then
         type_error loc ("Expression has type\n   " ^ (str_typ typ') ^ "\nbut is used here as type\n   " ^ (str_typ typ))

(************************************************************************
 * EVALUATING                                                           *
 ************************************************************************)

let runtime_error loc =
   Stdpp.raise_with_loc loc (Invalid_argument "Mptop: type mismatch not caught by type-checking")

(*
 * Lookup a variable from the table.
 *)
let eval_var_expr base v =
   let _, expr, _  = Table.find base v in expr

(*
 * Convert a list to a term list.
 *)
let term_expr loc = function
   TermExpr t -> t
 | _ -> runtime_error loc

let int_expr loc = function
   IntExpr t -> t
 | _ -> runtime_error loc

let string_expr loc = function
   StringExpr t -> t
 | _ -> runtime_error loc

let tactic_expr loc = function
   TacticExpr t -> t
 | _ -> runtime_error loc

let conv_expr loc = function
   ConvExpr t -> t
 | _ -> runtime_error loc

(*
 * For an application, we lookup the function and try to
 * specialize the argument.
 *)
let rec eval_apply_expr base loc f a =
   match eval_expr base f, eval_expr base a with
      FunExpr f, a ->
         f a
    | BoolFunExpr f, BoolExpr a  ->
         f a
    | IntFunExpr f, IntExpr a ->
         f a
    | StringFunExpr f, StringExpr a ->
         f a
    | TermFunExpr f, TermExpr a ->
         f a
    | TacticFunExpr f, TacticExpr a ->
         f a
    | ConvFunExpr f, ConvExpr a  ->
         f a
    | AddressFunExpr f, AddressExpr a ->
         f a
    | UnitFunExpr f, UnitExpr _ ->
         f ()
    | IntTacticFunExpr f, IntFunExpr f' ->
         let tac i =
            match f' i with
               TacticExpr tac -> tac
             | _ -> runtime_error loc
         in
            f tac
    | AddressFunExpr f, ListExpr l ->
         f (make_address (List.map (int_expr loc) l))
    | IntListFunExpr f, ListExpr l ->
         f (List.map (int_expr loc) l)
    | StringListFunExpr f, ListExpr l ->
         f (List.map (string_expr loc) l)
    | TermListFunExpr f, ListExpr l ->
         f (List.map (term_expr loc) l)
    | TacticListFunExpr f, ListExpr l ->
         f (List.map (tactic_expr loc) l)
    | ConvListFunExpr f, ListExpr l ->
         f (List.map (conv_expr loc) l)
    | _ ->
         runtime_error loc

and eval_expr base = function
   loc, ApplyExpr(f, a) ->
      eval_apply_expr base loc f a
 | loc, VarProjExpr(names,v) ->
      fst (find_proj_expr base loc names v)
 | _, VarExpr v ->
      eval_var_expr base v
 | _, expr ->
      expr

(************************************************************************
 * RESOURCES                                                            *
 ************************************************************************)

(*
 * Include the common library functions.
 *)
let int_int_fun_int_expr f =
   IntFunExpr (fun i -> IntFunExpr (fun j -> IntExpr (f i j)))

let int_int_fun_typ = FunType (IntType, FunType (IntType, IntType))

let cons_expr =
   FunExpr (fun e1 ->
         FunExpr (fun e2 ->
               match e2 with
                  ListExpr e2 ->
                     ListExpr (e1 :: e2)
                | _ ->
                     raise (RefineError ("cons_expr", StringError "type mismatch"))))

let resource toploop +=
   ["Pervasives", "+",     int_int_fun_int_expr ( + ), int_int_fun_typ;
    "Pervasives", "-",     int_int_fun_int_expr ( - ), int_int_fun_typ;
    "Pervasives", "*",     int_int_fun_int_expr ( * ), int_int_fun_typ;
    "Pervasives", "/",     int_int_fun_int_expr ( / ), int_int_fun_typ;
    "Pervasives", "::",    cons_expr,                  ConsType;
    "Pervasives", "()",    UnitExpr (),                UnitType;
    "Pervasives", "[]",    ListExpr [],                NilType;
    "Pervasives", "True",  BoolExpr true,              BoolType;
    "Pervasives", "False", BoolExpr false,             BoolType]

let tactic_of_ocaml_expr base expr =
   let (loc, expr) as lexpr = mk_expr expr in
      expr_typechk base loc TacticType expr;
      match eval_expr base lexpr with
         TacticExpr tac -> tac
       | _ -> runtime_error loc

let evaluate_ocaml_expr base expr =
   let expr = mk_expr expr in
   let typ = expr_type base expr in
      eval_expr base expr, typ

let evaluate_ocaml_str_item base item =
   let expr = mk_str_item item in
   let typ = expr_type base expr in
      eval_expr base expr, typ

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
