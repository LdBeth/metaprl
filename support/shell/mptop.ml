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
  
   Author: Jason Hickey
   @email{jyh@cs.caltech.edu}
  
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

open Refiner.Refiner.TermType
open Refiner.Refiner.TermAddr
open Refiner.Refiner.TermMan
open Refiner.Refiner.RefineError
open Mp_resource

open Tactic_type.Tacticals
open Tactic_type.Conversionals

(* XXX Bootstrapping HACK *)
let _ = Theory.substitute_dforms "comment" "summary"
let _ = Theory.substitute_dforms "perv" "summary"

(************************************************************************
 * TYPES                                                                *
 ************************************************************************)

doc <:doc< 
   @begin[doc]
   The valid expression types are given with the following type
   definition.
  
   @begin[verbatim]
   type expr =
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
    | ListExpr of expr list
    | TupleExpr of expr list
    | FunExpr of (expr -> expr)
  
      (* Common cases are typed *)
    | UnitFunExpr of (unit -> expr)
    | BoolFunExpr of (bool -> expr)
    | IntFunExpr of (int -> expr)
    | StringFunExpr of (string -> expr)
    | TermFunExpr of (term -> expr)
    | TacticFunExpr of (tactic -> expr)
    | IntTacticFunExpr of ((int -> tactic) -> expr)
    | ConvFunExpr of (conv -> expr)
    | AddressFunExpr of (address -> expr)
  
      (* These functions take lists *)
    | AddrFunExpr of (int list -> expr)
    | StringListFunExpr of (string list -> expr)
    | TermListFunExpr of (term list -> expr)
    | TacticListFunExpr of (tactic list -> expr)
    | ConvListFunExpr of (conv list -> expr)
   @end[verbatim]
   @end[doc]
>>
type expr =
   (* Base types *)
   UnitExpr of unit
 | BoolExpr of bool
 | IntExpr of int
 | StringExpr of string
 | TermExpr of term
 | TacticExpr of tactic
 | ConvExpr of conv
 | AddressExpr of address

   (* Uptyped tuples and functions *)
 | ListExpr of expr list
 | TupleExpr of expr list
 | FunExpr of (expr -> expr)

   (* Common cases are typed *)
 | UnitFunExpr of (unit -> expr)
 | BoolFunExpr of (bool -> expr)
 | IntFunExpr of (int -> expr)
 | StringFunExpr of (string -> expr)
 | TermFunExpr of (term -> expr)
 | TacticFunExpr of (tactic -> expr)
 | IntTacticFunExpr of ((int -> tactic) -> expr)
 | ConvFunExpr of (conv -> expr)
 | AddressFunExpr of (address -> expr)

   (* These functions take lists *)
 | AddrFunExpr of (int list -> expr)
 | StringListFunExpr of (string list -> expr)
 | TermListFunExpr of (term list -> expr)
 | TacticListFunExpr of (tactic list -> expr)
 | ConvListFunExpr of (conv list -> expr)

(*
 * The resource maps strings to values.
 *)
type top_table =
   (string, string * expr) Hashtbl.t

(************************************************************************
 * IMPLEMENTATION                                                       *
 ************************************************************************)

let create () =
   Hashtbl.create 201

let add tbl (module_name,name,expr) =
   Hashtbl.add tbl name ((String.capitalize module_name),expr)

let add_commands tbl =
   List.iter (fun (name, expr) -> Hashtbl.add tbl name ("",expr))

let retr tbl = tbl

doc <:doc< 
   @begin[doc]
   Toplevel values are added to the @Comment!resource[toploop_resource] resource.
   The argument has type @code{string * expr}, which includes
   the string name of the value, and it's value.
   @docoff
   @end[doc]
>>
let resource toploop = Imperative {
   imp_create = create;
   imp_add = add;
   imp_retr = retr
}

(************************************************************************
 * COMPILING                                                            *
 ************************************************************************)

(*
 * Right now most things are not supported.
 *)
let not_supported loc str =
   Stdpp.raise_with_loc loc (RefineError ("mptop", StringStringError ("operation is not implemented", str)))

let type_error loc str =
   Stdpp.raise_with_loc loc (RefineError ("type error", StringError str))

(*
 * Convert a list to a term list.
 *)
let some_list_of_list f loc = function
   ListExpr l ->
      List.map f l
 | _ ->
      type_error loc "expr should be a list"

let term_expr loc = function
   TermExpr t ->
      t
 | _ ->
      type_error loc "expr should be a term"

let int_expr loc = function
   IntExpr t ->
      t
 | _ ->
      type_error loc "expr should be an int"

let string_expr loc = function
   StringExpr t ->
      t
 | _ ->
      type_error loc "expr should be a string"

let tactic_expr loc = function
   TacticExpr t ->
      t
 | _ ->
      type_error loc "expr should be a tactic"

let conv_expr loc = function
   ConvExpr t ->
      t
 | _ ->
      type_error loc "expr should be a conv"

let term_list_of_list loc = some_list_of_list (term_expr loc) loc
let int_list_of_list loc = some_list_of_list (int_expr loc) loc
let string_list_of_list loc = some_list_of_list (string_expr loc) loc
let tactic_list_of_list loc = some_list_of_list (tactic_expr loc) loc
let conv_list_of_list loc = some_list_of_list (conv_expr loc) loc

(*
 * Want an int tactic.
 *)
let int_tactic_expr loc = function
   IntFunExpr f ->
      (fun i ->
            match f i with
               TacticExpr tac ->
                  tac
             | _ ->
                  type_error loc "int tactic expected")
 | _ ->
      type_error loc "int tactic expected"

(*
 * Lookup a variable from the table.
 *)
let rec mk_var_expr base loc v =
   try snd (Hashtbl.find base v) with
      Not_found ->
         Stdpp.raise_with_loc loc (RefineError ("mk_var_expr", StringStringError ("undefined variable", v)))

and mk_proj_expr base loc expr =
   let rec search modname v = function
      (modname', expr) :: tl ->
         if modname' = modname then
            expr
         else
            search modname v tl
    | [] ->
         Stdpp.raise_with_loc loc (**)
            (RefineError ("mk_proj_expr",
                          StringStringError ("undefined variable", modname ^ "." ^ v)))
   in
   let lookup names v =
      match names with
         [modname] ->
            search modname v (Hashtbl.find_all base v)
       | _ ->
            Stdpp.raise_with_loc loc (**)
               (RefineError ("mk_proj_expr", StringError "nested modules are not implemented"))
   in
   let rec collect names expr =
      match expr with
         (<:expr< $uid: name$ . $e2$ >>) ->
            collect (name :: names) e2
       | (<:expr< $lid: v$ >>) ->
            lookup names v
       | _ ->
            not_supported loc "expr projection"
   in
      collect [] expr

(*
 * For an application, we lookup the function and try to
 * specialize the argument.
 *)
and mk_apply_expr base loc f a =
   let a = mk_expr base a in
      match mk_expr base f with
         FunExpr f ->
            f a
       | UnitFunExpr f ->
            begin
               match a with
                  UnitExpr () ->
                     f ()
                | _ ->
                     type_error loc "expr should be unit"
            end
       | BoolFunExpr f ->
            begin
               match a with
                  BoolExpr a ->
                     f a
                | _ ->
                     type_error loc "expr should be a bool"
            end
       | IntFunExpr f ->
            begin
               match a with
                  IntExpr a ->
                     f a
                | _ ->
                     type_error loc "expr should be int"
            end
       | StringFunExpr f ->
            begin
               match a with
                  StringExpr a ->
                     f a
                | _ ->
                     type_error loc "expr should be a string"
            end
       | TermFunExpr f ->
            begin
               match a with
                  TermExpr a ->
                     f a
                | _ ->
                     type_error loc "expr should be a term"
            end
       | TacticFunExpr f ->
            begin
               match a with
                  TacticExpr a ->
                     f a
                | _ ->
                     type_error loc "expr should be a tactic"
            end
       | IntTacticFunExpr f ->
            f (int_tactic_expr loc a)
       | ConvFunExpr f ->
            begin
               match a with
                  ConvExpr a ->
                     f a
                | _ ->
                     type_error loc "expr should be a conversion"
            end
       | AddressFunExpr f ->
            begin
               match a with
                  AddressExpr a ->
                     f a
                | ListExpr _ ->
                     f (make_address (int_list_of_list loc a))
                | _ ->
                     type_error loc "expr should be an address"
            end
       | AddrFunExpr f ->
            f (int_list_of_list loc a)
       | StringListFunExpr f ->
            f (string_list_of_list loc a)
       | TermListFunExpr f ->
            f (term_list_of_list loc a)
       | TacticListFunExpr f ->
            f (tactic_list_of_list loc a)
       | ConvListFunExpr f ->
            f (conv_list_of_list loc a)
       | UnitExpr _
       | BoolExpr _
       | IntExpr _
       | StringExpr _
       | TermExpr _
       | TacticExpr _
       | ConvExpr _
       | AddressExpr _
       | ListExpr _
       | TupleExpr _ ->
            type_error loc "expr should be a function"

(*
 * A tuple of expressions.
 * We only support unit for now.
 *)
and mk_tuple_expr base loc = function
   [] ->
      UnitExpr ()
 | _ ->
      not_supported loc "tuple expression"

and mk_expr base expr =
   let loc = loc_of_expr expr in
      match expr with
         (<:expr< $e1$ . $e2$ >> as expr) ->
            mk_proj_expr base loc expr
       | (<:expr< $e1$ $e2$ >>) ->
            mk_apply_expr base loc e1 e2
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
            mk_var_expr base loc s
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
            mk_tuple_expr base loc el
       | (<:expr< ( $e$ : $_$ ) >>) ->
            mk_expr base e
(* 3.02
       | MLast.ExXnd (_, _, e) ->
            mk_expr base e
 *)
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

and mk_patt base patt =
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
(* 3.02
       | MLast.PaXnd _ ->
            not_supported loc "patterm PaXnd"
 *)
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

and mk_type base t =
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

and mk_sig_item base si =
   let loc = loc_of_sig_item si in
      match si with
(*
         (<:sig_item< class $list:ctl$ >>) ->
*)
         MLast.SgCls _
       | MLast.SgClt _ ->
            not_supported loc "sig class"
       | (<:sig_item< declare $list:sil$ end >>) ->
            mk_sig_item base (List_util.last sil)
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

and mk_str_item base si =
   let loc = loc_of_str_item si in
      match si with
         MLast.StCls _
       | MLast.StClt _ ->
            not_supported loc "str class"
       | (<:str_item< declare $list:stl$ end >>) ->
            mk_str_item base (List_util.last stl)
       | (<:str_item< exception $s$ of $list:tl$ >>) ->
            not_supported loc "str exception"
       | (<:str_item< $exp:e$ >>) ->
            mk_expr base e
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

and mk_module_type base mt =
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

and mk_wc base = function
   WcTyp (loc, sl1, sl2, t) ->
      not_supported loc "with clause type"
 | WcMod (loc, sl1, mt) ->
      not_supported loc "with clause module"

and mk_module_expr base me =
   let loc = loc_of_module_expr me in
      match me with
         (<:module_expr< $me1$ . $me2$ >>) ->
            not_supported loc "module expr projection"
       | (<:module_expr< $me1$ $me2$ >>) ->
            not_supported loc "module expr application"
       | (<:module_expr< functor ( $s$ : $mt$ ) -> $me$ >>) ->
            not_supported loc "module expr functor"
       | (<:module_expr< struct $list:sil$ end >>) ->
            not_supported loc "module expr struct"
       | (<:module_expr< ( $me$ : $mt$) >>) ->
            not_supported loc "module expr type"
       | (<:module_expr< $uid:i$ >>) ->
            not_supported loc "module expr id"

(************************************************************************
 * RESOURCES                                                            *
 ************************************************************************)

(*
 * Include the common library functions.
 *)
let int_int_fun_int_expr f =
   IntFunExpr (fun i -> IntFunExpr (fun j -> IntExpr (f i j)))

let cons_expr =
   FunExpr (fun e1 ->
         FunExpr (fun e2 ->
               match e2 with
                  ListExpr e2 ->
                     ListExpr (e1 :: e2)
                | _ ->
                     raise (RefineError ("cons_expr", StringError "type mismatch"))))

let resource toploop +=
   ["Pervasives", "+",     int_int_fun_int_expr ( + );
    "Pervasives", "-",     int_int_fun_int_expr ( - );
    "Pervasives", "*",     int_int_fun_int_expr ( * );
    "Pervasives", "/",     int_int_fun_int_expr ( / );
    "Pervasives", "::",    cons_expr;
    "Pervasives", "()",    UnitExpr ();
    "Pervasives", "[]",    ListExpr [];
    "Pervasives", "True",  BoolExpr true;
    "Pervasives", "False", BoolExpr false]

let expr_of_ocaml_expr = mk_expr
let expr_of_ocaml_str_item = mk_str_item

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
