(*
 * Built-in terms.
 * ----------------------------------------------------------------
 *
 * Copyright (C) 2002 Adam Granicz, Caltech
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
 * Author: Adam Granicz
 * Email: granicz@cs.caltech.edu
 *
 *)

(*
 * All the internal terms.
 * Invariant: the first element in this list must be
 * ("@", basic built-in terms).
 *)
let built_in_terms = [
   (*
    * Built-in terms.
    *)
   "@", [
      "__pos__";
      "__prod__";
      "__token__";
      "__EPSILON__";
      "union_pos";
      "var"
   ];
   (*
    * Other terms.
    *)
   "Phobos_base", [
      "true";
      "false";
      "eq";
      "lt";
      "le";
      "gt";
      "ge";
      "error";
      "print";
      "print_var";
      "print_term";
      "param_add_string"
   ];
   "Itt_int_base", [
      "number"
   ];
   "Itt_list", [
      "cons";
      "nil"
   ];
   "Itt_list2", [
      "is_nil";
      "append";
      "map";
      "length";
      "nth";
      "rev"
   ];
   (*
    * FC AST term set.
    *)
   "Fc_ast", [
      "list";
      "option";
      "Some";
      "None";
      "list_create";
      "list_create2";
      "list_create3";
      "list_create4";
      "list_create5";
      "list_append";
      "list_insert";
      "list_merge";
      "empty_list";
      "PreOp";
      "PostOp";
      "true";
      "false";
      "Int8";
      "Int16";
      "Int32";
      "Int64";
      "Single";
      "Double";
      "LongDouble";
      "int";
      "float";
      "rawint";
      "rawfloat";
      "enum_decl";
      "union_enum_decl";
      "field_decl";
      "patt_decl";
      "var_decl";
      "StatusConst";
      "StatusVolatile";
      "StatusNormal";
      "StoreAuto";
      "StoreRegister";
      "StoreStatic";
      "StoreExtern";
      "InitNone";
      "InitExpr";
      "TryNormal";
      "TryAtomic";
      "TypeUnit";
      "TypePoly";
      "TypeChar";
      "TypeInt";
      "TypeFloat";
      "TypeArray";
      "TypeConfArray";
      "TypePointer";
      "TypeRef";
      "TypeProduct";
      "TypeFun";
      "TypeVar";
      "TypeLambda";
      "TypeApply";
      "TypeElide";
      "ty";
      "CharPattern";
      "IntPattern";
      "FloatPattern";
      "StringPattern";
      "VarPattern";
      "StructPattern";
      "EnumPattern";
      "AsPattern";
      "pattern";
      "symbol";
      "expr";
      "UnitExpr";
      "CharExpr";
      "IntExpr";
      "FloatExpr";
      "StringExpr";
      "VarExpr";
      "OpExpr";
      "FunCallExpr";
      "SubscriptExpr";
      "ProjectExpr";
      "SizeofExpr";
      "SizeofType";
      "CastExpr";
      "IfExpr";
      "ForExpr";
      "WhileExpr";
      "DoExpr";
      "TryExpr";
      "SwitchExpr";
      "LabelExpr";
      "CaseExpr";
      "DefaultExpr";
      "ReturnExpr";
      "RaiseExpr";
      "BreakExpr";
      "ContinueExpr";
      "GotoExpr";
      "SeqExpr";
      "VarDefs";
      "FunDef"
   ]]


