(*
 * Module for printing terms to an ML module.
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
 *
 *)

open Printf

open Nl_debug
open Opname
open Refiner.Refiner.Term
open Refiner.Refiner.TermMeta
open Ml_format_sig
open Ml_format
open Ml_print_sig

(*
 * Show the file loading.
 *)
let _ =
   if !debug_load then
      eprintf "Loading ML_print%t" eflush

module MakePrinter (File : FileSig) =
struct
   (************************************************************************
    * ML PRINTING                                                          *
    ************************************************************************)

   (*
    * Print an expression as a string.
    *)
   let print_ml_expr ofile expr =
      let rec print_var = function
         [h] ->
            File.put ofile h;
       | h::t ->
            File.put ofile h;
            File.put ofile ".";
            print_var t
       | [] ->
            raise (Invalid_argument "print_var")
      in
      let rec print = function
         ML_Var v ->
            File.put ofile (sprintf " %s" v)
       | ML_Int i ->
            File.put ofile (sprintf " %d" i)
       | ML_Num n ->
            File.put ofile
                     ("(Nl_num.num_of_string \"" ^ (Nl_num.string_of_num n) ^ "\")")
       | ML_String s ->
            File.put ofile (sprintf " \"%s\"" (String.escaped s))
       | ML_List l ->
            File.put ofile " [";
            print_list ";" l;
            File.put ofile "]"
       | ML_Let (v, expr, body) ->
            File.put ofile (sprintf "\nlet %s =" v);
            print expr;
            File.put ofile " in ";
            print body
       | ML_Apply l ->
            File.put ofile " (";
            print_list " " l;
            File.put ofile ")"
       | ML_Tuple l ->
            File.put ofile " (";
            print_list ", " l;
            File.put ofile ")"
       | ML_Record l ->
            File.put ofile " {";
            print_record l;
            File.put ofile "}"
       | ML_Module_Var l ->
            File.put ofile " ";
            print_var l
      and print_list sep = function
         [h] ->
            print h
       | h::t ->
            print h;
            File.put ofile sep;
            print_list sep t
       | [] ->
            ()
      and print_record = function
         [name, expr] ->
            print name;
            File.put ofile " =";
            print expr
       | (name, expr)::t ->
            print name;
            File.put ofile " =";
            print expr;
            File.put ofile "; ";
            print_record t
       | [] ->
            ()
      in
         print expr

   (************************************************************************
    * TYPES                                                                *
    ************************************************************************)

   (* Use the file types *)
   type t = File.t
   type out = File.out

   (************************************************************************
    * FILE INTERFACE                                                       *
    ************************************************************************)

   (*
    * Interface.
    *)
   let print_named_term ofile name term =
      (* Interface *)
      File.puti ofile (sprintf "val %s : term\n" name);

      (* Definition *)
      let expr = FormatTerm.format_term term in
         File.put ofile (sprintf "let %s =\n" name);
         print_ml_expr ofile expr

   (*
    * Interface.
    *)
   let print_term ofile term =
      print_ml_expr ofile (FormatTerm.format_term term)

   (*
    * Meta terms.
    *)
   let print_mterm ofile mterm =
      (* Print output *)
      print_ml_expr ofile (FormatTerm.format_mterm mterm)

   (*
    * List of terms.
    *)
   let print_term_list ofile l =
      print_ml_expr ofile (FormatTerm.format_term_list l)
end

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner.run"
 * End:
 * -*-
 *)
